;; a simple input method written as an emacs minor mode
;; (load-file "~/lisp/elisp/zero/zero-table.elc")

;;=======
;; utils
;;=======

;; this function is from ibus.el
(defun ibus-compute-pixel-position (&optional pos window)
  "Return geometry of object at POS in WINDOW as a list like \(X Y H).
X and Y are pixel coordinates relative to top left corner of frame which
WINDOW is in. H is the pixel height of the object.

Omitting POS and WINDOW means use current position and selected window,
respectively."
  (let* ((frame (window-frame (or window (selected-window))))
	 (posn (posn-at-point (or pos (window-point window)) window))
	 (line (cdr (posn-actual-col-row posn)))
	 (line-height (and line
			   (or (window-line-height line window)
			       (and (redisplay t)
				    (window-line-height line window)))))
	 (x-y (or (posn-x-y posn)
		  (let ((geom (pos-visible-in-window-p
			       (or pos (window-point window)) window t)))
		    (and geom (cons (car geom) (cadr geom))))
		  '(0 . 0)))
	 (ax (+ (car (window-inside-pixel-edges window))
		(car x-y)))
	 (ay (+ (cadr (window-pixel-edges window))
		(or (nth 2 line-height) (cdr x-y))))
	 (height (or (car line-height)
		     (with-current-buffer (window-buffer window)
		       (cond
			;; `posn-object-width-height' returns an incorrect value
			;; when the header line is displayed (Emacs bug #4426).
			((and posn
			      (null header-line-format))
			 (cdr (posn-object-width-height posn)))
			((and (bound-and-true-p text-scale-mode)
			      (not (zerop (with-no-warnings
					    text-scale-mode-amount))))
			 (round (* (frame-char-height frame)
				   (with-no-warnings
				     (expt text-scale-mode-step
					   text-scale-mode-amount)))))
			(t
			 (frame-char-height frame)))))))
    (list ax ay height)))

;;===============================
;; basic data and emacs facility
;;===============================

(defvar zero-table-table nil "zero-table's table, map string to string")
(defvar zero-table-sequence-initials nil "used in `zero-table-can-start-sequence'")

;;=====================
;; key logic functions
;;=====================

;; FSM state
(defconst *zero-table-state-im-off* 'IM-OFF)
(defconst *zero-table-state-im-waiting-input* 'IM-WAITING-INPUT)
(defconst *zero-table-state-im-preediting* 'IM-PREEDITING)

(defvar zero-table-state *zero-table-state-im-off*)
(defvar zero-table-preedit-str "")
(defvar zero-table-candidates nil)

(defun zero-table-debug (string &rest objects)
  (with-current-buffer (get-buffer-create "*zero-debug*")
    (insert (apply 'format string objects))
    (goto-char (point-max))))

;; (zero-table-debug "msg1\n")
;; (zero-table-debug "msg2: %s\n" "some obj")
;; (zero-table-debug "msg3: %s\n" 24)
;; (zero-table-debug "msg4: %s %s\n" 24 1)

(defun zero-table-set-state (state)
  "set state to given state"
  (zero-table-debug "set state to %s\n" state)
  (setq zero-table-state state))

(defun zero-table-show-candidates (candidates)
  "show candidates using zero-panel via IPC/RPC"
  ;; TODO
  (zero-table-debug "candidates: %s\n  " (s-join "\n  " candidates)))

(defun zero-table-sort-key (lhs rhs)
  "a predicate function to sort candidates. return t if lhs
should sort before rhs."
  (string< (car lhs) (car rhs)))

(defun zero-table-build-candidates (preedit-str)
  (mapcar 'cdr (sort (cl-remove-if-not (lambda (pair) (string-prefix-p preedit-str (car pair))) zero-table-table) 'zero-table-sort-key)))

(ert-deftest zero-table-build-candidates-async ()
  (should (equal (zero-table-build-candidates "ph") '("18612345678")))
  (should (equal (zero-table-build-candidates "m") '("https://msdn.microsoft.com/en-us"
						     "foo@example.com"
						     "https://ditu.amap.com/"))))

(defun zero-table-build-candidates-async (preedit-str)
  "build candidate list, when done show it via `zero-table-show-candidates'"
  (zero-table-debug "building candidate list\n")
  (let ((candidates (zero-table-build-candidates preedit-str)))
    ;; update cache to make SPC and digit key selection possible.
    (setq zero-table-candidates candidates)
    (zero-table-show-candidates candidates)))

(defun zero-table-handle-punctuation (n)
  "if n is a punctuation character, insert Chinese punctuation for it and return true, otherwise, return false."
  (case n
    (?, (insert "，") t)
    (?. (insert "。") t)
    (?? (insert "？") t)
    (?! (insert "！") t)
    (?\ (insert "、") t)
    (otherwise nil)))

(defun zero-table-append-char-to-preedit-str (ch)
  "append char ch to preedit str, update and show candidate list"
  (setq zero-table-preedit-str
	(concat zero-table-preedit-str (make-string 1 ch)))
  (zero-table-debug "appended %c, preedit str is: %s\n" ch zero-table-preedit-str)
  (zero-table-preedit-str-changed))

(defun zero-table-can-start-sequence (ch)
  "return t if char ch can start a preedit sequence."
  (member (make-string 1 ch) zero-table-sequence-initials))

(ert-deftest zero-table-can-start-sequence ()
  (should (zero-table-can-start-sequence ?a))
  (should (zero-table-can-start-sequence ?m))
  (should-not (zero-table-can-start-sequence ?1))
  (should-not (zero-table-can-start-sequence ?b)))

(defun zero-table-self-insert-command (n)
  "handle character self-insert-command. This includes characters and digits"
  (interactive "p")
  (let ((ch (elt (this-command-keys-vector) 0)))
    (zero-table-debug "user typed: %c\n" ch)
    (cond
     ((eq zero-table-state *zero-table-state-im-waiting-input*)
      (if (zero-table-can-start-sequence ch)
	  (progn
	    (zero-table-debug "can start sequence, state=IM_PREEDITING\n")
	    (zero-table-set-state *zero-table-state-im-preediting*)
	    (zero-table-append-char-to-preedit-str ch))
	(zero-table-debug "cannot start sequence, state=IM_WAITING_INPUT\n")
	(unless (zero-table-handle-punctuation ch)
	  (self-insert-command n))))
     ((eq zero-table-state *zero-table-state-im-preediting*)
      (zero-table-debug "still preediting\n")
      (if (and (>= ch ?0) (<= ch ?9))
	  (unless (zero-table-commit-nth-candidate (- ch ?0))
	    (zero-table-append-char-to-preedit-str ch))
	(zero-table-append-char-to-preedit-str ch)))
     (t
      (zero-table-debug "unexpected state: %s\n" zero-table-state)
      (self-insert-command n)))))

(defun zero-table-preedit-str-changed ()
  "called when preedit str is changed and not empty. update and show candidate list"
  (zero-table-build-candidates-async zero-table-preedit-str))

(defun zero-table-delete-backward-char (n)
  "handle backspace key"
  (interactive "p")
  (unless (integerp n)
    (signal 'wrong-type-argument (list 'integerp n)))
  (if (eq zero-table-state *zero-table-state-im-preediting*)
      (let ((len (length zero-table-preedit-str)))
	(if (> len 1)
	    (progn
	      (setq zero-table-preedit-str
		    (substring zero-table-preedit-str 0 (1- len)))
	      (zero-table-preedit-str-changed))
	  (zero-table-set-state *zero-table-state-im-waiting-input*)
	  (zero-table-reset)))
    (delete-char (- n))))

(defun zero-table-commit-text (text)
  "commit given text, reset preedit str, hide candidate list"
  (zero-table-debug "commit text: %s\n" text)
  (insert text)
  (setq zero-table-preedit-str "")
  (setq zero-table-candidates nil)
  (zero-table-hide-candidate-list))

(defun zero-table-return ()
  "handle RET key press"
  (interactive)
  (if (eq zero-table-state *zero-table-state-im-preediting*)
      (progn
	(zero-table-set-state *zero-table-state-im-waiting-input*)
	(zero-table-commit-text zero-table-preedit-str))
    (newline-and-indent)))

(defun zero-table-commit-nth-candidate (n)
  "commit nth candidate and return true if it exists, otherwise, return false"
  (let ((candidate (nth n zero-table-candidates)))
    (if candidate
	(progn
	  (zero-table-set-state *zero-table-state-im-waiting-input*)
	  (zero-table-commit-text candidate)
	  t)
      nil)))

(defun zero-table-commit-preedit-str ()
  (zero-table-set-state *zero-table-state-im-waiting-input*)
  (zero-table-commit-text zero-table-preedit-str))

(defun zero-table-space (n)
  "handle SPC key press"
  (interactive "p")
  (if (eq zero-table-state *zero-table-state-im-preediting*)
      ;; commit first candidate if there is one, otherwise commit preedit str
      (unless (zero-table-commit-nth-candidate 0)
	(zero-table-commit-preedit-str))
    (self-insert-command n)))

(defun zero-table-hide-candidate-list ()
  ;; TODO do IPC/RPC call to hide candidate
  (zero-table-debug "hide candidate list\n")
  )

(defun zero-table-reset ()
  (zero-table-debug "reset\n")
  (setq zero-table-preedit-str "")
  (zero-table-hide-candidate-list))

;;============
;; minor mode
;;============

(defvar zero-table-mode-map nil "zero-table-mode keymap")
(setq zero-table-mode-map
      (list 'keymap
	    '(13 . zero-table-return)
	    '(32 . zero-table-space)
	    '(remap keymap
		    (self-insert-command . zero-table-self-insert-command)
		    ;; (forward-char . zero-table-forward-char)
		    ;; (backward-char . zero-table-backward-char)
		    ;; (forward-word . zero-table-forward-word)
		    ;; (backward-word . zero-table-backward-word)
		    (delete-backward-char . zero-table-delete-backward-char)
		    ;; (delete-char . zero-table-delete-char)
		    )))

(define-minor-mode zero-table-mode
  "a simple input method written as an emacs minor mode"
  nil
  "Table"
  zero-table-mode-map
  (set (make-local-variable 'zero-table-state) *zero-table-state-im-off*)
  (set (make-local-variable 'zero-table-preedit-str) "")
  (set (make-local-variable 'zero-table-candidates) nil))

;;============
;; public API
;;============

(defun zero-table-set-table (alist)
  "set the conversion table.

the alist should be a list of (key . value) pairs. when user type
(part of) key, the IM will show all matching value.

e.g.
'((\"phone\" . \"18612345678\")
  (\"mail\" . \"foo@example.com\")
  (\"map\" . \"https://ditu.amap.com/\")
  (\"m\" . \"https://msdn.microsoft.com/en-us\")
  (\"address\" . \"123 Happy Street\"))
"
  (setq zero-table-table alist)
  (setq zero-table-sequence-initials
	(delete-dups (mapcar (lambda (pair) (substring (car pair) 0 1))
			     zero-table-table))))

(defun zero-table-on ()
  (interactive)
  (zero-table-debug "zero-table-on\n")
  (zero-table-mode 1)
  (if (eq zero-table-state *zero-table-state-im-off*)
      (zero-table-set-state *zero-table-state-im-waiting-input*)))

(defun zero-table-off ()
  (interactive)
  (zero-table-debug "zero-table-off\n")
  (zero-table-mode -1)
  (zero-table-reset)
  (zero-table-set-state *zero-table-state-im-off*))

(defun zero-table-toggle ()
  (interactive)
  (if zero-table-mode
      (zero-table-off)
    (zero-table-on)))

;;===========
;; test data
;;===========

(unless zero-table-table
  (zero-table-set-table
   '(("phone" . "18612345678")
     ("pyl" . "http://localdocs.emacsos.com/python2/library/%s.html")
     ("pyli" . "http://localdocs.emacsos.com/python2/index.html")
     ("pylm" . "http://localdocs.emacsos.com/python2/py-modindex.html")
     ("py3li" . "http://localdocs.emacsos.com/python2/index.html")
     ("py3l" . "http://localdocs.emacsos.com/python3/library/%s.html")
     ("py3lm" . "http://localdocs.emacsos.com/python3/py-modindex.html")
     ("pyop" . "http://docs.python.org/library/operator.html")
     ("pyopl" . "http://localdocs.emacsos.com/python2/library/operator.html")
     ("pympl" . "http://localdocs.emacsos.com/python2/library/multiprocessing.html")
     ("py2" . "http://docs.python.org/2/library/%s.html")
     ("py3" . "http://docs.python.org/3/library/%s.html")
     ("py2i" . "http://docs.python.org/2/")
     ("py2m" . "http://docs.python.org/2/py-modindex.html")
     ("py3i" . "http://docs.python.org/3/")
     ("py3m" . "http://docs.python.org/3/py-modindex.html")
     ("pycodec" . "http://localdocs.emacsos.com/python2/library/codecs.html#standard-encodings")
     ("pycodecs" . "http://localdocs.emacsos.com/python2/library/codecs.html#standard-encodings")
     ("pycodecsr" . "http://docs.python.org/library/codecs.html#standard-encodings")
     ("pycodecr" . "http://docs.python.org/library/codecs.html#standard-encodings")
     ("pep328" . "http://www.python.org/dev/peps/pep-0328/")
     ("mail" . "foo@example.com")
     ("map" . "https://ditu.amap.com/")
     ("m" . "https://msdn.microsoft.com/en-us")
     ("address" . "123 Happy Street")
     ("da" . "__da__")
     ("now" . "__now__"))))

(provide 'zero-table)
