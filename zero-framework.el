;; the zero Chinese input method framework for emacs.
;; implemented as a minor mode.
;; (load-file "~/lisp/elisp/zero/zero-panel.elc")
;; (load-file "~/lisp/elisp/zero/zero-framework.elc")

;;==============
;; dependencies
;;==============

(require 's)
(require 'zero-panel)

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

(defun zero-get-point-position ()
  "return current point's position (x y) based on origin of screen top left corner"
  (destructuring-bind (x y h) (ibus-compute-pixel-position)
    (list (+ (frame-parameter nil 'left) x)
	  (+ (frame-parameter nil 'top) h y))))

;;=====================
;; key logic functions
;;=====================

;; FSM state
(defconst *zero-state-im-off* 'IM-OFF)
(defconst *zero-state-im-waiting-input* 'IM-WAITING-INPUT)
(defconst *zero-state-im-preediting* 'IM-PREEDITING)

;;; concrete input method should define these functions and set them in the
;;; corresponding *-func variable.
(defun zero-build-candidates-default (preedit-str)
  nil)
(defun zero-can-start-sequence-default (ch)
  nil)
(defvar zero-build-candidates-func 'zero-build-candidates-default
  "contains a function to build candidates from preedit-str")
(defvar zero-can-start-sequence-func 'zero-can-start-sequence-default
  "contains a function to decide whether a char can start a preedit sequence")
;;; TODO provide hook functions. more complex IM may need to do clean up on these.
;; (defvar zero-on-hook nil)
;; (defvar zero-off-hook nil)

(defvar zero-im nil
  "current input method. if nil, the empty input method will be used.
in the empty input method, only punctuation is handled. Other keys are pass through")
(defvar zero-ims nil
  "a list of registered input methods")

(defvar zero-buffer nil
  "stores the associated buffer.
this is used to help with buffer focus in/out events")

(defvar zero-state *zero-state-im-off*)
(defvar zero-preedit-str "")
(defvar zero-candidates nil)

(defvar zero-enable-debug t
  "whether to enable debug.
if t, `zero-debug' will output debug msg in *zero-debug* buffer")

(defun zero-debug (string &rest objects)
  "log debug message in *zero-debug* buffer"
  (if zero-enable-debug
      (with-current-buffer (get-buffer-create "*zero-debug*")
	(insert (apply 'format string objects))
	(goto-char (point-max)))))

;; (zero-debug "msg1\n")
;; (zero-debug "msg2: %s\n" "some obj")
;; (zero-debug "msg3: %s\n" 24)
;; (zero-debug "msg4: %s %s\n" 24 1)

(defun zero-set-state (state)
  "set state to given state"
  (zero-debug "set state to %s\n" state)
  (setq zero-state state))

(defun zero-show-candidates (candidates)
  "show candidates using zero-panel via IPC/RPC"
  (zero-panel-show-candidates zero-preedit-str (length candidates) candidates)
  (zero-debug "candidates: %s\n  " (s-join "\n  " candidates))
  (destructuring-bind (x y) (zero-get-point-position)
    (zero-panel-move x y)))

(defun zero-build-candidates (preedit-str)
  "build candidates list synchronously"
  (if (functionp zero-build-candidates-func)
      (funcall zero-build-candidates-func preedit-str)
    (error "`zero-build-candidates-func' is not a function")))

(defun zero-build-candidates-async (preedit-str)
  "build candidate list, when done show it via `zero-show-candidates'"
  (zero-debug "building candidate list\n")
  (let ((candidates (zero-build-candidates preedit-str)))
    ;; update cache to make SPC and digit key selection possible.
    (setq zero-candidates candidates)
    (zero-show-candidates candidates)))

(defun zero-handle-punctuation (n)
  "if n is a punctuation character, insert Chinese punctuation for it and return true, otherwise, return false."
  (case n
    (?, (insert "，") t)
    (?. (insert "。") t)
    (?? (insert "？") t)
    (?! (insert "！") t)
    (?\ (insert "、") t)
    (otherwise nil)))

(defun zero-append-char-to-preedit-str (ch)
  "append char ch to preedit str, update and show candidate list"
  (setq zero-preedit-str
	(concat zero-preedit-str (make-string 1 ch)))
  (zero-debug "appended %c, preedit str is: %s\n" ch zero-preedit-str)
  (zero-preedit-str-changed))

(defun zero-can-start-sequence (ch)
  "return t if char ch can start a preedit sequence."
  (if (functionp zero-can-start-sequence-func)
      (funcall zero-can-start-sequence-func ch)
    (error "`zero-can-start-sequence-func' is not a function")))

(ert-deftest zero-can-start-sequence ()
  (should (zero-can-start-sequence ?a))
  (should (zero-can-start-sequence ?m))
  (should-not (zero-can-start-sequence ?1))
  (should-not (zero-can-start-sequence ?b)))

(defun zero-self-insert-command (n)
  "handle character self-insert-command. This includes characters and digits"
  (interactive "p")
  (let ((ch (elt (this-command-keys-vector) 0)))
    (zero-debug "user typed: %c\n" ch)
    (cond
     ((eq zero-state *zero-state-im-waiting-input*)
      (if (zero-can-start-sequence ch)
	  (progn
	    (zero-debug "can start sequence, state=IM_PREEDITING\n")
	    (zero-set-state *zero-state-im-preediting*)
	    (zero-append-char-to-preedit-str ch))
	(zero-debug "cannot start sequence, state=IM_WAITING_INPUT\n")
	(unless (zero-handle-punctuation ch)
	  (self-insert-command n))))
     ((eq zero-state *zero-state-im-preediting*)
      (zero-debug "still preediting\n")
      (if (and (>= ch ?0) (<= ch ?9))
	  ;; 1 commit the 0th candidate
	  ;; 2 commit the 1st candidate
	  ;; ...
	  ;; 0 commit the 9th candidate
	  (unless (zero-commit-nth-candidate (mod (- (- ch ?0) 1) 10))
	    (zero-append-char-to-preedit-str ch))
	(zero-append-char-to-preedit-str ch)))
     (t
      (zero-debug "unexpected state: %s\n" zero-state)
      (self-insert-command n)))))

(defun zero-preedit-str-changed ()
  "called when preedit str is changed and not empty. update and show candidate list"
  (zero-build-candidates-async zero-preedit-str))

(defun zero-delete-backward-char (n)
  "handle backspace key"
  (interactive "p")
  (unless (integerp n)
    (signal 'wrong-type-argument (list 'integerp n)))
  (if (eq zero-state *zero-state-im-preediting*)
      (let ((len (length zero-preedit-str)))
	(if (> len 1)
	    (progn
	      (setq zero-preedit-str
		    (substring zero-preedit-str 0 (1- len)))
	      (zero-preedit-str-changed))
	  (zero-set-state *zero-state-im-waiting-input*)
	  (zero-reset)))
    (delete-char (- n))))

(defun zero-commit-text (text)
  "commit given text, reset preedit str, hide candidate list"
  (zero-debug "commit text: %s\n" text)
  (insert text)
  (setq zero-preedit-str "")
  (setq zero-candidates nil)
  (zero-hide-candidate-list))

(defun zero-return ()
  "handle RET key press"
  (interactive)
  (if (eq zero-state *zero-state-im-preediting*)
      (progn
	(zero-set-state *zero-state-im-waiting-input*)
	(zero-commit-text zero-preedit-str))
    (newline-and-indent)))

(defun zero-commit-nth-candidate (n)
  "commit nth candidate and return true if it exists, otherwise, return false"
  (let ((candidate (nth n zero-candidates)))
    (if candidate
	(progn
	  (zero-set-state *zero-state-im-waiting-input*)
	  (zero-commit-text candidate)
	  t)
      nil)))

(defun zero-commit-preedit-str ()
  (zero-set-state *zero-state-im-waiting-input*)
  (zero-commit-text zero-preedit-str))

(defun zero-space (n)
  "handle SPC key press"
  (interactive "p")
  (if (eq zero-state *zero-state-im-preediting*)
      ;; commit first candidate if there is one, otherwise commit preedit str
      (unless (zero-commit-nth-candidate 0)
	(zero-commit-preedit-str))
    (self-insert-command n)))

(defun zero-hide-candidate-list ()
  (zero-panel-hide)
  (zero-debug "hide candidate list\n"))

(defun zero-reset ()
  (zero-debug "reset\n")
  (setq zero-preedit-str "")
  (zero-hide-candidate-list))

(defun zero-focus-in ()
  "a hook function. run when focus in a zero-mode buffer"
  (if (eq zero-state *zero-state-im-preediting*)
      (zero-show-candidates zero-candidates)))

(defun zero-focus-out ()
  "a hook function. run when focus out a zero-mode buffer"
  (if (eq zero-state *zero-state-im-preediting*)
      (zero-hide-candidate-list)))

(defun zero-buffer-list-changed ()
  "a hook function, run when buffer list has changed. This includes user has switched buffer"
  (if (eq (car (buffer-list)) zero-buffer)
      (zero-focus-in)))

;;============
;; minor mode
;;============

;; TODO when zero-framework is stable, move default value to defvar.
;; this will allow user to customize the keymap.
(defvar zero-mode-map nil "zero-mode keymap")
(setq zero-mode-map
      (list 'keymap
	    '(13 . zero-return)
	    '(32 . zero-space)
	    '(remap keymap
		    (self-insert-command . zero-self-insert-command)
		    ;; (forward-char . zero-forward-char)
		    ;; (backward-char . zero-backward-char)
		    ;; (forward-word . zero-forward-word)
		    ;; (backward-word . zero-backward-word)
		    (delete-backward-char . zero-delete-backward-char)
		    ;; (delete-char . zero-delete-char)
		    )))

(define-minor-mode zero-mode
  "a simple input method written as an emacs minor mode"
  nil
  " Zero"
  zero-mode-map
  (set (make-local-variable 'zero-state) *zero-state-im-off*)
  (set (make-local-variable 'zero-preedit-str) "")
  (set (make-local-variable 'zero-candidates) nil)
  (make-local-variable 'zero-im)
  (make-local-variable 'zero-build-candidates-func)
  (make-local-variable 'zero-can-start-sequence-func)
  (zero-set-im zero-im)
  (add-hook 'focus-in-hook 'zero-focus-in)
  (add-hook 'focus-out-hook 'zero-focus-out)
  (set (make-local-variable 'zero-buffer) (current-buffer))
  (add-hook 'buffer-list-update-hook 'zero-buffer-list-changed))

;;==================
;; IM developer API
;;==================

(defun zero-register-im (im-name im-functions-alist)
  "(re)register an input method in zero. After registration, you can use `zero-set-default-im' and `zero-set-im' to select input method to use.

im-name should be a symbol.
im-functions-alist should be a list of form
  '((:build-candidates . build-candidates-func)
    (:can-start-sequence . can-start-sequence-func))

registered input method is saved in `zero-ims'"
  ;; add or replace entry in `zero-ims'
  (unless (symbolp im-name)
    (signal 'wrong-type-argument (list 'symbolp im-name)))
  (setq zero-ims (assq-delete-all im-name zero-ims))
  (setq zero-ims (push (cons im-name im-functions-alist) zero-ims)))

;;============
;; public API
;;============

(defun zero-set-im (im-name)
  "select zero input method for current buffer.

if im-name is nil, use default empty input method"
  ;; TODO provide completion
  (interactive "SSet input method to: ")
  (if im-name
      (let ((im-functions (cdr (assq im-name zero-ims))))
	(if im-functions
	    (progn
	      (setq zero-build-candidates-func
		    (or (cdr (assq :build-candidates im-functions))
			'zero-build-candidates-default))
	      (setq zero-can-start-sequence-func
		    (or (cdr (assq :can-start-sequence im-functions))
			'zero-can-start-sequence-default))
	      (set (make-local-variable 'zero-im) im-name))
	  (error "input method %s not registered in zero" im-name)))
    (zero-debug "using default empty input method")
    (setq zero-build-candidates-func 'zero-build-candidates-default)
    (setq zero-can-start-sequence-func 'zero-can-start-sequence-default)))

(defun zero-set-default-im (im-name)
  "set given im as default zero input method"
  (unless (symbolp im-name)
    (signal 'wrong-type-argument (list 'symbolp im-name)))
  (setq-default zero-im im-name))

(defun zero-on ()
  (interactive)
  (zero-debug "zero-on\n")
  (zero-mode 1)
  (if (eq zero-state *zero-state-im-off*)
      (zero-set-state *zero-state-im-waiting-input*)))

(defun zero-off ()
  (interactive)
  (zero-debug "zero-off\n")
  (zero-mode -1)
  (zero-reset)
  (zero-set-state *zero-state-im-off*))

(defun zero-toggle ()
  (interactive)
  (if zero-mode
      (zero-off)
    (zero-on)))

(provide 'zero-framework)
