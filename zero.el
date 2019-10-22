;;; zero.el --- Zero Chinese input method framework -*- lexical-binding: t -*-

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;; Version: 1.3.1
;; URL: https://gitlab.emacsos.com/sylecn/zero-el
;; Package-Version: 1.3.1
;; Package-Requires: ((emacs "24.3") (s "1.2.0"))

;;; Commentary:

;; zero.el is auto-generated from multiple other files.  see zero.el.in and
;; build.py for details.  It's created because package-lint doesn't support
;; multi-file package yet (issue #111).
;;
;; zero is a Chinese input method framework for Emacs, implemented
;; as an Emacs minor mode.
;;
;; zero-pinyin is bundled with zero, to use pinyin input method, add to
;; ~/.emacs file:
;;
;;   (require 'zero-pinyin)
;;   (zero-set-default-im 'pinyin)
;;   ;; Now you may bind a key to zero-toggle to make it easy to
;;   ;; switch on/off the input method.
;;   (global-set-key (kbd "<f5>") 'zero-toggle)
;;
;; zero supports Chinese punctuation mapping.  There are three modes, none,
;; basic, and full.  The default is basic mode, which only map most essential
;; punctuations.  You can cycle zero-punctuation-level in current buffer by
;; C-c , , You can change default Chinese punctuation level:
;;
;;   (setq-default zero-punctuation-level *zero-punctuation-level-full*)
;;
;; zero supports full-width mode.  You can toggle full-width mode in current
;; buffer by C-c , . You can enable full-width mode by default:
;;
;;   (setq-default zero-full-width-mode t)
;;

;;; Code:

(require 'dbus)
(eval-when-compile (require 'cl-lib))
(require 's)

;; body of zero-panel.el

;;================
;; implementation
;;================


(defun zero-panel-error-handler (event error)
  "Handle dbus errors.

EVENT and ERROR are error-handler arguments."
  (when (or (string-equal "com.emacsos.zero.Panel"
			  (dbus-event-interface-name event))
	    (s-contains-p "com.emacsos.zero.Panel" (cadr error)))
    (error "Zero-panel dbus failed: %S" (cadr error))))

(add-hook 'dbus-event-error-functions 'zero-panel-error-handler)

(defun zero-panel-async-call (method _handler &rest args)
  "Call METHOD on zero-panel service asynchronously.

This is a wrapper around `dbus-call-method-asynchronously'.
ARGS optional extra args to pass to the wrapped function."
  (apply 'dbus-call-method-asynchronously
	 :session
	 "com.emacsos.zero.Panel1"	; well known name
	 "/com/emacsos/zero/Panel1"	; object path
	 "com.emacsos.zero.Panel1.PanelInterface" ; interface name
	 method nil :timeout 500 args))

;;=========================
;; public utility function
;;=========================

(defun zero-alist-to-asv (hints)
  "Convert Lisp alist to dbus a{sv} data structure.

HINTS should be an alist of form '((k1 [v1type] v1) (k2 [v2type] v2)).

For example,
\(zero-alist-to-asv
  '((\"name\" \"foo\")
    (\"timeout\" :int32 10)))
=>
'(:array
  (:dict-entry \"name\" (:variant \"foo\"))
  (:dict-entry \"timeout\" (:variant :int32 10)))"
  (if (null hints)
      '(:array :signature "{sv}")
    (let ((result '(:array)))
      (dolist (item hints)
	(push (list :dict-entry (car item) (cons :variant (cdr item))) result))
      (reverse result))))

;;============
;; public API
;;============

(defun zero-panel-move (x y)
  "Move panel to specific coordinate (X, Y).
Origin (0, 0) is at screen top left corner."
  (zero-panel-async-call "Move" nil :int32 x :int32 y))

(defun zero-panel-show-candidates (preedit_str candidate_length candidates &optional hints)
  "Show CANDIDATES.
Argument PREEDIT_STR the preedit string.
Argument CANDIDATE_LENGTH how many candidates are in candidates list."
  (zero-panel-async-call "ShowCandidates" nil
			 :string preedit_str
			 :uint32 candidate_length
			 (or candidates '(:array))
			 (zero-alist-to-asv hints)))

(defun zero-panel-show ()
  "Show panel."
  (zero-panel-async-call "Show" nil))

(defun zero-panel-hide ()
  "Hide panel."
  (zero-panel-async-call "Hide" nil))

(defun zero-panel-quit ()
  "Quit panel application."
  (interactive)
  (zero-panel-async-call "Quit" nil))

(provide 'zero-panel)

;; body of zero-framework.el

;;==============
;; dependencies
;;==============


;;=======
;; utils
;;=======

;; this function is from ibus.el
(defun zero--ibus-compute-pixel-position (&optional pos window)
  "Return geometry of object at POS in WINDOW as a list like \(X Y H).
X and Y are pixel coordinates relative to top left corner of frame which
WINDOW is in.  H is the pixel height of the object.

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
  "Return current point's position (x y).
Origin (0, 0) is at screen top left corner."
  (cl-destructuring-bind (x y line-height) (zero--ibus-compute-pixel-position)
    (cond
     ((functionp 'window-absolute-pixel-position)
      ;; introduced in emacs 26
      (cl-destructuring-bind (x . y) (window-absolute-pixel-position)
	(list x (+ y line-height))))
     ((functionp 'frame-edges)
      ;; introduced in emacs 25
      (cl-destructuring-bind (frame-x frame-y &rest rest)
	  (frame-edges nil 'inner-edges)
	(list (+ frame-x x) (+ frame-y y line-height))))
     (t
      ;; <= emacs 24, used guessed pixel size for tool-bar, menu-bar, WM title
      ;; bar. Since I can't get that from elisp.
      (list (+ (frame-parameter nil 'left)
	       (if (and (> (frame-parameter nil 'tool-bar-lines) 0)
			(eq (frame-parameter nil 'tool-bar-position) 'left))
		   96 0)
	       x)
	    (+ (frame-parameter nil 'top)
	       (if (and (> (frame-parameter nil 'tool-bar-lines) 0)
			(eq (frame-parameter nil 'tool-bar-position) 'top))
		   42 0)
	       (if (> (frame-parameter nil 'menu-bar-lines) 0) (+ 30 30) 0)
	       line-height
	       y))))))

(defun zero-cycle-list (lst item)
  "Return the object next to given ITEM in LST.

If item is the last object, return the first object in lst.
If item is not in lst, return nil."
  (let ((r (member item lst)))
    (cond
     ((null r) nil)
     (t (or (cadr r)
	    (car lst))))))

;;=====================
;; key logic functions
;;=====================

;; zero-el version
(defvar zero-version nil "Zero package version.")
(setq zero-version "1.3.1")

;; FSM state
(defconst zero--state-im-off 'IM-OFF)
(defconst zero--state-im-waiting-input 'IM-WAITING-INPUT)
(defconst zero--state-im-preediting 'IM-PREEDITING)

(defconst zero-punctuation-level-basic 'BASIC)
(defconst zero-punctuation-level-full 'FULL)
(defconst zero-punctuation-level-none 'NONE)

(defvar zero-im nil
  "Stores current input method.

If nil, the empty input method will be used.  In the empty input
method, only punctuation is handled.  Other keys are pass
through")
(defvar zero-ims nil
  "A list of registered input methods.")

(defvar zero-buffer nil
  "Stores the associated buffer.
this is used to help with buffer focus in/out events")

(defvar zero-state zero--state-im-off)
(defvar zero-full-width-mode nil
  "Set to t to enable full-width mode.
In full-width mode, commit ascii char will insert full-width char if there is a
corresponding full-width char.  This full-width char map is
independent from punctuation map.  You can change this via
`zero-toggle-full-width-mode'")
(defvar zero-punctuation-level zero-punctuation-level-basic
  "Punctuation level.

Should be one of
`zero-punctuation-level-basic'
`zero-punctuation-level-full'
`zero-punctuation-level-none'")
(defvar zero-punctuation-levels (list zero-punctuation-level-basic
				      zero-punctuation-level-full
				      zero-punctuation-level-none)
  "Punctuation levels to use when `zero-cycle-punctuation-level'.")
(defvar zero-double-quote-flag nil
  "Non-nil means next double quote insert close quote.

Used when converting double quote to Chinese quote.
If nil, next double quote insert open quote.
Otherwise, next double quote insert close quote.")
(defvar zero-single-quote-flag nil
  "Non-nil means next single quote insert close quote.

Used when converting single quote to Chinese quote.
If nil, next single quote insert open quote.
Otherwise, next single quote insert close quote.")
(defvar zero-preedit-str "")
(defvar zero-candidates nil)
(defcustom zero-candidates-per-page 10
  "How many candidates to show on each page."
  :group 'zero
  :type 'integer)
(defvar zero-current-page 0 "Current page number.  count from 0.")
(defvar zero-initial-fetch-size 20
  "How many candidates to fetch for the first call to GetCandidates.")
;; zero-fetch-size is reset to 0 when preedit-str changes.
;; zero-fetch-size is set to fetch-size in build-candidates-async complete-func
;; lambda.
(defvar zero-fetch-size 0 "Last GetCandidates call's fetch-size.")
(defvar zero-previous-page-key ?\- "Previous page key.")
(defvar zero-next-page-key ?\= "Next page key.")

;;; concrete input method should define these functions and set them in the
;;; corresponding *-func variable.
(defun zero-build-candidates-default (_preedit-str _fetch-size)
  "Default implementation for `zero-build-candidates-func'."
  nil)
(defun zero-can-start-sequence-default (_ch)
  "Default implementation for `zero-can-start-sequence-func'."
  nil)
(defun zero-get-preedit-str-for-panel-default ()
  "Default implementation for `zero-get-preedit-str-for-panel-func'."
  zero-preedit-str)
(defvar zero-build-candidates-func 'zero-build-candidates-default
  "Contains a function to build candidates from preedit-str.  The function accepts param preedit-str, fetch-size, returns candidate list.")
(defvar zero-build-candidates-async-func 'zero-build-candidates-async-default
  "Contains a function to build candidates from preedit-str.  The function accepts param preedit-str, fetch-size, and a complete-func that should be called on returned candidate list.")
(defvar zero-can-start-sequence-func 'zero-can-start-sequence-default
  "Contains a function to decide whether a char can start a preedit sequence.")
(defvar zero-handle-preedit-char-func 'zero-handle-preedit-char-default
  "Contains a function to handle IM-PREEDITING state char insert.
The function should return t if char is handled.
This allow input method to override default logic.")
(defvar zero-get-preedit-str-for-panel-func 'zero-get-preedit-str-for-panel-default
  "Contains a function that return preedit-str to show in zero-panel.")
(defvar zero-backspace-func 'zero-backspace-default
  "Contains a function to handle <backward> char.")
(defvar zero-handle-preedit-char-func 'zero-handle-preedit-char-default
  "Hanlde character insert in `zero--state-im-preediting' mode.")
(defvar zero-preedit-start-func 'nil
  "Called when enter `zero--state-im-preediting' state.")
(defvar zero-preedit-end-func 'nil
  "Called when leave `zero--state-im-preediting' state.")

(defvar zero-enable-debug nil
  "Whether to enable debug.
if t, `zero-debug' will output debug msg in *zero-debug* buffer")
(defvar zero-debug-buffer-max-size 30000
  "Max characters in *zero-debug* buffer.  If reached, first half data will be deleted.")

(defun zero-debug (string &rest objects)
  "Log debug message in *zero-debug* buffer.

STRING and OBJECTS are passed to `format'"
  (if zero-enable-debug
      (with-current-buffer (get-buffer-create "*zero-debug*")
	(goto-char (point-max))
	(insert (apply 'format string objects))
	(when (> (point) zero-debug-buffer-max-size)
	  (insert "removing old data\n")
	  (delete-region (point-min) (/ zero-debug-buffer-max-size 2))))))

;; (zero-debug "msg1\n")
;; (zero-debug "msg2: %s\n" "some obj")
;; (zero-debug "msg3: %s\n" 24)
;; (zero-debug "msg4: %s %s\n" 24 1)

(defun zero-enter-preedit-state ()
  "Config keymap when enter preedit state."
  (zero-enable-preediting-map)
  (if (functionp zero-preedit-start-func)
      (funcall zero-preedit-start-func)))

(defun zero-leave-preedit-state ()
  "Config keymap when leave preedit state."
  (zero-disable-preediting-map)
  (if (functionp zero-preedit-end-func)
      (funcall zero-preedit-end-func)))

(defun zero-set-state (state)
  "Set zero state to given STATE."
  (zero-debug "set state to %s\n" state)
  (setq zero-state state)
  (if (eq state zero--state-im-preediting)
      (zero-enter-preedit-state)
    (zero-leave-preedit-state)))

(defun zero-candidates-on-page (candidates)
  "Return candidates on current page for given CANDIDATES list."
  (cl-flet ((take (n lst)
	       "take the first n element from lst. if there is not
enough elements, return lst as it is."
	       (cl-loop
		for lst* = lst then (cdr lst*)
		for n* = n then (1- n*)
		until (or (zerop n*) (null lst*))
		collect (car lst*)))
	    (drop (n lst)
	       "drop the first n elements from lst"
	       (cl-loop
		for lst* = lst then (cdr lst*)
		for n* = n then (1- n*)
		until (or (zerop n*) (null lst*))
		finally (return lst*))))
    (take zero-candidates-per-page
	  (drop (* zero-candidates-per-page zero-current-page) candidates))))

(defun zero-show-candidates (&optional candidates)
  "Show CANDIDATES using zero-panel via IPC/RPC."
  (let ((candidates-on-page (zero-candidates-on-page (or candidates
							 zero-candidates))))
    (cl-destructuring-bind (x y) (zero-get-point-position)
      (zero-panel-show-candidates
       (funcall zero-get-preedit-str-for-panel-func)
       (length candidates-on-page)
       candidates-on-page
       `(("in_emacs" t)
	 ("filename" ,(or (buffer-file-name) ""))
	 ("page_number" ,(1+ zero-current-page))
	 ("has_next_page" ,(or (> (length (or candidates zero-candidates)) (* zero-candidates-per-page (1+ zero-current-page))) (< zero-fetch-size (* zero-candidates-per-page (+ 2 zero-current-page)))))
	 ("has_previous_page" ,(> zero-current-page 0))
	 ("move_x" :int32 ,x)
	 ("move_y" :int32 ,y)))
      (zero-debug "candidates: %s\n" (s-join ", " candidates-on-page)))))

(defun zero-build-candidates (preedit-str fetch-size)
  "Build candidates list synchronously.

Try to find at least FETCH-SIZE number of candidates for PREEDIT-STR."
  ;; (zero-debug "zero-build-candidates\n")
  (unless (functionp zero-build-candidates-func)
    (signal 'wrong-type-argument (list 'functionp zero-build-candidates-func)))
  (prog1 (funcall zero-build-candidates-func preedit-str fetch-size)
    (setq zero-fetch-size (max fetch-size (length zero-candidates)))))

(defun zero-build-candidates-complete (candidates)
  "Called when `zero-build-candidates-async' return.

CANDIDATES is returned candidates list from async call."
  (setq zero-candidates candidates)
  (zero-show-candidates candidates))

(defun zero-build-candidates-async-default (preedit-str fetch-size complete-func)
  "Build candidate list, when done show it via `zero-show-candidates'.

PREEDIT-STR the preedit-str.
FETCH-SIZE try to find at least this many candidates for preedit-str.
COMPLETE-FUNC the function to call when build candidates completes."
  ;; (zero-debug "zero-build-candidates-async-default\n")
  (let ((candidates (zero-build-candidates preedit-str fetch-size)))
    ;; update cache to make SPC and digit key selection possible.
    (funcall complete-func candidates)))

(defvar zero-full-width-char-map
  ;; ascii 33 to 126 map to
  ;; unicode FF01 to FF5E
  (cl-loop
   for i from 33 to 126
   collect (cons (make-char 'ascii i) (make-char 'unicode 0 255 (- i 32))))
  "An alist that map half-width char to full-width char.")

(defun zero-convert-ch-to-full-width (ch)
  "Convert half-width char CH to full-width.

If there is no full-width char for CH, return it unchanged."
  (let ((pair (assoc ch zero-full-width-char-map)))
    (if pair (cdr pair) ch)))

(defun zero-convert-str-to-full-width (s)
  "Convert each char in S to their full-width char if there is one."
  (concat (mapcar 'zero-convert-ch-to-full-width s)))

(defun zero-convert-str-to-full-width-maybe (s)
  "If in `zero-full-width-mode', convert char in S to their full-width char; otherwise, return s unchanged."
  (if zero-full-width-mode (zero-convert-str-to-full-width s) s))

(defun zero-insert-full-width-char (ch)
  "If in `zero-full-width-mode', insert full-width char for given CH and return true, otherwise just return nil."
  (when zero-full-width-mode
    (let ((full-width-ch (zero-convert-ch-to-full-width ch)))
      (insert full-width-ch)
      full-width-ch)))

(defun zero-convert-punctuation-basic (ch)
  "Convert punctuation for `zero-punctuation-level-basic'.

Return CH's Chinese punctuation if CH is converted.  Return nil otherwise."
  (cl-case ch
    (?, "，")
    (?. "。")				; 0x3002
    (?? "？")
    (?! "！")
    (?\\ "、")				; 0x3001
    (?: "：")
    (otherwise nil)))

(defun zero-convert-punctuation-full (ch)
  "Convert punctuation for `zero-punctuation-level-full'.

Return CH's Chinese punctuation if CH is converted.  Return nil otherwise"
  (cl-case ch
    (?_ "——")
    (?< "《")				;0x300A
    (?> "》")				;0x300B
    (?\( "（")
    (?\) "）")
    (?\[ "【")				;0x3010
    (?\] "】")				;0x3011
    (?^ "……")
    (?\" (setq zero-double-quote-flag (not zero-double-quote-flag))
	 (if zero-double-quote-flag "“" "”"))
    (?\' (setq zero-single-quote-flag (not zero-single-quote-flag))
	 (if zero-single-quote-flag "‘" "’"))
    (?~ "～")
    (?\; "；")
    (?$ "￥")
    (t (zero-convert-punctuation-basic ch))))

(defun zero-convert-punctuation (ch)
  "Convert punctuation based on `zero-punctuation-level'.
Return CH's Chinese punctuation if CH is converted.  Return nil otherwise."
  (cond
   ((eq zero-punctuation-level zero-punctuation-level-basic)
    (zero-convert-punctuation-basic ch))
   ((eq zero-punctuation-level zero-punctuation-level-full)
    (zero-convert-punctuation-full ch))
   (t nil)))

(defun zero-handle-punctuation (ch)
  "If CH is a punctuation character, insert mapped Chinese punctuation and return true; otherwise, return false."
  (let ((str (zero-convert-punctuation ch)))
    (when str
      (insert str)
      t)))

(defun zero-append-char-to-preedit-str (ch)
  "Append char CH to preedit str, update and show candidate list."
  (setq zero-preedit-str
	(concat zero-preedit-str (make-string 1 ch)))
  (zero-debug "appended %c, preedit str is: %s\n" ch zero-preedit-str)
  (zero-preedit-str-changed))

(defun zero-can-start-sequence (ch)
  "Return t if char CH can start a preedit sequence."
  (if (functionp zero-can-start-sequence-func)
      (funcall zero-can-start-sequence-func ch)
    (error "`zero-can-start-sequence-func' is not a function")))

(defun zero-page-up ()
  "If not at first page, show candidates on previous page."
  (interactive)
  (when (> zero-current-page 0)
    (setq zero-current-page (1- zero-current-page))
    (zero-show-candidates)))

(defun zero-just-page-down ()
  "Just page down using existing candidates."
  (let ((len (length zero-candidates)))
    (when (> len (* zero-candidates-per-page (1+ zero-current-page)))
      (setq zero-current-page (1+ zero-current-page))
      (zero-show-candidates))))

(defun zero-page-down ()
  "If there is still candidates to be displayed, show candidates on next page."
  (interactive)
  (let ((len (length zero-candidates))
	(new-fetch-size (* zero-candidates-per-page (+ 2 zero-current-page))))
    (if (and (< len new-fetch-size)
	     (< zero-fetch-size new-fetch-size))
	(funcall zero-build-candidates-async-func
		 zero-preedit-str
		 new-fetch-size
		 (lambda (candidates)
		   (zero-build-candidates-complete candidates)
		   (setq zero-fetch-size (max new-fetch-size
					      (length candidates)))
		   (zero-just-page-down)))
      (zero-just-page-down))))

(defun zero-handle-preedit-char-default (ch)
  "Hanlde character insert in `zero--state-im-preediting' state.

CH is the char user has typed."
  (cond
   ((= ch ?\s)
    (zero-commit-first-candidate-or-preedit-str))
   ((and (>= ch ?0) (<= ch ?9))
    ;; 1 commit the 0th candidate
    ;; 2 commit the 1st candidate
    ;; ...
    ;; 0 commit the 9th candidate
    (unless (zero-commit-nth-candidate (mod (- (- ch ?0) 1) 10))
      (zero-append-char-to-preedit-str ch)))
   ((= ch zero-previous-page-key)
    (zero-page-up))
   ((= ch zero-next-page-key)
    (zero-page-down))
   (t (let ((str (zero-convert-punctuation ch)))
	(if str
	    (progn
	      (zero-set-state zero--state-im-waiting-input)
	      (zero-commit-first-candidate-or-preedit-str)
	      (insert str))
	  (zero-append-char-to-preedit-str ch))))))

(defun zero-self-insert-command (n)
  "Handle character `self-insert-command'.  This includes characters and digits.

N is the argument passed to `self-insert-command'."
  (interactive "p")
  (let ((ch (elt (this-command-keys-vector) 0)))
    (zero-debug "user typed: %c\n" ch)
    (cond
     ((eq zero-state zero--state-im-waiting-input)
      (if (zero-can-start-sequence ch)
	  (progn
	    (zero-debug "can start sequence, state=IM_PREEDITING\n")
	    (zero-set-state zero--state-im-preediting)
	    (zero-append-char-to-preedit-str ch))
	(zero-debug "cannot start sequence, state=IM_WAITING_INPUT\n")
	(unless (zero-handle-punctuation ch)
	  (unless (zero-insert-full-width-char ch)
	    (self-insert-command n)))))
     ((eq zero-state zero--state-im-preediting)
      (zero-debug "still preediting\n")
      (funcall zero-handle-preedit-char-func ch))
     (t
      (zero-debug "unexpected state: %s\n" zero-state)
      (self-insert-command n)))))

(defun zero-preedit-str-changed ()
  "Called when preedit str is changed and not empty.  Update and show candidate list."
  (setq zero-fetch-size 0)
  (setq zero-current-page 0)
  (funcall zero-build-candidates-async-func zero-preedit-str zero-initial-fetch-size 'zero-build-candidates-complete))

(defun zero-backspace-default ()
  "Handle backspace key in `zero--state-im-preediting' state."
  (let ((len (length zero-preedit-str)))
    (if (> len 1)
	(progn
	  (setq zero-preedit-str
		(substring zero-preedit-str 0 (1- len)))
	  (zero-preedit-str-changed))
      (zero-set-state zero--state-im-waiting-input)
      (zero-reset))))

(defun zero-backspace ()
  "Handle backspace key in `zero--state-im-preediting' state."
  (interactive)
  (unless (eq zero-state zero--state-im-preediting)
    (error "Error: zero-backspace called in non preediting state"))
  (zero-debug "zero-backspace\n")
  (funcall zero-backspace-func))

(defun zero-commit-text (text)
  "Commit given TEXT, reset preedit str, hide candidate list."
  (zero-debug "commit text: %s\n" text)
  (insert text)
  (setq zero-preedit-str "")
  (setq zero-candidates nil)
  (setq zero-current-page 0)
  (zero-hide-candidate-list))

(defun zero-return ()
  "Handle RET key press in `zero--state-im-preediting' state."
  (interactive)
  (unless (eq zero-state zero--state-im-preediting)
    (error "Error: zero-return called in non preediting state"))
  (zero-debug "zero-return\n")
  (zero-set-state zero--state-im-waiting-input)
  (zero-commit-text (zero-convert-str-to-full-width-maybe zero-preedit-str)))

(defun zero-commit-nth-candidate (n)
  "Commit Nth candidate and return true if it exists; otherwise, return false."
  (let ((candidate (nth n (zero-candidates-on-page zero-candidates))))
    (if candidate
	(progn
	  (zero-set-state zero--state-im-waiting-input)
	  (zero-commit-text candidate)
	  t)
      nil)))

(defun zero-commit-preedit-str ()
  "Commit current preedit-str."
  (zero-set-state zero--state-im-waiting-input)
  (zero-commit-text (zero-convert-str-to-full-width-maybe zero-preedit-str)))

(defun zero-commit-first-candidate-or-preedit-str ()
  "Commit first candidate if there is one, otherwise commit preedit str."
  (unless (zero-commit-nth-candidate 0)
    (zero-commit-preedit-str)))

(defun zero-hide-candidate-list ()
  "Hide candidate list."
  (zero-panel-hide)
  (zero-debug "hide candidate list\n"))

(defun zero-reset ()
  "Reset zero states."
  (interactive)
  (zero-debug "zero-reset\n")
  (zero-set-state zero--state-im-waiting-input)
  (setq zero-preedit-str "")
  (setq zero-candidates nil)
  (setq zero-current-page 0)
  (zero-hide-candidate-list))

(defun zero-focus-in ()
  "A hook function, run when focus in a buffer."
  (when (eq zero-state zero--state-im-preediting)
    (zero-show-candidates zero-candidates)
    (zero-enter-preedit-state)))

(defun zero-focus-out ()
  "A hook function, run when focus out a buffer."
  (when (eq zero-state zero--state-im-preediting)
    (zero-hide-candidate-list)
    (zero-leave-preedit-state)))

(defun zero-buffer-list-changed ()
  "A hook function, run when buffer list has changed.  This includes user has switched buffer."
  (if (eq (car (buffer-list)) zero-buffer)
      (zero-focus-in)))

;;============
;; minor mode
;;============

(defvar zero-mode-map
  (let ((map (make-sparse-keymap)))
    ;; build zero-prefix-map
    (defvar zero-prefix-map (define-prefix-command 'zero-prefix-map))
    (let ((bindings '(("," zero-cycle-punctuation-level)
		      ("." zero-toggle-full-width-mode))))
      (dolist (b bindings)
	(define-key zero-prefix-map (car b) (cadr b))))
    ;; mount zero-prefix-map in C-c , prefix key.
    (define-key map (kbd "C-c ,") zero-prefix-map)

    ;; other keybindings
    (define-key map [remap self-insert-command]
      'zero-self-insert-command)
    map)
  "Keymap for `zero-mode'.")

(defun zero-enable-preediting-map ()
  "Enable preediting keymap in `zero-mode-map'."
  (zero-debug "zero-enable-preediting-map\n")
  (define-key zero-mode-map (kbd "<backspace>") 'zero-backspace)
  (define-key zero-mode-map (kbd "RET") 'zero-return)
  (define-key zero-mode-map (kbd "<escape>") 'zero-reset))

(defun zero-disable-preediting-map ()
  "Disable preediting keymap in `zero-mode-map'."
  (zero-debug "zero-disable-preediting-map\n")
  (define-key zero-mode-map (kbd "<backspace>") nil)
  (define-key zero-mode-map (kbd "RET") nil)
  (define-key zero-mode-map (kbd "<escape>") nil))

(defun zero-modeline-string ()
  "Build `zero-mode' modeline string aka lighter.

If full-width mode is enabled, show ZeroF;
Otherwise, show Zero."
  (if zero-full-width-mode " ZeroF" " Zero"))

(define-minor-mode zero-mode
  "a Chinese input method framework written as an emacs minor mode.

\\{zero-mode-map}"
  nil
  (:eval (zero-modeline-string))
  zero-mode-map
  ;; local variables and variable init
  (make-local-variable 'zero-state)
  (zero-set-state  zero--state-im-off)
  (make-local-variable 'zero-punctuation-level)
  (make-local-variable 'zero-full-width-mode)
  (make-local-variable 'zero-double-quote-flag)
  (make-local-variable 'zero-single-quote-flag)
  (set (make-local-variable 'zero-preedit-str) "")
  (set (make-local-variable 'zero-candidates) nil)
  (make-local-variable 'zero-candidates-per-page)
  (make-local-variable 'zero-current-page)
  (make-local-variable 'zero-fetch-size)
  (make-local-variable 'zero-im)
  (make-local-variable 'zero-build-candidates-func)
  (make-local-variable 'zero-can-start-sequence-func)
  (zero-set-im zero-im)
  ;; hooks
  (add-hook 'focus-in-hook 'zero-focus-in)
  (add-hook 'focus-out-hook 'zero-focus-out)
  (set (make-local-variable 'zero-buffer) (current-buffer))
  (add-hook 'buffer-list-update-hook 'zero-buffer-list-changed))

;;==================
;; IM developer API
;;==================

(defun zero-register-im (im-name im-functions-alist)
  "(Re)register an input method in zero.

After registration, you can use `zero-set-default-im' and
`zero-set-im' to select input method to use.

IM-NAME should be a symbol.
IM-FUNCTIONS-ALIST should be a list of form
  '((:virtual-function-name . implementation-function-name))

virtual functions                       corresponding variable
===========================================================================
:build-candidates                       `zero-build-candidates-func'
:can-start-sequence                     `zero-can-start-sequence-func'
:handle-preedit-char                    `zero-handle-preedit-char-func'
:get-preedit-str-for-panel              `zero-get-preedit-str-for-panel-func'
:handle-backspace                       `zero-backspace-func'
:init                                   nil
:shutdown                               nil
:preedit-start                          `zero-preedit-start-func'
:preedit-end                            `zero-preedit-end-func'

registered input method is saved in `zero-ims'"
  ;; add or replace entry in `zero-ims'
  (unless (symbolp im-name)
    (signal 'wrong-type-argument (list 'symbolp im-name)))
  (setq zero-ims (assq-delete-all im-name zero-ims))
  (setq zero-ims (push (cons im-name im-functions-alist) zero-ims)))

;;============
;; public API
;;============

(defun zero-toggle-full-width-mode ()
  "Toggle `zero-full-width-mode' on/off."
  (interactive)
  (setq zero-full-width-mode (not zero-full-width-mode))
  (message (if zero-full-width-mode
	       "Enabled full-width mode"
	     "Enabled half-width mode")))

(defun zero-set-punctuation-level (level)
  "Set `zero-punctuation-level'.

LEVEL the level to set to."
  (interactive)
  (if (not (member level (list zero-punctuation-level-basic
			       zero-punctuation-level-full
			       zero-punctuation-level-none)))
      (error "Level not supported: %s" level)
    (setq zero-punctuation-level level)))

(defun zero-set-punctuation-levels (levels)
  "Set `zero-punctuation-levels'.

`zero-cycle-punctuation-level' will cycle current
`zero-punctuation-level' among defined LEVELS."
  (dolist (level levels)
    (if (not (member level (list zero-punctuation-level-basic
				 zero-punctuation-level-full
				 zero-punctuation-level-none)))
	(error "Level not supported: %s" level)))
  (setq zero-punctuation-levels levels))

(defun zero-cycle-punctuation-level ()
  "Cycle `zero-punctuation-level' among `zero-punctuation-levels'."
  (interactive)
  (setq zero-punctuation-level
	(zero-cycle-list zero-punctuation-levels zero-punctuation-level))
  (message "punctuation level set to %s" zero-punctuation-level))

;;;###autoload
(defun zero-set-im (im-name)
  "Select zero input method for current buffer.

if IM-NAME is nil, use default empty input method"
  ;; TODO provide auto completion for im-name
  (interactive "SSet input method to: ")
  ;; when switch away from an IM, run last IM's :shutdown function.
  (if zero-im
      (let ((shutdown-func (cdr (assq :shutdown (cdr (assq zero-im zero-ims))))))
	(if (functionp shutdown-func)
	    (funcall shutdown-func))))
  (if im-name
      (let ((im-functions (cdr (assq im-name zero-ims))))
	(if im-functions
	    (progn
	      ;; TODO create a macro to reduce code duplication and human
	      ;; error.
	      ;;
	      ;; TODO do some functionp check for the slot functions. if check
	      ;; fail, keep (or revert to) the old IM.
	      (setq zero-build-candidates-func
		    (or (cdr (assq :build-candidates im-functions))
			'zero-build-candidates-default))
	      (setq zero-build-candidates-async-func
		    (or (cdr (assq :build-candidates-async im-functions))
			'zero-build-candidates-async-default))
	      (setq zero-can-start-sequence-func
		    (or (cdr (assq :can-start-sequence im-functions))
			'zero-can-start-sequence-default))
	      (setq zero-handle-preedit-char-func
		    (or (cdr (assq :handle-preedit-char im-functions))
			'zero-handle-preedit-char-default))
	      (setq zero-get-preedit-str-for-panel-func
		    (or (cdr (assq :get-preedit-str-for-panel im-functions))
			'zero-get-preedit-str-for-panel-default))
	      (setq zero-backspace-func
		    (or (cdr (assq :handle-backspace im-functions))
			'zero-backspace-default))
	      (setq zero-preedit-start-func
		    (cdr (assq :preedit-start im-functions)))
	      (setq zero-preedit-end-func
		    (cdr (assq :preedit-end im-functions)))
	      (unless (functionp zero-backspace-func)
		(signal 'wrong-type-argument
			(list 'functionp zero-backspace-func)))
	      ;; when switch to a IM, run its :init function
	      (let ((init-func (cdr (assq :init im-functions))))
		(if (functionp init-func)
		    (funcall init-func)))
	      (set (make-local-variable 'zero-im) im-name))
	  (error "Input method %s not registered in zero" im-name)))
    (zero-debug "using default empty input method")
    (setq zero-build-candidates-func 'zero-build-candidates-default)
    (setq zero-build-candidates-async-func 'zero-build-candidates-async-default)
    (setq zero-can-start-sequence-func 'zero-can-start-sequence-default)
    (setq zero-handle-preedit-char-func 'zero-handle-preedit-char-default)
    (setq zero-get-preedit-str-for-panel-func 'zero-get-preedit-str-for-panel-default)
    (setq zero-backspace-func 'zero-backspace-default)
    (setq zero-preedit-start-func nil)
    (setq zero-preedit-end-func nil)))

;;;###autoload
(defun zero-set-default-im (im-name)
  "Set given IM-NAME as default zero input method."
  (unless (symbolp im-name)
    (signal 'wrong-type-argument (list 'symbolp im-name)))
  (setq-default zero-im im-name))

;;;###autoload
(defun zero-on ()
  "Turn on `zero-mode'."
  (interactive)
  (zero-debug "zero-on\n")
  (zero-mode 1)
  (if (eq zero-state zero--state-im-off)
      (zero-set-state zero--state-im-waiting-input)))

(defun zero-off ()
  "Turn off `zero-mode'."
  (interactive)
  (zero-debug "zero-off\n")
  (zero-mode -1)
  (zero-reset)
  (zero-set-state zero--state-im-off))

;;;###autoload
(defun zero-toggle ()
  "Toggle `zero-mode'."
  (interactive)
  (if zero-mode
      (zero-off)
    (zero-on)))

(provide 'zero-framework)

;; body of zero-table.el

;;==============
;; dependencies
;;==============


;;===============================
;; basic data and emacs facility
;;===============================

(defvar zero-table-table nil
  "The table used by zero-table input method, map string to string.")
(defvar zero-table-sequence-initials nil "Used in `zero-table-can-start-sequence'.")

;;=====================
;; key logic functions
;;=====================

(defun zero-table-sort-key (lhs rhs)
  "A predicate function to sort candidates.  Return t if LHS should sort before RHS."
  (string< (car lhs) (car rhs)))

(defun zero-table-build-candidates (preedit-str &optional _fetch-size)
  "Build candidates by looking up PREEDIT-STR in `zero-table-table'."
  (mapcar 'cdr (sort (cl-remove-if-not (lambda (pair) (string-prefix-p preedit-str (car pair))) zero-table-table) 'zero-table-sort-key)))

;; (defun zero-table-build-candidates-async (preedit-str)
;;   "build candidate list, when done show it via `zero-table-show-candidates'"
;;   (zero-table-debug "building candidate list\n")
;;   (let ((candidates (zero-table-build-candidates preedit-str)))
;;     ;; update cache to make SPC and digit key selection possible.
;;     (setq zero-table-candidates candidates)
;;     (zero-table-show-candidates candidates)))

(defun zero-table-can-start-sequence (ch)
  "Return t if char CH can start a preedit sequence."
  (member (make-string 1 ch) zero-table-sequence-initials))

;;===============================
;; register IM to zero framework
;;===============================

(zero-register-im
 'zero-table
 '((:build-candidates . zero-table-build-candidates)
   (:can-start-sequence . zero-table-can-start-sequence)))

;;============
;; public API
;;============

(defun zero-table-set-table (alist)
  "Set the conversion table.

the ALIST should be a list of (key . value) pairs.  when user type
\(part of) key, the IM will show all matching value.

e.g.
'((\"phone\" . \"18612345678\")
  (\"mail\" . \"foo@example.com\")
  (\"map\" . \"https://ditu.amap.com/\")
  (\"m\" . \"https://msdn.microsoft.com/en-us\")
  (\"address\" . \"123 Happy Street\"))"
  (setq zero-table-table alist)
  (setq zero-table-sequence-initials
	(delete-dups (mapcar (lambda (pair) (substring (car pair) 0 1))
			     zero-table-table))))

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

;; body of zero-pinyin-service.el

;;================
;; implementation
;;================


(defvar zero-pinyin-service-service-name
  "com.emacsos.zero.ZeroPinyinService1")
(defvar zero-pinyin-service-path
  "/com/emacsos/zero/ZeroPinyinService1")
(defvar zero-pinyin-service-interface
  "com.emacsos.zero.ZeroPinyinService1.ZeroPinyinServiceInterface")

(defun zero-pinyin-service-error-handler (event error)
  "Handle dbus errors.

EVENT, ERROR are arguments passed to the handler."
  (when (or (string-equal zero-pinyin-service-service-name
			  (dbus-event-interface-name event))
	    (s-contains-p zero-pinyin-service-service-name (cadr error)))
    (error "`zero-pinyin-service' dbus failed: %S" (cadr error))))

(add-hook 'dbus-event-error-functions 'zero-pinyin-service-error-handler)

(defun zero-pinyin-service-async-call (method handler &rest args)
  "Call METHOD on zero-pinin-service asynchronously.
This is a wrapper around `dbus-call-method-asynchronously'.
Argument HANDLER the handler function.
Optional argument ARGS extra arguments to pass to the wrapped function."
  (apply 'dbus-call-method-asynchronously
	 :session zero-pinyin-service-service-name
	 zero-pinyin-service-path
	 zero-pinyin-service-interface
	 method handler :timeout 1000 args))

(defun zero-pinyin-service-call (method &rest args)
  "Call METHOD on zero-pinin-service synchronously.
This is a wrapper around `dbus-call-method'.
Optional argument ARGS extra arguments to pass to the wrapped function."
  (apply 'dbus-call-method
	 :session zero-pinyin-service-service-name
	 zero-pinyin-service-path
	 zero-pinyin-service-interface
	 method :timeout 1000 args))

;;============
;; public API
;;============

(defun zero-pinyin-service-get-candidates (preedit-str fetch-size)
  "Get candidates for pinyin in PREEDIT-STR synchronously.

preedit-str the preedit-str, should be pure pinyin string
FETCH-SIZE try to fetch this many candidates or more"
  (zero-pinyin-service-call "GetCandidates" :string preedit-str :uint32 fetch-size))

(defun zero-pinyin-service-get-candidates-async (preedit-str fetch-size get-candidates-complete)
  "Get candidates for pinyin in PREEDIT-STR asynchronously.

PREEDIT-STR the preedit string, should be pure pinyin string.
FETCH-SIZE try to fetch this many candidates or more.
GET-CANDIDATES-COMPLETE the async handler function."
  (zero-pinyin-service-async-call
   "GetCandidates" get-candidates-complete :string preedit-str :uint32 fetch-size))

(defun zero-pinyin-candidate-pinyin-indices-to-dbus-format (candidate_pinyin_indices)
  "Convert CANDIDATE_PINYIN_INDICES to Emacs dbus format."
  (let (result)
    (push :array result)
    ;; (push :signature result)
    ;; (push "(ii)" result)
    (dolist (pypair candidate_pinyin_indices)
      (push (list :struct :int32 (cl-first pypair) :int32 (cl-second pypair))
	    result))
    (reverse result)))

(defun zero-pinyin-service-commit-candidate-async (candidate candidate_pinyin_indices)
  "Commit candidate asynchronously.

CANDIDATE the candidate user selected.
CANDIDATE_PINYIN_INDICES the candidate's pinyin shengmu and yunmu index."
  ;; don't care about the result, so no callback.
  (zero-pinyin-service-async-call
   "CommitCandidate" nil
   :string candidate
   (zero-pinyin-candidate-pinyin-indices-to-dbus-format candidate_pinyin_indices)))

(defun zero-pinyin-service-delete-candidates-async (candidate delete-candidate-complete)
  "Delete CANDIDATE asynchronously.

DELETE-CANDIDATE-COMPLETE the async handler function."
  (zero-pinyin-service-async-call
   "DeleteCandidate" delete-candidate-complete :string candidate))

(defun zero-pinyin-service-quit ()
  "Quit panel application."
  (zero-pinyin-service-async-call "Quit" nil))

(defun zero-pinyin-service-set-fuzzy-flag (fuzzy-flag)
  "Set FuzzyFlag property.

FUZZY-FLAG should be a natural number.  See service interface XML
for flag value and meaning"
  (interactive)
  (dbus-set-property
   :session zero-pinyin-service-service-name
   zero-pinyin-service-path
   zero-pinyin-service-interface
   "FuzzyFlag" fuzzy-flag))

(provide 'zero-pinyin-service)

;; body of zero-pinyin.el

;;==============
;; dependencies
;;==============


;;===============================
;; basic data and emacs facility
;;===============================

;; these two var is only used in docstring to avoid checkdoc line-too-long
;; error.
(defvar zero-pinyin-service-interface-xml-file
  "/usr/share/dbus-1/interfaces/com.emacsos.zero.ZeroPinyinService1.ZeroPinyinServiceInterface.xml")
(defvar zero-pinyin-service-interface-xml-url
  "https://gitlab.emacsos.com/sylecn/zero-pinyin-service/blob/master/com.emacsos.zero.ZeroPinyinService1.ZeroPinyinServiceInterface.xml")
(defcustom zero-pinyin-fuzzy-flag 0
  "Non-nil means use this value as FuzzyFlag.
see zero-pinyin-service dbus interface xml for flag value and meaning.

You can find the xml file locally at `zero-pinyin-service-interface-xml-file'
or online at `zero-pinyin-service-interface-xml-url'."
  :type 'integer
  :group 'zero-pinyin)

(defvar zero-pinyin-state nil "Zero-pinyin internal state.  could be nil or `*zero-pinyin-state-im-partial-commit*'.")
(defconst zero-pinyin--state-im-partial-commit 'IM-PARTIAL-COMMIT)

(defvar zero-pinyin-used-preedit-str-lengths nil
  "Accompany `zero-candidates', marks how many preedit-str chars are used for each candidate.")
(defvar zero-pinyin-candidates-pinyin-indices nil
  "Store GetCandidates dbus method candidates_pinyin_indices field.")
(defvar zero-pinyin-pending-str "")
(defvar zero-pinyin-pending-preedit-str "")
(defvar zero-pinyin-pending-pinyin-indices nil
  "Stores `zero-pinyin-pending-str' corresponds pinyin indices.")

;;=====================
;; key logic functions
;;=====================

(defun zero-pinyin-reset ()
  "Reset states."
  (setq zero-pinyin-state nil)
  (setq zero-pinyin-used-preedit-str-lengths nil)
  (setq zero-pinyin-pending-str "")
  (setq zero-pinyin-pending-preedit-str "")
  (when (null (zero-pinyin-service-set-fuzzy-flag zero-pinyin-fuzzy-flag))
    (unless (zerop zero-pinyin-fuzzy-flag)
      (display-warning 'zero-pinyin "Requires zero-pinyin-service v0.9.0 or later to support `zero-pinyin-fuzzy-flag'." :warning))))

(defun zero-pinyin-init ()
  "Called when this im is turned on."
  (make-local-variable 'zero-pinyin-state)
  (zero-pinyin-reset))

(defun zero-pinyin-preedit-start ()
  "Called when enter `*zero-state-im-preediting*' state."
  (define-key zero-mode-map [remap digit-argument] 'zero-digit-argument))

(defun zero-pinyin-preedit-end ()
  "Called when leave `*zero-state-im-preediting*' state."
  (define-key zero-mode-map [remap digit-argument] nil))

(defun zero-pinyin-shutdown ()
  "Called when this im is turned off."
  (define-key zero-mode-map [remap digit-argument] nil))

(defvar zero-pinyin--build-candidates-use-test-data nil
  "If t, `zero-pinyin-build-candidates' will use `zero-pinyin-build-candidates-test'.")

(defun zero-pinyin-build-candidates (preedit-str fetch-size)
  "Synchronously build candidates list.

PREEDIT-STR the preedit string.
FETCH-SIZE fetch at least this many candidates if possible."
  (if zero-pinyin--build-candidates-use-test-data
      (progn
	(zero-pinyin-build-candidates-test preedit-str)
	(setq zero-fetch-size (max fetch-size (length zero-candidates))))
    (zero-debug "zero-pinyin building candidate list synchronously\n")
    (let ((result (zero-pinyin-service-get-candidates preedit-str fetch-size)))
      (setq zero-fetch-size (max fetch-size (length (cl-first result))))
      (setq zero-pinyin-used-preedit-str-lengths (cl-second result))
      (setq zero-pinyin-candidates-pinyin-indices (cl-third result))
      (cl-first result))))

(defun zero-pinyin-build-candidates-async (preedit-str fetch-size complete-func)
  "Asynchronously build candidate list, when done call complete-func on it.

PREEDIT-STR the preedit string.
FETCH-SIZE fetch at least this many candidates if possible.
COMPLETE-FUNC the callback function when async call completes.  it's called with
              fetched candidates list as parameter."
  (zero-debug "zero-pinyin building candidate list asynchronously\n")
  (zero-pinyin-service-get-candidates-async
   preedit-str
   fetch-size
   (lambda (candidates matched_preedit_str_lengths candidates_pinyin_indices)
     (setq zero-pinyin-used-preedit-str-lengths matched_preedit_str_lengths)
     (setq zero-pinyin-candidates-pinyin-indices candidates_pinyin_indices)
     (setq zero-fetch-size (max fetch-size (length candidates)))
     ;; Note: with dynamic binding, this command result in (void-variable
     ;; complete-func) error.
     (funcall complete-func candidates))))

(defun zero-pinyin-can-start-sequence (ch)
  "Return t if char CH can start a preedit sequence."
  (and (>= ch ?a)
       (<= ch ?z)
       (not (= ch ?i))
       (not (= ch ?u))
       (not (= ch ?v))))

(defun zero-pinyin-pending-preedit-str-changed ()
  "Update zero states when pending preedit string changed."
  (setq zero-fetch-size 0)
  (setq zero-current-page 0)
  (zero-pinyin-build-candidates-async zero-pinyin-pending-preedit-str zero-initial-fetch-size 'zero-build-candidates-complete))

(defun zero-pinyin-commit-nth-candidate (n)
  "Commit Nth candidate and return true if it exists, otherwise, return false."
  (let* ((n-prime (+ n (* zero-candidates-per-page zero-current-page)))
	 (candidate (nth n-prime zero-candidates))
	 (used-len (when candidate
		     (nth n-prime zero-pinyin-used-preedit-str-lengths))))
    (when candidate
      (zero-debug
       "zero-pinyin-commit-nth-candidate\n    n=%s candidate=%s used-len=%s zero-pinyin-pending-preedit-str=%S\n"
       n candidate used-len zero-pinyin-pending-preedit-str)
      (cond
       ((null zero-pinyin-state)
	(if (= used-len (length zero-preedit-str))
	    (progn
	      (zero-debug "commit in full\n")
	      (zero-set-state zero--state-im-waiting-input)
	      (zero-commit-text candidate)
	      (zero-pinyin-service-commit-candidate-async
	       candidate
	       (nth n-prime zero-pinyin-candidates-pinyin-indices))
	      t)
	  (zero-debug "partial commit, in partial commit mode now.\n")
	  (setq zero-pinyin-state zero-pinyin--state-im-partial-commit)
	  (setq zero-pinyin-pending-str candidate)
	  (setq zero-pinyin-pending-preedit-str (substring zero-preedit-str used-len))
	  (setq zero-pinyin-pending-pinyin-indices
		(nth n-prime zero-pinyin-candidates-pinyin-indices))
	  (zero-pinyin-pending-preedit-str-changed)
	  t))
       ((eq zero-pinyin-state zero-pinyin--state-im-partial-commit)
	(if (= used-len (length zero-pinyin-pending-preedit-str))
	    (progn
	      (zero-debug "finishes partial commit\n")
	      (setq zero-pinyin-state nil)
	      (zero-set-state zero--state-im-waiting-input)
	      (zero-commit-text (concat zero-pinyin-pending-str candidate))
	      (zero-pinyin-service-commit-candidate-async
	       (concat zero-pinyin-pending-str candidate)
	       (append zero-pinyin-pending-pinyin-indices
		       (nth n-prime zero-pinyin-candidates-pinyin-indices)))
	      t)
	  (zero-debug "continue partial commit\n")
	  (setq zero-pinyin-pending-str (concat zero-pinyin-pending-str candidate))
	  (setq zero-pinyin-pending-preedit-str (substring zero-pinyin-pending-preedit-str used-len))
	  (setq zero-pinyin-pending-pinyin-indices
		(append zero-pinyin-pending-pinyin-indices
			(nth n-prime zero-pinyin-candidates-pinyin-indices)))
	  (zero-pinyin-service-commit-candidate-async
	   zero-pinyin-pending-str
	   zero-pinyin-pending-pinyin-indices)
	  (zero-pinyin-pending-preedit-str-changed)
	  t))
       (t (error "Unexpected zero-pinyin-state: %s" zero-pinyin-state))))))

(defun zero-pinyin-commit-first-candidate-or-preedit-str ()
  "Commit first candidate if there is one, otherwise, commit preedit string."
  (unless (zero-pinyin-commit-nth-candidate 0)
    (zero-commit-preedit-str)))

(defun zero-pinyin-commit-first-candidate-in-full ()
  "Commit first candidate and return t if it consumes all preedit-str.
Otherwise, just return nil."
  (let ((candidate (nth 0 (zero-candidates-on-page zero-candidates)))
	(used-len (nth (* zero-candidates-per-page zero-current-page) zero-pinyin-used-preedit-str-lengths)))
    (when candidate
      (cond
       ((null zero-pinyin-state)
	(when (= used-len (length zero-preedit-str))
	  (zero-set-state zero--state-im-waiting-input)
	  (zero-commit-text candidate)
	  t))
       ((eq zero-pinyin-state zero-pinyin--state-im-partial-commit)
	(when (= used-len (length zero-pinyin-pending-preedit-str))
	  (setq zero-pinyin-state nil)
	  (zero-set-state zero--state-im-waiting-input)
	  (zero-commit-text (concat zero-pinyin-pending-str candidate))
	  t))
       (t (error "Unexpected zero-pinyin-state: %s" zero-pinyin-state))))))

(defun zero-pinyin-page-down ()
  "Handle page down for zero-pinyin.

This is different from zero-framework because I need to support partial commit"
  (let ((len (length zero-candidates))
	(new-fetch-size (* zero-candidates-per-page (+ 2 zero-current-page))))
    (if (and (< len new-fetch-size)
	     (< zero-fetch-size new-fetch-size))
	(let ((preedit-str (if (eq zero-pinyin-state zero-pinyin--state-im-partial-commit) zero-pinyin-pending-preedit-str zero-preedit-str)))
	  (zero-pinyin-build-candidates-async
	   preedit-str
	   new-fetch-size
	   (lambda (candidates)
	     (zero-build-candidates-complete candidates)
	     (zero-just-page-down))))
      (zero-just-page-down))))

(defun zero-pinyin-handle-preedit-char (ch)
  "Hanlde character insert in `*zero-state-im-preediting*' state.
Override `zero-handle-preedit-char-default'.

CH the character user typed."
  (cond
   ((= ch ?\s)
    (zero-pinyin-commit-first-candidate-or-preedit-str))
   ((and (>= ch ?0) (<= ch ?9))
    ;; 1 commit the 0th candidate
    ;; 2 commit the 1st candidate
    ;; ...
    ;; 0 commit the 9th candidate
    (unless (zero-pinyin-commit-nth-candidate (mod (- (- ch ?0) 1) 10))
      (zero-append-char-to-preedit-str ch)
      (setq zero-pinyin-state nil)))
   ((= ch zero-previous-page-key)
    (zero-handle-preedit-char-default ch))
   ((= ch zero-next-page-key)
    (zero-pinyin-page-down))
   (t (let ((str (zero-convert-punctuation ch)))
	(if str
	    (when (zero-pinyin-commit-first-candidate-in-full)
	      (zero-set-state zero--state-im-waiting-input)
	      (insert str))
	  (setq zero-pinyin-state nil)
	  (zero-append-char-to-preedit-str ch))))))

(defun zero-pinyin-get-preedit-str-for-panel ()
  "Return the preedit string that should show in panel."
  (if (eq zero-pinyin-state zero-pinyin--state-im-partial-commit)
      (concat zero-pinyin-pending-str zero-pinyin-pending-preedit-str)
    zero-preedit-str))

(defun zero-pinyin-preedit-str-changed ()
  "Start over for candidate selection process."
  (setq zero-pinyin-state nil)
  (zero-preedit-str-changed))

(defun zero-pinyin-backspace ()
  "Handle backspace key in `*zero-state-im-preediting*' state."
  (if (eq zero-pinyin-state zero-pinyin--state-im-partial-commit)
      (zero-pinyin-preedit-str-changed)
    (zero-backspace-default)))

(defun zero-pinyin-delete-candidate (digit)
  "Tell backend to delete candidate at DIGIT position.

DIGIT is the digit key used to select nth candidate.
DIGIT 1 means delete 1st candidate.
DIGIT 2 means delete 2st candidate.
...
DIGIT 0 means delete 10th candidate."
  (let ((candidate (nth (mod (- digit 1) 10)
			(zero-candidates-on-page zero-candidates))))
    (when candidate
      (zero-pinyin-service-delete-candidates-async
       candidate 'zero-pinyin-preedit-str-changed))))

(defun zero-digit-argument ()
  "Allow C-<digit> to DeleteCandidate in `*zero-state-im-preediting*' state."
  (interactive)
  (unless (eq zero-state zero--state-im-preediting)
    (error "`zero-digit-argument' called in non preediting state"))
  (if (memq 'control (event-modifiers last-command-event))
      (let* ((char (if (integerp last-command-event)
		       last-command-event
		     (get last-command-event 'ascii-character)))
	     (digit (- (logand char ?\177) ?0)))
	(zero-pinyin-delete-candidate digit))))

;;===============================
;; register IM to zero framework
;;===============================

(zero-register-im
 'pinyin
 '((:build-candidates . zero-pinyin-build-candidates)
   ;; comment to use sync version, uncomment to use async version.
   ;; (:build-candidates-async . zero-pinyin-build-candidates-async)
   (:can-start-sequence . zero-pinyin-can-start-sequence)
   (:handle-preedit-char . zero-pinyin-handle-preedit-char)
   (:get-preedit-str-for-panel . zero-pinyin-get-preedit-str-for-panel)
   (:handle-backspace . zero-pinyin-backspace)
   (:init . zero-pinyin-init)
   (:shutdown . zero-pinyin-shutdown)
   (:preedit-start . zero-pinyin-preedit-start)
   (:preedit-end . zero-pinyin-preedit-end)))

;;============
;; public API
;;============

;;===========
;; test data
;;===========

(defun zero-pinyin-build-candidates-test (preedit-str)
  "Test data for testing partial commit.

PREEDIT-STR the preedit string."
  (cond
   ((equal preedit-str "liyifeng")
    (setq zero-pinyin-used-preedit-str-lengths '(8 4 4 4 2 2 2))
    '("李易峰" "利益" "礼仪" "离异" "里" "理" "力"))
   ((equal preedit-str "feng")
    (setq zero-pinyin-used-preedit-str-lengths '(4 4 4 4 4))
    '("风" "封" "疯" "丰" "凤"))
   ((equal preedit-str "yifeng")
    (setq zero-pinyin-used-preedit-str-lengths '(6 6 2 2 2 2))
    '("一封" "遗风" "艺" "依" "一" "以"))
   (t nil)))

(provide 'zero-pinyin)


(provide 'zero)

;;; zero.el ends here
