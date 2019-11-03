;;; zero-input-framework.el --- Zero Chinese input method framework -*- lexical-binding: t -*-

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

;;; Commentary:

;; zero-input-framework is a Chinese input method framework for Emacs,
;; implemented as an Emacs minor mode.
;;
;; You can cycle zero-input-punctuation-level in current buffer by C-c , ,
;; You can change default Chinese punctuation level:
;;
;;   (setq-default zero-input-punctuation-level zero-input-punctuation-level-full)
;;
;; You can toggle full-width mode in current buffer by C-c , .
;; You can enable full-width mode by default:
;;
;;   (setq-default zero-input-full-width-p t)
;;

;;; Code:

;;==============
;; dependencies
;;==============

(eval-when-compile (require 'cl-lib))
(require 's)
(require 'zero-input-panel)

;;=======
;; utils
;;=======

;; this function is from ibus.el
(defun zero-input--ibus-compute-pixel-position (&optional pos window)
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

(defun zero-input-get-point-position ()
  "Return current point's position (x y).
Origin (0, 0) is at screen top left corner."
  (cl-destructuring-bind (x y line-height) (zero-input--ibus-compute-pixel-position)
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

(defun zero-input-cycle-list (lst item)
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

;; zero-input-el version
(defvar zero-input-version nil "Zero package version.")
(setq zero-input-version "2.0.2")

;; FSM state
(defconst zero-input--state-im-off 'IM-OFF)
(defconst zero-input--state-im-waiting-input 'IM-WAITING-INPUT)
(defconst zero-input--state-im-preediting 'IM-PREEDITING)

(defconst zero-input-punctuation-level-basic 'BASIC)
(defconst zero-input-punctuation-level-full 'FULL)
(defconst zero-input-punctuation-level-none 'NONE)

(defvar-local zero-input-im nil
  "Stores current input method.

If nil, the empty input method will be used.  In the empty input
method, only punctuation is handled.  Other keys are pass
through")
(defvar zero-input-ims nil
  "A list of registered input methods.")

(defvar-local zero-input-buffer nil
  "Stores the associated buffer.
this is used to help with buffer focus in/out events")

(defvar-local zero-input-state zero-input--state-im-off)
(defvar-local zero-input-full-width-p nil
  "Set to t to enable full-width mode.
In full-width mode, commit ascii char will insert full-width char if there is a
corresponding full-width char.  This full-width char map is
independent from punctuation map.  You can change this via
`zero-input-toggle-full-width-mode'")
(defvar-local zero-input-punctuation-level zero-input-punctuation-level-basic
  "Punctuation level.

Should be one of
`zero-input-punctuation-level-basic'
`zero-input-punctuation-level-full'
`zero-input-punctuation-level-none'")
(defvar zero-input-punctuation-levels (list zero-input-punctuation-level-basic
				      zero-input-punctuation-level-full
				      zero-input-punctuation-level-none)
  "Punctuation levels to use when `zero-input-cycle-punctuation-level'.")
(defvar-local zero-input-double-quote-flag nil
  "Non-nil means next double quote insert close quote.

Used when converting double quote to Chinese quote.
If nil, next double quote insert open quote.
Otherwise, next double quote insert close quote.")
(defvar-local zero-input-single-quote-flag nil
  "Non-nil means next single quote insert close quote.

Used when converting single quote to Chinese quote.
If nil, next single quote insert open quote.
Otherwise, next single quote insert close quote.")
(defvar-local zero-input-preedit-str "")
(defvar-local zero-input-candidates nil)
(defcustom zero-input-candidates-per-page 10
  "How many candidates to show on each page.

Change will be effective only in new `zero-input-mode' buffer."
  :group 'zero
  :type 'integer)
(defvar-local zero-input-current-page 0 "Current page number.  count from 0.")
(defvar-local zero-input-initial-fetch-size 20
  "How many candidates to fetch for the first call to GetCandidates.")
;; zero-input-fetch-size is reset to 0 when preedit-str changes.
;; zero-input-fetch-size is set to fetch-size in build-candidates-async complete-func
;; lambda.
(defvar-local zero-input-fetch-size 0 "Last GetCandidates call's fetch-size.")
(defvar zero-input-previous-page-key ?\- "Previous page key.")
(defvar zero-input-next-page-key ?\= "Next page key.")

;;; concrete input method should define these functions and set them in the
;;; corresponding *-func variable.
(defun zero-input-build-candidates-default (_preedit-str _fetch-size)
  "Default implementation for `zero-input-build-candidates-func'."
  nil)
(defun zero-input-can-start-sequence-default (_ch)
  "Default implementation for `zero-input-can-start-sequence-func'."
  nil)
(defun zero-input-get-preedit-str-for-panel-default ()
  "Default implementation for `zero-input-get-preedit-str-for-panel-func'."
  zero-input-preedit-str)
(defvar-local zero-input-build-candidates-func
  'zero-input-build-candidates-default
  "Contains a function to build candidates from preedit-str.  The function accepts param preedit-str, fetch-size, returns candidate list.")
(defvar-local zero-input-build-candidates-async-func
  'zero-input-build-candidates-async-default
  "Contains a function to build candidates from preedit-str.  The function accepts param preedit-str, fetch-size, and a complete-func that should be called on returned candidate list.")
(defvar-local zero-input-can-start-sequence-func
  'zero-input-can-start-sequence-default
  "Contains a function to decide whether a char can start a preedit sequence.")
(defvar-local zero-input-handle-preedit-char-func
  'zero-input-handle-preedit-char-default
  "Contains a function to handle IM-PREEDITING state char insert.
The function should return t if char is handled.
This allow input method to override default logic.")
(defvar-local zero-input-get-preedit-str-for-panel-func
  'zero-input-get-preedit-str-for-panel-default
  "Contains a function that return preedit-str to show in zero-input-panel.")
(defvar-local zero-input-backspace-func
  'zero-input-backspace-default
  "Contains a function to handle <backward> char.")
(defvar-local zero-input-handle-preedit-char-func
  'zero-input-handle-preedit-char-default
  "Hanlde character insert in `zero-input--state-im-preediting' mode.")
(defvar-local zero-input-preedit-start-func 'nil
  "Called when enter `zero-input--state-im-preediting' state.")
(defvar-local zero-input-preedit-end-func 'nil
  "Called when leave `zero-input--state-im-preediting' state.")

(defvar zero-input-enable-debug nil
  "Whether to enable debug.
if t, `zero-input-debug' will output debug msg in *zero-input-debug* buffer")
(defvar zero-input-debug-buffer-max-size 30000
  "Max characters in *zero-input-debug* buffer.  If reached, first half data will be deleted.")

(defun zero-input-debug (string &rest objects)
  "Log debug message in *zero-input-debug* buffer.

STRING and OBJECTS are passed to `format'"
  (if zero-input-enable-debug
      (with-current-buffer (get-buffer-create "*zero-input-debug*")
	(goto-char (point-max))
	(insert (apply 'format string objects))
	(when (> (point) zero-input-debug-buffer-max-size)
	  (insert "removing old data\n")
	  (delete-region (point-min) (/ zero-input-debug-buffer-max-size 2))))))

;; (zero-input-debug "msg1\n")
;; (zero-input-debug "msg2: %s\n" "some obj")
;; (zero-input-debug "msg3: %s\n" 24)
;; (zero-input-debug "msg4: %s %s\n" 24 1)

(defun zero-input-enter-preedit-state ()
  "Config keymap when enter preedit state."
  (zero-input-enable-preediting-map)
  (if (functionp zero-input-preedit-start-func)
      (funcall zero-input-preedit-start-func)))

(defun zero-input-leave-preedit-state ()
  "Config keymap when leave preedit state."
  (zero-input-disable-preediting-map)
  (if (functionp zero-input-preedit-end-func)
      (funcall zero-input-preedit-end-func)))

(defun zero-input-set-state (state)
  "Set zero state to given STATE."
  (zero-input-debug "set state to %s\n" state)
  (setq zero-input-state state)
  (if (eq state zero-input--state-im-preediting)
      (zero-input-enter-preedit-state)
    (zero-input-leave-preedit-state)))

(defun zero-input-candidates-on-page (candidates)
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
    (take zero-input-candidates-per-page
	  (drop (* zero-input-candidates-per-page zero-input-current-page) candidates))))

(defun zero-input-show-candidates (&optional candidates)
  "Show CANDIDATES using zero-input-panel via IPC/RPC."
  (let ((candidates-on-page (zero-input-candidates-on-page (or candidates
							 zero-input-candidates))))
    (cl-destructuring-bind (x y) (zero-input-get-point-position)
      (zero-input-panel-show-candidates
       (funcall zero-input-get-preedit-str-for-panel-func)
       (length candidates-on-page)
       candidates-on-page
       `(("in_emacs" t)
	 ("filename" ,(or (buffer-file-name) ""))
	 ("page_number" ,(1+ zero-input-current-page))
	 ("has_next_page" ,(or (> (length (or candidates zero-input-candidates)) (* zero-input-candidates-per-page (1+ zero-input-current-page))) (< zero-input-fetch-size (* zero-input-candidates-per-page (+ 2 zero-input-current-page)))))
	 ("has_previous_page" ,(> zero-input-current-page 0))
	 ("move_x" :int32 ,x)
	 ("move_y" :int32 ,y)))
      (zero-input-debug "candidates: %s\n" (s-join ", " candidates-on-page)))))

(defun zero-input-build-candidates (preedit-str fetch-size)
  "Build candidates list synchronously.

Try to find at least FETCH-SIZE number of candidates for PREEDIT-STR."
  ;; (zero-input-debug "zero-input-build-candidates\n")
  (unless (functionp zero-input-build-candidates-func)
    (signal 'wrong-type-argument (list 'functionp zero-input-build-candidates-func)))
  (prog1 (funcall zero-input-build-candidates-func preedit-str fetch-size)
    (setq zero-input-fetch-size (max fetch-size (length zero-input-candidates)))))

(defun zero-input-build-candidates-complete (candidates)
  "Called when `zero-input-build-candidates-async' return.

CANDIDATES is returned candidates list from async call."
  (setq zero-input-candidates candidates)
  (zero-input-show-candidates candidates))

(defun zero-input-build-candidates-async-default (preedit-str fetch-size complete-func)
  "Build candidate list, when done show it via `zero-input-show-candidates'.

PREEDIT-STR the preedit-str.
FETCH-SIZE try to find at least this many candidates for preedit-str.
COMPLETE-FUNC the function to call when build candidates completes."
  ;; (zero-input-debug "zero-input-build-candidates-async-default\n")
  (let ((candidates (zero-input-build-candidates preedit-str fetch-size)))
    ;; update cache to make SPC and digit key selection possible.
    (funcall complete-func candidates)))

(defvar zero-input-full-width-char-map
  ;; ascii 33 to 126 map to
  ;; unicode FF01 to FF5E
  (cl-loop
   for i from 33 to 126
   collect (cons (make-char 'ascii i) (make-char 'unicode 0 255 (- i 32))))
  "An alist that map half-width char to full-width char.")

(defun zero-input-convert-ch-to-full-width (ch)
  "Convert half-width char CH to full-width.

If there is no full-width char for CH, return it unchanged."
  (let ((pair (assoc ch zero-input-full-width-char-map)))
    (if pair (cdr pair) ch)))

(defun zero-input-convert-str-to-full-width (s)
  "Convert each char in S to their full-width char if there is one."
  (concat (mapcar 'zero-input-convert-ch-to-full-width s)))

(defun zero-input-convert-str-to-full-width-maybe (s)
  "If in `zero-input-full-width-p', convert char in S to their full-width char; otherwise, return s unchanged."
  (if zero-input-full-width-p (zero-input-convert-str-to-full-width s) s))

(defun zero-input-insert-full-width-char (ch)
  "If in `zero-input-full-width-p', insert full-width char for given CH and return true, otherwise just return nil."
  (when zero-input-full-width-p
    (let ((full-width-ch (zero-input-convert-ch-to-full-width ch)))
      (insert full-width-ch)
      full-width-ch)))

(defun zero-input-convert-punctuation-basic (ch)
  "Convert punctuation for `zero-input-punctuation-level-basic'.

Return CH's Chinese punctuation if CH is converted.  Return nil otherwise."
  (cl-case ch
    (?, "，")
    (?. "。")				; 0x3002
    (?? "？")
    (?! "！")
    (?\\ "、")				; 0x3001
    (?: "：")
    (otherwise nil)))

(defun zero-input-convert-punctuation-full (ch)
  "Convert punctuation for `zero-input-punctuation-level-full'.

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
    (?\" (setq zero-input-double-quote-flag (not zero-input-double-quote-flag))
	 (if zero-input-double-quote-flag "“" "”"))
    (?\' (setq zero-input-single-quote-flag (not zero-input-single-quote-flag))
	 (if zero-input-single-quote-flag "‘" "’"))
    (?~ "～")
    (?\; "；")
    (?$ "￥")
    (t (zero-input-convert-punctuation-basic ch))))

(defun zero-input-convert-punctuation (ch)
  "Convert punctuation based on `zero-input-punctuation-level'.
Return CH's Chinese punctuation if CH is converted.  Return nil otherwise."
  (cond
   ((eq zero-input-punctuation-level zero-input-punctuation-level-basic)
    (zero-input-convert-punctuation-basic ch))
   ((eq zero-input-punctuation-level zero-input-punctuation-level-full)
    (zero-input-convert-punctuation-full ch))
   (t nil)))

(defun zero-input-handle-punctuation (ch)
  "If CH is a punctuation character, insert mapped Chinese punctuation and return true; otherwise, return false."
  (let ((str (zero-input-convert-punctuation ch)))
    (when str
      (insert str)
      t)))

(defun zero-input-append-char-to-preedit-str (ch)
  "Append char CH to preedit str, update and show candidate list."
  (setq zero-input-preedit-str
	(concat zero-input-preedit-str (make-string 1 ch)))
  (zero-input-debug "appended %c, preedit str is: %s\n" ch zero-input-preedit-str)
  (zero-input-preedit-str-changed))

(defun zero-input-can-start-sequence (ch)
  "Return t if char CH can start a preedit sequence."
  (if (functionp zero-input-can-start-sequence-func)
      (funcall zero-input-can-start-sequence-func ch)
    (error "`zero-input-can-start-sequence-func' is not a function")))

(defun zero-input-page-up ()
  "If not at first page, show candidates on previous page."
  (interactive)
  (when (> zero-input-current-page 0)
    (setq zero-input-current-page (1- zero-input-current-page))
    (zero-input-show-candidates)))

(defun zero-input-just-page-down ()
  "Just page down using existing candidates."
  (let ((len (length zero-input-candidates)))
    (when (> len (* zero-input-candidates-per-page (1+ zero-input-current-page)))
      (setq zero-input-current-page (1+ zero-input-current-page))
      (zero-input-show-candidates))))

(defun zero-input-page-down ()
  "If there is still candidates to be displayed, show candidates on next page."
  (interactive)
  (let ((len (length zero-input-candidates))
	(new-fetch-size (* zero-input-candidates-per-page (+ 2 zero-input-current-page))))
    (if (and (< len new-fetch-size)
	     (< zero-input-fetch-size new-fetch-size))
	(funcall zero-input-build-candidates-async-func
		 zero-input-preedit-str
		 new-fetch-size
		 (lambda (candidates)
		   (zero-input-build-candidates-complete candidates)
		   (setq zero-input-fetch-size (max new-fetch-size
					      (length candidates)))
		   (zero-input-just-page-down)))
      (zero-input-just-page-down))))

(defun zero-input-handle-preedit-char-default (ch)
  "Hanlde character insert in `zero-input--state-im-preediting' state.

CH is the char user has typed."
  (cond
   ((= ch ?\s)
    (zero-input-commit-first-candidate-or-preedit-str))
   ((and (>= ch ?0) (<= ch ?9))
    ;; 1 commit the 0th candidate
    ;; 2 commit the 1st candidate
    ;; ...
    ;; 0 commit the 9th candidate
    (unless (zero-input-commit-nth-candidate (mod (- (- ch ?0) 1) 10))
      (zero-input-append-char-to-preedit-str ch)))
   ((= ch zero-input-previous-page-key)
    (zero-input-page-up))
   ((= ch zero-input-next-page-key)
    (zero-input-page-down))
   (t (let ((str (zero-input-convert-punctuation ch)))
	(if str
	    (progn
	      (zero-input-set-state zero-input--state-im-waiting-input)
	      (zero-input-commit-first-candidate-or-preedit-str)
	      (insert str))
	  (zero-input-append-char-to-preedit-str ch))))))

(defun zero-input-self-insert-command (n)
  "Handle character `self-insert-command'.  This includes characters and digits.

N is the argument passed to `self-insert-command'."
  (interactive "p")
  (let ((ch (elt (this-command-keys-vector) 0)))
    (zero-input-debug "user typed: %c\n" ch)
    (cond
     ((eq zero-input-state zero-input--state-im-waiting-input)
      (if (zero-input-can-start-sequence ch)
	  (progn
	    (zero-input-debug "can start sequence, state=IM_PREEDITING\n")
	    (zero-input-set-state zero-input--state-im-preediting)
	    (zero-input-append-char-to-preedit-str ch))
	(zero-input-debug "cannot start sequence, state=IM_WAITING_INPUT\n")
	(unless (zero-input-handle-punctuation ch)
	  (unless (zero-input-insert-full-width-char ch)
	    (self-insert-command n)))))
     ((eq zero-input-state zero-input--state-im-preediting)
      (zero-input-debug "still preediting\n")
      (funcall zero-input-handle-preedit-char-func ch))
     (t
      (zero-input-debug "unexpected state: %s\n" zero-input-state)
      (self-insert-command n)))))

(defun zero-input-preedit-str-changed ()
  "Called when preedit str is changed and not empty.  Update and show candidate list."
  (setq zero-input-fetch-size 0)
  (setq zero-input-current-page 0)
  (funcall zero-input-build-candidates-async-func zero-input-preedit-str zero-input-initial-fetch-size 'zero-input-build-candidates-complete))

(defun zero-input-backspace-default ()
  "Handle backspace key in `zero-input--state-im-preediting' state."
  (let ((len (length zero-input-preedit-str)))
    (if (> len 1)
	(progn
	  (setq zero-input-preedit-str
		(substring zero-input-preedit-str 0 (1- len)))
	  (zero-input-preedit-str-changed))
      (zero-input-set-state zero-input--state-im-waiting-input)
      (zero-input-reset))))

(defun zero-input-backspace ()
  "Handle backspace key in `zero-input--state-im-preediting' state."
  (interactive)
  (unless (eq zero-input-state zero-input--state-im-preediting)
    (error "Error: zero-input-backspace called in non preediting state"))
  (zero-input-debug "zero-input-backspace\n")
  (funcall zero-input-backspace-func))

(defun zero-input-commit-text (text)
  "Commit given TEXT, reset preedit str, hide candidate list."
  (zero-input-debug "commit text: %s\n" text)
  (insert text)
  (setq zero-input-preedit-str "")
  (setq zero-input-candidates nil)
  (setq zero-input-current-page 0)
  (zero-input-hide-candidate-list))

(defun zero-input-return ()
  "Handle RET key press in `zero-input--state-im-preediting' state."
  (interactive)
  (unless (eq zero-input-state zero-input--state-im-preediting)
    (error "Error: zero-input-return called in non preediting state"))
  (zero-input-debug "zero-input-return\n")
  (zero-input-set-state zero-input--state-im-waiting-input)
  (zero-input-commit-text (zero-input-convert-str-to-full-width-maybe zero-input-preedit-str)))

(defun zero-input-commit-nth-candidate (n)
  "Commit Nth candidate and return true if it exists; otherwise, return false."
  (let ((candidate (nth n (zero-input-candidates-on-page zero-input-candidates))))
    (if candidate
	(progn
	  (zero-input-set-state zero-input--state-im-waiting-input)
	  (zero-input-commit-text candidate)
	  t)
      nil)))

(defun zero-input-commit-preedit-str ()
  "Commit current preedit-str."
  (zero-input-set-state zero-input--state-im-waiting-input)
  (zero-input-commit-text (zero-input-convert-str-to-full-width-maybe zero-input-preedit-str)))

(defun zero-input-commit-first-candidate-or-preedit-str ()
  "Commit first candidate if there is one, otherwise commit preedit str."
  (unless (zero-input-commit-nth-candidate 0)
    (zero-input-commit-preedit-str)))

(defun zero-input-hide-candidate-list ()
  "Hide candidate list."
  (zero-input-panel-hide)
  (zero-input-debug "hide candidate list\n"))

(defun zero-input-reset ()
  "Reset zero states."
  (interactive)
  (zero-input-debug "zero-input-reset\n")
  (zero-input-set-state zero-input--state-im-waiting-input)
  (setq zero-input-preedit-str "")
  (setq zero-input-candidates nil)
  (setq zero-input-current-page 0)
  (zero-input-hide-candidate-list))

(defun zero-input-focus-in ()
  "A hook function, run when focus in a buffer."
  (when (eq zero-input-state zero-input--state-im-preediting)
    (zero-input-show-candidates zero-input-candidates)
    (zero-input-enter-preedit-state)))

(defun zero-input-focus-out ()
  "A hook function, run when focus out a buffer."
  (when (eq zero-input-state zero-input--state-im-preediting)
    (zero-input-hide-candidate-list)
    (zero-input-leave-preedit-state)))

(defun zero-input-buffer-list-changed ()
  "A hook function, run when buffer list has changed.  This includes user has switched buffer."
  (if (eq (car (buffer-list)) zero-input-buffer)
      (zero-input-focus-in)))

;;============
;; minor mode
;;============

(defvar zero-input-mode-map
  (let ((map (make-sparse-keymap)))
    ;; build zero-input-prefix-map
    (defvar zero-input-prefix-map (define-prefix-command 'zero-input-prefix-map))
    (let ((bindings '(("," zero-input-cycle-punctuation-level)
		      ("." zero-input-toggle-full-width-mode))))
      (dolist (b bindings)
	(define-key zero-input-prefix-map (car b) (cadr b))))
    ;; mount zero-input-prefix-map in C-c , prefix key.
    (define-key map (kbd "C-c ,") zero-input-prefix-map)

    ;; other keybindings
    (define-key map [remap self-insert-command]
      'zero-input-self-insert-command)
    map)
  "Keymap for `zero-input-mode'.")

(defun zero-input-enable-preediting-map ()
  "Enable preediting keymap in `zero-input-mode-map'."
  (zero-input-debug "zero-input-enable-preediting-map\n")
  (define-key zero-input-mode-map (kbd "<backspace>") 'zero-input-backspace)
  (define-key zero-input-mode-map (kbd "RET") 'zero-input-return)
  (define-key zero-input-mode-map (kbd "<escape>") 'zero-input-reset))

(defun zero-input-disable-preediting-map ()
  "Disable preediting keymap in `zero-input-mode-map'."
  (zero-input-debug "zero-input-disable-preediting-map\n")
  (define-key zero-input-mode-map (kbd "<backspace>") nil)
  (define-key zero-input-mode-map (kbd "RET") nil)
  (define-key zero-input-mode-map (kbd "<escape>") nil))

(defun zero-input-modeline-string ()
  "Build `zero-input-mode' modeline string aka lighter.

If full-width mode is enabled, show ZeroF;
Otherwise, show Zero."
  (if zero-input-full-width-p " ZeroF" " Zero"))

(define-minor-mode zero-input-mode
  "a Chinese input method framework written as an emacs minor mode.

\\{zero-input-mode-map}"
  nil
  (:eval (zero-input-modeline-string))
  zero-input-mode-map
  ;; local variables and variable init
  (zero-input-reset)
  (make-local-variable 'zero-input-candidates-per-page)
  (zero-input-set-im zero-input-im)
  ;; hooks
  (add-hook 'focus-in-hook 'zero-input-focus-in)
  (add-hook 'focus-out-hook 'zero-input-focus-out)
  (setq zero-input-buffer (current-buffer))
  (add-hook 'buffer-list-update-hook 'zero-input-buffer-list-changed))

;;==================
;; IM developer API
;;==================

(defun zero-input-register-im (im-name im-functions-alist)
  "(Re)register an input method in zero.

After registration, you can use `zero-input-set-default-im' and
`zero-input-set-im' to select input method to use.

IM-NAME should be a symbol.
IM-FUNCTIONS-ALIST should be a list of form
  '((:virtual-function-name . implementation-function-name))

virtual functions                   corresponding variable
===========================================================================
:build-candidates                   `zero-input-build-candidates-func'
:can-start-sequence                 `zero-input-can-start-sequence-func'
:handle-preedit-char                `zero-input-handle-preedit-char-func'
:get-preedit-str-for-panel          `zero-input-get-preedit-str-for-panel-func'
:handle-backspace                   `zero-input-backspace-func'
:init                               nil
:shutdown                           nil
:preedit-start                      `zero-input-preedit-start-func'
:preedit-end                        `zero-input-preedit-end-func'

registered input method is saved in `zero-input-ims'"
  ;; add or replace entry in `zero-input-ims'
  (unless (symbolp im-name)
    (signal 'wrong-type-argument (list 'symbolp im-name)))
  (setq zero-input-ims (assq-delete-all im-name zero-input-ims))
  (setq zero-input-ims (push (cons im-name im-functions-alist) zero-input-ims)))

;;============
;; public API
;;============

(defun zero-input-toggle-full-width ()
  "Toggle `zero-input-full-width-p' on/off."
  (interactive)
  (setq zero-input-full-width-p (not zero-input-full-width-p))
  (message (if zero-input-full-width-p
	       "Enabled full-width mode"
	     "Enabled half-width mode")))

(defun zero-input-set-punctuation-level (level)
  "Set `zero-input-punctuation-level'.

LEVEL the level to set to."
  (interactive)
  (if (not (member level (list zero-input-punctuation-level-basic
			       zero-input-punctuation-level-full
			       zero-input-punctuation-level-none)))
      (error "Level not supported: %s" level)
    (setq zero-input-punctuation-level level)))

(defun zero-input-set-punctuation-levels (levels)
  "Set `zero-input-punctuation-levels'.

`zero-input-cycle-punctuation-level' will cycle current
`zero-input-punctuation-level' among defined LEVELS."
  (dolist (level levels)
    (if (not (member level (list zero-input-punctuation-level-basic
				 zero-input-punctuation-level-full
				 zero-input-punctuation-level-none)))
	(error "Level not supported: %s" level)))
  (setq zero-input-punctuation-levels levels))

(defun zero-input-cycle-punctuation-level ()
  "Cycle `zero-input-punctuation-level' among `zero-input-punctuation-levels'."
  (interactive)
  (setq zero-input-punctuation-level
	(zero-input-cycle-list zero-input-punctuation-levels zero-input-punctuation-level))
  (message "punctuation level set to %s" zero-input-punctuation-level))

;;;###autoload
(defun zero-input-set-im (im-name)
  "Select zero input method for current buffer.

if IM-NAME is nil, use default empty input method"
  ;; TODO provide auto completion for im-name
  (interactive "SSet input method to: ")
  ;; when switch away from an IM, run last IM's :shutdown function.
  (if zero-input-im
      (let ((shutdown-func (cdr (assq :shutdown (cdr (assq zero-input-im zero-input-ims))))))
	(if (functionp shutdown-func)
	    (funcall shutdown-func))))
  (if im-name
      (let ((im-functions (cdr (assq im-name zero-input-ims))))
	(if im-functions
	    (progn
	      ;; TODO create a macro to reduce code duplication and human
	      ;; error.
	      ;;
	      ;; TODO do some functionp check for the slot functions. if check
	      ;; fail, keep (or revert to) the old IM.
	      (setq zero-input-build-candidates-func
		    (or (cdr (assq :build-candidates im-functions))
			'zero-input-build-candidates-default))
	      (setq zero-input-build-candidates-async-func
		    (or (cdr (assq :build-candidates-async im-functions))
			'zero-input-build-candidates-async-default))
	      (setq zero-input-can-start-sequence-func
		    (or (cdr (assq :can-start-sequence im-functions))
			'zero-input-can-start-sequence-default))
	      (setq zero-input-handle-preedit-char-func
		    (or (cdr (assq :handle-preedit-char im-functions))
			'zero-input-handle-preedit-char-default))
	      (setq zero-input-get-preedit-str-for-panel-func
		    (or (cdr (assq :get-preedit-str-for-panel im-functions))
			'zero-input-get-preedit-str-for-panel-default))
	      (setq zero-input-backspace-func
		    (or (cdr (assq :handle-backspace im-functions))
			'zero-input-backspace-default))
	      (setq zero-input-preedit-start-func
		    (cdr (assq :preedit-start im-functions)))
	      (setq zero-input-preedit-end-func
		    (cdr (assq :preedit-end im-functions)))
	      (unless (functionp zero-input-backspace-func)
		(signal 'wrong-type-argument
			(list 'functionp zero-input-backspace-func)))
	      ;; when switch to a IM, run its :init function
	      (let ((init-func (cdr (assq :init im-functions))))
		(if (functionp init-func)
		    (funcall init-func)))
	      (setq zero-input-im im-name))
	  (error "Input method %s not registered in zero" im-name)))
    (zero-input-debug "using default empty input method")
    (setq zero-input-build-candidates-func 'zero-input-build-candidates-default)
    (setq zero-input-build-candidates-async-func 'zero-input-build-candidates-async-default)
    (setq zero-input-can-start-sequence-func 'zero-input-can-start-sequence-default)
    (setq zero-input-handle-preedit-char-func 'zero-input-handle-preedit-char-default)
    (setq zero-input-get-preedit-str-for-panel-func 'zero-input-get-preedit-str-for-panel-default)
    (setq zero-input-backspace-func 'zero-input-backspace-default)
    (setq zero-input-preedit-start-func nil)
    (setq zero-input-preedit-end-func nil)))

;;;###autoload
(defun zero-input-set-default-im (im-name)
  "Set given IM-NAME as default zero input method."
  (unless (symbolp im-name)
    (signal 'wrong-type-argument (list 'symbolp im-name)))
  (setq-default zero-input-im im-name))

;;;###autoload
(defun zero-input-on ()
  "Turn on `zero-input-mode'."
  (interactive)
  (zero-input-mode 1))

(defun zero-input-off ()
  "Turn off `zero-input-mode'."
  (interactive)
  (zero-input-mode -1))

(define-obsolete-function-alias 'zero-input-toggle 'zero-input-mode
  "Zero-input v2.0.2" "Toggle `zero-input-mode'.")

(provide 'zero-input-framework)

;;; zero-input-framework.el ends here
