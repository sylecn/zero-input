;;; -*- lexical-binding: t -*-
;;; zero-framework.el --- zero Chinese input method framework

;; Copyright 2019 Yuanle Song <sylecn@gmail.com>
;;
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

;; zero-framework is a Chinese input method framework for emacs, implemented
;; as an emacs minor mode.

;;; Code:

;;==============
;; dependencies
;;==============

(require 'cl-macs)
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
  (cl-destructuring-bind (x y line-height) (ibus-compute-pixel-position)
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
  "return the object next to given item in lst, if item is the last object, return the first object in lst.

if item is not in lst, return nil"
  (let ((r (member item lst)))
    (cond
     ((null r) nil)
     (t (or (cadr r)
	    (car lst))))))

(ert-deftest zero-cycle-list ()
  (should (= (zero-cycle-list '(1 2 3) 1) 2))
  (should (eq (zero-cycle-list '(a b c) 'a) 'b))
  (should (eq (zero-cycle-list '(a b c) 'b) 'c))
  (should (eq (zero-cycle-list '(a b c) 'c) 'a))
  (should (eq (zero-cycle-list '(a b c) 'd) nil)))

;;=====================
;; key logic functions
;;=====================

;; zero-el version
(defvar zero-version nil "zero-el package version")
(setq zero-version "1.0.2")

;; FSM state
(defconst *zero-state-im-off* 'IM-OFF)
(defconst *zero-state-im-waiting-input* 'IM-WAITING-INPUT)
(defconst *zero-state-im-preediting* 'IM-PREEDITING)

(defconst *zero-punctuation-level-basic* 'BASIC)
(defconst *zero-punctuation-level-full* 'FULL)
(defconst *zero-punctuation-level-none* 'NONE)

(defvar zero-im nil
  "current input method. if nil, the empty input method will be used.
in the empty input method, only punctuation is handled. Other keys are pass through")
(defvar zero-ims nil
  "a list of registered input methods")

(defvar zero-buffer nil
  "stores the associated buffer.
this is used to help with buffer focus in/out events")

(defvar zero-state *zero-state-im-off*)
(defvar zero-punctuation-level *zero-punctuation-level-basic*
  "punctuation level. should be one of
*zero-punctuation-level-basic*
*zero-punctuation-level-full*
*zero-punctuation-level-none*")
(defvar zero-punctuation-levels (list *zero-punctuation-level-basic*
				      *zero-punctuation-level-full*
				      *zero-punctuation-level-none*)
  "punctuation levels to use when `zero-cycle-punctuation-level'")
(defvar zero-double-quote-flag nil
  "used when converting double quote to Chinese quote.
if nil, next double quote insert open quote.
otherwise, next double quote insert close quote")
(defvar zero-single-quote-flag nil
  "used when converting single quote to Chinese quote.
if nil, next single quote insert open quote.
otherwise, next single quote insert close quote")
(defvar zero-preedit-str "")
(defvar zero-candidates nil)
(defcustom zero-candidates-per-page 10
  "how many candidates to show on each page"
  :group 'zero)
(defvar zero-current-page 0 "current page number. count from 0")
(defvar zero-initial-fetch-size 20
  "how many candidates to fetch for the first call to GetCandidates")
;; zero-fetch-size is reset to 0 when preedit-str changes.
;; zero-fetch-size is set to fetch-size in build-candidates-async complete-func
;; lambda.
(defvar zero-fetch-size 0 "last GetCandidates call's fetch-size")
(defvar zero-previous-page-key ?\- "previous page key")
(defvar zero-next-page-key ?\= "next page key")

;;; concrete input method should define these functions and set them in the
;;; corresponding *-func variable.
(defun zero-build-candidates-default (_preedit-str _fetch-size) nil)
(defun zero-can-start-sequence-default (_ch) nil)
(defun zero-get-preedit-str-for-panel-default () zero-preedit-str)
(defvar zero-build-candidates-func 'zero-build-candidates-default
  "contains a function to build candidates from preedit-str. The function accepts param preedit-str, fetch-size, returns candidate list.")
(defvar zero-build-candidates-async-func 'zero-build-candidates-async-default
  "contains a function to build candidates from preedit-str. The function accepts param preedit-str, fetch-size, and a complete-func that should be called on returned candidate list.")
(defvar zero-can-start-sequence-func 'zero-can-start-sequence-default
  "contains a function to decide whether a char can start a preedit sequence")
(defvar zero-handle-preedit-char-func 'zero-handle-preedit-char-default
  "contains a function to handle IM-PREEDITING state char insert.
The function should return t if char is handled.
This allow input method to override default logic.")
(defvar zero-get-preedit-str-for-panel-func 'zero-get-preedit-str-for-panel-default
  "contains a function that return preedit-str to show in zero-panel")
(defvar zero-backspace-func 'zero-backspace-default
  "contains a function to handle <backward> char")
(defvar zero-handle-preedit-char-func 'zero-handle-preedit-char-default
  "hanlde character insert in `*zero-state-im-preediting*' mode")
(defvar zero-preedit-start-func 'nil
  "called when enter `*zero-state-im-preediting*' state")
(defvar zero-preedit-end-func 'nil
  "called when leave `*zero-state-im-preediting*' state")

(defvar zero-enable-debug nil
  "whether to enable debug.
if t, `zero-debug' will output debug msg in *zero-debug* buffer")
(defvar zero-debug-buffer-max-size 30000
  "max characters in *zero-debug* buffer. if reached, first half data will be deleted")

(defun zero-debug (string &rest objects)
  "log debug message in *zero-debug* buffer"
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
  "config keymap when enter preedit state"
  (zero-enable-preediting-map)
  (if (functionp zero-preedit-start-func)
      (funcall zero-preedit-start-func)))

(defun zero-leave-preedit-state ()
  "config keymap when leave preedit state"
  (zero-disable-preediting-map)
  (if (functionp zero-preedit-end-func)
      (funcall zero-preedit-end-func)))

(defun zero-set-state (state)
  "set state to given state"
  (zero-debug "set state to %s\n" state)
  (setq zero-state state)
  (if (eq state *zero-state-im-preediting*)
      (zero-enter-preedit-state)
    (zero-leave-preedit-state)))

(defun zero-candidates-on-page (candidates)
  "return candidates on current page for given candidates list"
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
  "show candidates using zero-panel via IPC/RPC"
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
  "build candidates list synchronously"
  ;; (zero-debug "zero-build-candidates\n")
  (unless (functionp zero-build-candidates-func)
    (signal 'wrong-type-argument (list 'functionp zero-build-candidates-func)))
  (prog1 (funcall zero-build-candidates-func preedit-str fetch-size)
    (setq zero-fetch-size (max fetch-size (length zero-candidates)))))

(defun zero-build-candidates-complete (candidates)
  "called when `zero-build-candidates-async' returns"
  (setq zero-candidates candidates)
  (zero-show-candidates candidates))

(defun zero-build-candidates-async-default (preedit-str fetch-size complete-func)
  "build candidate list, when done show it via `zero-show-candidates'"
  ;; (zero-debug "zero-build-candidates-async-default\n")
  (let ((candidates (zero-build-candidates preedit-str fetch-size)))
    ;; update cache to make SPC and digit key selection possible.
    (funcall complete-func candidates)))

(defun zero-convert-punctuation-basic (ch)
  "convert punctuation for *zero-punctuation-level-basic*
return ch's Chinese punctuation if ch is converted. return nil otherwise"
  (cl-case ch
    (?, "，")
    (?. "。")
    (?? "？")
    (?! "！")
    (?\\ "、")
    (?: "：")
    (otherwise nil)))

(defun zero-convert-punctuation-full (ch)
  "convert punctuation for *zero-punctuation-level-full*
return ch's Chinese punctuation if ch is converted. return nil otherwise"
  (cl-case ch
    (?_ "——")
    (?< "《")
    (?> "》")
    (?\( "（")
    (?\) "）")
    (?\[ "【")
    (?\] "】")
    (?^ "……")
    (?\" (setq zero-double-quote-flag (not zero-double-quote-flag))
	 (if zero-double-quote-flag "“" "”"))
    (?\' (setq zero-single-quote-flag (not zero-single-quote-flag))
	 (if zero-single-quote-flag "‘" "’"))
    (?~ "～")
    (?\; "；")
    (t (zero-convert-punctuation-basic ch))))

(defun zero-convert-punctuation (ch)
  "convert punctuation based on `zero-punctuation-level'
return ch's Chinese punctuation if ch is converted. return nil otherwise"
  (cond
   ((eq zero-punctuation-level *zero-punctuation-level-basic*)
    (zero-convert-punctuation-basic ch))
   ((eq zero-punctuation-level *zero-punctuation-level-full*)
    (zero-convert-punctuation-full ch))
   (t nil)))

(defun zero-handle-punctuation (ch)
  "if n is a punctuation character, insert mapped Chinese punctuation and return true; otherwise, return false."
  (let ((str (zero-convert-punctuation ch)))
    (when str
      (insert str)
      t)))

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

(defun zero-page-up ()
  "if not at first page, show candidates on previous page."
  (when (> zero-current-page 0)
    (setq zero-current-page (1- zero-current-page))
    (zero-show-candidates)))

(defun zero-just-page-down ()
  "just page down using existing candidates"
  (let ((len (length zero-candidates)))
    (when (> len (* zero-candidates-per-page (1+ zero-current-page)))
      (setq zero-current-page (1+ zero-current-page))
      (zero-show-candidates))))

(defun zero-page-down ()
  "if there is still candidates to be displayed, show candidates on next page."
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
  "hanlde character insert in `*zero-state-im-preediting*' state"
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
	      (zero-set-state *zero-state-im-waiting-input*)
	      (zero-commit-first-candidate-or-preedit-str)
	      (insert str))
	  (zero-append-char-to-preedit-str ch))))))

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
      (funcall zero-handle-preedit-char-func ch))
     (t
      (zero-debug "unexpected state: %s\n" zero-state)
      (self-insert-command n)))))

(defun zero-preedit-str-changed ()
  "called when preedit str is changed and not empty. update and show candidate list"
  (setq zero-fetch-size 0)
  (setq zero-current-page 0)
  (funcall zero-build-candidates-async-func zero-preedit-str zero-initial-fetch-size 'zero-build-candidates-complete))

(defun zero-backspace-default ()
  "handle backspace key in `*zero-state-im-preediting*' state"
  (let ((len (length zero-preedit-str)))
    (if (> len 1)
	(progn
	  (setq zero-preedit-str
		(substring zero-preedit-str 0 (1- len)))
	  (zero-preedit-str-changed))
      (zero-set-state *zero-state-im-waiting-input*)
      (zero-reset))))

(defun zero-backspace ()
  "handle backspace key in `*zero-state-im-preediting*' state"
  (interactive)
  (unless (eq zero-state *zero-state-im-preediting*)
    (error "zero-backspace called in non preediting state"))
  (zero-debug "zero-backspace\n")
  (funcall zero-backspace-func))

(defun zero-commit-text (text)
  "commit given text, reset preedit str, hide candidate list"
  (zero-debug "commit text: %s\n" text)
  (insert text)
  (setq zero-preedit-str "")
  (setq zero-candidates nil)
  (setq zero-current-page 0)
  (zero-hide-candidate-list))

(defun zero-return ()
  "handle RET key press in `*zero-state-im-preediting*' state"
  (interactive)
  (unless (eq zero-state *zero-state-im-preediting*)
    (error "zero-return called in non preediting state"))
  (zero-debug "zero-return\n")
  (zero-set-state *zero-state-im-waiting-input*)
  (zero-commit-text zero-preedit-str))

(defun zero-commit-nth-candidate (n)
  "commit nth candidate and return true if it exists, otherwise, return false"
  (let ((candidate (nth n (zero-candidates-on-page zero-candidates))))
    (if candidate
	(progn
	  (zero-set-state *zero-state-im-waiting-input*)
	  (zero-commit-text candidate)
	  t)
      nil)))

(defun zero-commit-preedit-str ()
  (zero-set-state *zero-state-im-waiting-input*)
  (zero-commit-text zero-preedit-str))

(defun zero-commit-first-candidate-or-preedit-str ()
  "commit first candidate if there is one, otherwise commit preedit str"
  (unless (zero-commit-nth-candidate 0)
    (zero-commit-preedit-str)))

(defun zero-hide-candidate-list ()
  (zero-panel-hide)
  (zero-debug "hide candidate list\n"))

(defun zero-reset ()
  (interactive)
  (zero-debug "zero-reset\n")
  (zero-set-state *zero-state-im-waiting-input*)
  (setq zero-preedit-str "")
  (setq zero-candidates nil)
  (setq zero-current-page 0)
  (zero-hide-candidate-list))

(defun zero-focus-in ()
  "a hook function. run when focus in a zero-mode buffer"
  (when (eq zero-state *zero-state-im-preediting*)
    (zero-show-candidates zero-candidates)
    (zero-enter-preedit-state)))

(defun zero-focus-out ()
  "a hook function. run when focus out a zero-mode buffer"
  (when (eq zero-state *zero-state-im-preediting*)
    (zero-hide-candidate-list)
    (zero-leave-preedit-state)))

(defun zero-buffer-list-changed ()
  "a hook function, run when buffer list has changed. This includes user has switched buffer"
  (if (eq (car (buffer-list)) zero-buffer)
      (zero-focus-in)))

;;============
;; minor mode
;;============

(defvar zero-mode-map
  '(keymap
    ;; C-.
    (67108910 . zero-cycle-punctuation-level)
    (remap keymap
	   (self-insert-command . zero-self-insert-command)))
  "`zero-mode' keymap")

(defun zero-enable-preediting-map ()
  "enable preediting keymap in `zero-mode-map'"
  (zero-debug "zero-enable-preediting-map\n")
  (define-key zero-mode-map (kbd "<backspace>") 'zero-backspace)
  (define-key zero-mode-map (kbd "RET") 'zero-return)
  (define-key zero-mode-map (kbd "<escape>") 'zero-reset))

(defun zero-disable-preediting-map ()
  "disable preediting keymap in `zero-mode-map'"
  (zero-debug "zero-disable-preediting-map\n")
  (define-key zero-mode-map (kbd "<backspace>") nil)
  (define-key zero-mode-map (kbd "RET") nil)
  (define-key zero-mode-map (kbd "<escape>") nil))

(define-minor-mode zero-mode
  "a Chinese input method framework written as an emacs minor mode.

\\{zero-mode-map}"
  nil
  " Zero"
  zero-mode-map
  ;; local variables and variable init
  (make-local-variable 'zero-state)
  (zero-set-state  *zero-state-im-off*)
  (make-local-variable 'zero-punctuation-level)
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
  "(re)register an input method in zero. After registration, you can use `zero-set-default-im' and `zero-set-im' to select input method to use.

im-name should be a symbol.
im-functions-alist should be a list of form
  '((:virtual-function-name . implementation-function-name))

virtual functions                       corresponding variable
===========================================================================
:build-candidates                       zero-build-candidates-func
:can-start-sequence                     zero-can-start-sequence-func
:handle-preedit-char                    zero-handle-preedit-char-func
:get-preedit-str-for-panel              zero-get-preedit-str-for-panel-func
:handle-backspace                       zero-backspace-func
:init                                   nil
:shutdown                               nil
:preedit-start                          zero-preedit-start-func
:preedit-end                            zero-preedit-end-func

registered input method is saved in `zero-ims'"
  ;; add or replace entry in `zero-ims'
  (unless (symbolp im-name)
    (signal 'wrong-type-argument (list 'symbolp im-name)))
  (setq zero-ims (assq-delete-all im-name zero-ims))
  (setq zero-ims (push (cons im-name im-functions-alist) zero-ims)))

;;============
;; public API
;;============

(defun zero-set-punctuation-level (level)
  "set `zero-punctuation-level'"
  (interactive)
  (if (not (member level (list *zero-punctuation-level-basic*
			       *zero-punctuation-level-full*
			       *zero-punctuation-level-none*)))
      (error "Level not supported: %s" level)
    (setq zero-punctuation-level level)))

(defun zero-set-punctuation-levels (levels)
  "set `zero-punctuation-levels'. `zero-cycle-punctuation-level' will cycle current `zero-punctuation-level' among defined levels"
  (dolist (level levels)
    (if (not (member level (list *zero-punctuation-level-basic*
				 *zero-punctuation-level-full*
				 *zero-punctuation-level-none*)))
	(error "Level not supported: %s" level)))
  (setq zero-punctuation-levels levels))

(defun zero-cycle-punctuation-level ()
  "cycle `zero-punctuation-level' among `zero-punctuation-levels'"
  (interactive)
  (setq zero-punctuation-level
	(zero-cycle-list zero-punctuation-levels zero-punctuation-level))
  (message "punctuation level set to %s" zero-punctuation-level))

(defun zero-set-im (im-name)
  "select zero input method for current buffer.

if im-name is nil, use default empty input method"
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

;;; zero-framework.el ends here
