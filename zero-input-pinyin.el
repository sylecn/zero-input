;;; zero-input-pinyin.el --- A pinyin input method for zero-input-framework -*- lexical-binding: t -*-

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

;; To use this input method, add in Emacs init file:
;;
;;   (add-to-list 'load-path "~/fromsource/zero")  ;; omit if install from melpa
;;   (require 'zero-input-pinyin)
;;   (zero-input-set-default-im 'pinyin)
;;   ;; Now you may bind a key to zero-input-mode to make it easy to
;;   ;; switch on/off the input method.
;;   (global-set-key (kbd "<f5>") 'zero-input-mode)

;;; Code:

;;==============
;; dependencies
;;==============

(require 'zero-input-framework)
(require 'zero-input-pinyin-service)

;;===============================
;; basic data and emacs facility
;;===============================

;; these two var is only used in docstring to avoid checkdoc line-too-long
;; error.
(defvar zero-input-pinyin-service-interface-xml-file
  "/usr/share/dbus-1/interfaces/com.emacsos.zero.ZeroPinyinService1.ZeroPinyinServiceInterface.xml")
(defvar zero-input-pinyin-service-interface-xml-url
  "https://gitlab.emacsos.com/sylecn/zero-input-pinyin-service/blob/master/com.emacsos.zero.ZeroPinyinService1.ZeroPinyinServiceInterface.xml")
(defcustom zero-input-pinyin-fuzzy-flag 0
  "Non-nil means use this value as GetCandidatesV2 fuzzy_flag param.
see zero-input-pinyin-service dbus interface xml for document.

You can find the xml file locally at
`zero-input-pinyin-service-interface-xml-file' or online at
`zero-input-pinyin-service-interface-xml-url'."
  :type 'integer
  :group 'zero-input-pinyin)

(defvar-local zero-input-pinyin-state nil
  "Zero-input-pinyin internal state.  could be nil or
`zero-input-pinyin--state-im-partial-commit'.")
(defconst zero-input-pinyin--state-im-partial-commit 'IM-PARTIAL-COMMIT)

(defvar zero-input-pinyin-used-preedit-str-lengths nil
  "Accompany `zero-input-candidates', marks how many preedit-str chars are used for each candidate.")
(defvar zero-input-pinyin-candidates-pinyin-indices nil
  "Store GetCandidates dbus method candidates_pinyin_indices field.")
(defvar zero-input-pinyin-pending-str "")
(defvar zero-input-pinyin-pending-preedit-str "")
(defvar zero-input-pinyin-pending-pinyin-indices nil
  "Stores `zero-input-pinyin-pending-str' corresponds pinyin indices.")

;;=====================
;; key logic functions
;;=====================

(defun zero-input-pinyin-reset ()
  "Reset states."
  (setq zero-input-pinyin-state nil)
  (setq zero-input-pinyin-used-preedit-str-lengths nil)
  (setq zero-input-pinyin-pending-str "")
  (setq zero-input-pinyin-pending-preedit-str ""))

(defun zero-input-pinyin-init ()
  "Called when this im is turned on."
  (zero-input-pinyin-reset))

(defun zero-input-pinyin-preedit-start ()
  "Called when enter `zero-input--state-im-preediting' state."
  (define-key zero-input-mode-map [remap digit-argument] 'zero-input-digit-argument))

(defun zero-input-pinyin-preedit-end ()
  "Called when leave `zero-input--state-im-preediting' state."
  (define-key zero-input-mode-map [remap digit-argument] nil))

(defun zero-input-pinyin-shutdown ()
  "Called when this im is turned off."
  (define-key zero-input-mode-map [remap digit-argument] nil))

(defun zero-input-pinyin-build-candidates (preedit-str fetch-size)
  "Synchronously build candidates list.

PREEDIT-STR the preedit string.
FETCH-SIZE fetch at least this many candidates if possible."
  (zero-input-debug "zero-input-pinyin building candidate list synchronously\n")
  (let ((result (zero-input-pinyin-service-get-candidates preedit-str fetch-size)))
    (setq zero-input-fetch-size (max fetch-size (length (cl-first result))))
    (setq zero-input-pinyin-used-preedit-str-lengths (cl-second result))
    (setq zero-input-pinyin-candidates-pinyin-indices (cl-third result))
    (cl-first result)))

(defun zero-input-pinyin-build-candidates-async (preedit-str fetch-size complete-func)
  "Asynchronously build candidate list, when done call complete-func on it.

PREEDIT-STR the preedit string.
FETCH-SIZE fetch at least this many candidates if possible.
COMPLETE-FUNC the callback function when async call completes.  it's called with
              fetched candidates list as parameter."
  (zero-input-debug "zero-input-pinyin building candidate list asynchronously\n")
  (zero-input-pinyin-service-get-candidates-async
   preedit-str
   fetch-size
   (lambda (candidates matched_preedit_str_lengths candidates_pinyin_indices)
     (setq zero-input-pinyin-used-preedit-str-lengths matched_preedit_str_lengths)
     (setq zero-input-pinyin-candidates-pinyin-indices candidates_pinyin_indices)
     (setq zero-input-fetch-size (max fetch-size (length candidates)))
     ;; Note: with dynamic binding, this command result in (void-variable
     ;; complete-func) error.
     (funcall complete-func candidates))))

(defun zero-input-pinyin-can-start-sequence (ch)
  "Return t if char CH can start a preedit sequence."
  (and (>= ch ?a)
       (<= ch ?z)
       (not (= ch ?i))
       (not (= ch ?u))
       (not (= ch ?v))))

(defun zero-input-pinyin-pending-preedit-str-changed ()
  "Update zero states when pending preedit string changed."
  (setq zero-input-fetch-size 0)
  (setq zero-input-current-page 0)
  (zero-input-pinyin-build-candidates-async zero-input-pinyin-pending-preedit-str zero-input-initial-fetch-size 'zero-input-build-candidates-complete))

(defun zero-input-pinyin-commit-nth-candidate (n)
  "Commit Nth candidate and return true if it exists, otherwise, return false."
  (let* ((n-prime (+ n (* zero-input-candidates-per-page zero-input-current-page)))
	 (candidate (nth n-prime zero-input-candidates))
	 (used-len (when candidate
		     (nth n-prime zero-input-pinyin-used-preedit-str-lengths))))
    (when candidate
      (zero-input-debug
       "zero-input-pinyin-commit-nth-candidate\n    n=%s candidate=%s used-len=%s zero-input-pinyin-pending-preedit-str=%S\n"
       n candidate used-len zero-input-pinyin-pending-preedit-str)
      (cond
       ((null zero-input-pinyin-state)
	(if (= used-len (length zero-input-preedit-str))
	    (progn
	      (zero-input-debug "commit in full\n")
	      (zero-input-set-state zero-input--state-im-waiting-input)
	      (zero-input-commit-text candidate)
	      (zero-input-pinyin-service-commit-candidate-async
	       candidate
	       (nth n-prime zero-input-pinyin-candidates-pinyin-indices))
	      t)
	  (zero-input-debug "partial commit, in partial commit mode now.\n")
	  (setq zero-input-pinyin-state zero-input-pinyin--state-im-partial-commit)
	  (setq zero-input-pinyin-pending-str candidate)
	  (setq zero-input-pinyin-pending-preedit-str (substring zero-input-preedit-str used-len))
	  (setq zero-input-pinyin-pending-pinyin-indices
		(nth n-prime zero-input-pinyin-candidates-pinyin-indices))
	  (zero-input-pinyin-pending-preedit-str-changed)
	  t))
       ((eq zero-input-pinyin-state zero-input-pinyin--state-im-partial-commit)
	(if (= used-len (length zero-input-pinyin-pending-preedit-str))
	    (progn
	      (zero-input-debug "finishes partial commit\n")
	      (setq zero-input-pinyin-state nil)
	      (zero-input-set-state zero-input--state-im-waiting-input)
	      (zero-input-commit-text (concat zero-input-pinyin-pending-str candidate))
	      (zero-input-pinyin-service-commit-candidate-async
	       (concat zero-input-pinyin-pending-str candidate)
	       (append zero-input-pinyin-pending-pinyin-indices
		       (nth n-prime zero-input-pinyin-candidates-pinyin-indices)))
	      t)
	  (zero-input-debug "continue partial commit\n")
	  (setq zero-input-pinyin-pending-str (concat zero-input-pinyin-pending-str candidate))
	  (setq zero-input-pinyin-pending-preedit-str (substring zero-input-pinyin-pending-preedit-str used-len))
	  (setq zero-input-pinyin-pending-pinyin-indices
		(append zero-input-pinyin-pending-pinyin-indices
			(nth n-prime zero-input-pinyin-candidates-pinyin-indices)))
	  (zero-input-pinyin-service-commit-candidate-async
	   zero-input-pinyin-pending-str
	   zero-input-pinyin-pending-pinyin-indices)
	  (zero-input-pinyin-pending-preedit-str-changed)
	  t))
       (t (error "Unexpected zero-input-pinyin-state: %s" zero-input-pinyin-state))))))

(defun zero-input-pinyin-commit-first-candidate-or-preedit-str ()
  "Commit first candidate if there is one, otherwise, commit preedit string."
  (unless (zero-input-pinyin-commit-nth-candidate 0)
    (zero-input-commit-preedit-str)))

(defun zero-input-pinyin-commit-first-candidate-in-full ()
  "Commit first candidate and return t if it consumes all preedit-str.
Otherwise, just return nil."
  (let ((candidate (nth 0 (zero-input-candidates-on-page zero-input-candidates)))
	(used-len (nth (* zero-input-candidates-per-page zero-input-current-page) zero-input-pinyin-used-preedit-str-lengths)))
    (when candidate
      (cond
       ((null zero-input-pinyin-state)
	(when (= used-len (length zero-input-preedit-str))
	  (zero-input-set-state zero-input--state-im-waiting-input)
	  (zero-input-commit-text candidate)
	  t))
       ((eq zero-input-pinyin-state zero-input-pinyin--state-im-partial-commit)
	(when (= used-len (length zero-input-pinyin-pending-preedit-str))
	  (setq zero-input-pinyin-state nil)
	  (zero-input-set-state zero-input--state-im-waiting-input)
	  (zero-input-commit-text (concat zero-input-pinyin-pending-str candidate))
	  t))
       (t (error "Unexpected zero-input-pinyin-state: %s" zero-input-pinyin-state))))))

(defun zero-input-pinyin-page-down ()
  "Handle page down for zero-input-pinyin.

This is different from zero-input-framework because I need to support partial commit"
  (let ((len (length zero-input-candidates))
	(new-fetch-size (* zero-input-candidates-per-page (+ 2 zero-input-current-page))))
    (if (and (< len new-fetch-size)
	     (< zero-input-fetch-size new-fetch-size))
	(let ((preedit-str (if (eq zero-input-pinyin-state zero-input-pinyin--state-im-partial-commit) zero-input-pinyin-pending-preedit-str zero-input-preedit-str)))
	  (zero-input-pinyin-build-candidates-async
	   preedit-str
	   new-fetch-size
	   (lambda (candidates)
	     (zero-input-build-candidates-complete candidates)
	     (zero-input-just-page-down))))
      (zero-input-just-page-down))))

(defun zero-input-pinyin-handle-preedit-char (ch)
  "Hanlde character insert in `zero-input--state-im-preediting' state.
Override `zero-input-handle-preedit-char-default'.

CH the character user typed."
  (cond
   ((= ch ?\s)
    (zero-input-pinyin-commit-first-candidate-or-preedit-str))
   ((and (>= ch ?0) (<= ch ?9))
    ;; 1 commit the 0th candidate
    ;; 2 commit the 1st candidate
    ;; ...
    ;; 0 commit the 9th candidate
    (unless (zero-input-pinyin-commit-nth-candidate (mod (- (- ch ?0) 1) 10))
      (zero-input-append-char-to-preedit-str ch)
      (setq zero-input-pinyin-state nil)))
   ((= ch zero-input-previous-page-key)
    (zero-input-handle-preedit-char-default ch))
   ((= ch zero-input-next-page-key)
    (zero-input-pinyin-page-down))
   (t (let ((str (zero-input-convert-punctuation ch)))
	(if str
	    (when (zero-input-pinyin-commit-first-candidate-in-full)
	      (zero-input-set-state zero-input--state-im-waiting-input)
	      (insert str))
	  (setq zero-input-pinyin-state nil)
	  (zero-input-append-char-to-preedit-str ch))))))

(defun zero-input-pinyin-get-preedit-str-for-panel ()
  "Return the preedit string that should show in panel."
  (if (eq zero-input-pinyin-state zero-input-pinyin--state-im-partial-commit)
      (concat zero-input-pinyin-pending-str zero-input-pinyin-pending-preedit-str)
    zero-input-preedit-str))

(defun zero-input-pinyin-preedit-str-changed ()
  "Start over for candidate selection process."
  (setq zero-input-pinyin-state nil)
  (zero-input-preedit-str-changed))

(defun zero-input-pinyin-backspace ()
  "Handle backspace key in `zero-input--state-im-preediting' state."
  (if (eq zero-input-pinyin-state zero-input-pinyin--state-im-partial-commit)
      (zero-input-pinyin-preedit-str-changed)
    (zero-input-backspace-default)))

(defun zero-input-pinyin-delete-candidate (digit)
  "Tell backend to delete candidate at DIGIT position.

DIGIT is the digit key used to select nth candidate.
DIGIT 1 means delete 1st candidate.
DIGIT 2 means delete 2st candidate.
...
DIGIT 0 means delete 10th candidate."
  (let ((candidate (nth (mod (- digit 1) 10)
			(zero-input-candidates-on-page zero-input-candidates))))
    (when candidate
      (zero-input-pinyin-service-delete-candidates-async
       candidate 'zero-input-pinyin-preedit-str-changed))))

(defun zero-input-digit-argument ()
  "Allow C-<digit> to DeleteCandidate in `zero-input--state-im-preediting' state."
  (interactive)
  (unless (eq zero-input-state zero-input--state-im-preediting)
    (error "`zero-input-digit-argument' called in non preediting state"))
  (if (memq 'control (event-modifiers last-command-event))
      (let* ((char (if (integerp last-command-event)
		       last-command-event
		     (get last-command-event 'ascii-character)))
	     (digit (- (logand char ?\177) ?0)))
	(zero-input-pinyin-delete-candidate digit))))

;;===============================
;; register IM to zero framework
;;===============================

(zero-input-register-im
 'pinyin
 '((:build-candidates . zero-input-pinyin-build-candidates)
   ;; comment to use sync version, uncomment to use async version.
   ;; (:build-candidates-async . zero-input-pinyin-build-candidates-async)
   (:can-start-sequence . zero-input-pinyin-can-start-sequence)
   (:handle-preedit-char . zero-input-pinyin-handle-preedit-char)
   (:get-preedit-str-for-panel . zero-input-pinyin-get-preedit-str-for-panel)
   (:handle-backspace . zero-input-pinyin-backspace)
   (:init . zero-input-pinyin-init)
   (:shutdown . zero-input-pinyin-shutdown)
   (:preedit-start . zero-input-pinyin-preedit-start)
   (:preedit-end . zero-input-pinyin-preedit-end)))

;;============
;; public API
;;============

(provide 'zero-input-pinyin)

;;; zero-input-pinyin.el ends here
