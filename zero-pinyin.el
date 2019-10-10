;;; zero-pinyin.el --- A pinyin input method for zero-framework -*- lexical-binding: t -*-

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
;;   (require 'zero-pinyin)
;;   (zero-set-default-im 'pinyin)

;;; Code:

;;==============
;; dependencies
;;==============

(require 'zero-framework)
(require 'zero-pinyin-service)

;;===============================
;; basic data and emacs facility
;;===============================

(defvar zero-pinyin-state nil "Zero-pinyin internal state.  could be nil or `*zero-pinyin-state-im-partial-commit*'.")
(defconst *zero-pinyin-state-im-partial-commit* 'IM-PARTIAL-COMMIT)

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
  (setq zero-pinyin-pending-preedit-str ""))

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

(ert-deftest zero-pinyin-can-start-sequence ()
  (should (zero-pinyin-can-start-sequence ?a))
  (should (zero-pinyin-can-start-sequence ?l))
  (should (zero-pinyin-can-start-sequence ?m))
  (should (zero-pinyin-can-start-sequence ?z))
  (should-not (zero-pinyin-can-start-sequence ?1))
  (should-not (zero-pinyin-can-start-sequence ?.))
  (should-not (zero-pinyin-can-start-sequence ?i))
  (should-not (zero-pinyin-can-start-sequence ?u))
  (should-not (zero-pinyin-can-start-sequence ?v)))

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
	      (zero-set-state *zero-state-im-waiting-input*)
	      (zero-commit-text candidate)
	      (zero-pinyin-service-commit-candidate-async
	       candidate
	       (nth n-prime zero-pinyin-candidates-pinyin-indices))
	      t)
	  (zero-debug "partial commit, in partial commit mode now.\n")
	  (setq zero-pinyin-state *zero-pinyin-state-im-partial-commit*)
	  (setq zero-pinyin-pending-str candidate)
	  (setq zero-pinyin-pending-preedit-str (substring zero-preedit-str used-len))
	  (setq zero-pinyin-pending-pinyin-indices
		(nth n-prime zero-pinyin-candidates-pinyin-indices))
	  (zero-pinyin-pending-preedit-str-changed)
	  t))
       ((eq zero-pinyin-state *zero-pinyin-state-im-partial-commit*)
	(if (= used-len (length zero-pinyin-pending-preedit-str))
	    (progn
	      (zero-debug "finishes partial commit\n")
	      (setq zero-pinyin-state nil)
	      (zero-set-state *zero-state-im-waiting-input*)
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
	  (zero-set-state *zero-state-im-waiting-input*)
	  (zero-commit-text candidate)
	  t))
       ((eq zero-pinyin-state *zero-pinyin-state-im-partial-commit*)
	(when (= used-len (length zero-pinyin-pending-preedit-str))
	  (setq zero-pinyin-state nil)
	  (zero-set-state *zero-state-im-waiting-input*)
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
	(let ((preedit-str (if (eq zero-pinyin-state *zero-pinyin-state-im-partial-commit*) zero-pinyin-pending-preedit-str zero-preedit-str)))
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
	      (zero-set-state *zero-state-im-waiting-input*)
	      (insert str))
	  (setq zero-pinyin-state nil)
	  (zero-append-char-to-preedit-str ch))))))

(defun zero-pinyin-get-preedit-str-for-panel ()
  "Return the preedit string that should show in panel."
  (if (eq zero-pinyin-state *zero-pinyin-state-im-partial-commit*)
      (concat zero-pinyin-pending-str zero-pinyin-pending-preedit-str)
    zero-preedit-str))

(defun zero-pinyin-preedit-str-changed ()
  "Start over for candidate selection process."
  (setq zero-pinyin-state nil)
  (zero-preedit-str-changed))

(defun zero-pinyin-backspace ()
  "Handle backspace key in `*zero-state-im-preediting*' state."
  (if (eq zero-pinyin-state *zero-pinyin-state-im-partial-commit*)
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
  (unless (eq zero-state *zero-state-im-preediting*)
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

;;; zero-pinyin.el ends here
