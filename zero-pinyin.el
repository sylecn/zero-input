;; a pinyin input method for zero-framework

;;==============
;; dependencies
;;==============

(require 's)
(require 'zero-framework)

;;===============================
;; basic data and emacs facility
;;===============================

(defvar zero-pinyin-state nil "zero-pinyin internal state. could be nil or `*zero-pinyin-state-im-partial-commit*'")
(defconst *zero-pinyin-state-im-partial-commit* 'IM-PARTIAL-COMMIT)

(defvar zero-pinyin-used-preedit-str-lengths nil
  "accompany `zero-candidates', marks how many preedit-str chars are used for each candidate")
(defvar zero-pinyin-pending-str "")
(defvar zero-pinyin-pending-preedit-str "")

;;=====================
;; key logic functions
;;=====================

(defun zero-pinyin-reset ()
  (setq zero-pinyin-state nil)
  (setq zero-pinyin-used-preedit-str-lengths nil)
  (setq zero-pinyin-pending-str "")
  (setq zero-pinyin-pending-preedit-str ""))

(defun zero-pinyin-init ()
  (make-local-variable 'zero-pinyin-state)
  (zero-pinyin-reset))

(defun zero-pinyin-build-candidates (preedit-str)
  (zero-pinyin-build-candidates-test preedit-str))

;; (defun zero-pinyin-build-candidates-async (preedit-str)
;;   "build candidate list, when done show it via `zero-pinyin-show-candidates'"
;;   (zero-pinyin-debug "building candidate list\n")
;;   (let ((candidates (zero-pinyin-build-candidates preedit-str)))
;;     ;; update cache to make SPC and digit key selection possible.
;;     (setq zero-pinyin-candidates candidates)
;;     (zero-pinyin-show-candidates candidates)))

(defun zero-pinyin-can-start-sequence (ch)
  "return t if char ch can start a preedit sequence."
  (and (>= ch ?a)
       (<= ch ?z)
       (not (= ch ?u))
       (not (= ch ?v))))

(ert-deftest zero-pinyin-can-start-sequence ()
  (should (zero-pinyin-can-start-sequence ?a))
  (should (zero-pinyin-can-start-sequence ?l))
  (should (zero-pinyin-can-start-sequence ?m))
  (should (zero-pinyin-can-start-sequence ?z))
  (should-not (zero-pinyin-can-start-sequence ?1))
  (should-not (zero-pinyin-can-start-sequence ?.))
  (should-not (zero-pinyin-can-start-sequence ?u))
  (should-not (zero-pinyin-can-start-sequence ?v)))

(defun zero-pinyin-pending-preedit-str-changed ()
  (zero-build-candidates-async zero-pinyin-pending-preedit-str))

(defun zero-pinyin-commit-nth-candidate (n)
  "commit nth candidate and return true if it exists, otherwise, return false"
  (let* ((candidate (nth n (zero-candidates-on-page zero-candidates)))
	 (used-len (when candidate
		     (nth (+ n (* zero-candidates-per-page zero-current-page)) zero-pinyin-used-preedit-str-lengths))))
    (when candidate
      (zero-debug
       "zero-pinyin-commit-nth-candidate n=%s candidate=%s used-len=%s zero-pinyin-pending-preedit-str=%s\n"
       n candidate used-len zero-pinyin-pending-preedit-str)
      (cond
       ((null zero-pinyin-state)
	(if (= used-len (length zero-preedit-str))
	    (progn
	      (zero-debug "commit in full\n")
	      (zero-set-state *zero-state-im-waiting-input*)
	      (zero-commit-text candidate)
	      t)
	  (zero-debug "partial commit, in partial commit mode now.\n")
	  (setq zero-pinyin-state *zero-pinyin-state-im-partial-commit*)
	  (setq zero-pinyin-pending-str candidate)
	  (setq zero-pinyin-pending-preedit-str (substring zero-preedit-str used-len))
	  (zero-pinyin-pending-preedit-str-changed)
	  t))
       ((eq zero-pinyin-state *zero-pinyin-state-im-partial-commit*)
	(if (= used-len (length zero-pinyin-pending-preedit-str))
	    (progn
	      (zero-debug "finishes partial commit\n")
	      (setq zero-pinyin-state nil)
	      (zero-set-state *zero-state-im-waiting-input*)
	      (zero-commit-text (concat zero-pinyin-pending-str candidate))
	      t)
	  (zero-debug "continue partial commit\n")
	  (setq zero-pinyin-pending-str (concat zero-pinyin-pending-str candidate))
	  (setq zero-pinyin-pending-preedit-str (substring zero-pinyin-pending-preedit-str used-len))
	  (zero-pinyin-pending-preedit-str-changed)
	  t))
       (t (error "unexpected zero-pinyin-state: %s" zero-pinyin-state))))))

(defun zero-pinyin-commit-first-candidate-or-preedit-str ()
  (unless (zero-pinyin-commit-nth-candidate 0)
    (zero-commit-preedit-str)))

(defun zero-pinyin-commit-first-candidate-in-full ()
  "commit first candidate and return t if first candidate consumes all preedit-str.
otherwise, just return nil"
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
       (t (error "unexpected zero-pinyin-state: %s" zero-pinyin-state))))))

(defun zero-pinyin-handle-preedit-char (ch)
  "hanlde character insert in `*zero-state-im-preediting*' state. overrides `zero-handle-preedit-char-default'"
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
   ((or (= ch zero-previous-page-key)
	(= ch zero-next-page-key))
    (zero-handle-preedit-char-default ch))
   (t (let ((str (zero-convert-punctuation ch)))
	(if str
	    (when (zero-pinyin-commit-first-candidate-in-full)
	      (zero-set-state *zero-state-im-waiting-input*)
	      (insert str))
	  (setq zero-pinyin-state nil)
	  (zero-append-char-to-preedit-str ch))))))

(defun zero-pinyin-get-preedit-str-for-panel ()
  (if (eq zero-pinyin-state *zero-pinyin-state-im-partial-commit*)
      (concat zero-pinyin-pending-str zero-pinyin-pending-preedit-str)
    zero-preedit-str))

(defun zero-pinyin-backspace ()
  "handle backspace key in `*zero-state-im-preediting*' state"
  (if (eq zero-pinyin-state *zero-pinyin-state-im-partial-commit*)
      (progn
	;; start over for candidate selection process.
	(setq zero-pinyin-state nil)
	(zero-preedit-str-changed))
    (zero-backspace-default)))

;;===============================
;; register IM to zero framework
;;===============================

(zero-register-im
 'pinyin
 '((:build-candidates . zero-pinyin-build-candidates)
   (:can-start-sequence . zero-pinyin-can-start-sequence)
   (:handle-preedit-char . zero-pinyin-handle-preedit-char)
   (:get-preedit-str-for-panel . zero-pinyin-get-preedit-str-for-panel)
   (:handle-backspace . zero-pinyin-backspace)
   (:init . zero-pinyin-init)))

;;============
;; public API
;;============

;;===========
;; test data
;;===========

(defun zero-pinyin-build-candidates-test (preedit-str)
  "test data for testing partial commit"
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
