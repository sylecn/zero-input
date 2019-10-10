;;; zero-pinyin-service.el --- Provide emacs interface for zero-pinyin-service dbus service. -*- lexical-binding: t -*-

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

;;; Code:

;;================
;; implementation
;;================

(require 'dbus)
(require 's)

(defun zero-pinyin-service-error-handler (event error)
  "Handle dbus errors.

EVENT, ERROR are arguments passed to the handler."
  (when (or (string-equal "com.emacsos.zero.ZeroPinyinService1"
			  (dbus-event-interface-name event))
	    (s-contains-p "com.emacsos.zero.ZeroPinyinService1" (cadr error)))
    (error "`zero-pinyin-service' dbus failed: %S" (cadr error))))

(add-hook 'dbus-event-error-functions 'zero-pinyin-service-error-handler)

(defun zero-pinyin-service-async-call (method handler &rest args)
  "Call METHOD on zero-pinin-service asynchronously.
This is a wrapper around `dbus-call-method-asynchronously'.
Argument HANDLER the handler function.
Optional argument ARGS extra arguments to pass to the wrapped function."
  (apply 'dbus-call-method-asynchronously
	 :session "com.emacsos.zero.ZeroPinyinService1"
	 "/com/emacsos/zero/ZeroPinyinService1"
	 "com.emacsos.zero.ZeroPinyinService1.ZeroPinyinServiceInterface"
	 method handler :timeout 1000 args))

(defun zero-pinyin-service-call (method &rest args)
  "Call METHOD on zero-pinin-service synchronously.
This is a wrapper around `dbus-call-method'.
Optional argument ARGS extra arguments to pass to the wrapped function."
  (apply 'dbus-call-method
	 :session "com.emacsos.zero.ZeroPinyinService1"
	 "/com/emacsos/zero/ZeroPinyinService1"
	 "com.emacsos.zero.ZeroPinyinService1.ZeroPinyinServiceInterface"
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

(ert-deftest zero-pinyin-candidate-pinyin-indices-to-dbus-format ()
  (should (equal (zero-pinyin-candidate-pinyin-indices-to-dbus-format '((22 31)))
		 '(:array (:struct :int32 22 :int32 31))))
  (should (equal (zero-pinyin-candidate-pinyin-indices-to-dbus-format
		  '((17 46) (7 55)))
		 '(:array (:struct :int32 17 :int32 46)
			  (:struct :int32 7 :int32 55)))))

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

;;================
;; some app test
;;================

(eval-when-compile (require 'cl-macs))

(ert-deftest zero-pinyin-service-get-candidates ()
  (cl-destructuring-bind (cs ls &rest rest)
      (zero-pinyin-service-get-candidates "liyifeng" 1)
    (should (or (and (equal (car cs) "李易峰")
		     (= (car ls) 8))
		(and (equal (car cs) "利益")
		     (= (car ls) 4)))))
  (cl-destructuring-bind (cs ls &rest rest)
      (zero-pinyin-service-get-candidates "wenti" 1)
    (should (equal (car cs) "问题"))
    (should (= (car ls) 5)))
  (cl-destructuring-bind (cs ls &rest rest)
      (zero-pinyin-service-get-candidates "meiyou" 1)
    (should (equal (car cs) "没有"))
    (should (= (car ls) 6)))
  (cl-destructuring-bind (cs ls &rest rest)
      (zero-pinyin-service-get-candidates "shi" 1)
    (should (equal (car cs) "是"))
    (should (= (car ls) 3)))
  (cl-destructuring-bind (cs ls &rest rest)
      (zero-pinyin-service-get-candidates "de" 1)
    (should (equal (car cs) "的"))
    (should (= (car ls) 2))))

(provide 'zero-pinyin-service)

;;; zero-pinyin-service.el ends here
