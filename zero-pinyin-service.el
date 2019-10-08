;;; -*- lexical-binding: t -*-
;;; zero-pinyin-service.el --- provide emacs interface for zero-pinyin-service dbus service.

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
(require 'cl-lib)

(defun zero-pinyin-service-error-handler (event error)
  "handle dbus errors"
  (when (or (string-equal "com.emacsos.zero.ZeroPinyinService1"
			  (dbus-event-interface-name event))
	    (s-contains-p "com.emacsos.zero.ZeroPinyinService1" (cadr error)))
    (error "zero-pinyin-service dbus failed: %S" (cadr error))))

(add-hook 'dbus-event-error-functions 'zero-pinyin-service-error-handler)

(defun zero-pinyin-service-async-call (method handler &rest args)
  "call Method on zero-pinin-service asynchronously. This is a wrapper around `dbus-call-method-asynchronously'"
  (apply 'dbus-call-method-asynchronously
	 :session "com.emacsos.zero.ZeroPinyinService1"
	 "/com/emacsos/zero/ZeroPinyinService1"
	 "com.emacsos.zero.ZeroPinyinService1.ZeroPinyinServiceInterface"
	 method handler :timeout 1000 args))

(defun zero-pinyin-service-call (method &rest args)
  "call Method on zero-pinin-service synchronously. This is a wrapper around `dbus-call-method'"
  (apply 'dbus-call-method
	 :session "com.emacsos.zero.ZeroPinyinService1"
	 "/com/emacsos/zero/ZeroPinyinService1"
	 "com.emacsos.zero.ZeroPinyinService1.ZeroPinyinServiceInterface"
	 method :timeout 1000 args))

;;============
;; public API
;;============

(defun zero-pinyin-service-get-candidates (preedit-str fetch-size)
  "get candidates for pinyin in preedit-str synchronously.

preedit-str the preedit-str, should be pure pinyin string
fetch-size try to fetch this many candidates or more"
  (zero-pinyin-service-call "GetCandidates" :string preedit-str :uint32 fetch-size))

(defun zero-pinyin-service-get-candidates-async (preedit-str fetch-size get-candidates-complete)
  "get candidates for pinyin in preedit-str asynchronously.

preedit-str the preedit-str, should be pure pinyin string
fetch-size try to fetch this many candidates or more"
  (zero-pinyin-service-async-call
   "GetCandidates" get-candidates-complete :string preedit-str :uint32 fetch-size))

(defun zero-pinyin-candidate-pinyin-indices-to-dbus-format (candidate_pinyin_indices)
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
  "commit candidate asynchronously"
  ;; don't care about the result, so no callback.
  (zero-pinyin-service-async-call
   "CommitCandidate" nil
   :string candidate
   (zero-pinyin-candidate-pinyin-indices-to-dbus-format candidate_pinyin_indices)))

(defun zero-pinyin-service-delete-candidates-async (candidate delete-candidate-complete)
  "delete candidate asynchronously"
  (zero-pinyin-service-async-call
   "DeleteCandidate" delete-candidate-complete :string candidate))

(defun zero-pinyin-service-quit ()
  "quit panel application"
  (zero-pinyin-service-async-call "Quit" nil))

;;================
;; some app test
;;================

(require 'cl-macs)

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
