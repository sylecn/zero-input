;;; zero-input-pinyin-service-test.el --- tests for zero-input-pinyin-service.el -*- lexical-binding: t -*-

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

;;

;;; Code:

(require 'zero-input-pinyin-service)
(require 'ert)
(eval-when-compile (require 'cl-macs))

(ert-deftest zero-input-pinyin-candidate-pinyin-indices-to-dbus-format ()
  (should (equal (zero-input-pinyin-candidate-pinyin-indices-to-dbus-format '((22 31)))
		 '(:array (:struct :int32 22 :int32 31))))
  (should (equal (zero-input-pinyin-candidate-pinyin-indices-to-dbus-format
		  '((17 46) (7 55)))
		 '(:array (:struct :int32 17 :int32 46)
			  (:struct :int32 7 :int32 55)))))

(ert-deftest zero-input-pinyin-service-get-candidates ()
  (cl-destructuring-bind (cs ls &rest rest)
      (zero-input-pinyin-service-get-candidates "liyifeng" 1)
    (should (or (and (equal (car cs) "李易峰")
		     (= (car ls) 8))
		(and (equal (car cs) "利益")
		     (= (car ls) 4)))))
  (cl-destructuring-bind (cs ls &rest rest)
      (zero-input-pinyin-service-get-candidates "wenti" 1)
    (should (equal (car cs) "问题"))
    (should (= (car ls) 5)))
  (cl-destructuring-bind (cs ls &rest rest)
      (zero-input-pinyin-service-get-candidates "meiyou" 1)
    (should (equal (car cs) "没有"))
    (should (= (car ls) 6)))
  (cl-destructuring-bind (cs ls &rest rest)
      (zero-input-pinyin-service-get-candidates "shi" 1)
    (should (equal (car cs) "是"))
    (should (= (car ls) 3)))
  (cl-destructuring-bind (cs ls &rest rest)
      (zero-input-pinyin-service-get-candidates "de" 1)
    (should (equal (car cs) "的"))
    (should (= (car ls) 2))))

(provide 'zero-input-pinyin-service-test)

;;; zero-input-pinyin-service-test.el ends here
