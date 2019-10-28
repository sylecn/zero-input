;;; zero-input-pinyin-test.el --- tests for zero-input-pinyin.el

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

(require 'zero-input-pinyin)
(require 'ert)

(ert-deftest zero-input-pinyin-can-start-sequence ()
  (should (zero-input-pinyin-can-start-sequence ?a))
  (should (zero-input-pinyin-can-start-sequence ?l))
  (should (zero-input-pinyin-can-start-sequence ?m))
  (should (zero-input-pinyin-can-start-sequence ?z))
  (should-not (zero-input-pinyin-can-start-sequence ?1))
  (should-not (zero-input-pinyin-can-start-sequence ?.))
  (should-not (zero-input-pinyin-can-start-sequence ?i))
  (should-not (zero-input-pinyin-can-start-sequence ?u))
  (should-not (zero-input-pinyin-can-start-sequence ?v)))

(provide 'zero-input-pinyin-test)

;;; zero-input-pinyin-test.el ends here
