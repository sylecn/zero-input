;;; zero-input-framework-test.el --- tests for zero-input-framework.el -*- lexical-binding: t -*-

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

;; tests for zero-input-framework.el

;;; Code:

(require 'zero-input-framework)
(require 'ert)

(ert-deftest zero-input-cycle-list ()
  (should (= (zero-input-cycle-list '(1 2 3) 1) 2))
  (should (eq (zero-input-cycle-list '(a b c) 'a) 'b))
  (should (eq (zero-input-cycle-list '(a b c) 'b) 'c))
  (should (eq (zero-input-cycle-list '(a b c) 'c) 'a))
  (should (eq (zero-input-cycle-list '(a b c) 'd) nil)))

(ert-deftest zero-input-convert-ch-to-full-width ()
  (should (= (zero-input-convert-ch-to-full-width ?\!) ?\！))
  (should (= (zero-input-convert-ch-to-full-width ?\s) ?\u3000)))

(ert-deftest zero-input-convert-str-to-full-width ()
  (should (string-equal "！" (zero-input-convert-str-to-full-width "!")))
  (should (string-equal "（" (zero-input-convert-str-to-full-width "(")))
  (should (string-equal "（：）" (zero-input-convert-str-to-full-width "(:)")))
  (should (string-equal "ＡＢａｂ" (zero-input-convert-str-to-full-width "ABab")))
  (should (string-equal "ｈｅｈｅ" (zero-input-convert-str-to-full-width "hehe")))
  (should (string-equal "（Ａ）" (zero-input-convert-str-to-full-width "(A)"))))

(provide 'zero-input-framework-test)

;;; zero-input-framework-test.el ends here
