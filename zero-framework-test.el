;;; zero-framework-test.el --- tests for zero-framework.el -*- lexical-binding: t -*-

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

;; tests for zero-framework.el

;;; Code:

(require 'zero-framework)
(require 'ert)

(ert-deftest zero-cycle-list ()
  (should (= (zero-cycle-list '(1 2 3) 1) 2))
  (should (eq (zero-cycle-list '(a b c) 'a) 'b))
  (should (eq (zero-cycle-list '(a b c) 'b) 'c))
  (should (eq (zero-cycle-list '(a b c) 'c) 'a))
  (should (eq (zero-cycle-list '(a b c) 'd) nil)))

(ert-deftest zero-convert-ch-to-full-width ()
  (should (= (zero-convert-ch-to-full-width ?\!) ?\！)))

(ert-deftest zero-convert-str-to-full-width ()
  (should (string-equal "！" (zero-convert-str-to-full-width "!")))
  (should (string-equal "（" (zero-convert-str-to-full-width "(")))
  (should (string-equal "（：）" (zero-convert-str-to-full-width "(:)")))
  (should (string-equal "ＡＢａｂ" (zero-convert-str-to-full-width "ABab")))
  (should (string-equal "ｈｅｈｅ" (zero-convert-str-to-full-width "hehe")))
  (should (string-equal "（Ａ）" (zero-convert-str-to-full-width "(A)"))))

(provide 'zero-framework-test)

;;; zero-framework-test.el ends here
