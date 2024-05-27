;;; zero-input-panel-minibuffer-test.el --- tests for zero-input-panel-minibuffer.el -*- lexical-binding: t -*-

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

(require 'ert)
(require 'zero-input-panel-minibuffer)

(ert-deftest zero-input-panel-minibuffer-zip-pair ()
  (should (equal
	   (zero-input-panel-minibuffer--zip-pair '(1 2 3 4 5 6) '("a" "b" "c"))
	   '((1 . "a") (2 . "b") (3 . "c"))))
  (should (equal
	   (zero-input-panel-minibuffer--zip-pair '(1 2 3 4 5 6) '())
	   nil)))

(ert-deftest zero-input-panel-minibuffer-make-string ()
  (should (equal
	   (zero-input-panel-minibuffer-make-string '("a" "b" "c"))
	   "1.a 2.b 3.c"))
  (should (equal
	   (zero-input-panel-minibuffer-make-string '("a" "b" "c" "d"))
	   "1.a 2.b 3.c 4.d")))

(provide 'zero-input-panel-minibuffer-test)

;;; zero-input-panel-minibuffer-test.el ends here
