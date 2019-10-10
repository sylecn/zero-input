;;; zero-panel-test.el --- tests for zero-panel.el -*- lexical-binding: t -*-

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
(require 'zero-panel)

(ert-deftest zero-alist-to-asv ()
  (should (equal (zero-alist-to-asv nil) '(:array :signature "{sv}")))
  (should (equal (zero-alist-to-asv
		  '(("name" "foo")
		    ("timeout" :int32 10)))
		 '(:array
		   (:dict-entry "name" (:variant "foo"))
		   (:dict-entry "timeout" (:variant :int32 10))))))

(provide 'zero-panel-test)

;;; zero-panel-test.el ends here
