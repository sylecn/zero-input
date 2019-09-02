;;; -*- no-byte-compile: t; -*-
;;; zero-reload-all.el --- reload zero-el in correct order

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

(defun zero-rebuild (&optional source-dir)
  "rebuild zero-el"
  (interactive)
  ;; for loading s
  (package-initialize)
  (let ((source-dir (or source-dir "~/lisp/elisp/zero/")))
    (dolist (f '("zero-quickdial.el"
		 "zero-panel.el"
		 "zero-framework.el"
		 "zero-pinyin-service.el"
		 "zero-pinyin.el"))
      (byte-compile-file (concat source-dir f) t))))

(defun zero-reload-all ()
  (interactive)
  (byte-recompile-directory "~/lisp/elisp/zero/" 0)
  (dolist (f '("zero-quickdial.elc"
	       "zero-panel.elc"
	       "zero-framework.elc"
	       "zero-table.el"
	       "zero-pinyin-service.elc"
	       "zero-pinyin.elc"))
    (load-file f)))

;;; zero-reload-all.el ends here
