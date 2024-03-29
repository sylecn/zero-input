;;; zero-input-reload-all.el --- reload zero-input-el in correct order -*- no-byte-compile: t; -*-

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

(defun zero-input-rebuild (&optional source-dir)
  "Rebuild zero-input-el.

SOURCE-DIR where to find the zero source dir."
  (interactive)
  ;; for loading s
  (package-initialize)
  (let ((source-dir (or source-dir "~/lisp/elisp/zero/")))
    (dolist (f '("zero-input-quickdial.el"
		 "zero-input-panel.el"
		 "zero-input-panel-test.el"
		 "zero-input-framework.el"
		 "zero-input-framework-test.el"
		 "zero-input-pinyin-service.el"
		 "zero-input-pinyin-service-test.el"
		 "zero-input-pinyin.el"
		 "zero-input-pinyin-test.el"
		 ))
      (byte-compile-file (concat source-dir f) t))))

(defun zero-input-reload-all ()
  "Recompile and load all zero files."
  (interactive)
  (byte-recompile-directory "~/lisp/elisp/zero/" 0)
  (dolist (f '("zero-input-quickdial.elc"
	       "zero-input-panel.elc"
	       "zero-input-panel-test.elc"
	       "zero-input-framework.elc"
	       "zero-input-framework-test.elc"
	       "zero-input-pinyin-service.elc"
	       "zero-input-pinyin-service-test.elc"
	       "zero-input-pinyin.elc"
	       "zero-input-pinyin-test.elc"
	       "zero-input-table.el"
	       "zero-input-table-test.el"
	       ))
    (load-file f)))

;;; zero-input-reload-all.el ends here
