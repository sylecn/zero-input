;;; zero-reload-all.el --- reload zero-el in correct order -*- no-byte-compile: t; -*-

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
  "Rebuild zero-el.

SOURCE-DIR where to find the zero source dir."
  (interactive)
  ;; for loading s
  (package-initialize)
  (let ((source-dir (or source-dir "~/lisp/elisp/zero/")))
    (dolist (f '("zero-quickdial.el"
		 "zero-panel.el"
		 "zero-panel-test.el"
		 "zero.el"
		 "zero-test.el"
		 "zero-pinyin-service.el"
		 "zero-pinyin-service-test.el"
		 "zero-pinyin.el"
		 "zero-pinyin-test.el"
		 ))
      (byte-compile-file (concat source-dir f) t))))

(defun zero-reload-all ()
  "Recompile and load all zero files."
  (interactive)
  (byte-recompile-directory "~/lisp/elisp/zero/" 0)
  (dolist (f '("zero-quickdial.elc"
	       "zero-panel.elc"
	       "zero-panel-test.elc"
	       "zero.elc"
	       "zero-test.elc"
	       "zero-pinyin-service.elc"
	       "zero-pinyin-service-test.elc"
	       "zero-pinyin.elc"
	       "zero-pinyin-test.elc"
	       "zero-table.el"
	       "zero-table-test.el"
	       ))
    (load-file f)))

;;; zero-reload-all.el ends here
