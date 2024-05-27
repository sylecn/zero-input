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

(defun zero-input-rebuild-zero-input (&optional source-dir)
  "Rebuild zero-input package.

SOURCE-DIR where to find the zero source dir."
  (interactive)
  ;; load dependency
  (let ((s-maybe (file-expand-wildcards "~/.emacs.d/elpa/s*/s.el")))
    (if (null s-maybe)
	(user-error "Package s is required for building zero-input")
      (load-file (car s-maybe))))
  (let ((source-dir (or source-dir "~/lisp/elisp/zero/")))
    (load-file (concat source-dir "byte-compile-flags.el"))
    (dolist (f '("zero-input-quickdial.el"
		 "zero-input-panel.el"
		 "zero-input-panel-test.el"
		 "zero-input-framework.el"
		 "zero-input-framework-test.el"
		 "zero-input-pinyin-service.el"
		 "zero-input-pinyin-service-test.el"
		 "zero-input-pinyin.el"
		 "zero-input-pinyin-test.el"
		 "zero-input-panel-minibuffer.el"
		 "zero-input-panel-minibuffer-test.el"
		 ))
      (byte-compile-file (concat source-dir f) t))))

(defun zero-input-rebuild-posframe (&optional source-dir)
  "Rebuild zero-input-panel-posframe.

SOURCE-DIR where to find the zero source dir."
  (interactive)
  ;; load dependency
  (zero-input-rebuild-zero-input)
  ;; use multiple file build of zero-input, just provide it.
  (provide 'zero-input)
  (let ((posframe-maybe (file-expand-wildcards "~/.emacs.d/elpa/posframe*/posframe.el")))
    (if (null posframe-maybe)
	(user-error "Package posframe is required for building zero-input"))
    (load-file (car posframe-maybe)))
  (let ((source-dir (or source-dir "~/lisp/elisp/zero/")))
    (load-file (concat source-dir "byte-compile-flags.el"))
    (dolist (f '("zero-input-panel-posframe.el"
		 ))
      (byte-compile-file (concat source-dir f) t))))

(defun zero-input-rebuild (&optional source-dir)
  "Rebuild zero-input and zero-input-panel-posframe.

SOURCE-DIR where to find the zero source dir."
  (interactive)
  (zero-input-rebuild-posframe))

(defun zero-input-reload-all (&optional source-dir)
  "Recompile and load all zero files.
Optional argument SOURCE-DIR path to zero-input source dir."
  (interactive)
  (let ((source-dir (or source-dir "~/lisp/elisp/zero/")))
    (load-file (concat source-dir "byte-compile-flags.el"))
    (byte-recompile-directory source-dir 0)
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
		 "zero-input-panel-minibuffer.elc"
		 "zero-input-panel-minibuffer-test.elc"
		 "zero-input-panel-posframe.elc"
		 ))
      (load-file (concat source-dir f)))))

;;; zero-input-reload-all.el ends here
