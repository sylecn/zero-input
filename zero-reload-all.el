;;; -*- no-byte-compile: t; -*-
;;; zero-reload-all.el --- reload zero-el in correct order

;; Copyright 2019 Yuanle Song <sylecn@gmail.com>
;;
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

(defun zero-reload-all ()
  (interactive)
  (byte-recompile-directory "~/lisp/elisp/zero/" 0)
  (dolist (f '("zero-quickdial.elc"
	       "zero-panel.elc"
	       "zero-framework.elc"
	       "zero-table.elc"
	       "zero-pinyin-service.elc"
	       "zero-pinyin.elc"))
    (load-file f)))

;;; zero-reload-all.el ends here
