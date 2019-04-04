;; -*- no-byte-compile: t; -*-
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
