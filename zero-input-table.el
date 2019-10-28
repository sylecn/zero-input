;;; zero-input-table.el --- a demo table based input method based on zero.el -*- no-byte-compile: t; lexical-binding: t -*-

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

;; when you type the key in `zero-input-table-table', IM will insert the
;; corresponding value.
;;
;; To use this demo IM,
;;   (add-to-list 'load-path "~/fromsource/zero")
;;   (require 'zero-input-table)
;;   (zero-input-set-default-im 'zero-input-table) ; set as default IM
;;   or (zero-input-set-im 'zero-input-table)      ; set as current buffer's IM

;;; Code:

;;==============
;; dependencies
;;==============

(require 'zero-input-framework)

;;===============================
;; basic data and emacs facility
;;===============================

(defvar zero-input-table-table nil
  "The table used by zero-input-table input method, map string to string.")
(defvar zero-input-table-sequence-initials nil "Used in `zero-input-table-can-start-sequence'.")

;;=====================
;; key logic functions
;;=====================

(defun zero-input-table-sort-key (lhs rhs)
  "A predicate function to sort candidates.  Return t if LHS should sort before RHS."
  (string< (car lhs) (car rhs)))

(defun zero-input-table-build-candidates (preedit-str &optional _fetch-size)
  "Build candidates by looking up PREEDIT-STR in `zero-input-table-table'."
  (mapcar 'cdr (sort (cl-remove-if-not (lambda (pair) (string-prefix-p preedit-str (car pair))) zero-input-table-table) 'zero-input-table-sort-key)))

;; (defun zero-input-table-build-candidates-async (preedit-str)
;;   "build candidate list, when done show it via `zero-input-table-show-candidates'"
;;   (zero-input-table-debug "building candidate list\n")
;;   (let ((candidates (zero-input-table-build-candidates preedit-str)))
;;     ;; update cache to make SPC and digit key selection possible.
;;     (setq zero-input-table-candidates candidates)
;;     (zero-input-table-show-candidates candidates)))

(defun zero-input-table-can-start-sequence (ch)
  "Return t if char CH can start a preedit sequence."
  (member (make-string 1 ch) zero-input-table-sequence-initials))

;;===============================
;; register IM to zero framework
;;===============================

(zero-input-register-im
 'zero-input-table
 '((:build-candidates . zero-input-table-build-candidates)
   (:can-start-sequence . zero-input-table-can-start-sequence)))

;;============
;; public API
;;============

(defun zero-input-table-set-table (alist)
  "Set the conversion table.

the ALIST should be a list of (key . value) pairs.  when user type
\(part of) key, the IM will show all matching value.

e.g.
'((\"phone\" . \"18612345678\")
  (\"mail\" . \"foo@example.com\")
  (\"map\" . \"https://ditu.amap.com/\")
  (\"m\" . \"https://msdn.microsoft.com/en-us\")
  (\"address\" . \"123 Happy Street\"))"
  (setq zero-input-table-table alist)
  (setq zero-input-table-sequence-initials
	(delete-dups (mapcar (lambda (pair) (substring (car pair) 0 1))
			     zero-input-table-table))))

;;===========
;; test data
;;===========

(unless zero-input-table-table
  (zero-input-table-set-table
   '(("phone" . "18612345678")
     ("pyl" . "http://localdocs.emacsos.com/python2/library/%s.html")
     ("pyli" . "http://localdocs.emacsos.com/python2/index.html")
     ("pylm" . "http://localdocs.emacsos.com/python2/py-modindex.html")
     ("py3li" . "http://localdocs.emacsos.com/python2/index.html")
     ("py3l" . "http://localdocs.emacsos.com/python3/library/%s.html")
     ("py3lm" . "http://localdocs.emacsos.com/python3/py-modindex.html")
     ("pyop" . "http://docs.python.org/library/operator.html")
     ("pyopl" . "http://localdocs.emacsos.com/python2/library/operator.html")
     ("pympl" . "http://localdocs.emacsos.com/python2/library/multiprocessing.html")
     ("py2" . "http://docs.python.org/2/library/%s.html")
     ("py3" . "http://docs.python.org/3/library/%s.html")
     ("py2i" . "http://docs.python.org/2/")
     ("py2m" . "http://docs.python.org/2/py-modindex.html")
     ("py3i" . "http://docs.python.org/3/")
     ("py3m" . "http://docs.python.org/3/py-modindex.html")
     ("pycodec" . "http://localdocs.emacsos.com/python2/library/codecs.html#standard-encodings")
     ("pycodecs" . "http://localdocs.emacsos.com/python2/library/codecs.html#standard-encodings")
     ("pycodecsr" . "http://docs.python.org/library/codecs.html#standard-encodings")
     ("pycodecr" . "http://docs.python.org/library/codecs.html#standard-encodings")
     ("pep328" . "http://www.python.org/dev/peps/pep-0328/")
     ("mail" . "foo@example.com")
     ("map" . "https://ditu.amap.com/")
     ("m" . "https://msdn.microsoft.com/en-us")
     ("address" . "123 Happy Street")
     ("da" . "__da__")
     ("now" . "__now__"))))

(provide 'zero-input-table)

;;; zero-input-table.el ends here
