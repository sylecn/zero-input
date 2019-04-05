;; -*- lexical-binding: t -*-
;; a demo table based input method based on zero-framework.el
;;
;; when you type the key in `zero-table-table', IM will insert the
;; corresponding value.
;;
;; To use this demo IM,
;;   (add-to-list 'load-path "~/lisp/elisp/zero")
;;   (require 'zero-table)
;;   (zero-set-default-im 'zero-table) ; set as default IM
;;   or (zero-set-im 'zero-table)      ; set as current buffer's IM

;;==============
;; dependencies
;;==============

(require 's)
(require 'zero-framework)

;;===============================
;; basic data and emacs facility
;;===============================

(defvar zero-table-table nil "zero-table's table, map string to string")
(defvar zero-table-sequence-initials nil "used in `zero-table-can-start-sequence'")

;;=====================
;; key logic functions
;;=====================

(defun zero-table-sort-key (lhs rhs)
  "a predicate function to sort candidates. return t if lhs
should sort before rhs."
  (string< (car lhs) (car rhs)))

(defun zero-table-build-candidates (preedit-str &optional _fetch-size)
  (mapcar 'cdr (sort (cl-remove-if-not (lambda (pair) (string-prefix-p preedit-str (car pair))) zero-table-table) 'zero-table-sort-key)))

(ert-deftest zero-table-build-candidates ()
  (should (equal (zero-table-build-candidates "ph") '("18612345678")))
  (should (equal (zero-table-build-candidates "m") '("https://msdn.microsoft.com/en-us"
						     "foo@example.com"
						     "https://ditu.amap.com/"))))

;; (defun zero-table-build-candidates-async (preedit-str)
;;   "build candidate list, when done show it via `zero-table-show-candidates'"
;;   (zero-table-debug "building candidate list\n")
;;   (let ((candidates (zero-table-build-candidates preedit-str)))
;;     ;; update cache to make SPC and digit key selection possible.
;;     (setq zero-table-candidates candidates)
;;     (zero-table-show-candidates candidates)))

(defun zero-table-can-start-sequence (ch)
  "return t if char ch can start a preedit sequence."
  (member (make-string 1 ch) zero-table-sequence-initials))

(ert-deftest zero-table-can-start-sequence ()
  (should (zero-table-can-start-sequence ?a))
  (should (zero-table-can-start-sequence ?m))
  (should-not (zero-table-can-start-sequence ?1))
  (should-not (zero-table-can-start-sequence ?b)))

;;===============================
;; register IM to zero framework
;;===============================

(zero-register-im
 'zero-table
 '((:build-candidates . zero-table-build-candidates)
   (:can-start-sequence . zero-table-can-start-sequence)))

;;============
;; public API
;;============

(defun zero-table-set-table (alist)
  "set the conversion table.

the alist should be a list of (key . value) pairs. when user type
(part of) key, the IM will show all matching value.

e.g.
'((\"phone\" . \"18612345678\")
  (\"mail\" . \"foo@example.com\")
  (\"map\" . \"https://ditu.amap.com/\")
  (\"m\" . \"https://msdn.microsoft.com/en-us\")
  (\"address\" . \"123 Happy Street\"))
"
  (setq zero-table-table alist)
  (setq zero-table-sequence-initials
	(delete-dups (mapcar (lambda (pair) (substring (car pair) 0 1))
			     zero-table-table))))

;;===========
;; test data
;;===========

(unless zero-table-table
  (zero-table-set-table
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

(provide 'zero-table)
