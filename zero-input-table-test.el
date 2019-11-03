;;; zero-input-table-test.el --- tests for zero-input-table.el -*- no-byte-compile: t; lexical-binding: t -*-

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

(require 'zero-input-table)
(require 'ert)

(defconst zero-input-table-table-demo
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
    ("now" . "__now__")))

(defmacro with-zero-input-table-demo-table (&rest body)
  `(let ((table-old zero-input-table-table)
	 (sequence-initials-old zero-input-table-sequence-initials))
     (zero-input-table-set-table zero-input-table-table-demo)
     ,@body
     (setq zero-input-table-table table-old)
     (setq zero-input-table-sequence-initials sequence-initials-old)))

(ert-deftest zero-input-table-build-candidates ()
  (with-zero-input-table-demo-table
   (should (equal (zero-input-table-build-candidates "ph") '("18612345678")))
   (should (equal (zero-input-table-build-candidates "m")
		  '("https://msdn.microsoft.com/en-us"
		    "foo@example.com"
		    "https://ditu.amap.com/")))))

(ert-deftest zero-input-table-can-start-sequence ()
  (with-zero-input-table-demo-table
   (should (zero-input-table-can-start-sequence ?a))
   (should (zero-input-table-can-start-sequence ?m))
   (should-not (zero-input-table-can-start-sequence ?1))
   (should-not (zero-input-table-can-start-sequence ?b))))

(provide 'zero-input-table-test)

;;; zero-input-table-test.el ends here
