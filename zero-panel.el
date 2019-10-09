;;; -*- lexical-binding: t -*-
;;; zero-panel --- provide emacs interface for zero-panel dbus service.

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

;;================
;; implementation
;;================

(require 'dbus)
(require 's)

(defun zero-panel-error-handler (event error)
  "Handle dbus errors.

EVENT and ERROR are error-handler arguments."
  (when (or (string-equal "com.emacsos.zero.Panel"
			  (dbus-event-interface-name event))
	    (s-contains-p "com.emacsos.zero.Panel" (cadr error)))
    (error "zero-panel dbus failed: %S" (cadr error))))

(add-hook 'dbus-event-error-functions 'zero-panel-error-handler)

(defun zero-panel-async-call (method _handler &rest args)
  "Call METHOD on zero-panel service asynchronously.

This is a wrapper around `dbus-call-method-asynchronously'.
ARGS optional extra args to pass to the wrapped function."
  (apply 'dbus-call-method-asynchronously
	 :session
	 "com.emacsos.zero.Panel1"	; well known name
	 "/com/emacsos/zero/Panel1"	; object path
	 "com.emacsos.zero.Panel1.PanelInterface" ; interface name
	 method nil :timeout 500 args))

;;=========================
;; public utility function
;;=========================

(defun zero-alist-to-asv (hints)
  "Convert Lisp alist to dbus a{sv} data structure.

HINTS should be an alist of form '((k1 [v1type] v1) (k2 [v2type] v2)).

For example,
\(zero-alist-to-asv
  '((\"name\" \"foo\")
    (\"timeout\" :int32 10)))
=>
'(:array
  (:dict-entry \"name\" (:variant \"foo\"))
  (:dict-entry \"timeout\" (:variant :int32 10)))"
  (if (null hints)
      '(:array :signature "{sv}")
    (let ((result '(:array)))
      (dolist (item hints)
	(push (list :dict-entry (car item) (cons :variant (cdr item))) result))
      (reverse result))))

(ert-deftest zero-alist-to-asv ()
  (should (equal (zero-alist-to-asv nil) '(:array :signature "{sv}")))
  (should (equal (zero-alist-to-asv
		  '(("name" "foo")
		    ("timeout" :int32 10)))
		 '(:array
		   (:dict-entry "name" (:variant "foo"))
		   (:dict-entry "timeout" (:variant :int32 10))))))

;;============
;; public API
;;============

(defun zero-panel-move (x y)
  "Move panel to specific coordinate (X, Y).
Origin (0, 0) is at screen top left corner."
  (zero-panel-async-call "Move" nil :int32 x :int32 y))

(defun zero-panel-show-candidates (preedit_str candidate_length candidates &optional hints)
  "Show CANDIDATES.
Argument PREEDIT_STR the preedit string.
Argument CANDIDATE_LENGTH how many candidates are in candidates list."
  (zero-panel-async-call "ShowCandidates" nil
			 :string preedit_str
			 :uint32 candidate_length
			 (or candidates '(:array))
			 (zero-alist-to-asv hints)))

(defun zero-panel-show ()
  "Show panel."
  (zero-panel-async-call "Show" nil))

(defun zero-panel-hide ()
  "Hide panel."
  (zero-panel-async-call "Hide" nil))

(defun zero-panel-quit ()
  "Quit panel application."
  (interactive)
  (zero-panel-async-call "Quit" nil))

(provide 'zero-panel)

;;; zero-panel.el ends here
