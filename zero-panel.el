;; -*- lexical-binding: t -*-
;; provide emacs interface for zero-panel dbus service.

;;================
;; implementation
;;================

(require 'dbus)

(defun zero-panel-error-handler (event error)
  "handle dbus errors"
  (when (or (string-equal "com.emacsos.zero.Panel"
			  (dbus-event-interface-name event))
	    (s-contains-p "com.emacsos.zero.Panel" (cadr error)))
    (error "zero-panel dbus failed: %S" (cadr error))))

(add-hook 'dbus-event-error-functions 'zero-panel-error-handler)

(defun zero-panel-async-call (method _handler &rest args)
  "call Method on zero-panel service asynchronously. This is a wrapper around `dbus-call-method-asynchronously'"
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
  "convert lisp alist to dbus a{sv} data structure.
alist should be of form '((k1 [v1type] v1) (k2 [v2type] v2)).

For example,
(zero-alist-to-asv
  '((\"name\" \"foo\")
    (\"timeout\" :int32 10)))
=>
'(:array
  (:dict-entry \"name\" (:variant \"foo\"))
  (:dict-entry \"timeout\" (:variant :int32 10)))
"
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
  "move panel to specific position
(x, y) are coordinates, (0, 0) is at screen top left corner"
  (zero-panel-async-call "Move" nil :int32 x :int32 y))

(defun zero-panel-show-candidates (preedit_str candidate_length candidates &optional hints)
  "show candidates"
  (zero-panel-async-call "ShowCandidates" nil
			 :string preedit_str
			 :uint32 candidate_length
			 (or candidates '(:array))
			 (zero-alist-to-asv hints)))

(defun zero-panel-show ()
  "show panel"
  (zero-panel-async-call "Show" nil))

(defun zero-panel-hide ()
  "hide panel"
  (zero-panel-async-call "Hide" nil))

(defun zero-panel-quit ()
  "quit panel application"
  (interactive)
  (zero-panel-async-call "Quit" nil))

(provide 'zero-panel)
