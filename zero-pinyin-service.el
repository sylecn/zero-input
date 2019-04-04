;; -*- lexical-binding: t -*-
;; provide emacs interface for zero-pinyin-service dbus service.

;;================
;; implementation
;;================

(require 'dbus)

(defun zero-pinyin-service-error-handler (event error)
  "handle dbus errors"
  (when (or (string-equal "com.emacsos.zero.ZeroPinyinService"
			  (dbus-event-interface-name event))
	    (s-contains-p "com.emacsos.zero.ZeroPinyinService" (cadr error)))
    (error "zero-pinyin-service dbus failed: %S" (cadr error))))

(add-hook 'dbus-event-error-functions 'zero-pinyin-service-error-handler)

(defun zero-pinyin-service-async-call (method handler &rest args)
  "call Method on zero-pinin-service asynchronously. This is a wrapper around `dbus-call-method-asynchronously'"
  (apply 'dbus-call-method-asynchronously
	 :session "com.emacsos.zero.ZeroPinyinService"
	 "/com/emacsos/zero/ZeroPinyinService"
	 "com.emacsos.zero.ZeroPinyinService"
	 method handler :timeout 1000 args))

(defun zero-pinyin-service-call (method &rest args)
  "call Method on zero-pinin-service synchronously. This is a wrapper around `dbus-call-method'"
  (apply 'dbus-call-method
	 :session "com.emacsos.zero.ZeroPinyinService"
	 "/com/emacsos/zero/ZeroPinyinService"
	 "com.emacsos.zero.ZeroPinyinService"
	 method :timeout 1000 args))

;;============
;; public API
;;============

(defun zero-pinyin-service-get-candidates (preedit-str)
  "get candidates for pinyin in preedit-str synchronously"
  (zero-pinyin-service-call "GetCandidates" :string preedit-str))

(defun zero-pinyin-service-get-candidates-async (preedit-str get-candidates-complete)
  "get candidates for pinyin in preedit-str asynchronously"
  (zero-pinyin-service-async-call
   "GetCandidates" get-candidates-complete :string preedit-str))

(defun zero-panel-quit ()
  "quit panel application"
  (zero-pinyin-service-async-call "Quit" nil))

(provide 'zero-panel)
