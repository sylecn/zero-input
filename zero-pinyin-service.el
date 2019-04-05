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

(defun zero-pinyin-service-get-candidates (preedit-str fetch-size)
  "get candidates for pinyin in preedit-str synchronously.

preedit-str the preedit-str, should be pure pinyin string
fetch-size try to fetch this many candidates or more"
  (zero-pinyin-service-call "GetCandidates" :string preedit-str :uint32 fetch-size))

(defun zero-pinyin-service-get-candidates-async (preedit-str fetch-size get-candidates-complete)
  "get candidates for pinyin in preedit-str asynchronously.

preedit-str the preedit-str, should be pure pinyin string
fetch-size try to fetch this many candidates or more"
  (zero-pinyin-service-async-call
   "GetCandidates" get-candidates-complete :string preedit-str :uint32 fetch-size))

(defun zero-pinyin-service-quit ()
  "quit panel application"
  (zero-pinyin-service-async-call "Quit" nil))

;;================
;; some app test
;;================

(ert-deftest zero-pinyin-service-get-candidates ()
  (destructuring-bind (cs ls) (zero-pinyin-service-get-candidates "liyifeng" 1)
    (should (equal (first cs) "李易峰"))
    (should (= (first ls) 8)))
  (destructuring-bind (cs ls) (zero-pinyin-service-get-candidates "wenti" 1)
    (should (equal (first cs) "问题"))
    (should (= (first ls) 5)))
  (destructuring-bind (cs ls) (zero-pinyin-service-get-candidates "meiyou" 1)
    (should (equal (first cs) "没有"))
    (should (= (first ls) 6)))
  (destructuring-bind (cs ls) (zero-pinyin-service-get-candidates "shi" 1)
    (should (equal (first cs) "是"))
    (should (= (first ls) 3)))
  (destructuring-bind (cs ls) (zero-pinyin-service-get-candidates "de" 1)
    (should (equal (first cs) "的"))
    (should (= (first ls) 2))))

(provide 'zero-pinyin-service)
