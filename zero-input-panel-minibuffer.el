;;; zero-input-panel-minibuffer.el --- minibuffer based zero-input panel implementation. -*- lexical-binding: t -*-

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

;; URL: https://gitlab.emacsos.com/sylecn/zero-el
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; Implements a zero-input panel service using Emacs minibuffer.  This service
;; works in tty, xorg and wayland sessions.
;;
;; To use this panel, add in your ~/.emacs.d/init.el file,
;;
;;   (require 'zero-input-panel-minibuffer)
;;   (zero-input-panel-minibuffer-init)
;;
;; If the service failed to start, quit the running zero-input panel service
;; first:
;;
;;   (zero-input-panel-quit)

;;; Code:

(require 'dbus)
(require 'zero-input)

;; utils

(defun zero-input-panel-minibuffer--zip-pair1 (lst1 lst2 result)
  "Zip two lists LST1 and LST2, return a list of pairs from both lists.
RESULT is the accumulating list."
  (if (or (null lst1) (null lst2))
      (nreverse result)
    (zero-input-panel-minibuffer--zip-pair1
     (cdr lst1) (cdr lst2)
     (cons (cons (car lst1) (car lst2)) result))))

(defun zero-input-panel-minibuffer--zip-pair (lst1 lst2)
  "Zip two lists LST1 and LST2, return a list of pairs from both lists."
  (zero-input-panel-minibuffer--zip-pair1 lst1 lst2 nil))

;; main logic

(defvar zero-input-panel-minibuffer-last-candidates nil
  "Store last candidates string shown in minibuffer.")
(defvar zero-input-panel-minibuffer-original-resize-mini-windows
  resize-mini-windows
  "Store the original value of `resize-mini-windows'.
minibuffer panel works best when minibuffer height is 2 or
greater.  minibuffer height will be auto adjusted when
`zero-input-mode' is on and auto restored when `zero-input-mode' is
off.")

(defun zero-input-panel-minibuffer-make-string (candidates)
  "Create CANDIDATES string for use with minibuffer panel."
  (mapconcat 'identity (mapcar (lambda (pair)
				 (concat (int-to-string (% (car pair) 10))
					 "." (cdr pair)))
			       (zero-input-panel-minibuffer--zip-pair
				(cl-loop for i from 1 to 10 collect i)
				candidates)) " "))

(ert-deftest zero-input-panel-minibuffer-make-string ()
  (should (equal
	   (zero-input-panel-minibuffer--zip-pair '(1 2 3 4 5 6) '("a" "b" "c"))
	   '((1 . "a") (2 . "b") (3 . "c"))))
  (should (equal
	   (zero-input-panel-minibuffer--zip-pair '(1 2 3 4 5 6) '())
	   nil))
  (should (equal
	   (zero-input-panel-minibuffer-make-string '("a" "b" "c"))
	   "1.a 2.b 3.c"))
  (should (equal
	   (zero-input-panel-minibuffer-make-string '("a" "b" "c" "d"))
	   "1.a 2.b 3.c 4.d")))

(defun zero-input-panel-minibuffer-show-candidates (preedit-str _candidate-count candidates hints)
  "Show CANDIDATES using minibuffer package.
Argument PREEDIT-STR user typed characters.
Argument CANDIDATE-COUNT how many candidates to show."
  (interactive)
  ;; (zero-input-debug "candidates: %s\n" candidates)
  (let ((has-next-page (caadr (assoc "has_next_page" hints)))
	(has-previous-page (caadr (assoc "has_previous_page" hints)))
	(page-number (caadr (assoc "page_number" hints))))
    (let ((candidate-str (zero-input-panel-minibuffer-make-string candidates))
	  (pagination-str (concat (if has-previous-page "<" " ")
				  " " (int-to-string page-number) " "
				  (if has-next-page ">" " "))))
      (let ((str (concat preedit-str "\n" candidate-str " " pagination-str)))
	(setq zero-input-panel-minibuffer-last-candidates str)
	(message "%s" str))))
  :ignore)

(defun zero-input-panel-minibuffer-move (_x _y)
  "Move panel to (X, Y), based on origin at top left corner."
  (interactive)
  ;; move is not needed for minibuffer panel.
  :ignore)

(defun zero-input-panel-minibuffer-show ()
  "Show minibuffer panel."
  (interactive)
  (when zero-input-panel-minibuffer-last-candidates
    (message "%s" zero-input-panel-minibuffer-last-candidates))
  :ignore)

(defun zero-input-panel-minibuffer-hide ()
  "Hide minibuffer panel."
  (interactive)
  (message "%s" "")
  :ignore)

(defun zero-input-panel-minibuffer-quit ()
  "Quit minibuffer panel dbus service."
  (interactive)
  (setq resize-mini-windows
	zero-input-panel-minibuffer-original-resize-mini-windows)
  (dbus-unregister-service :session zero-input-panel-dbus-service-known-name)
  (setq zero-input-panel-is-ephemeral nil)
  :ignore)

(defun zero-input-panel-minibuffer-hook ()
  "Hook function to run when activate or deactivate `zero-input-mode'."
  (if zero-input-mode
      ;; activate zero-input-mode
      (progn
	(let ((w (minibuffer-window)))
	   (when (< (window-size w) 2)
	     (setq zero-input-panel-minibuffer-original-resize-mini-windows
		   resize-mini-windows)
	     (setq resize-mini-windows nil)
	     (window-resize w 1))))
    ;; deactivate zero-input-mode
    (setq resize-mini-windows
	  zero-input-panel-minibuffer-original-resize-mini-windows)))

(defun zero-input-panel-minibuffer-init ()
  "Init minibuffer based dbus panel service."
  (interactive)
  (let ((service-name zero-input-panel-dbus-service-known-name))
    (let ((res (dbus-register-service :session service-name
				      :do-not-queue)))
      (when (eq res :exists)
	;; replace existing panel service with minibuffer based service.
	(zero-input-panel-quit)
	;; async dbus call will return before server handle it.
	(sleep-for 0.1)
	(setq res (dbus-register-service :session service-name :do-not-queue)))
      (if (not (member res '(:primary-owner :already-owner)))
	  (error "Register dbus service failed: %s" res))
      (dolist (method (list
		       (cons "ShowCandidates" #'zero-input-panel-minibuffer-show-candidates)
		       (cons "Move" #'zero-input-panel-minibuffer-move)
		       (cons "Show" #'zero-input-panel-minibuffer-show)
		       (cons "Hide" #'zero-input-panel-minibuffer-hide)
		       (cons "Quit"  #'zero-input-panel-minibuffer-quit)))
	(dbus-register-method
	 :session
	 service-name
	 "/com/emacsos/zero/Panel1"
	 "com.emacsos.zero.Panel1.PanelInterface"
	 (car method)
	 (cdr method)))
      (setq zero-input-panel-is-ephemeral t)
      (add-hook 'zero-input-mode-hook 'zero-input-panel-minibuffer-hook))))

(provide 'zero-input-panel-minibuffer)

;;; zero-input-panel-minibuffer.el ends here
