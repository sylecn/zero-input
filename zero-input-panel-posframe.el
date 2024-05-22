;;; zero-input-panel-posframe.el --- posframe based zero-input panel implementation. -*- lexical-binding: t -*-

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
;; Package-Requires: ((emacs "24.3") (posframe "1.4.3"))

;;; Commentary:

;; posframe is a GNU ELPA package that allow pop a posframe (just a
;; child-frame) at point.  This file implements a zero-input panel service
;; using posframe.  This service works in both xorg and wayland sessions.
;;
;; To use this panel, install posframe package, then add in your
;; ~/.emacs.d/init.el file,
;;
;;   (when (locate-library "posframe")
;;     (require 'zero-input-panel-posframe)
;;     (zero-input-panel-posframe-init))
;;
;; If the service failed to start, quit the running zero-input panel service
;; first:
;;
;;   (zero-input-panel-quit)

;;; Code:

(require 'dbus)
(require 'zero-input)

(require 'posframe)

(defvar zero-input-panel-posframe-buffer " *zero-input-panel-posframe-buffer*"
  "The posframe buffer used to show candidates.")

(defun zero-input-panel-posframe-show-candidates (preedit-str _candidate-count candidates hints)
  "Show CANDIDATES using posframe package.
Argument PREEDIT-STR user typed characters."
  (interactive)
  (when (posframe-workable-p)
    ;; example hints:
    ;; ((in_emacs (t)) (filename ()) (page_number (1)) (has_next_page (t)) (has_previous_page (nil)) (move_x (2951)) (move_y (93)))
    ;; it is an alist. Note that keys are strings, not symbols.
    (let ((has-next-page (caadr (assoc "has_next_page" hints)))
	  (has-previous-page (caadr (assoc "has_previous_page" hints)))
	  (page-number (caadr (assoc "page_number" hints)))
	  (hr-half (propertize "\n" 'face '(:height 25)))
	  (hr (propertize "\n" 'face '(:height 50))))
      (with-current-buffer (get-buffer-create zero-input-panel-posframe-buffer)
	(font-lock-mode -1)
	(erase-buffer)
	(insert " " preedit-str "\n")
	(insert hr-half)
	(let ((i 0))
	  (dolist (c candidates)
	    (setq i (1+ i))
	    (insert " " (int-to-string (% i 10)) "." c " \n"))
	  ;; make sure pagination indicators is at the same place.  note when
	  ;; only using "\n", line height is different than when the line
	  ;; containers Chinese characters.
	  (dotimes (_ (max 0 (- 10 i)))
	    (insert (propertize "占\n" 'face '(:foreground "#FFFFFF")))))
	(insert hr)
	(insert (format " %s  %s  %s "
			(if has-previous-page "<" " ")
			(or page-number " ")
			(if has-next-page ">" " "))))
      (posframe-show zero-input-panel-posframe-buffer
                     :position (point)
		     ;; min-width/min-height is character width and height of
		     ;; posframe, not pixels.
		     :min-width 10
		     :min-height 10
		     :background-color "#FFFFFF"
		     :foreground-color "#000000")))
  :ignore)

(defun zero-input-panel-posframe-move (x y)
  "Move panel to (X, Y), based on origin at top left corner."
  (interactive)
  (when (posframe-workable-p)
    (posframe-show zero-input-panel-posframe-buffer
                   :position (cons x y)))
  :ignore)

(defun zero-input-panel-posframe-show ()
  "Show posframe panel."
  (interactive)
  (when (posframe-workable-p)
    (posframe-show zero-input-panel-posframe-buffer))
  :ignore)

(defun zero-input-panel-posframe-hide ()
  "Hide posframe panel."
  (interactive)
  (when (posframe-workable-p)
    (posframe-hide zero-input-panel-posframe-buffer))
  :ignore)

(defun zero-input-panel-posframe-quit ()
  "Quit posframe panel dbus service."
  (interactive)
  (when (posframe-workable-p)
    (dbus-unregister-service :session zero-input-panel-dbus-service-known-name))
  :ignore)

(defun zero-input-panel-posframe-init ()
  "Init posframe based dbus panel service."
  (interactive)
  (let ((service-name zero-input-panel-dbus-service-known-name))
    (let ((res (dbus-register-service :session service-name
				      :do-not-queue)))
      (if (not (member res '(:primary-owner :already-owner)))
	  (error "Register dbus service failed: %s" res))
      (dolist (method (list
		       (cons "ShowCandidates" #'zero-input-panel-posframe-show-candidates)
		       (cons "Move" #'zero-input-panel-posframe-move)
		       (cons "Show" #'zero-input-panel-posframe-show)
		       (cons "Hide" #'zero-input-panel-posframe-hide)
		       (cons "Quit"  #'zero-input-panel-posframe-quit)))
	(dbus-register-method
	 :session
	 service-name
	 "/com/emacsos/zero/Panel1"
	 "com.emacsos.zero.Panel1.PanelInterface"
	 (car method)
	 (cdr method))))))

(provide 'zero-input-panel-posframe)

;;; zero-input-panel-posframe.el ends here
