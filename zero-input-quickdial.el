;;; zero-input-quickdial --- quickdial input method written as an emacs minor mode. -*- lexical-binding: t -*-

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

;; To use this input method,
;; M-x zero-input-quickdial-mode    ; turn on IM
;; type 1 will insert one
;; type 2 will insert two
;; type 3 will insert three.
;; M-x zero-input-quickdial-mode    ; turn off IM
;;
;; This is just a demo of how Emacs minor mode can work as input method.

;;; Code:

(defun zero-input-quickdial-insert-one ()
  "Insert \"one\"."
  (interactive)
  (insert "one"))

(defun zero-input-quickdial-insert-two ()
  "Insert \"two\"."
  (interactive)
  (insert "two"))

(defun zero-input-quickdial-insert-three ()
  "Insert \"three\"."
  (interactive)
  (insert "three"))

(defvar zero-input-quickdial-mode-map
  '(keymap
    (49 . zero-input-quickdial-insert-one)
    (50 . zero-input-quickdial-insert-two)
    (51 . zero-input-quickdial-insert-three))
  "Keymap for zero-input-quickdial-mode.")

(define-minor-mode zero-input-quickdial-mode
  "a simple input method written as an emacs minor mode"
  nil
  " Quickdial"
  zero-input-quickdial-mode-map)

(provide 'zero-input-quickdial)

;;; zero-input-quickdial.el ends here
