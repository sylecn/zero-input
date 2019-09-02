;;; -*- lexical-binding: t -*-
;;; zero-quickdial --- quickdial input method written as an emacs minor mode.

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
;; M-x zero-quickdial-mode    ; turn on IM
;; type 1 will insert one
;; type 2 will insert two
;; type 3 will insert three.
;; M-x zero-quickdial-mode    ; turn off IM
;;
;; This is just a demo of how emacs minor mode can work as input method.

;;; Code:

(defun zero-quickdial-insert-one ()
  (interactive)
  (insert "one"))

(defun zero-quickdial-insert-two ()
  (interactive)
  (insert "two"))

(defun zero-quickdial-insert-three ()
  (interactive)
  (insert "three"))

(defvar zero-quickdial-mode-map
  '(keymap
    (49 . zero-quickdial-insert-one)
    (50 . zero-quickdial-insert-two)
    (51 . zero-quickdial-insert-three))
  "zero-quickdial-mode keymap")

(define-minor-mode zero-quickdial-mode
  "a simple input method written as an emacs minor mode"
  nil
  " Quickdial"
  zero-quickdial-mode-map)

(provide 'zero-quickdial)

;;; zero-quickdial.el ends here
