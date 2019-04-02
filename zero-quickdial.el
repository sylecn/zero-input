;; a simple input method written as an emacs minor mode

(defun zero-quickdial-insert-one ()
  (interactive)
  (insert "one"))

(defun zero-quickdial-insert-two ()
  (interactive)
  (insert "two"))

(defun zero-quickdial-insert-three ()
  (interactive)
  (insert "three"))

(defvar zero-quickdial-mode-map nil "zero-quickdial-mode keymap")

(setq zero-quickdial-mode-map
      (list 'keymap
	    '(49 . zero-quickdial-insert-one)
	    '(50 . zero-quickdial-insert-two)
	    '(51 . zero-quickdial-insert-three)))

(define-minor-mode zero-quickdial-mode
  "a simple input method written as an emacs minor mode"
  nil
  "Quickdial"
  zero-quickdial-mode-map)

(provide 'zero-quickdial)
