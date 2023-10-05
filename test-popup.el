(require 'popup)
(setq popup (popup-create (point) 10 10))
(popup-set-list popup '("Foo" "Bar" "Baz"))
(popup-draw popup)
;; do something here
(popup-delete popup)
