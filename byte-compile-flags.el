;; disable some false positive warnings that I already handle in the code.
(require 'bytecomp)
(setq byte-compile-not-obsolete-vars
      (list 'focus-in-hook
	    'focus-out-hook))
