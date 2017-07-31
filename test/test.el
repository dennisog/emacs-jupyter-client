;; this is basically a scratch file for testing while I work on the initial
;; implementation
(add-to-list 'load-path (concat (getenv "HOME") "/repos/emacs-jupyter-client/lisp"))
(add-to-list 'load-path (concat (getenv "HOME") "/build/emacs-jupyter-client/src"))
(require 'emacs-jupyter-repl)


(define-key special-event-map [sigusr1]
  (ejr-callback))


(setq-local cp (ejr--start-kernel))
(setq-local state (ejr--start-client cp))

;; Local Variables:
;; mode: lisp-interaction
;; End:
