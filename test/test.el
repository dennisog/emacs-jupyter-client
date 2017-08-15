;; this is basically a scratch file for testing while I work on the initial
;; implementation

(toggle-debug-on-error)
(setq resize-mini-windows nil)
(windmove-default-keybindings)

;; load the sources
(add-to-list 'load-path (concat (getenv "HOME") "/repos/emacs-jupyter-client/lisp"))
(add-to-list 'load-path (concat (getenv "HOME") "/build/emacs-jupyter-client/src"))
(require 'jupyter-repl)

;; set up the window
(setq jupyter-repl-debug t)
(let ((bot (split-window-below)))
  (with-selected-window bot
    (get-buffer-create jupyter-repl-debug-buffer)
    (pop-to-buffer-same-window jupyter-repl-debug-buffer)
    (emacs-lisp-mode)))

;; register the handler
(define-key special-event-map [sigusr1] 'jupyter-repl-handler)

;; start a repl
(jupyter-repl)

;; Local Variables:
;; mode: lisp-interaction
;; End:
