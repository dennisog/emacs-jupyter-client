;; this is basically a scratch file for testing while I work on the initial
;; implementation
(require 'libemacs-jupyter-client)
(message "%d" (ejc/get-42))

(let ((conn (ejc/connect ["lala" "lulu" "lele"] "filename")))
  (sleep-for 2)
  (ejc/disconnect conn))
