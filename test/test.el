;; this is basically a scratch file for testing while I work on the initial
;; implementation
(require 'libemacs-jupyter-client)
(message "%d" (ejc/get-42))

(let ((client (ejc/connect ["lala" "lulu" "lele"]
                         (concat (getenv "HOME")
                                 "/repos/emacs-jupyter-client/test/test_connection.json")))
      (nmsgs 5))
  (if (user-ptrp client) (progn
                           (while (> nmsgs 0)
                             (message "kernel is %s" (if (ejc/alive? client) "alive" "dead"))
                             (sleep-for 2)
                             ;;(setq nmsgs (- nmsgs 1))
                             )
                           (ejc/disconnect client))
    (message "Something went wrong: %s" client)))
