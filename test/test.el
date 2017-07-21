;; this is basically a scratch file for testing while I work on the initial
;; implementation
(require 'libemacs-jupyter-client)
(message "%d" (ejc/get-42))

(let ((client (ejc/connect ["lala" "lulu" "lele"]
                         ;; (concat (getenv "HOME")
                           ;;         "/repos/emacs-jupyter-client/test/test_connection.json")
                           "/run/user/1000/jupyter/kernel-a1213e69-3c3b-42c2-91a3-7596c7974572.json"
                         ))
      (nmsgs 1))
  (if (user-ptrp client) (progn
                           (while (> nmsgs 0)
                             ;; query server with low-ish probability (less noise in output)
                             (when (>  (random 10) 7)
                               (message "kernel is %s" (if (ejc/alive? client) "alive" "dead")))
                             ;; send some code every 2 seconds
                             (sleep-for 2)
                             ;;(message "%s" (ejc/execute-code client "This λ some code."))
                             (message "%s" (ejc/execute-code client "λ = 5"))
                             (setq nmsgs (- nmsgs 1))
                             )
                           (sleep-for 10)
                           (ejc/disconnect client))
    (message "Something went wrong: %s" client)))
