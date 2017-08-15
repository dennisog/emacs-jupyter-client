;;; jupyter-repl.el --- Major mode for an inferior REPL communicating with Jupyter kernels
;;
;; Author: Dennis Ogbe <dogbe@purdue.edu>
;;
;;; Usage:
;; TODO
;;
;;; Commentary:
;; Try to rip off CIDER's repl a bit
;;
;;; TODO
;; - build this thing
;;
;;; License:
;;
;; Copyright (c) 2017 Dennis Ogbe
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;; load the module
(require 'libemacs-jupyter-client)

;; dependencies
(require 'json)
(require 'ansi-color)

;; Variables ------------------------------------------------------------------

(defvar jupyter-repl-kernel "julia-0.6"
  "The currently selected Jupyter kernel.")

(defvar jupyter-repl-history-file
  (concat user-emacs-directory "/jupyter-repl-history")
  "File to save the persistend REPL history to.")

(defvar jupyter-repl-buffer-name "*Jupyter*"
  "Name of the Jupyter REPL buffer.")

(defvar jupyter-repl-prompt ">> "
  "Prompt string.")

(defvar jupyter-repl-debug nil
  "If true, we are in debug mode and we log everything to the
  `jupyter-repl-debug-buffer.'")

(defvar jupyter-repl-debug-buffer "*Jupyter REPL DEBUG*"
  "Name of the debug buffer.")

;; Kernel management ----------------------------------------------------------

(defun jupyter-repl--kernelspecs ()
  "Return a list of the Jupyter Kernels found on this machine."
  (interactive)
  (let* ((system-locs (if (eq system-type 'windows-nt)
                          (list "%PROGRAMDATA%\\jupyter\\kernels")
                        '("/usr/share/jupyter/kernels" "/usr/local/share/jupyter/kernels")))
         (user-locs (if (eq system-type 'windows-nt)
                        (list "%APPDATA%\\jupyter\\kernels")
                      `(,(concat (file-name-as-directory (getenv "HOME")) ".local/share/jupyter/kernels")
                        ,(concat (file-name-as-directory (getenv "HOME")) "Library/Jupyter/kernels"))))
         (locs (append system-locs user-locs))
         (kernels))
    (dolist (loc locs)
      (when (file-exists-p loc)
        (let ((filenames (directory-files loc t directory-files-no-dot-files-regexp)))
          (dolist (abs-kernelname filenames)
            (let* ((kernelname (file-name-nondirectory abs-kernelname))
                   (json (json-read-file (concat (file-name-as-directory abs-kernelname) "kernel.json"))))
              (setq kernels (cons (cons kernelname json) kernels)))))))
    kernels))

(defun jupyter-repl--uuidgen ()
  "Generate a pseudo-UUID.

Since the use case below is very limited, using an OS-dependent
version like uuidgen would be the Wrong Thing To Do.  Stolen from
`http://ergoemacs.org/emacs/elisp_generate_uuid.html'."
  (let ((myStr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                            (user-uid)
                            (emacs-pid)
                            (system-name)
                            (user-full-name)
                            (current-time)
                            (emacs-uptime)
                            (garbage-collect)
                            (buffer-string)
                            (random)
                            (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
            (substring myStr 0 8)
            (substring myStr 8 12)
            (substring myStr 13 16)
            (format "%x" (+ 8 (random 4)))
            (substring myStr 17 20)
            (substring myStr 20 32))))

(defun jupyter-repl--tempdir ()
  "Determine the temporary directory to store the connection file."
  ;; FIXME: use /run/user/$pid/jupyter
  "/tmp/emacs-jupyter-client")

(defun jupyter-repl--start-kernel ()
  "Start the jupyter kernel as Emacs subprocess."
  (let* ((key (jupyter-repl--uuidgen))
         (connection-file (concat (file-name-as-directory (jupyter-repl--tempdir)) "kernel-" key ".json"))
         (port0 56787)
         (connection-profile `((ip . "127.0.0.1")
                               (transport . "tcp")
                               ("signature_scheme" . "hmac-sha256")
                               (stdin_port . ,port0)
                               (control_port . ,(+ port0 1))
                               (hb_port . ,(+ port0 2))
                               (shell_port . ,(+ port0 3))
                               (iopub_port . ,(+ port0 4))
                               (key . ,key)
                               (kernel_name . ,jupyter-repl-kernel)))
         (kernelspec (cdr (assoc jupyter-repl-kernel (jupyter-repl--kernelspecs)))))
    (unless kernelspec
        (error "Could not find kernel %s.  Make sure it is installed correctly" jupyter-repl-kernel))
    (let* ((argv (alist-get 'argv kernelspec))
             (pos (seq-position argv "{connection_file}")))
      (unless pos (error "ERROR parsing kernel spec:  Could not find {connection_file}"))
      ;; replace {connection_file} with path to the actual connection file
      (aset argv pos connection-file)
      ;; save the connection profile as json
      (with-temp-buffer
        (insert (json-encode connection-profile))
        (json-pretty-print (point-min) (point-max))
        (write-file connection-file nil))
      ;; launch the kernel. kernelspec vector is [argv display_name language]
      (let* ((kspec (vector (mapconcat 'identity argv " ")
                            (alist-get 'display_name kernelspec)
                            (alist-get 'language kernelspec)))
             ;; it looks strange to put the key in the name here, but it's not
             ;; like this is protected in any form anyways...
             (process-name (concat "jupyter-kernel-" key))
             (process (make-process
                       :name process-name
                       :command (append argv nil)
                       :buffer nil
                       :filter nil)))
        (puthash 'procname process-name jupyter-repl--state)
        (puthash 'kernelspec kspec jupyter-repl--state)
        (puthash 'connection-file connection-file jupyter-repl--state)))))

(defun jupyter-repl--kernel-available? ()
  (not (eq 'busy (gethash 'status jupyter-repl--state))))

(defun jupyter-repl--start-client ()
  "Start a client.  Call this only after the kernel is started."
  (puthash 'client (ejc/connect (gethash 'kernelspec jupyter-repl--state)
                                (gethash 'connection-file jupyter-repl--state))
           jupyter-repl--state)
  (puthash 'current-msg "" jupyter-repl--state)
  (puthash 'status 'dead jupyter-repl--state)
  (puthash 'connected? t jupyter-repl--state))

(defun jupyter-repl--get-client ()
  "Get the user_ptr to the jupyter client."
  (gethash 'client jupyter-repl--state))

(defun jupyter-repl--stop-client ()
  "Stop a running client."
  (if (not (gethash 'connected? jupyter-repl--state))
      (message "No client for this buffer.")
    (ejc/disconnect (jupyter-repl--get-client))
    (puthash 'connected? nil jupyter-repl--state)))

(defun jupyter-repl--stop-kernel ()
  "Stop the kernel."
  ;; TODO (using kill-process, but not forget to send a shutdown_request first)
  )

(defun jupyter-repl--connected? ()
  "Do we have a connection between a client and kernel?"
  (and (boundp 'jupyter-repl--state)
       (local-variable-p 'jupyter-repl--state)
       (gethash 'connected? jupyter-repl--state)))

;;; Handlers ------------------------------------------------------------------

(defun jupyter-repl-handler ()
  "Respond to asyncrounous notifications from the module."
  (interactive)
  (with-current-buffer jupyter-repl-buffer-name
      (when (jupyter-repl--connected?)
        (let ((data (when (jupyter-repl--connected?)
                      (ejc/flush-queue (jupyter-repl--get-client)))))
          (dolist (msg data)
            (when jupyter-repl-debug
              ;; log the received messages to a debug buffer
              (with-current-buffer (get-buffer-create jupyter-repl-debug-buffer)
                (insert "\n")
                (insert (pp-to-string msg))))
            (let* ((header (alist-get 'header msg))
                   (msg-type (alist-get 'msg_type header)))
              ;; dispatch a handler
              (cond ((string= msg-type "execute_reply")
                     (jupyter-repl--handle-exec 'reply msg))
                    ((string= msg-type "execute_input")
                     (jupyter-repl--handle-exec 'input msg))
                    ((string= msg-type "execute_result")
                     (jupyter-repl--handle-exec 'result msg))
                    ((string= msg-type "error")
                     (jupyter-repl--handle-exec 'result msg))
                    ((string= msg-type "status")
                     (jupyter-repl--handle-status msg))
                    ((string= msg-type "stream")
                     (jupyter-repl--handle-stream msg))
                    (t (message "Error: unhandled message of type %s" msg-type)))))))))


(defun jupyter-repl--handle-exec (type msg)
  "Dcoumentation."
  (unless (jupyter-repl--ready?)
    (let* ((parent-header (alist-get 'parent_header msg))
           (parent-id (alist-get 'msg_id parent-header))
           (waiting-for-header (gethash 'header jupyter-repl--waiting-for))
           (waiting-for-msg-id (alist-get 'msg_id waiting-for-header)))
      (when (and (string= parent-id waiting-for-msg-id)
                 (not (gethash type jupyter-repl--waiting-for)))
        (puthash type msg jupyter-repl--waiting-for)
        (jupyter-repl--process-exec)))))

(defun jupyter-repl--process-exec ()
  "We wait until we have received the execute_reply (shell
  channel), the execute_input (iopub channel), and the
  execute_result or error (iopub channel) messages."
  (let ((reply (gethash 'reply jupyter-repl--waiting-for))
        (input (gethash 'input jupyter-repl--waiting-for))
        (result (gethash 'result jupyter-repl--waiting-for)))
    (when (and reply input result)
      (let* ((reply-content (alist-get 'content reply))
             (status (alist-get 'status reply-content)))
        (if (string= status "ok")
            ;; Success. Insert the result into the buffer
            (let* ((result-content (alist-get 'content result))
                   (data-elements (alist-get 'data result-content)))
              (dolist (d data-elements)
                (let ((data-type (car d))
                      (data (cdr d)))
                  (cond ((eq data-type 'text/plain)
                         (insert (jupyter-repl--format-output data))
                         (puthash 'repl-state 'ready jupyter-repl--state)
                         (setq-local jupyter-repl--waiting-for nil)
                         (jupyter-repl--insert-prompt))
                        (t (error "Unhandled data type %s" (symbol-name data-type)))))))
          ;; Fail. Insert the traceback into the buffer.
          (let* ((result-content (alist-get 'content result))
                 (traceback (with-temp-buffer
                              (dolist (str (alist-get 'traceback result-content))
                                (insert str)
                                (insert "\n"))
                              (buffer-string))))
            (insert (jupyter-repl--format-output traceback))
            (puthash 'repl-state 'ready jupyter-repl--state)
            (setq-local jupyter-repl--waiting-for nil)
            (jupyter-repl--insert-prompt)))))))

(defun jupyter-repl--format-output (str)
  "Documentation."
  (with-temp-buffer
    (insert str)
    (ansi-color-apply-on-region (point-min) (point-max))
    (buffer-substring (point-min) (point-max))))

(defun jupyter-repl--handle-status (msg)
  "Update the kernel status."
  (let* ((content (alist-get 'content msg))
         (exec-state (intern (alist-get 'execution_state content))))
    (puthash 'status exec-state jupyter-repl--state)))

(defun jupyter-repl--handle-stream (msg)
  "Insert the stream MSG into the buffer."
  (let* ((cmd (jupyter-repl--grab-current-command))
         (content (alist-get 'content msg))
         (text (alist-get 'text content)))
    (goto-char (point-max))
    (delete-region (point-at-bol) (point-max))
    (insert (jupyter-repl--format-output text))
    (jupyter-repl--insert-prompt)
    (insert cmd)))


;; History --------------------------------------------------------------------


;; REPL housekeeping ----------------------------------------------------------

(defun jupyter-repl--grab-current-command ()
  "Return the current command on the prompt."
  (re-search-backward jupyter-repl--prompt-regexp)
  (buffer-substring (match-end 0) (point-max)))

(defun jupyter-repl--return ()
  "If the kernel is not busy, send the current input."
  (interactive)
  (when (and (jupyter-repl--kernel-available?) (jupyter-repl--ready?))
    (save-excursion
      (let ((cmd (jupyter-repl--grab-current-command)))
        (when (>  (length cmd) 0)
          ;; send a command to the kernel, enter WAITING state
          (let* ((header (ejc/execute-request (jupyter-repl--get-client) cmd)))
            (puthash 'repl-state 'waiting jupyter-repl--state)
            (setq-local jupyter-repl--waiting-for (make-hash-table))
            (puthash 'header header jupyter-repl--waiting-for)
            ;; The kernel will publish an execute_reply on the shell channel as
            ;; well as execute_input and execute_result (or error) message on
            ;; the iopub channel. We enter the response in the buffer once we
            ;; have received all three.
            (puthash 'reply nil jupyter-repl--waiting-for)
            (puthash 'input nil jupyter-repl--waiting-for)
            (puthash 'result nil jupyter-repl--waiting-for)))))
    (goto-char (point-max))
    (insert "\n")))

(defun jupyter-repl--ready? ()
  "Return t if the repl is not currently waiting for input."
  (not (eq (gethash 'repl-state jupyter-repl--state) 'waiting)))

(defun jupyter-repl--tab ()
  "Send a completion request to the kernel."
  (interactive)
  (message "JUPYTER-REPL TAB"))

(defun jupyter-repl--previous-input ()
  "Cycle backwards through input history."
  (interactive)
  (message "JUPYTER-REPL PREV INPUT"))

(defun jupyter-repl--next-input ()
  "Cycle forward through input history."
  (interactive)
  (message "JUPYTER-REPL NEXT INPUT"))

;; Prompt ---------------------------------------------------------------------

(defun jupyter-repl--insert-prompt ()
  "Insert the prompt."
  (insert "\n")
  (insert jupyter-repl-prompt))

(defun jupyter-repl--on-prompt? ()
  "Return t if on a prompt."
  (save-excursion
    (let ((inhibit-field-text-motion t))
      (goto-char (point-max))
      (beginning-of-line)
      (looking-at jupyter-repl--prompt-regexp))))

(defun jupyter-repl---on-empty-prompt? ()
  "Return t if on an empty prompt."
  (save-excursion
    (let ((inhibit-field-text-motion t))
      (goto-char (point-max))
      (beginning-of-line)
      (looking-at (concat jupyter-repl--prompt-regexp "\\s-*$")))))

(defun jupyter-repl--delete-backwards-no-prompt ()
  "Delete one char backwards without destroying the prompt."
  (interactive)
  (let ((promptend (save-excursion
                     (let ((inhibit-field-text-motion t))
                       (beginning-of-line)
                       (looking-at jupyter-repl--prompt-regexp)
                       (match-end 0)))))
    (unless (<= (point) promptend)
      (delete-char -1))))


;; Keymap ---------------------------------------------------------------------

(defvar jupyter-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'jupyter-repl--return)
    (define-key map (kbd "TAB") #'jupyter-repl--tab)
    (define-key map (kbd "C-<up>") #'jupyter-repl--previous-input)
    (define-key map (kbd "C-<down>") #'jupyter-repl--next-input)
    (define-key map (kbd "M-p") #'jupyter-repl--previous-input)
    (define-key map (kbd "M-n") #'jupyter-repl--next-input)
    map))

;; Mode definition ------------------------------------------------------------

(define-derived-mode jupyter-repl-mode fundamental-mode "Jupyter REPL"
  "Major mode for Jupyter REPL interactions.

\\{jupyter-repl-mode-map}"
  (setq-local jupyter-repl--prompt-regexp jupyter-repl-prompt))

(defun jupyter-repl--banner ()
  "Generate the REPL buffer banner"
  (format "
# This is the Jupyter REPL.
# The rest of this banner is TODO."))

(defun jupyter-repl--init ()
  "Initialize a new Jupyter REPL."
  (setq-local jupyter-repl--state (make-hash-table))
  (jupyter-repl--start-kernel)
  (jupyter-repl--start-client)
  (insert (jupyter-repl--banner))
  (puthash 'repl-state 'ready jupyter-repl--state)
  (jupyter-repl--insert-prompt))

(defun jupyter-repl ()
  "Run a Jupyter REPL buffer."
  (interactive)
  (let* ((buffer (get-buffer-create jupyter-repl-buffer-name)))
    (pop-to-buffer-same-window buffer)
    (jupyter-repl-mode)
    (jupyter-repl--init)))

(provide 'jupyter-repl)
;;; jupyter-repl ends here
