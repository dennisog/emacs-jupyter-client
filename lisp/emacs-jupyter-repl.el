;;; emacs-jupyter-repl.el --- Major mode for an inferior REPL communicating with Jupyter kernels
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


(defvar ejr-kernel "julia-0.6"
  "The currently selected Jupyter kernel.")

(defun ejr--kernelspecs ()
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

(defun ejr--uuidgen ()
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

(defun ejr--tempdir ()
  "Determine the temporary directory to store the connection file."
  ;; FIXME: use /run/user/$pid/jupyter
  "/tmp/emacs-jupyter-client")

(defun ejr--start-kernel ()
  "Start the jupyter kernel as Emacs subprocess.

Returns an alist containing the kernelspec and the path to the
connection file."
  (let* ((key (ejr--uuidgen))
         (connection-file (concat (file-name-as-directory (ejr--tempdir)) "kernel-" key ".json"))
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
                               (kernel_name . ,ejr-kernel)))
         (kernelspec (cdr (assoc ejr-kernel (ejr--kernelspecs)))))
    (unless kernelspec
        (error "Could not find kernel %s.  Make sure it is installed correctly" ejr-kernel))
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
        (list (cons 'kernelspec kspec)
              (cons 'connection-file connection-file))))))

(defun ejr--start-client (client-alist)
  "Start a client with the data returned from `ejr--start-kernel'.

Returns a state variable"
  (let ((table (make-hash-table)))
    (puthash 'client (ejc/connect (alist-get 'kernelspec client-alist)
                                  (alist-get 'connection-file client-alist))
             table)
    ;; this will be taken up by the latest unanswered request
    (puthash 'current-msg "" table)
    (puthash 'status 'dead table)
    table))

(defun ejr--stop-client (state)
  "Stop a running client."
  (ejc/disconnect (gethash 'client state)))

(provide 'emacs-jupyter-repl)
;;; emacs-jupyter-repl ends here
