;; mu4e-dbus.el -- part of mu4e, the mu mail user agent
;;
;; Copyright (C) 2011-2012 Dirk-Jan C. Binnema

;; Author: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>
;; Maintainer: Dirk-Jan C. Binnema <djcb@djcbsoftware.nl>

;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'dbus)

(require 'mu4e-server)
(require 'mu4e-utils)
(require 'mu4e-vars)

(defconst mu4e~dbus-message-bus :session
  "D-bus for communication with the mu server, normally `:session'.")

(defconst mu4e~dbus-server-base "org.example.mail.mu.Maildir"
  "Mu server's root D-bus name for communication.")

(defconst mu4e~dbus-object-path "/mu/maildir"
  "Mu server's D-bus object path.")

(defconst mu4e~dbus-interface
  "org.example.mail.mu.Maildir.ObjectManager.MaildirManager"
  "Mu server's D-bus interface name")

(defconst mu4e~dbus-method "Execute"
  "Mu server's D-bus method for executing instructions.")

(defvar mu4e~dbus-timeout 60000
  "Time to wait for a response from the Mu server.")

(defun mu4e~dbus-server-name ()
  "Returns the Mu server name to contact over D-bus.

If `mu4e-mu-home' is nil, we assume that the server uses a name based
on \"$HOME/.mu\"."
  (concat mu4e~dbus-server-base "."
          (replace-regexp-in-string "/" ""
                                    (or mu4e-mu-home
                                        (expand-file-name ".mu" "~/")))))

(defun mu4e~dbus-handler (response)
  "A handler for the 'mu dbus' output.
It parses the responses as a series of sexps and passes them for evaluation."
;;  (message "mu4e~dbus-handler: %S" last-input-event)
  (let ((all-sexps nil))
    (with-temp-buffer
      (insert response)
      (goto-char (point-min))
      (let ((sexp (ignore-errors (read (current-buffer)))))
        (while sexp
          (push sexp all-sexps)
          (setq sexp (ignore-errors (read (current-buffer)))))))
    (dolist (sexp (nreverse all-sexps))
      (mu4e~server-dispatch-on-sexp sexp))
    (make-frame-visible (selected-frame))))

(defun mu4e~dbus-send-command (frm &rest args)
  "Send as command to the mu server process."
  (let ((cmd (apply 'format frm args)))
    (mu4e-log 'to-server "%s" cmd)
    (dbus-call-method-asynchronously
     mu4e~dbus-message-bus
     (mu4e~dbus-server-name)
     mu4e~dbus-object-path
     mu4e~dbus-interface
     mu4e~dbus-method
     'mu4e~dbus-handler
     :timeout mu4e~dbus-timeout
     cmd)))

(defun mu4e~dbus-running-p  ()
  "Whether the mu server is running."
  (member (mu4e~dbus-server-name)
          (dbus-list-names mu4e~dbus-message-bus)))

(defun mu4e-dbus-use-dbus ()
  (interactive)
  (setq mu4e~server-send-command 'mu4e~dbus-send-command)
  (setq mu4e~server-running-p-command 'mu4e~dbus-running-p)
  (setq mu4e~server-kill-command 'ignore))

(provide 'mu4e-dbus)
;; End of mu4e-dbus.el
