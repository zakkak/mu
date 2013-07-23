;; mu4e-proc.el -- part of mu4e, the mu mail user agent
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
(require 'mu4e-vars)
(require 'mu4e-utils)
(require 'mu4e-meta)
(require 'mu4e-server)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal vars

(defvar mu4e~proc-buf nil
  "Buffer (string) for data received from the backend.")
(defconst mu4e~proc-name " *mu4e-proc*"
  "Name of the server process, buffer.")
(defvar mu4e~proc-process nil
  "The mu-server process.")

;; dealing with the length cookie that precedes expressions
(defconst mu4e~cookie-pre "\376"
  "Each expression we get from the backend (mu server) starts with
a length cookie:
  <`mu4e~cookie-pre'><length-in-hex><`mu4e~cookie-post'>.")
(defconst mu4e~cookie-post "\377"
    "Each expression we get from the backend (mu server) starts with
a length cookie:
  <`mu4e~cookie-pre'><length-in-hex><`mu4e~cookie-post'>.")
(defconst mu4e~cookie-matcher-rx
  (purecopy (concat mu4e~cookie-pre "\\([[:xdigit:]]+\\)" mu4e~cookie-post))
  "Regular expression matching the length cookie.
Match 1 will be the length (in hex).")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defsubst mu4e~proc-send-command (frm &rest args)
  "Send as command to the mu server process.
Start the process if needed."
  (unless (mu4e~proc-running-p)
    (mu4e~proc-start))
  (let ((cmd (apply 'format frm args)))
    (mu4e-log 'to-server "%s" cmd)
    (process-send-string mu4e~proc-process (concat cmd "\n"))))

(defun mu4e~proc-start ()
  "Start the mu server process."
  (unless (file-executable-p mu4e-mu-binary)
    (mu4e-error (format "`mu4e-mu-binary' (%S) not found" mu4e-mu-binary)))
  (let* ((process-connection-type nil) ;; use a pipe
	  (args '("server"))
	  (args (append args (when mu4e-mu-home
			       (list (concat "--muhome=" mu4e-mu-home))))))
    (setq mu4e~proc-buf "")
    (setq mu4e~proc-process (apply 'start-process
			      mu4e~proc-name mu4e~proc-name
			      mu4e-mu-binary args))
    ;; register a function for (:info ...) sexps
    (unless mu4e~proc-process
      (mu4e-error "Failed to start the mu4e backend"))
    (set-process-query-on-exit-flag mu4e~proc-process nil)
    (set-process-coding-system mu4e~proc-process 'binary 'utf-8-unix)
    (set-process-filter mu4e~proc-process 'mu4e~proc-filter)
    (set-process-sentinel mu4e~proc-process 'mu4e~proc-sentinel)))

(defun mu4e~proc-kill ()
  "Kill the mu server process."
  (let* ((buf (get-buffer mu4e~proc-name))
	  (proc (and (buffer-live-p buf) (get-buffer-process buf))))
    (when proc
      (let ((delete-exited-processes t))
	;; the mu server signal handler will make it quit after 'quit'
	(mu4e~proc-send-command "cmd:quit"))
	;; try sending SIGINT (C-c) to process, so it can exit gracefully
      (ignore-errors
	(signal-process proc 'SIGINT))))
  (setq
    mu4e~proc-process nil
    mu4e~proc-buf nil))

(defun mu4e~proc-running-p  ()
  "Whether the mu process is running."
  (when (and mu4e~proc-process
	  (memq (process-status mu4e~proc-process)
	    '(run open listen connect stop)))
    t))

(defsubst mu4e~proc-eat-sexp-from-buf ()
  "'Eat' the next s-expression from `mu4e~proc-buf'.
Note: this is a string, not an emacs-buffer. `mu4e~proc-buf gets
its contents from the mu-servers in the following form:
   <`mu4e~cookie-pre'><length-in-hex><`mu4e~cookie-post'>
Function returns this sexp, or nil if there was none.
`mu4e~proc-buf' is updated as well, with all processed sexp data
removed."
  (ignore-errors ;; the server may die in the middle...
    ;; mu4e~cookie-matcher-rx:
    ;;  (concat mu4e~cookie-pre "\\([[:xdigit:]]+\\)]" mu4e~cookie-post)
    (let ((b (string-match mu4e~cookie-matcher-rx mu4e~proc-buf))
	   (sexp-len) (objcons))
      (when b
	(setq sexp-len (string-to-number (match-string 1 mu4e~proc-buf) 16))
	;; does mu4e~proc-buf contain the full sexp?
	(when (>= (length mu4e~proc-buf) (+ sexp-len (match-end 0)))
	  ;; clear-up start
	  (setq mu4e~proc-buf (substring mu4e~proc-buf (match-end 0)))
	  ;; note: we read the input in binary mode -- here, we take the part
	  ;; that is the sexp, and convert that to utf-8, before we interpret
	  ;; it.
	  (setq objcons (read-from-string
			  (decode-coding-string
			    (substring mu4e~proc-buf 0 sexp-len)
			    'utf-8 t)))
	  (when objcons
	    (setq mu4e~proc-buf (substring mu4e~proc-buf sexp-len))
	    (car objcons)))))))


(defun mu4e~proc-filter (proc str)
  "A process-filter for the 'mu server' output.
It accumulates the strings into valid sexps by checking of the
';;eox' end-of-sexp marker, and then passing them for evaluation."
  (mu4e-log 'misc "* Received %d byte(s)" (length str))
  (setq mu4e~proc-buf (concat mu4e~proc-buf str)) ;; update our buffer
  (let ((sexp (mu4e~proc-eat-sexp-from-buf)))
    (with-local-quit
      (while sexp
        (mu4e~server-dispatch-on-sexp sexp)
	(setq sexp (mu4e~proc-eat-sexp-from-buf))))))


;; error codes are defined in src/mu-util
;;(defconst mu4e-xapian-empty 19 "Error code: xapian is empty/non-existent")

(defun mu4e~proc-sentinel (proc msg)
  "Function that will be called when the mu-server process terminates."
  (let ((status (process-status proc)) (code (process-exit-status proc)))
    (setq mu4e~proc-process nil)
    (setq mu4e~proc-buf "") ;; clear any half-received sexps
    (cond
      ((eq status 'signal)
	(cond
	  ((eq code 9) (message nil))
	    ;;(message "the mu server process has been stopped"))
	  (t (error (format "mu server process received signal %d" code)))))
      ((eq status 'exit)
	(cond
	  ((eq code 0)
	    (message nil)) ;; don't do anything
	  ((eq code 11)
	    (error "Database is locked by another process"))
	  ((eq code 15)
	    (error "Database needs upgrade; try `mu index --rebuild' from the command line"))
	  ((eq code 19)
	    (error "Database empty; try indexing some messages"))
	  (t (error "mu server process ended with exit code %d" code))))
      (t
	(error "Something bad happened to the mu server process")))))

(defun mu4e-proc-use-proc ()
  (interactive)
  (setq mu4e~server-send-command 'mu4e~proc-send-command)
  (setq mu4e~server-running-p-command 'mu4e~proc-running-p)
  (setq mu4e~server-kill-command 'mu4e~proc-kill))

(provide 'mu4e-proc)
;; End of mu4e-proc.el
