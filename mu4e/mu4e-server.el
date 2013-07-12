;; mu4e-server.el -- part of mu4e, the mu mail user agent
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

(require 'mu4e-utils)
(require 'mu4e-vars)

(defvar mu4e~server-send-command 'mu4e~proc-send-command)
(defvar mu4e~server-running-p-command 'mu4e~proc-running-p)
(defvar mu4e~server-kill-command 'mu4e~proc-kill)

(defun mu4e~server-running-p ()
  "Whether mu4e is running.
Checks whether the server process is live."
  (funcall mu4e~server-running-p-command))

(defun mu4e~server-kill ()
  "Kill the mu server.
If the server is running as an independent daemon, does nothing."
  (funcall mu4e~server-kill-command))

(defun mu4e~server-dispatch-on-sexp (sexp)
  "Dispatch to a handler function based on a server message.

Server message is the S-expression SEXP, and dispatch happens by
interpreting SEXP as a plist and looking for properties in a
defined order.  The return value is undefined.

The server output is as follows:

   1. an error
      (:error 2 :message \"unknown command\")
   => this will be passed to `mu4e-error-func'.

   2a. a message sexp looks something like:
 \(
  :docid 1585
  :from ((\"Donald Duck\" . \"donald@example.com\"))
  :to ((\"Mickey Mouse\" . \"mickey@example.com\"))
  :subject \"Wicked stuff\"
  :date (20023 26572 0)
  :size 15165
  :references (\"200208121222.g7CCMdb80690@msg.id\")
  :in-reply-to \"200208121222.g7CCMdb80690@msg.id\"
  :message-id \"foobar32423847ef23@pluto.net\"
  :maildir: \"/archive\"
  :path \"/home/mickey/Maildir/inbox/cur/1312254065_3.32282.pluto,4cd5bd4e9:2,\"
  :priority high
  :flags (new unread)
  :attachments ((2 \"hello.jpg\" \"image/jpeg\") (3 \"laah.mp3\" \"audio/mp3\"))
  :body-txt \" <message body>\"
\)
   => this will be passed to `mu4e-header-func'.

  2b. After the list of message sexps has been returned (see 2a.),
  we'll receive a sexp that looks like
  (:found <n>) with n the number of messages found. The <n> will be
  passed to `mu4e-found-func'.

  3. a view looks like:
  (:view <msg-sexp>)
  => the <msg-sexp> (see 2.) will be passed to `mu4e-view-func'.

  4. a database update looks like:
  (:update <msg-sexp> :move <nil-or-t>)

   => the <msg-sexp> (see 2.) will be passed to
   `mu4e-update-func', :move tells us whether this is a move to
   another maildir, or merely a flag change.

  5. a remove looks like:
  (:remove <docid>)
  => the docid will be passed to `mu4e-remove-func'

  6. a compose looks like:
  (:compose <reply|forward|edit|new> [:original<msg-sexp>] [:include <attach>])
  `mu4e-compose-func'."
  (mu4e-log 'from-server "%S" sexp)
  (cond
   ;; a header plist can be recognized by the existence of a :date field
   ((plist-get sexp :date)
    (funcall mu4e-header-func sexp))

   ;; the found sexp, we receive after getting all the headers
   ((plist-get sexp :found)
    (funcall mu4e-found-func (plist-get sexp :found)))

   ;; viewing a specific message
   ((plist-get sexp :view)
    (funcall mu4e-view-func (plist-get sexp :view)))

   ;; receive an erase message
   ((plist-get sexp :erase)
    (funcall mu4e-erase-func))

   ;; receive a :sent message
   ((plist-get sexp :sent)
    (funcall mu4e-sent-func
      (plist-get sexp :docid)
      (plist-get sexp :path)))

   ;; received a pong message
   ((plist-get sexp :pong)
    (funcall mu4e-pong-func
      (plist-get sexp :props)))

   ;; received a contacts message
   ;; note: we use 'member', to match (:contacts nil)
   ((plist-member sexp :contacts)
    (funcall mu4e-contacts-func
      (plist-get sexp :contacts)))

   ;; something got moved/flags changed
   ((plist-get sexp :update)
    (funcall mu4e-update-func
      (plist-get sexp :update) (plist-get sexp :move)))

   ;; a message got removed
   ((plist-get sexp :remove)
    (funcall mu4e-remove-func (plist-get sexp :remove)))

   ;; start composing a new message
   ((plist-get sexp :compose)
    (funcall mu4e-compose-func
      (plist-get sexp :compose)
      (plist-get sexp :original)
      (plist-get sexp :include)))

   ;; do something with a temporary file
   ((plist-get sexp :temp)
    (funcall mu4e-temp-func
      (plist-get sexp :temp)    ;; name of the temp file
      (plist-get sexp :what)    ;; what to do with it
                                ;; (pipe|emacs|open-with...)
      (plist-get sexp :docid)   ;; docid of the message
      (plist-get sexp :param))) ;; parameter for the action

   ;; get some info
   ((plist-get sexp :info)
    (funcall mu4e-info-func sexp))

   ;; receive an error
   ((plist-get sexp :error)
    (funcall mu4e-error-func
      (plist-get sexp :error)
      (plist-get sexp :message)))

   (t (mu4e-message "Unexpected data from server [%S]" sexp))))

(defsubst mu4e--docid-msgid-param (docid-or-msgid)
  "Construct a backend parameter based on DOCID-OR-MSGID."
  (format
   (if (stringp docid-or-msgid)
       "msgid:\"%s\""
     "docid:%d")
   docid-or-msgid))

(defun mu4e~server-remove (docid)
  "Remove message identified by docid.
The results are reporter through either (:update ... ) or (:error)
sexp, which are handled my `mu4e-error-func', respectively."
  (funcall mu4e~server-send-command "cmd:remove docid:%d" docid))

(defun mu4e~server-escape (str)
  "Escape STRING for transport -- put it in quotes, and escape existing quotation.
In particular, backslashes and double-quotes."
  (let ((esc (replace-regexp-in-string "\\\\" "\\\\\\\\" str)))
    (format "\"%s\"" (replace-regexp-in-string "\"" "\\\\\"" esc))))

(defun mu4e~server-find (query threads sortfield sortdir maxnum skip-dups include-related)
  "Start a database query for QUERY.
If THREADS is non-nil, show results in threaded fasion, SORTFIELD
is a symbol describing the field to sort by (or nil); see
`mu4e~headers-sortfield-choices'. If SORT is `descending', sort
Z->A, if it's `ascending', sort A->Z. MAXNUM determines the maximum
number of results to return, or nil for 'unlimited'. If SKIP-DUPS
is non-nil, show only one of duplicate messages (see
`mu4e-headers-skip-duplicates').  If INCLUDE-RELATED is non-nil,
include messages related to the messages matching the search
query (see `mu4e-headers-include-related').

For each
result found, a function is called, depending on the kind of
result. The variables `mu4e-error-func' contain the function that
will be called for, resp., a message (header row) or an error."
  (funcall mu4e~server-send-command
   (format 
    (concat
     "cmd:find query:%s threads:%s sortfield:%s reverse:%s maxnum:%d "
     "skip-dups:%s include-related:%s")
    (mu4e~server-escape query)
    (if threads "true" "false")
    ;; sortfield is e.g. ':subject'; this removes the ':'
    (if (null sortfield) "nil" (substring (symbol-name sortfield) 1))
    ;; TODO: use ascending/descending in backend too (it's clearer than 'reverse'
    (if (eq sortdir 'descending) "true" "false")
    (if maxnum maxnum -1)
    (if skip-dups "true" "false")
    (if include-related "true" "false"))))

(defun mu4e~server-move (docid-or-msgid &optional maildir flags)
  "Move message identified by DOCID-OR-MSGID.
At least one of MAILDIR and FLAGS should be specified. Note, even
if MAILDIR is nil, this is still a move, since a change in flags
still implies a change in message filename.

MAILDIR (), optionally
setting FLAGS (keyword argument :flags).  optionally setting FLAGS
in the process. If MAILDIR is nil, message will be moved within the
same maildir.

MAILDIR must be a maildir, that is, the part _without_ cur/ or new/
or the root-maildir-prefix. E.g. \"/archive\". This directory must
already exist.

The FLAGS parameter can have the following forms:
  1. a list of flags such as '(passed replied seen)
  2. a string containing the one-char versions of the flags, e.g. \"PRS\"
  3. a delta-string specifying the changes with +/- and the one-char flags,
     e.g. \"+S-N\" to set Seen and remove New.

The flags are any of `deleted', `flagged', `new', `passed', `replied' `seen' or
`trashed', or the corresponding \"DFNPRST\" as defined in [1]. See
`mu4e-string-to-flags' and `mu4e-flags-to-string'.
The server reports the results for the operation through
`mu4e-update-func'.
The results are reported through either (:update ... )
or (:error ) sexp, which are handled my `mu4e-update-func' and
`mu4e-error-func', respectively."
  (unless (or maildir flags)
    (mu4e-error "At least one of maildir and flags must be specified"))
  (unless (or (not maildir) (file-exists-p (concat mu4e-maildir "/" maildir "/")))
    (mu4e-error "Target dir does not exist"))
  (let* ((idparam (mu4e--docid-msgid-param docid-or-msgid))
         (flagstr
          (when flags
            (concat " flags:"
                    (if (stringp flags) flags (mu4e-flags-to-string flags)))))
         (path
          (when maildir
            (format " maildir:%s" (mu4e~server-escape maildir)))))
    (funcall mu4e~server-send-command "cmd:move %s %s %s"
                            idparam (or flagstr "") (or path ""))))

(defun mu4e~server-index (path my-addresses)
  "Update the message database for filesystem PATH, which should
point to some maildir directory structure. MY-ADDRESSES is a list
of 'my' email addresses (see `mu4e-user-mail-address-list')."
  (let ((path (mu4e~server-escape path))
        (addrs (when my-addresses (mapconcat 'identity my-addresses ","))))
    (if addrs
        (funcall mu4e~server-send-command "cmd:index path:%s my-addresses:%s" path addrs)
      (funcall mu4e~server-send-command "cmd:index path:%s" path))))

(defun mu4e~server-add (path maildir)
  "Add the message at PATH to the database.
With MAILDIR set to the maildir this message resides in,
e.g. '/drafts'; if this works, we will receive (:info add :path
<path> :docid <docid>) as well as (:update <msg-sexp>)."
  (funcall mu4e~server-send-command "cmd:add path:%s %s"
           (mu4e~server-escape path)
           (if maildir
               (format "maildir:%s" (mu4e~server-escape maildir))
             maildir)))

(defun mu4e~server-sent (path maildir)
  "Add the message at PATH to the database.
With MAILDIR set to the maildir this message resides in,
e.g. '/drafts'.

 if this works, we will receive (:info add :path <path> :docid
<docid> :fcc <path>)."
  (funcall mu4e~server-send-command "cmd:sent path:%s maildir:%s"
           (mu4e~server-escape path) (mu4e~server-escape maildir)))

(defun mu4e~server-compose (type &optional docid)
  "Start composing a message of certain TYPE (a symbol, either
`forward', `reply', `edit' or `new', based on an original
message (ie, replying to, forwarding, editing) with DOCID or nil
for type `new'.

The result will be delivered to the function registered as
`mu4e-compose-func'."
  (unless (member type '(forward reply edit new))
    (mu4e-error "Unsupported compose-type %S" type))
  (unless (eq (null docid) (eq type 'new))
    (mu4e-error "`new' implies docid not-nil, and vice-versa"))
  (funcall mu4e~server-send-command "cmd:compose type:%s docid:%d"
           (symbol-name type) docid))

(defun mu4e~server-mkdir (path)
  "Create a new maildir-directory at filesystem PATH."
  (funcall mu4e~server-send-command "cmd:mkdir path:%s" (mu4e~server-escape path)))

(defun mu4e~server-extract (action docid partidx &optional path what param)
  "Extract an attachment with index PARTIDX from message with DOCID
and perform ACTION on it (as symbol, either `save', `open', `temp') which
mean:
  * save: save the part to PARAM1 (a path) (non-optional for save)
  * open: open the part with the default application registered for doing so
  * temp: save to a temporary file, then respond with
             (:temp <path> :what <what> :param <param>)."
  (let ((cmd
         (concat "cmd:extract "
                 (case action
                   (save
                    (format "action:save docid:%d index:%d path:%s"
                            docid partidx (mu4e~server-escape path)))
                   (open (format "action:open docid:%d index:%d" docid partidx))
                   (temp
                    (format "action:temp docid:%d index:%d what:%s%s"
                            docid partidx what
                            (if param
                                (if (stringp param)
                                    (format " param:%s" (mu4e~server-escape param))
                                  (format " param:%S" param)) "")))
                   (otherwise (mu4e-error "Unsupported action %S" action))))))
    (funcall mu4e~server-send-command cmd)))


(defun mu4e~server-ping ()
  "Sends a ping to the mu server, expecting a (:pong ...) in response."
  (funcall mu4e~server-send-command "cmd:ping"))

(defun mu4e~server-contacts (personal after)
  "Sends the contacts command to the mu server.
A (:contacts (<list>)) is expected in response. If PERSONAL is
non-nil, only get personal contacts, if AFTER is non-nil, get
only contacts seen AFTER (the time_t value)."
  (funcall mu4e~server-send-command
   "cmd:contacts personal:%s after:%d"
   (if personal "true" "false")
   (or after 0)))

(defun mu4e~server-view (docid-or-msgid &optional images decrypt)
  "Get one particular message based on its DOCID-OR-MSGID.
Optionally, if IMAGES is non-nil, backend will any images
attached to the message, and return them as temp files.
The result will be delivered to the function registered as
`mu4e-message-func'."
  (funcall mu4e~server-send-command
   "cmd:view %s extract-images:%s extract-encrypted:%s use-agent:true"
   (mu4e--docid-msgid-param docid-or-msgid)
   (if images "true" "false")
   (if decrypt "true" "false")))

(defun mu4e~server-view-path (path &optional images decrypt)
  "View message at PATH (keyword argument).
Optionally, if IMAGES is non-nil, backend will any images
attached to the message, and return them as temp files. The
result will be delivered to the function registered as
`mu4e-message-func'."
  (funcall mu4e~server-send-command
   "cmd:view path:\"%s\" extract-images:%s extract-encrypted:%s use-agent:true"
   (mu4e~server-escape path)
   (if images "true" "false")
   (if decrypt "true" "false")))

(provide 'mu4e-server)
;; End of mu4e-server.el
