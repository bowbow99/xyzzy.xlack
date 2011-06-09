;;; -*- mode: lisp; package: xlack -*-
;;;
;;; xlack.l --- DESCRIPTION
;;;
;;; Author:     bowbow99  <bowbow99@gmail.com>
;;; Version:    0.00.00
;;; Category:
;;; License:    MIT (see COPYING.mit)
;;; Commentary: (see README.md)
;;; ChangeLog:  (see ChangeLog)
;;; See: Clack - Web Application Environment for Common Lisp http://clacklisp.org/
;;; See: PEP 333 -- Python Web Server Gateway Interface v1.0 http://www.python.org/dev/peps/pep-0333/

;;; Code:

(eval-when (:execute :compile-toplevel)
  (require "symbol-macrolet")
  (require "cmu_loop")
  (require "destructuring-bind"))

(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "assertions"))

(defpackage :xlack
  (:use :lisp :editor))

(in-package :xlack)

(export '(start-server
          stop-server))

;;;;
;;;; * Configuration




;;;;
;;;; * Utilities

(defun %keyword (name)
  (intern (string name) :keyword))

(defun %operator-form-p (form operator)
  "Return true if car of FORM is `eq' to OPERATOR."
  (and (consp form)
       (eq (car form) operator)))

(defmacro with-struct* ((struct &rest slot-names) object &body body)
  (let ((package (symbol-package struct)))
    (labels ((accessor (slot)
               (let ((name (format nil "~A-~A" struct slot)))
                 (or (find-symbol name package)
                     (error "Accessor `~A' not found in package `~A'"
                            name (package-name package))))))
      `(let ((,struct ,object))
         (symbol-macrolet
             (,@(mapcar (lambda (slot)
                          `(,slot (,(accessor slot) ,struct)))
                    slot-names))
           ,@body)))))
#+xyzzy
(setf (get 'with-struct* 'ed:lisp-indent-hook) 2)

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test &body body)
  `(let ((it ,test))
     (if it (progn ,@body))))

(defmacro awhile (test &body body)
  `(let (it)
     (while (setq it ,test) ,@body)))
#+xyzzy
(setf (get 'aif #1='ed:lisp-indent-hook) 1
      (get 'awhen #1#) 1
      (get 'awhile #1#) 1)


;; modified version of http://paste.lisp.org/display/24480
(defun proper-list-p (object)
  (if (null object) t
    (and (listp object)
         (do ((fast (cdr object) (cddr fast))
              (slow object (cdr slow)))
             ()
           (cond ((null fast) (return t))
                 ((atom fast) (return nil))
                 ((null (cdr fast)) (return t))
                 ((atom (cdr fast)) (return nil))
                 ((eql fast slow) (return nil)))))))

(defun group (source n)
  (if (zerop n)
    (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                 (rec rest (cons (subseq source 0 n)
                                 acc))
                 (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))


;;;;
;;;; * Hacks/Fixes

;; defstruct
;; - Allow not yet defined function-name on :print-function option
(shadow 'defstruct)

(defmacro defstruct (&environment env &whole form  name-and-options &rest slot-defs)
  (labels ((fix-print-function (form)
             (cond ((atom form) form)
                   ((and (%operator-form-p form 'si:*make-structure-definition)
                         (eql (car (nth 5 form)) 'function))
                    (append (subseq form 0 5)
                            `(',(second (nth 5 form)))
                            (subseq form (1+ 5))))
                   (t
                    (cons (fix-print-function (car form))
                          (fix-print-function (cdr form)))))))
    (fix-print-function
     (macroexpand-1 `(lisp:defstruct ,@(cdr form)) env))))
#+xyzzy
(setf (get 'defstruct 'ed:lisp-indent-hook) 1)


;;;;
;;;; * Logging/Monitering

(defparameter *monitor-buffer-name* "*xlack server*")

(defun make-monitor-stream ()
  (let* ((buffer (or (find-buffer *monitor-buffer-name*)
                     (let ((buffer (create-new-buffer *monitor-buffer-name*)))
                       (setup-temp-buffer buffer)
                       buffer)))
         (stream (make-buffer-stream buffer (buffer-size buffer))))
    (make-general-output-stream
     (lambda (output)
       (princ output stream)
       (when (get-buffer-window buffer)
         (refresh-screen))))))

(defmacro with-output-to-monitor (&body body)
  "Evaluate BODY with redirecting output to *STANDARD-OUTPUT* to monitor buffer."
  `(with-open-stream (*standard-output*
                      (make-monitor-stream))
     ,@body))
#+xyzzy
(setf (get 'with-output-to-monitor 'ed:lisp-indent-hook) 0)

(defun @log (fmt &rest args)
  (apply #'format t (concat "~&~A " fmt "~&")
         (format-date-string "%Y-%m-%d %H:%M:%S")
         args))

(defun @msg (fmt &rest args)
  (apply '@log (concat "[ " fmt " ]") args)
  (apply 'message (concat "HTTP Server: " fmt) args))


;;;;
;;;; * Clack ENVIRONMENT

(defun parse-request-line (line)
  (unless (string-matchp "\\([^ ]+\\) +\\([^ ]+\\) +HTTP/\\([0-9]+\\)\.\\([0-9]+\\)$"
                         line)
    (error "Invalid request-line: ~S" line))
  (values
   (let ((method-name (match-string 1)))
     (keyword (string-upcase method-name)))
   (match-string 2)
   (list (parse-integer (match-string 3))
         (parse-integer (match-string 4)))))

(defun parse-request-uri (request-uri)
  (let ((path-string (cond ((string-match "^/\\(?:$\\|[^/].*\\)" request-uri)
                            request-uri)
                           ((string-match "^https?://[^/]\\(/.*\\)$" request-uri)
                            (match-string 1))
                           (t
                            (error "Unrecognizable Request URI: ~S" request-uri)))))
    (aif (position #\? path-string)
      (values "" (subseq path-string 0 it) (subseq path-string (1+ it)))
      (values "" path-string ""))))

(defun parse-request-message-header (connection)
  "Read http header from connection and return them as alist."
  (@log "parse-request-message-header: enter")
  (let ((header nil) line)
    (while (and (listen connection)
                (setq line (read-line connection nil))
                (not (string= line "")))
      (@log "parsing header: ~S" line)
      (aif (position #\: line)
        (let ((field-name  (subseq line 0 it))
              (field-value (string-trim "\t " (subseq line (1+ it)))))
          (while (and (listen connection)
                      (member (peek-char nil connection) '(#\SPC #\TAB)))
            (let ((cont (read-line connection nil)))
              (setf field-value (concat field-value " " (string-trim "\t " cont)))))
          (setf header (acons (keyword (string-downcase field-name))
                              field-value
                              header)))
        (error "Invalid header in request: ~S" line)))
    (@log "parse-request-message-header: return")
    (nreverse header)))

(defun make-clack-environment (connection)
  (assert (open-stream-p connection))
  (multiple-value-bind (http-method request-uri http-version)
      (parse-request-line (read-line connection))
    (multiple-value-bind (script-name path-info query-string)
        (parse-request-uri request-uri)
      (let ((header (parse-request-message-header connection)))
        ;; return
        (append
         (list
          :request-method http-method
          :script-name    script-name
          :path-info      path-info
          :query-string   query-string
          :server-name    (socket-stream-local-name connection)
          :server-port    (socket-stream-local-port connection)
          :server-protocol (keyword (format nil "HTTP/~D.~D"
                                      (first http-version)
                                      (second http-version)))
          :request-uri    request-uri
          ;; FIXME: As its nature, once someone read the contents of
          ;; stream, no one can read it again. I think I should do
          ;; something on this matter.
          ;; NOTE: Official implementation seems doing something with
          ;; "flexistream". I don't know what actually it does.
          :raw-body       connection
          ;:http-user-agent
          ;:http-referer
          :remote-addr    (socket-stream-peer-address connection)
          :remote-port    (socket-stream-peer-port connection)
          :http-server    :xlack ; NOTE: ª...
          )
         ;; NOTE: AFAIK, documentation doesn't specify what to do
         ;; with header informations of request.
         ;; Both official implementation of "handlers" for Apache and
         ;; Hunchentoot include them directly in ENVIRONMENT plist
         ;; with "HTTP-" prefix on each key.
         (mapcan #'(lambda (name.value)
                     (destructuring-bind (name . value) name.value
                       (list (keyword (format nil "http-~(~A~)" name))
                             value)))
           header))))))

;;;;
;;;; * Clack's RESPONSE

(defun valid-response-p (response)
  (and (listp response)
       (= (list-length response) 3)
       (destructuring-bind (status-code header body) response
         (and (integerp status-code)
              (and (proper-list-p header)
                   (evenp (length header))
                   (every #'keyword (mapcar #'first (group header 2))))
              (or #-xyzzy
                  (pathnamep body)
                  (and (listp body)
                       (every #'stringp body)))))))

(defparameter *status/phrase-alist*
  '((200 . "OK")))

(defun find-reason-phrase (status-code)
  (cdr (assoc status-code *status/phrase-alist*)))

(defun send-response (connection response)
  (unless (valid-response-p response)
    (error "Invalid response: ~S" response))
  (destructuring-bind (status-code header body) response
    ;; Status Line
    (@log "Sending status-line...")
    (format connection "HTTP/1.1 ~S ~A~%"
      status-code (find-reason-phrase status-code))
    ;; Header
    (@log "Sending header...")
    (loop for (name value) on header by #'cddr
      do (let ((lines (split-string value #\LFD t)))
           (format connection "~A: ~A~%" name (first lines))
           (dolist (line (rest lines))
             (format connection "\t~A~%" line))))
    (fresh-line connection)
    (terpri connection)
    ;; Body
    (@log "Sending body...")
    (cond
     #-xyzzy
     ((pathnamep body) (error "Sorry, there's no pathname..."))
     ((and (listp body) (every #'stringp body))
      (dolist (text body)
        (format connection "~A~%" text))))))

;;;;
;;;; * HTTP Server

(defstruct (server
            (:print-function print-server))
  socket
  app
  port
  acceptor)

(defun print-server (server stream &optional (n 0))
  (declare (ignore n))
  (with-struct* (server app socket port) server
    (format stream "#<xlack:server ~S (~:[--~*~;~S~]) #x~@:(~X~)>"
      app
      (and (streamp socket) (open-stream-p socket))
      port
      (si:address-of server))))

(defun create-server (app &optional (port 8333))
  (make-server :app app :port 8333))

(defun server-running-p (server)
  (let ((socket (server-socket server)))
    (and (streamp socket)
         (open-stream-p socket)
         ;; NOTE: Isn't there any way to check a function is running in background or not?
         )))

(defun make-acceptor (socket app)
  (lambda ()
    (with-output-to-monitor
      (when (listen socket)
        (with-open-stream (connection (accept-connection socket))
          (@msg "Accept connection")
          (let* ((env (make-clack-environment connection))
                 (res (handler-case
                          (funcall app env)
                        (error (err)
                          `(500
                            (:content-type "text/plain")
                            (,(with-output-to-string (*standard-output*)
                                (princ "Sorry, following error occured while handling your request.\n\n  ")
                                (princ err)
                                (terpri))))))))
            (@msg "Sending response...")
            (send-response connection res)))))))

(defun start-server (server)
  (with-output-to-monitor
    (assert (not (server-running-p server)))
    (with-struct* (server socket app port acceptor)
        server
      (setf socket (make-listen-socket nil port)
            acceptor (make-acceptor socket app))
      (@msg "Server ~S Started" server)
      (start-timer 0.1 acceptor))))

(defun stop-server (server)
  (with-output-to-monitor
    (when (server-running-p server)
      (with-struct* (server socket acceptor) server
        (close socket)
        (assert (not (open-stream-p socket)))
        (while (stop-timer acceptor))
        (setf socket nil
              acceptor nil)
        (@msg "Server ~S Stopped" server)
        t))))


;;; xlack.l ends here.
