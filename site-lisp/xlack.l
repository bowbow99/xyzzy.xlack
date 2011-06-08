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

(defmacro with-output-to-monitor (&body body)
  "Evaluate BODY with redirecting output to *STANDARD-OUTPUT* to monitor buffer."
  `(with-open-stream (*standard-output*
                      (make-buffer-stream
                       (or (find-buffer *monitor-buffer-name*)
                           (let ((buffer (create-new-buffer *monitor-buffer-name*)))
                             (setup-temp-buffer buffer)
                             buffer))))
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
;;;; * HTTP Server

;;; NOTE: Just for prototyping purpose.
(defun http-server ()
  (with-output-to-monitor
   (with-open-stream (server (make-listen-socket nil 8333))
     (@msg "Started")
     (loop
       (sleep-for 0.01)
       (when (listen server)
         (with-open-stream (conn (accept-connection server))
           (@msg "Accept connection")
           (@msg "Sending response...")
           (format conn "HTTP/1.1 200 OK\n\nÇ©ÇﬂÅ[\n")
           (@msg "Sending response...done."))))
     (@msg "Terminated"))))


;;; xlack.l ends here.
