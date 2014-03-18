;;;; package.lisp

(defpackage #:fancy-command
  (:nicknames :fc)
  (:use #:cl #:bordeaux-threads #:iolib #:iolib/os)
  (:export
   #:*ansi-colors*
   #:*decorations*
   #:msg-info
   #:msg-debug
   #:msg-ok
   #:msg-warn
   #:msg-error
   #:shell-quote
   #:run))

