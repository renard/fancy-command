;;;; package.lisp

(defpackage #:fancy-command
  (:nicknames :fc)
  (:use #:cl #:bordeaux-threads #:external-program #:local-time)
  (:export
   #:*ansi-colors*
   #:*decorations*
   #:proc-return
   #:proc-return-rc
   #:proc-return-out
   #:proc-return-err
   #:proc-return-all
   #:msg-info
   #:msg-debug
   #:msg-ok
   #:msg-warn
   #:msg-error
   #:shell-quote
   #:run))

