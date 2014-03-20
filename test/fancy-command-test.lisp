#!/bin/bash
#| -*- common-lisp -*-
exec sbcl --script "$0" "$@"
|#

#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(with-output-to-string (*standard-output*)
  (ql:quickload '(:fancy-command :fancy-command/test)))

(in-package #:fancy-command/test)

(main SB-EXT:*POSIX-ARGV*)
