(in-package #:fancy-command/test)

(defun main(argv)

  (let ((ret (fancy-command:run (cdr argv) :debug t :ignore-join nil)))))
