(in-package #:fancy-command/test)

(defun main(argv)

  (let ((ret (fancy-command:run (cdr argv) :debug t :ignore-join nil)))))
;;    (loop for i in (getf ret :all) do (format t "~a~%" i))))
