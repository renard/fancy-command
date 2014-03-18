;;;; fancy-command.asd

(asdf:defsystem #:fancy-command
  :serial t
  :description "Run command and get both its output and error in a list and
  on the screen"
  :author "Sébastien Gross <seb•ɑƬ•chezwam•ɖɵʈ•org>"
  :license "WTFPL"
  :depends-on (#:bordeaux-threads #:iolib #:iolib/os)
  :components ((:file "package")
               (:file "fancy-command")))

