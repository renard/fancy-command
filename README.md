# Common Lisp FANCY-COMMAND

`FANCY-COMMAND` helps to run commands from Common Lisp applications. The
command outputs (both standard and error) can be displayed on the screen and
capture in variables for further analysis.

## Installation

Clone `git` repository in your `local-projects` Quicklisp directory (by
default `~/quicklisp/local-projects`):

	git clone https://github.com/renard/fancy-command.git ~/quicklisp/local-projects

and load it using `(ql:quickload :fancy-command)`.

### Requirements

You will need:

- `bordeaux-threads`
- `iolib`


If you are using Quicklisp those modules would be automatically installed.

## Usage

Basic usage list `/tmp` directory:

	(fancy-command:run '("ls" "-al" "/tmp"))

Silently list `/tmp` directory for further processing:


    (let* ((fc (fc:run '("ls" "-al" "/tmp/") :output nil :error nil))
           (rc (fc:proc-return-rc fc))
           (out (fc:proc-return-out fc))
           (err (fc:proc-return-err fc)))
      (if (/= 0 rc)
          (fc:msg-error (format nil "Error:~%~{~a~%~}~a" (butlast err) (car (last err))))
        (fc:msg-ok (format nil "OK:~%~{~a~%~}~a" (butlast out) (car (last out)))))
      rc)


A typical example may be for an output parser / colorizer:

test.sh is something like:

    #!/bin/sh
    
    for i in $(seq 10); do
        echo O $i >&1
        echo E $i >&2
        sleep 0.1
    done

You can run it using: `(fc:run '("./test.sh") :debug t)` and see the magic.


For further information see `(documentation #'fc:run 'function)` and
`iolib/os:create-process`.
