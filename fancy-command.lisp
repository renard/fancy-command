;;;; fancy-command.lisp

(in-package #:fancy-command)

(defvar *ansi-colors*
  (loop for (k v) on '(;; Standard colors
			:black "[30m"
			:red "[31m"
			:green "[32m"
			:yellow "[33m"
			:blue "[34m"
			:magenta "[35m"
			:cyan "[36m"
			:white "[37m"
			;; bright colors
			:bright-black "[90m"
			:bright-red "[91m"
			:bright-green "[92m"
			:bright-yellow "[93m"
			:bright-blue "[94m"
			:bright-magenta "[95m"
			:bright-cyan "[96m"
			:bright-white "[97m"
			;; background colors
			:back-black "[40m"
			:back-red "[41m"
			:back-green "[42m"
			:back-yellow "[43m"
			:back-blue "[44m"
			:back-magenta "[45m"
			:back-cyan "[46m"
			:back-white "[47m"
			;; text attribute
			:bold "[1m"
			:underline "[4m"
			:blink "[5m"
			:inverse "[7m"
			:strikethough "[8m"
			:ansi-off "[0m")
	  by #'cddr
	nconc (list k (format nil "~C~a" #\Esc v)))
  "List of all known ANSI colors")

(defvar *decorations*
  '(:info  ("●" "[I]" :magenta)
    :debug ("✓" "[D]" :bright-blue)
    :ok    ("✔" "[O]" :green)
    :warn  ("❢" "[W]" :yellow)
    :error ("✘" "[E]" :red))

  "Ideograms used to prefix lines")


(defvar *safe-chars*
  (nconc
   (loop for c from (char-code #\A) upto (char-code #\Z) collect (code-char c))
   (loop for c from (char-code #\a) upto (char-code #\z) collect (code-char c))
   (loop for c from (char-code #\0) upto (char-code #\9) collect (code-char c))
   '(#\@ #\% #\_ #\- #\+ #\= #\: #\, #\. #\/))
  "Safe chars in a shell command")



(defun print-line (stream line type &key (colored t) (full-color nil))
  "Print LINE on STREAM using TYPE as decoration (taken from
`*decorations*`).

If COLORED is not NIL, only a pure B&W text will be printed, UTF-8 symbols
are used otherwise.

If FULL-COLOR is T the full LINE will be colored, otherwise only the
decoration.

Note: A new line (~%) is added to the LINE.
"
  (let* ((decoration (getf *decorations* type))
	 (color (getf *ansi-colors* (nth 2 decoration)))
	 (color-reset (getf *ansi-colors* :ansi-off)))
    (if colored
	(if full-color
	    (format stream "~a~a ~a~a~%" color (nth 0 decoration) line
		    color-reset)
	    (format stream "~a~a~a ~a~%" color (nth 0 decoration) color-reset
		    line))
	(format stream "~a ~a~%" (nth 1 decoration) line))))

(defun msg-info (line &key (stream *standard-output*) (colored t) (full-color nil))
  "Print an information message LINE. See `print-line`."
  (print-line stream line :info :colored colored :full-color full-color))

(defun msg-debug (line &key (stream *standard-output*) (colored t) (full-color nil))
  "Print a debug message LINE. See `print-line`."
  (print-line stream line :debug :colored colored :full-color full-color))

(defun msg-ok (line &key (stream *standard-output*) (colored t) (full-color nil))
  "Print a success message LINE. See `print-line`."
  (print-line stream line :ok :colored colored :full-color full-color))

(defun msg-warn (line &key (stream *standard-output*) (colored t) (full-color nil))
  "Print a warning message LINE. See `print-line`."
  (print-line stream line :warn :colored colored :full-color full-color))

(defun msg-error (line &key (stream *error-output*) (colored t) (full-color nil))
  "Print an error message LINE. See `print-line`."
  (print-line stream line :error :colored colored :full-color full-color))


(defun %get-nsec ()
  "Return time in nano second."
  (multiple-value-bind (sec nsec)
      (isys:clock-gettime isys:clock-realtime)
    (+ (* 1000000000 sec) nsec)))

;; Todo get file descriptor from output
(defun read-stream (stream output fd color)
  "Read STREAM until EOF and display each line to OUTPUT stream if it is not
`nil`.

FD is the OUTPUT stream file descriptor `iolib/os:+stderr+` (2) for standard
output and iolib/os:+stderr+ (2) for error output.

COLOR is stream color used to display each lines as a key of `+ANSI-COLORS+`.

Returns the stream output as a list of one line per item."

  (let* ((isatty (/= 0 (isys:fd-tty-p fd)))
	 (color-reset (getf *ansi-colors* :ansi-off))
	 (acolor (getf *ansi-colors* color color-reset))
	 (clock-res (multiple-value-bind (a b)
			(isys:clock-getres isys:clock-realtime) (- a b)))
	 ret)
    (loop for line = (read-line stream nil 'eof)
	  until (eq line 'eof)
	  do (progn
	       (push (list (%get-nsec) line) ret)
	       (let ((txt
		       (if (and isatty acolor color-reset)
			   (format nil "~a~a~a~%"
				   acolor
				   line
				   color-reset)
			   (format nil "~a~%" line))))
		 (format output txt)
		 (force-output output))))
    (nreverse ret)))

;; Shamelessly stolen from http://cl-cookbook.sourceforge.net/strings.html#manip
(defun %replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
	  for old-pos = 0 then (+ pos part-length)
	  for pos = (search part string
			    :start2 old-pos
			    :test test)
	  do (write-string string out
			   :start old-pos
			   :end (or pos (length string)))
	  when pos do (write-string replacement out)
            while pos)))

(defun shell-quote (prog-and-args)
  (let ((escaped
	  (loop for item in prog-and-args
		collect (loop for c across item
			      do (unless (member c *safe-chars*)
				   (return
				     (format nil "'~a'"
					     (%replace-all item "'" "'\"'\"'"))))
			      finally (return item)))))
    (format nil "~{~a ~}~a" (butlast escaped) (car (last escaped)))))

(defun run (prog-and-args
	    &key
	      (cp-keys nil)
	      (debug nil)
	      (output *standard-output*)
	      (output-color :green)
	      (error *error-output*)
	      (error-color :red)
	      (ignore-join nil)
	      (remove-timestamp t))
"Run PROG-AND-ARGS given as a list of '(executable arguments) using
`iolib/os:create-process`.

CP-KEYS is a list of key arguments suitable to `create-process` (see its
documentation for further information.

OUTPUT and ERROR are the real-time display stream of the process outputs
which are displayed using OUTPUT-COLOR and ERROR-COLOR (as a key of
`+ANSI-COLORS+`) respectively is output streams are a TTY. If an output
stream is `n#\l no display would be done on it.

If DEBUG is non NIL a small message indicating the start and the finish of
the command will be displayed.

If IGNORE-JOIN is non NIL, both command standard and error outputs will be
mixed in a single stream sorted by their arrival time on the stream during
the read operation. Please note if both streams are written to rapidly the
result might be unpredictable.

When REMOVE-TIMESTAMP is NIL the nanosecond timestamp of when the lines are
read on the streams is keep for each line. Thus each line is in the form of:

   (timestamp line)

The return value is a PLIST with following indicators:

- `:rc`: the command return (exit) code as integer.
- `:out`: the command standard output as a list (one line per item).
- `:err`: the command error output as a list (one line per item).
- `:all`: the command sorted output and error channel mix (one line per
  item).
"

  (let* ((cmd-str (shell-quote prog-and-args))
	 (proc (apply #'iolib/os:create-process (cons prog-and-args cp-keys)))
	 (return '(:out nil :err nil :all nil))
	 threads)

    (when debug
      (msg-debug (format nil "Running \"~a\"" cmd-str)))

    ;; handle output
    (push 
     (bordeaux-threads:make-thread
      #'(lambda()
	  (read-stream
	   (iolib/os:process-stdout proc)
	   output
	   iolib/os:+stdout+
	   output-color))
      :name "out")
     threads)
    (push 
     (bordeaux-threads:make-thread
      #'(lambda()
	  (read-stream
	   (iolib/os:process-stderr proc)
	   error
	   iolib/os:+stderr+
	   error-color))
      :name "err")
     threads)

    (setf (getf return :rc)
	  (iolib/os:process-status proc :wait t))
    (loop for thread in threads
	  do (setf (getf return (if
				  (string= "out" (thread-name thread))
				  :out :err))
		   (bordeaux-threads:join-thread thread)))
    (unless ignore-join
      (setf (getf return :all)
	    (sort (append
		   (copy-tree (getf return :out))
		   (copy-tree (getf return :err)))
		  #'(lambda (a b) (< (car a) (car b))))))

    (when remove-timestamp
      (loop for id in '(:out :err :all)
	    do (setf (getf return id)
		     (loop for i in (getf return id)
			   collect (cadr i)))))

    (when debug
      (if (= 0 (getf return :rc))
	  (msg-ok (format nil "Success"))
	  (msg-error (format nil "Error"))))

    return))


;;; "fancy-command" goes here. Hacks and glory await!

