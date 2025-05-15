(defpackage #:cl-markless-standalone
  (:nicknames #:org.shirakumo.markless.standalone)
  (:use #:cl)
  (:export #:start))
(in-package #:org.shirakumo.markless.standalone)

(defparameter *command-line-spec*
  '((("input" #\i) :type string :optional T :documentation "The input file. If left out, the input is taken from standard in.")
    (("output" #\o) :type string :optional T :documentation "The output file. If left out, output is printed to standard out.")
    (("format" #\f) :type string :optional T :documentation "The format of the output document.")
    (("input-format") :type string :optional T :documentation "The format of the input document.")
    (("directives" #\d) :type string :optional T :documentation "A comma-separated list of directives to use.")
    (("line-break-mode" #\l) :type string :optional T :documentation "Which line break mode to use, show (default) or hide.")
    (("extension" #\e) :type string :optional T :documentation "Load an extension.")
    (("styling" #\s) :type string :optional T :documentation "Path to a file with extra styling information. Depends on the output format.")
    (("help" #\h #\?) :type boolean :optional T :documentation "Show a brief help about the tool.")
    (("verbose" #\v) :type boolean :optional T :documentation "Print verbose output if available.")
    (("version") :type boolean :optional T :documentation "Print the version.")))

(defun start ()
  (command-line-arguments:handle-command-line
   *command-line-spec* #'cli :name "cl-markless" :positional-arity 0))

(defun parse-line-break-mode (line-break-mode)
  (cond ((string-equal line-break-mode "hide") :hide)
        ((string-equal line-break-mode "show") :show)
        (T (error "line-break-mode must be either \"hide\" or \"show\", not ~s" line-break-mode))))

(defun parse-format (format)
  (let ((symbol (find format (cl-markless:list-output-formats) :test #'string-equal)))
    (unless symbol
      (error "No output format named ~s is known." format))
    symbol))

(defun parse-input-format (format)
  (cond ((string-equal "markless" format) #'cl-markless:parse)
        ((string-equal "markdown" format) (lambda (input parser)
                                            (cl-markless-markdown:parse input)))
        (T
         (error "No input format named ~s is known." format))))

(defun parse-directives (directives)
  (if (and directives (string/= "" directives))
      (let ((parts (cl-markless:split-string directives #\,)))
        (loop for name in parts
              for symb = (find-symbol (cl-markless:to-readtable-case
                                       (string-trim " " name)
                                       #.(readtable-case *readtable*))
                                      '#:org.shirakumo.markless)
              if (and symb (subtypep symb 'cl-markless:directive))
              collect symb
              else
              do (error "No directive named ~s is known." name)))
      cl-markless:*default-directives*))

(defun parse-input (input)
  (etypecase input
    (string (pathname-utils:parse-native-namestring input))
    (pathname input)
    (stream input)
    (null *standard-input*)))

(defun parse-output (output input format)
  (etypecase output
    (string
     (if (string= "" output)
         (etypecase input
           (stream
            *standard-output*)
           ((or string pathname)
            (make-pathname :type (infer-file-type format) :defaults input)))
         (parse-output (pathname-utils:parse-native-namestring output) input format)))
    (pathname
     (if (pathname-utils:directory-p output)
         (make-pathname :type (infer-file-type format) :name (pathname-name input) :defaults output)
         output))
    (stream output)
    (null *standard-output*)))

(defun infer-file-type (format)
  (etypecase format
    (cl-markless:markless "mess")
    (cl-markless:gemtext "gmi")
    (cl-markless:bbcode "bb")
    (cl-markless:debug "txt")
    (cl-markless:highlighted "html")
    (cl-markless-plump:plump "html")
    (cl-markless-latex:latex "pdf")
    (cl-markless-epub:epub "epub")))

(defun infer-format (file &optional (default "plump"))
  (or (when (and file (pathname-type file))
        (let ((type (pathname-type file)))
          (cond ((string-equal "mess" type) "markless")
                ((string-equal "gmi" type) "gemtext")
                ((string-equal "gemini" type) "gemtext")
                ((string-equal "md" type) "markdown")
                ((string-equal "html" type) "plump")
                ((string-equal "htm" type) "plump")
                ((string-equal "epub" type) "epub")
                ((string-equal "bb" type) "bbcode")
                ((string-equal "tex" type) "latex")
                ((string-equal "pdf" type) "latex"))))
      default))

(defun cli (&key input output styling format input-format directives (line-break-mode "show") extension help verbose version)
  (unwind-protect
       (handler-bind ((warning 
                        (lambda (w)
                          (format *error-output* "~&[WARN] ~a~%" w)
                          (muffle-warning w)))
                      (error 
                        (lambda (e)
                          (format *error-output* "~&[ERROR] ~a~%" e)
                          (uiop:print-condition-backtrace e)
                          (uiop:quit 2)))
                      #+sbcl
                      (sb-sys:interactive-interrupt 
                        (lambda (e)
                          (declare (ignore e))
                          (uiop:quit 1))))
         (cond (help
                (format *error-output* "cl-markless args...~%")
                (command-line-arguments:show-option-help *command-line-spec* :stream *error-output*)
                (format *error-output* "~&~%Available output formats:~%  ~{~a~^, ~}~%"
                        (mapcar #'string-downcase (cl-markless:list-output-formats)))
                (format *error-output* "~&~%Available input formats:~%  ~{~a~^, ~}~%"
                        (mapcar #'string-downcase '(:markless :markdown)))
                (format *error-output* "~&~%Available directives:~%  ~{~a~^, ~}~%"
                        (mapcar #'string-downcase cl-markless:*default-directives*)))
               (version
                (format *error-output* "cl-markless v~a~%" (asdf:component-version (asdf:find-system :cl-markless))))
               (T
                (when (and extension (string/= "" extension))
                  (let ((*standard-output* *error-output*)
                        (*terminal-io* *error-output*)
                        (*package* #.(find-package "CL-USER")))
                    (load extension)))
                (let* ((line-break-mode (parse-line-break-mode line-break-mode))
                       (format (make-instance (if (and format (string/= "" format))
                                                  (parse-format format)
                                                  (parse-format (infer-format output "plump")))
                                              :verbose verbose
                                              :styling (when (and styling (string/= "" styling))
                                                         (pathname-utils:parse-native-namestring styling))
                                              :allow-other-keys T))
                       (directives (parse-directives directives))
                       (input (parse-input input))
                       (output (parse-output output input format)))
                  (let* ((parser (make-instance 'cl-markless:parser
                                                :line-break-mode line-break-mode
                                                :directives directives))
                         (parse-function (if (and input-format (string/= "" input-format))
                                             (parse-input-format input-format)
                                             (parse-input-format (infer-format input))))
                         (input (funcall parse-function input parser)))
                    (when (and (pathnamep output) (probe-file output))
                      (delete-file output))
                    (cl-markless:output input :format format :target output))))))
    
    (uiop:finish-outputs)))

(defmethod asdf:perform ((o asdf:program-op) (c asdf:system))
  (let ((file (asdf:output-file o c)))
    #+(and windows ccl)
    (ccl:save-application file
                          :prepend-kernel T
                          :purify T
                          :toplevel-function #'uiop:restore-image
                          :application-type :console)
    #-(and windows ccl)
    (apply #'uiop:dump-image file
           :executable T
           (append '(:executable T)
                   #+sb-core-compression
                   '(:compression T)
                   #+(and sbcl os-windows)
                   '(:application-type :console)))))
