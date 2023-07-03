(defpackage #:cl-markless-standalone
  (:nicknames #:org.shirakumo.markless.standalone)
  (:use #:cl)
  (:export #:start))
(in-package #:org.shirakumo.markless.standalone)

(defparameter *command-line-spec*
  '((("input" #\i) :type string :optional T :documentation "The input file. If left out, the input is taken from standard in.")
    (("output" #\o) :type string :optional T :documentation "The output file. If left out, output is printed to standard out.")
    (("format" #\f) :type string :optional T :documentation "The format of the output document. Defaults to \"plump\" (HTML).")
    (("input-format") :type string :optional T :documentation "The format of the input document. Defaults to \"markless\".")
    (("directives" #\d) :type string :optional T :documentation "A comma-separated list of directives to use.")
    (("line-break-mode" #\l) :type string :optional T :documentation "Which line break mode to use, show (default) or hide.")
    (("extension" #\e) :type string :optional T :documentation "Load an extension.")
    (("help" #\h #\?) :type boolean :optional T :documentation "Show a brief help about the tool.")
    (("version" #\v) :type boolean :optional T :documentation "Print the version.")))

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
              for symb = (find-symbol (cl-markless:to-readtable-case name #.(readtable-case *readtable*))
                                      '#:org.shirakumo.markless)
              if (and symb (subtypep symb 'cl-markless:directive))
              collect symb
              else
              do (error "No directive named ~s is known." name)))
      cl-markless:*default-directives*))

(defun parse-input (input)
  (etypecase input
    (string (uiop:parse-native-namestring input))
    (pathname input)
    (stream input)
    (null *standard-input*)))

(defun parse-output (output)
  (etypecase output
    (string (uiop:parse-native-namestring output))
    (pathname output)
    (stream output)
    (null *standard-output*)))

(defun cli (&key input output (format "plump") (input-format "markless") directives (line-break-mode "show") extension help version)
  (unwind-protect
       (handler-case
           (handler-bind ((warning (lambda (w)
                                     (format *error-output* "~&[WARN] ~a~%" w))))
             (cond (help
                    (format *error-output* "cl-markless args...~%")
                    (command-line-arguments:show-option-help *command-line-spec* :stream *error-output*)
                    (format *error-output* "~&~%Available output formats:~%  ~{~a~^, ~}~%"
                            (mapcar #'string-downcase (cl-markless:list-output-formats)))
                    (format *error-output* "~&~%Available input formats:~%  ~{~a~^, ~}~%"
                            (mapcar #'string-downcase '(:markless :markdown))))
                   (version
                    (format *error-output* "cl-markless v~a~%" (asdf:component-version (asdf:find-system :cl-markless))))
                   (T
                    (when extension
                      (let ((*standard-output* *error-output*)
                            (*terminal-io* *error-output*)
                            (*package* #.(find-package "CL-USER")))
                        (load extension)))
                    (let ((line-break-mode (parse-line-break-mode line-break-mode))
                          (format (parse-format format))
                          (directives (parse-directives directives))
                          (input (parse-input input))
                          (output (parse-output output)))
                      (let ((parser (make-instance 'cl-markless:parser
                                                   :line-break-mode line-break-mode
                                                   :directives directives))
                            (parse-function (parse-input-format input-format)))
                        (when (and (pathnamep output)
                                   (probe-file output))
                          (delete-file output))
                        (cl-markless:output (funcall parse-function input parser)
                                            :format format
                                            :target output))))))
         (error (e)
           (format *error-output* "~&[ERROR] ~a~%" e)
           (uiop:quit 2)))
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
