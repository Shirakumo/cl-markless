#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:cl-markless-standalone
  (:nicknames #:org.shirakumo.markless.standalone)
  (:use #:cl)
  (:export #:start))
(in-package #:org.shirakumo.markless.standalone)

(defparameter *command-line-spec*
  '((("input" #\i) :type string :optional T :documentation "The input file. If left out, the input is taken from standard in.")
    (("output" #\o) :type string :optional T :documentation "The output file. If left out, output is printed to standard out.")
    (("format" #\f) :type string :optional T :documentation "The format of the output document.")
    (("directives" #\d) :type string :optional T :documentation "A comma-separated list of directives to use.")
    (("line-break-mode" #\l) :type string :optional T :documentation "Which line break mode to use, show(default)) or hide.")
    (("extension" #\e) :type string :optional T :documentation "Load an extension.")
    (("help" #\h #\?) :type boolean :optional T :documentation "Show a brief help about the tool.")
    (("version" #\v) :type boolean :optional T :documentation "Print the version.")))

(defun start ()
  (command-line-arguments:handle-command-line
   *command-line-spec* #'cli :name "cl-markless" :positional-arity 0 :rest-arity T))

(defun parse-line-break-mode (line-break-mode)
  (cond ((string-equal line-break-mode "hide") :hide)
        ((string-equal line-break-mode "show") :show)
        (T (error "line-break-mode must be either \"hide\" or \"show\", not ~s" line-break-mode))))

(defun parse-format (format)
  (let ((symbol (find-symbol (cl-markless:to-readtable-case format #.(readtable-case *readtable*))
                             "KEYWORD")))
    (unless (and symbol (compute-applicable-methods #'cl-markless:output-component
                                                    (list (make-instance 'cl-markless-components:root-component)
                                                          *standard-output*
                                                          symbol)))
      (error "No output format named ~s is known. ~s" format symbol))
    symbol))

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

(defun cli (args &key input output (format "plump") directives (line-break-mode "show") extension help version)
  (destructuring-bind (&optional o-input o-output) args
    (unwind-protect
         (handler-case
             (handler-bind ((warning (lambda (w)
                                       (format *error-output* "~&[WARN] ~a~%" w))))
               (cond (help
                      (command-line-arguments:show-option-help *command-line-spec* :stream *error-output*))
                     (version
                      (format *error-output* "cl-markless v~a~%" (asdf:system-version :cl-markless)))
                     (T
                      (when (and input o-input (string/= input o-input))
                        (error "Please do not specify the input as both a positional argument and a flag."))
                      (when (and output o-output (string/= output o-output))
                        (error "Please do not specify the output as both a positional argument and a flag."))
                      (when extension
                        (let ((*standard-output* *error-output*)
                              (*terminal-io* *error-output*)
                              (*package* #.(find-package "CL-USER")))
                          (load extension)))
                      (let ((line-break-mode (parse-line-break-mode line-break-mode))
                            (format (parse-format format))
                            (directives (parse-directives directives))
                            (input (parse-input (or input o-input)))
                            (output (parse-output (or output o-output))))
                        (let ((parser (make-instance 'cl-markless:parser
                                                     :line-break-mode line-break-mode
                                                     :directives directives)))
                          (when (and (pathnamep output)
                                     (probe-file output))
                            (delete-file output))
                          (cl-markless:output (cl-markless:parse input parser)
                                              :format format
                                              :target output))))))
           (error (e)
             (format *error-output* "~&[ERROR] ~a~%" e)
             (uiop:quit 2)))
      (uiop:finish-outputs))))

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
