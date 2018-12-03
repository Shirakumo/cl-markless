#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless)

(defparameter *default-directives* ())

(defun parse (thing &rest initargs &key (parser-class 'parser) &allow-other-keys)
  (let ((initargs (copy-list initargs)))
    (remhash :parser-class initargs)
    (%parse thing class initargs)))

(defmethod %parse ((string string) class initargs)
  (%parse (make-string-input-stream string) class initargs))

(defmethod %parse ((pathname pathname) class initargs)
  (with-open-file (stream pathname :element-type 'character)
    (%parse stream class initargs)))

(defmethod %parse ((stream stream) class initargs)
  (let ((parser (apply #'make-instance class :input stream initargs)))
    (consume-input parser)
    (values (output parser)
            parser)))

(defclass parser ()
  ((line-break-mode :initarg :line-break-mode :initform :unescaped :accessor line-break-mode)
   (directives :initform () :accessor directives)
   (label-table :initform (make-hash-table :test 'equalp) :accessor label-table)
   (directive-stack :accessor directive-stack)
   (input :initarg :input :initform (error "STREAM required") :accessor input)
   (output :initarg :output :initform (make-instance 'root-component) :accessor output)))

(defmethod initialize-instance :after ((parser parser) &key (stack-depth-limit 32) (directives *default-directives*) disabled-directives)
  (setf (directive-stack parser) (make-array stack-depth-limit :fill-pointer T))
  (setf (directives parser) (mapcar #'directives:ensure-directive directives))
  ;; FIXME: Possible user-error trap: catchall directives like the paragraph must be
  ;;        last, and there should not be any duplicates.
  (dolist (directive disabled-directives)
    (setf (directives:enabled-p (directive directive parser)) NIL)))

(defmethod label ((label string) (parser parser))
  (gethash label (label-table parser)))

(defmethod (setf label) ((value component) (label string) (parser parser))
  (setf (gethash label (label-table parser)) value))

(defmethod (setf label) ((value null) (label string) (parser parser))
  (remhash label (label-table parser))
  NIL)

(defmethod directive ((name string) (parser parser))
  (directive (find-symbol (string-upcase name) '#:org.shirakumo.markless.directives) parser))

(defmethod directive ((name symbol) (parser parser))
  (find name (directives parser) :key #'type-of))

(defmethod evaluate-instruction ((parser parser) instruction)
  (error "FIXME: custom condition"))

(defmethod evaluate-instruction ((parser parser) (instruction components:set))
  (case (components:variable instruction)
    (:line-break-mode)
    (T (error "FIXME: custom condition"))))

(defmethod evaluate-instruction ((parser parser) (instruction components:message))
  (format *error-output* "~&[INFO ] ~a~%" (components:message instruction)))

(defmethod evaluate-instruction ((parser parser) (instruction components:warning))
  (format *error-output* "~&[WARN ] ~a~%" (components:message instruction))
  (warn "FIXME: custom condition"))

(defmethod evaluate-instruction ((parser parser) (instruction components:error))
  (format *error-output* "~&[ERROR] ~a~%" (components:message instruction))
  (error "FIXME: custom condition"))

(defmethod evaluate-instruction ((parser parser) (instruction components:include))
  (setf (input parser) (make-concatenated-stream
                        (open (components:file instruction) :element-type 'character)
                        (input parser))))

(defmethod evaluate-instruction ((parser parser) (instruction components:disable-directives))
  (dolist (directive (components:directives instruction))
    (let ((directive (directive directive parser)))
      (when directive
        (setf (directives:enabled-p directive) NIL)))))

(defmethod evaluate-instruction ((parser parser) (instruction components:enable-directives))
  (dolist (directive (components:directives instruction))
    (let ((directive (directive directive parser)))
      (when directive
        (setf (directives:enabled-p directive) T)))))

(defmethod consume-input ((parser parser))
  (handler-case
      (loop (loop for directive across (directive-stack parser)
                  do (process directive parser))
            (process NIL parser))
    (end-of-file (e)
      (declare (ignore e)))))

(defmethod process ((directive null) (parser parser))
  (let ((input (input parser)))
    (cond ((char= #\Return (peek-char NIL input))
           ;; FIXME: Proper line end handling
           (read-char input))
          (T
           (let ((directive (detect-block parser input)))
             (if directive
                 (start-directive directive)
                 (error 'end-of-file)))))))

(defmethod insert-component ((component component) (parser parser))
  )

(defmethod detect-block ((parser parser) input)
  ;; FIXME: This could be sped up, of course.
  (dolist (directive (directives parser))
    (when (and (enabled-p directive)
               (typep directive 'directives:block-directive)
               (detect-block directive input))
      (return directive))))

(defmethod detect-inline ((parser parser) input)
  (dolist (directive (directives parser))
    (when (and (enabled-p directive)
               (typep directive 'directives:inline-directive)
               (detect-inline directive input))
      (return directive))))

(defmethod start-directive :after (directive (parser parser))
  (vector-push directive (directive-stack parser)))
