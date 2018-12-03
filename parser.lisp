#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless)

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
   (disabled-directives :initarg :diasbled-directives :initform () :accessor disabled-directives)
   (label-table :initform (make-hash-table :test 'equalp) :accessor label-table)
   (directive-stack :accessor directive-stack)
   (input :initarg :input :initform (error "STREAM required") :accessor input)
   (output :initarg :output :initform (make-instance 'root-component) :accessor output)))

(defmethod initialize-instance :after ((parser parser) &key (stack-depth-limit 32))
  (setf (directive-stack parser) (make-array stack-depth-limit :fill-pointer T)))

(defmethod label ((label string) (parser parser))
  (gethash label (label-table parser)))

(defmethod (setf label) ((value component) (label string) (parser parser))
  (setf (gethash label (label-table parser)) value))

(defmethod (setf label) ((value null) (label string) (parser parser))
  (remhash label (label-table parser))
  NIL)

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
  (setf (disabled-directives parser)
        (union (disabled-directives parser)
               (components:directives instruction))))

(defmethod evaluate-instruction ((parser parser) (instruction components:enable-directives))
  (setf (disabled-directives parser)
        (set-difference (disabled-directives parser)
                        (components:directives instruction))))

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
           (read-char input))
          (T
           (let ((directive (make-instance (or (detect-block parser input)
                                               'directives:paragraph)
                                           :parser parser)))
             (vector-push directive (directive-stack parser)))))))

(defmethod insert-component ((component component) (parser parser))
  )

(defmethod detect-block ((parser parser) input)
  )
