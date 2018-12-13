#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless)

(defparameter *default-directives* ())

(defun compile-dispatch-table (directives)
  (labels ((make-table (candidates i)
             (loop with table = (make-hash-table :test #+sbcl 'eq #-sbcl 'eql :size (1+ (length candidates)) :rehash-threshold 1)
                   for candidate in canditates
                   do (push candidate (gethash (char (car candidate) i) table))
                   finally (return (fill-table table (1+ i)))))
           (fill-table (table i)
             (loop for candidates being the hash-values of table
                   do (setf (gethash key table)
                            ;; If we have multiples, deepen the dispatch.
                            (if (rest candidates)
                                (make-table candidates i)
                                (cdr (first candidates))))
                   finally (return table))))
    (make-table (loop for directive in directives
                      collect (cons (directives:dispatch directive) directive))
                0)))

(defun dispatch (table stream)
  (let ((consumed (make-array 3 :element-type 'character :fill-pointer 0)))
    (loop with target = table
          for char of-type character = (read-char stream)
          do (vector-push char consumed)
             (setf target (gethash char table))
          until (typep target 'directives:directive)
          finally (return (values target consumed)))))

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
   (block-dispatch-table :accessor block-dispatch-table)
   (inline-dispatch-table :accessor inline-dispatch-table)
   (input :initarg :input :initform (error "STREAM required") :accessor input)
   (output :initarg :output :initform (make-instance 'components:root-component) :accessor output)))

(defmethod initialize-instance :after ((parser parser) &key (stack-depth-limit 32) (directives *default-directives*) disabled-directives)
  (setf (directive-stack parser) (make-array stack-depth-limit :fill-pointer T))
  (setf (directives parser) (mapcar #'directives:ensure-directive directives))
  ;; FIXME: Possible user-error trap: catchall directives like the paragraph must be
  ;;        last, and there should not be any duplicates.
  (dolist (directive disabled-directives)
    (setf (directives:enabled-p (directive directive parser)) NIL))
  (setf (block-dispatch-table parser) (compile-dispatch-table (directives-of 'directives:block-directive parser)))
  (setf (inline-dispatch-table parser) (compile-dispatch-table (directives-of 'directives:inline-directive parser))))

(defmethod label ((label string) (parser parser))
  (gethash label (label-table parser)))

(defmethod (setf label) ((value components:component) (label string) (parser parser))
  (setf (gethash label (label-table parser)) value))

(defmethod (setf label) ((value null) (label string) (parser parser))
  (remhash label (label-table parser))
  NIL)

(defmethod directive ((name string) (parser parser))
  (directive (find-symbol (string-upcase name) '#:org.shirakumo.markless.directives) parser))

(defmethod directive ((name symbol) (parser parser))
  (find name (directives parser) :key #'type-of))

(defmethod directives-of (type (parser parser))
  (remove-if-not (lambda (d) (typep d type)) (directives parser)))

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
  (let ((stack (directive-stack parser)))
    (handler-case
        (loop (loop for i from 0 below (length stack)
                    do (unless (process (aref stack i) parser)
                         (setf (fill-pointer stack) i)
                         ;; FIXME: Close associated block
                         (return)))
              (process NIL parser))
      (end-of-file (e)
        (declare (ignore e))))))

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

(defmethod insert-component ((component components:component) (parser parser))
  )

(defmethod detect-block ((parser parser) input)
  )

(defmethod detect-inline ((parser parser) input)
  (dolist (directive (directives parser))
    (when (and (enabled-p directive)
               (typep directive 'directives:inline-directive)
               (detect-inline directive input))
      (return directive))))

(defmethod start-directive :after (directive (parser parser))
  (vector-push directive (directive-stack parser)))
