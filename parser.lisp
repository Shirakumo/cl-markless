#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless)

(defparameter *default-directives*
  '(directives:paragraph
    directives:blockquote
    directives:unordered-list
    directives:ordered-list
    directives:header
    directives:code-block
    directives:instruction
    directives:comment
    directives:embed
    directives:footnote))

(defun compile-dispatch-table (directives)
  (let ((dispatch-depth (if directives
                            (length (directives:dispatch (first directives)))
                            0)))
    (labels ((make-table (candidates i)
               (loop with table = (make-hash-table :test #+sbcl 'eq #-sbcl 'eql :size (1+ (length candidates)) :rehash-threshold 1)
                     for candidate in candidates
                     do (loop for char across (aref (car candidate) i)
                              do (push candidate (gethash char table)))
                     finally (return (fill-table table (1+ i)))))
             (fill-table (table i)
               (loop for key being the hash-keys of table
                     for candidates being the hash-values of table
                     do (setf (gethash key table)
                              (if (< i dispatch-depth)
                                  (make-table candidates i)
                                  (cdr (first candidates))))
                     finally (return table))))
      (make-table (loop for directive in directives
                        collect (cons (directives:dispatch directive) directive))
                  0))))

(defun dispatch (table stream)
  (declare (type hash-table table))
  (declare (type stream stream))
  (let ((consumed (make-array 3 :element-type 'character :fill-pointer 0)))
    (loop with target = table
          for char of-type character = (read-char stream NIL #\Nul)
          do (vector-push char consumed)
             (setf target (gethash char target))
          while (typep target 'hash-table)
          finally (return (values
                           (when (and (not (null target)) (directives:enabled-p target))
                             target)
                           consumed)))))

(defclass parser ()
  ((line-break-mode :initarg :line-break-mode :initform :unescaped :accessor line-break-mode)
   (directives :initform () :accessor directives)
   (label-table :initform (make-hash-table :test 'equalp) :accessor label-table)
   (block-dispatch-table :accessor block-dispatch-table)
   (inline-dispatch-table :accessor inline-dispatch-table)
   (input :initform NIL :accessor input)
   (output :initform NIL :accessor output)))

(defmethod initialize-instance :after ((parser parser) &key (stack-depth-limit 32) (directives *default-directives*) disabled-directives)
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

(defmethod disable ((parser parser) test)
  (dolist (directive (directives parser) parser)
    (setf (directives:enabled-p directive) (funcall test directive))))

(defmethod enable ((parser parser) test)
  (dolist (directive (directives parser) parser)
    (setf (directives:enabled-p directive) (funcall test directive))))

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
