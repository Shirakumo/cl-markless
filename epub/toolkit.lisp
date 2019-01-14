#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.epub)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* (error "COMPILE-FILE or LOAD this.")))

(defun resource-file (name type)
  (make-pathname :name name :type type :defaults *here*))

(defun make-element (parent tag attributes)
  (let ((element (plump-dom:make-element parent tag)))
    (loop for (key val) in attributes
          do (setf (plump-dom:attribute element key) val))
    element))

(defmacro with-element (parent &body elements)
  (let ((parentg (gensym "PARENT")))
    `(let ((,parentg ,parent))
       ,@(loop for (tag attributes . children) in elements
               collect (case tag
                         (:text `(plump-dom:make-text-node ,parentg ,attributes))
                         (:extra `(let ((,(first attributes) ,parentg))
                                    ,@children))
                         (T
                          `(with-element (make-element ,parentg ,tag ',attributes)
                             ,@children))))
       ,parentg)))

(defmacro with-xml (&body elements)
  (let ((root (gensym "ROOT")))
    `(let ((plump:*tag-dispatchers* plump:*xml-tags*)
           (,root (plump-dom:make-root)))
       (let ((header (plump-dom:make-xml-header ,root)))
         (setf (plump-dom:attribute header "version") "1.0")
         (setf (plump-dom:attribute header "encoding") "utf-8"))
       (with-element ,root
         ,@elements)
       (plump-dom:serialize ,root NIL))))

(trivial-indent:define-indentation with-element (4 (&whole 2 4 6 (&whole 2 4 6))))
(trivial-indent:define-indentation with-xml ((&whole 2 4 &body)))

(defun format-date (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (s m h dd mm yy) (decode-universal-time universal-time 0)
    (format NIL "~4,'0d-~2,'0d-~2,'0d" yy mm dd)))
