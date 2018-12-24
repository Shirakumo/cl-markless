#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:cl-markless-test
  (:nicknames #:org.shirakumo.markless.test)
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:components #:org.shirakumo.markless.components)))
(in-package #:org.shirakumo.markless.test)

(defparameter *here* (make-pathname :name NIL :type NIL
                                    :defaults #.(or *compile-file-pathname* *load-pathname*
                                                    (error "COMPILE-FILE or LOAD this."))))
(defparameter *test-case-directory* (merge-pathnames "tests/" *here*))

(define-test cl-markless)

(defun reloc-symbol (symbol)
  (intern (string symbol) #.*package*))

(defgeneric to-ast (component))

(defmethod to-ast ((_ string)) _)

(defmethod to-ast ((_ components:component))
  (list (reloc-symbol (type-of _))))

(defmethod to-ast ((_ components:root-component))
  (list* 'root
         (loop for child across (cl-markless::condense-children (components:children _))
               collect (to-ast child))))

(defmethod to-ast ((_ components:text-component))
  (list (reloc-symbol (type-of _))
        (to-ast (components:text _))))

(defmethod to-ast ((_ components:parent-component))
  (list* (reloc-symbol (type-of _))
         (loop for child across (cl-markless::condense-children (components:children _))
               collect (to-ast child))))

(defmethod to-ast ((_ components:ordered-list-item))
  (list* (reloc-symbol (type-of _))
         (list (components:number _))
         (loop for child across (cl-markless::condense-children (components:children _))
               collect (to-ast child))))

(defmethod to-ast ((_ components:header))
  (list* (reloc-symbol (type-of _))
         (list (components:depth _))
         (loop for child across (cl-markless::condense-children (components:children _))
               collect (to-ast child))))

(defmethod to-ast ((_ components:code-block))
  (list (reloc-symbol (type-of _))
        (when (components:language _) (list* (components:language _) (components:options _)))
        (to-ast (components:text _))))

(defmethod to-ast ((_ components:embed))
  (list* 'embed
         (reloc-symbol (type-of _))
         (components:target _)
         (mapcar #'to-ast (components:options _))))

(defmethod to-ast ((_ components:autoplay-option))
  (list 'autoplay))

(defmethod to-ast ((_ components:loop-option))
  (list 'loop))

(defmethod to-ast ((_ components:width-option))
  (list 'width
        (components:size _)
        (reloc-symbol (components:unit _))))

(defmethod to-ast ((_ components:height-option))
  (list 'height
        (components:size _)
        (reloc-symbol (components:unit _))))

(defmethod to-ast ((_ components:float-option))
  (list 'float
        (reloc-symbol (components:direction _))))

(defmethod to-ast ((_ components:footnote))
  (list* (reloc-symbol (type-of _))
         (list (components:target _))
         (loop for child across (cl-markless::condense-children (components:children _))
               collect (to-ast child))))

(defmethod to-ast ((_ components:footnote-reference))
  (list (reloc-symbol (type-of _))
        (components:target _)))

(defmethod to-ast ((_ components:compound))
  (list* (reloc-symbol (type-of _))
         (mapcar #'to-ast (components:options _))
         (loop for child across (cl-markless::condense-children (components:children _))
               collect (to-ast child))))

(defmethod to-ast ((_ components:bold-option))
  (list 'bold))

(defmethod to-ast ((_ components:italic-option))
  (list 'italic))

(defmethod to-ast ((_ components:underline-option))
  (list 'underline))

(defmethod to-ast ((_ components:strikethrough-option))
  (list 'strikethrough))

(defmethod to-ast ((_ components:spoiler-option))
  (list 'spoiler))

(defmethod to-ast ((_ components:font-option))
  (list 'font
        (components:font-family _)))

(defmethod to-ast ((_ components:color-option))
  (list 'color
        (components:red _)
        (components:green _)
        (components:blue _)))

(defmethod to-ast ((_ components:size-option))
  (list 'size
        (components:size _)
        (reloc-symbol (components:unit _))))

(defmethod to-ast ((_ components:hyperlink-option))
  (list 'hyperlink
        (components:target _)))

(defun ensure-ast (thing)
  (etypecase thing
    (components:component
     (to-ast thing))
    (list
     thing)
    (string
     (to-ast (cl-markless:parse thing T)))))

(defun ast= (a b)
  (labels ((r (a b)
             (typecase a
               (number (and (numberp b) (= a b)))
               (list (and (listp b)
                          (= (length a) (length b))
                          (loop for ai in a
                                for bi in b
                                always (r ai bi))))
               (T (equal a b)))))
    (r (ensure-ast a)
       (ensure-ast b))))

(defclass ast-result (value-result)
  ((expected :initarg :expected :accessor expected)))

(defmethod format-result ((result ast-result) (type (eql :extensive)))
  (let ((*print-right-margin* 600))
    (format NIL "~a
was parsed to ~20t~s
which is not ast= ~20t~s"
            (expression result)
            (if (typep (value result) 'components:component)
                (to-ast (value result))
                (value result))
            (expected result))))

(defmethod eval-in-context (context (result ast-result))
  (call-next-method)
  (when (eql :unknown (status result))
    (if (ast= (value result) (expected result))
        (setf (status result) :passed)
        (setf (status result) :failed))))

(defun parse-test-file (file)
  (let ((*package* #.*package*)
        (source (make-string-output-stream)))
    (with-open-file (stream file :direction :input
                                 :element-type 'character)
      (loop while (peek-char NIL stream NIL)
            do (loop for line = (read-line stream)
                     until (string= line "~~")
                     do (write-line line source))
            collect (list (let ((string (get-output-stream-string source)))
                            (subseq string 0 (max 0 (1- (length string)))))
                          (read stream))
            do (read-line stream NIL)))))

(defun compile-test-case (test-case)
  (destructuring-bind (input expected-structure) test-case
    (lambda ()
      (eval-in-context *context*
                       (make-instance 'ast-result
                                      :expression input
                                      :body (lambda () (cl-markless:parse input T))
                                      :expected expected-structure)))))

(defun create-test-from-file (file)
  (let* ((name (pathname-name file))
         (test-cases (parse-test-file file))
         (compiled-tests (mapcar #'compile-test-case test-cases)))
    (setf (find-test name #.*package*)
          (make-instance 'test
                         :name name
                         :home #.*package*
                         :tests compiled-tests
                         :parent 'cl-markless))))

(defun list-test-cases (&optional (directory *test-case-directory*))
  (directory (make-pathname :name :wild :type "test" :defaults directory)))

(defun compile-all-test-cases (&optional (directory *test-case-directory*))
  (mapcar #'create-test-from-file (list-test-cases directory)))

(compile-all-test-cases)
