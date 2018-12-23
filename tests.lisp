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

(defgeneric to-ast (component))

(defmethod to-ast ((_ string)) _)

(defmethod to-ast ((_ components:component))
  (list (intern (string (type-of _)) #.*package*)))

(defmethod to-ast ((_ components:root-component))
  (list* 'root
         (loop for child across (components:children _)
               collect (to-ast child))))

(defmethod to-ast ((_ components:parent-component))
  (list* (intern (string (type-of _)) #.*package*)
         (loop for child across (components:children _)
               collect (to-ast child))))

(defun ensure-ast (thing)
  (etypecase thing
    (components:component
     (to-ast thing))
    (list
     thing)
    (string
     (to-ast (cl-markless:parse thing T)))))

(defun ast= (a b)
  (let ((a (ensure-ast a))
        (b (ensure-ast b)))
    ;; FIXME: some leniency is allowed.
    (equal a b)))

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
            collect (cons (string-right-trim '(#\Linefeed) (get-output-stream-string source))
                          (read stream))))))

(defun compile-test-case (test-case)
  (destructuring-bind (input . expected-structure) test-case
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
