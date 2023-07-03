(defpackage #:cl-markless-markdown
  (:nicknames #:org.shirakumo.markless.markdown)
  (:use #:cl #:org.shirakumo.markless)
  (:local-nicknames
   (#:components #:org.shirakumo.markless.components))
  (:shadow #:parse)
  (:shadowing-import-from #:org.shirakumo.markless #:debug)
  (:export
   #:parse
   #:convert
   #:html-tag))
(in-package #:org.shirakumo.markless.markdown)

(defvar *root*)

(defclass markdown (output-format) ())

(defun ensure-string (stringish)
  (etypecase stringish
    (string
     stringish)
    (pathname
     (with-open-file (stream stringish :direction :input
                                       :element-type 'character)
       (ensure-string stream)))
    (stream
     (with-output-to-string (stream)
       (loop with buffer = (make-string 1024)
             for read = (read-sequence buffer stringish)
             while (< 0 read)
             do (write-sequence buffer stream :end read))))))

(defun parse (markdown)
  (let* ((3bmd-code-blocks::*colorize-name-map* (make-hash-table :size 0))
         (3bmd-code-blocks:*code-blocks* T)
         (string (ensure-string markdown))
         (input (3bmd::expand-tabs string :add-newlines t))
         (ast (3bmd-grammar::parse-doc input)))
    (translate :root ast)))

(defun convert (markdown &key (output (make-pathname :type "mess" :defaults markdown))
                              (if-exists :error))
  (with-open-file (stream output :direction :output
                                 :if-exists if-exists)
    (output (parse markdown) :target stream)))

(defgeneric translate (type body))

(defmacro define-translation (type structure &body body)
  (let ((typeg (gensym "TYPE")) (bodyg (gensym "BODY")))
    `(defmethod translate ((,typeg (eql ',type)) ,bodyg)
       (flet ((translate (structure)
                (translate (car structure) (cdr structure))))
         (declare (ignorable #'translate))
         (destructuring-bind ,structure ,bodyg
           ,@body)))))

(defun insert-children (parent items)
  (loop with children = (components:children parent)
        for item in items
        for child = (etypecase item
                      (cons (translate (car item) (cdr item)))
                      (string item))
        do (vector-push-extend child children))
  parent)

(define-translation :root (&rest items)
  (let ((*root* (make-instance 'components:root-component)))
    (insert-children *root* items)))

(define-translation :plain (&rest items)
  (let ((parent (make-instance 'components:parent-component)))
    (insert-children parent items)))

(define-translation :heading (&key level contents)
  (let ((header (make-instance 'components:header :depth level)))
    (insert-children header contents)
    (setf (components:label (components:text header) *root*) header)
    header))

(define-translation :paragraph (&rest items)
  (let ((paragraph (make-instance 'components:paragraph)))
    (insert-children paragraph items)))

(define-translation :block-quote (&rest items)
  (let ((blockquote (make-instance 'components:blockquote)))
    (insert-children blockquote items)))

(define-translation :explicit-link (&key label source title)
  (declare (ignore title))
  (let ((compound (make-instance 'components:compound)))
    (push (make-instance 'components:link-option :target source)
          (components:options compound))
    (insert-children compound label)))

(define-translation :image (link)
  (destructuring-bind (&key label source title) (cdr link)
    (declare (ignore label title))
    (make-instance 'components:image :target source)))

(define-translation :emph (&rest items)
  (let ((italic (make-instance 'components:italic)))
    (insert-children italic items)))

(define-translation :strong (&rest items)
  (let ((bold (make-instance 'components:bold)))
    (insert-children bold items)))

(define-translation :horizontal-rule ()
  (make-instance 'components:horizontal-rule))

(define-translation :bullet-list (&rest items)
  (let ((list (make-instance 'components:unordered-list)))
    (dolist (item items) (setf (car item) :bullet-list-item))
    (insert-children list items)))

(define-translation :bullet-list-item (&rest items)
  (let ((item (make-instance 'components:unordered-list-item)))
    (insert-children item items)))

(define-translation :counted-list (&rest items)
  (let ((list (make-instance 'components:ordered-list)))
    (loop for item in items
          for i from 1
          do (setf (car item) :counted-list-item)
             (setf (cdr item) (cons i (cdr item))))
    (insert-children list items)))

(define-translation :counted-list-item (number &rest items)
  (let ((item (make-instance 'components:ordered-list-item :number number)))
    (insert-children item items)))

(define-translation :link (target)
  (make-instance 'components:url :target target))

(define-translation :code (text)
  (let ((code (make-instance 'components:code)))
    (vector-push-extend text (components:children code))
    code))

(define-translation :verbatim (text)
  (make-instance 'components:code-block :text text))

(define-translation 3bmd-code-blocks::code-block (&key lang params content)
  (make-instance 'components:code-block :language lang :options params :text content))

(define-translation :reference-link (&key label definition tail)
  (declare (ignore tail))
  (let ((target (format NIL "~{~a~}" label)))
    (cond ((and (null definition) (loop for char across target
                                        always (<= (char-code #\0) (char-code char) (char-code #\9))))
           (make-instance 'components:footnote-reference :target target))
          (T
           (let ((compound (make-instance 'components:compound)))
             (push (make-instance 'components:link-option :target (or definition target))
                   (components:options compound))
             (insert-children compound label))))))

(define-translation :reference (&key label source title)
  (let* ((target (format NIL "~{~a~}" label))
         (footnote (make-instance 'components:footnote :target target)))
    (setf (components:label target *root*) footnote)
    (vector-push-extend (make-instance 'components:url :target source)
                        (components:children footnote))
    (when title
      (vector-push-extend (format NIL " ~a" title)
                          (components:children footnote)))
    footnote))

(define-translation :line-break ()
  (make-instance 'components:newline))

(defclass html-tag (components:unit-component)
  ((tag :initarg :tag :initform (error "TAG required") :accessor tag)))

(defmethod output-component ((component html-tag) target format))

(define-translation :raw-html (raw)
  (make-instance 'html-tag :tag raw))
