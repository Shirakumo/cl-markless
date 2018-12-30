#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless)

(defun output (component &key (target T) (format 'markless))
  (typecase component
    (components:component
     (typecase target
       (pathname
        (with-open-file (target target :direction :output
                                       :element-type 'character)
          (output-component component target format)))
       ((eql NIL)
        (with-output-to-string (target)
          (output-component component target format)))
       ((eql T)
        (output-component component *standard-output* format))
       (T
        (output-component component target format))))
    (T
     (output (parse component T) :target target :format format))))

(defclass output-format () ())

(defun list-output-formats ()
  (flet ((subclasses (class)
           #+abcl      (mop:class-direct-subclasses class)
           #+allegro   (mop:class-direct-subclasses class)
           #+clisp     (clos:class-direct-subclasses class)
           #+clozure   (ccl:class-direct-subclasses class)
           #+cmu       (clos-mop:class-direct-subclasses class)
           #+ecl       (clos:class-direct-subclasses class)
           #+lispworks (clos:class-direct-subclasses class)
           #+mcl       (ccl:class-direct-subclasses class)
           #+sbcl      (sb-mop:class-direct-subclasses class)
           #+scl       (clos:class-direct-subclasses class)))
    (let ((formats ()))
      (labels ((traverse (class)
                 (dolist (subclass (subclasses class))
                   (pushnew subclass formats)
                   (traverse subclass))))
        (traverse (find-class 'output-format))
        (sort (mapcar #'class-name formats) #'string<)))))

(defgeneric output-component (component target format))

(defmethod output-component (component target (format symbol))
  (output-component component target (make-instance format)))

(defmethod output-component ((component components:unit-component) target format))

(defmethod output-component ((string string) (stream stream) format)
  (write-string string stream))

(defmacro define-output (format (component stream) &body methods)
  `(progn
     (defclass ,format (output-format) ())
     ,@(loop for (class qualifiers . body) in methods
             collect `(defmethod output-component ,@qualifiers ((,component ,class) (,stream stream) (_ ,format))
                        (labels ((output (,component)
                                   (output-component ,component ,stream _))
                                 (output-children ()
                                   (loop for child across (components:children ,component)
                                         do (output child))))
                          (declare (ignorable #'output #'output-children))
                          ,@body)))))

(trivial-indent:define-indentation define-output (4 6 &rest (&whole 2 4 &body)))

(defvar *level* 0)
(define-output debug (c s)
  (T (:before)
    (format s "~&~v@{|  ~}" *level* NIL))

  (string ()
    (format s " ")
    (loop for char across c
          do (case char
               (#\Newline (write-char #\Return s))
               (T (write-char char s)))))

  (components:component ()
    (format s " ~a" (type-of c)))
  
  (components:parent-component ()
    (format s "/~a" (type-of c)))
  
  (components:parent-component (:after)
    (let ((*level* (1+ *level*)))
      (output-children)))

  (components:ordered-list-item ()
    (format s "/~a (~d)" (type-of c) (components:number c)))

  (components:header ()
    (format s "/~a (~d)" (type-of c) (components:depth c)))

  (components:code-block ()
    (format s "/~a ~s~{ ~a~}" (type-of c) (components:language c) (components:options c)))

  (components:message-instruction ()
    (format s " ~a ~s" (type-of c) (components:text c)))

  (components:set ()
    (format s " ~a ~a => ~a" (type-of c) (components:variable c) (components:value c)))

  (components:include ()
    (format s " ~a ~s" (type-of c) (components:file c)))

  (components:directives-instruction ()
    (format s " ~a ~a" (type-of c) (components:directives c)))

  (components:embed ()
    (format s " ~a ~s" (type-of c) (components:target c)))

  (components:footnote ()
    (format s "/~a (~d)" (type-of c) (components:target c)))

  (components:footnote-reference ()
    (format s " ~a (~d)" (type-of c) (components:target c)))

  (components:compound ()
    (format s "/~a ~{~a~}" (type-of c) (components:options c))))

;; FIXME: stacks on lines
(defvar *prefixes* ())
(define-output markless (c s)
  ;; FIXME: escape string writes
  
  (components:block-component (:before)
    ;; FIXME: only do this on new lines...
    (format s "~{~a~}" (reverse *prefixes*)))
  
  (components:block-component (:after)
    (fresh-line))
  
  (components:paragraph ()
    (output-children))

  (components:blockquote-header ()
    (format s "~~ ")
    (output-children))
  
  (components:blockquote ()
    (format s "| ")
    (let ((*prefixes* (list* "| " *prefixes*)))
      (output-children)))

  (components:ordered-list-item ()
    (format s "~d. " (components:number c))
    (output-children))

  (components:unordered-list-item ()
    (format s "- ")
    (output-children))

  (components:header ()
    (format s "~v@{#~} " (components:depth c) NIL)
    (output-children))

  (components:horizontal-rule ()
    (format s "=="))

  (components:code-block ()
    (format s "::~@[ ~a~{ ~a~}~]" (components:language c) (components:options c))
    (write-string (components:text c) s)
    (format s "~&::"))
  
  (components:instruction ()
    (format s "! ")
    ;; FIXME: decode instructions
    )

  (components:comment ()
    (format s "; ~a" (components:text c)))

  (components:embed ()
    (format s "[ ~(~a~) ~a"
            (type-of c) (components:target c))
    (loop for option in (components:options c)
          do (format s ", ")
             (output option)))

  (components:footnote ()
    (format s "[~d] " (components:target c))
    (output-children))

  (components:bold ()
    (format s "**")
    (output-children)
    (format s "**"))

  (components:italic ()
    (format s "//")
    (output-children)
    (format s "//"))

  (components:underline ()
    (format s "__")
    (output-children)
    (format s "__"))

  (components:strikethrough ()
    (format s "<-")
    (output-children)
    (format s "->"))

  (components:code ()
    (format s "``~a``" (components:text c)))

  (components:subtext ()
    (format s "v(")
    (output-children)
    (format s ")"))

  (components:supertext ()
    (format s "^(")
    (output-children)
    (format s ")"))

  (components:url ()
    (write-string (components:text c) s))

  (components:compound ()
    (format s "\"")
    (output-children)
    (format s "\"(in")
    (dolist (option (components:options c))
      (output option))
    (format s ")"))

  ;; FIXME: compound options

  (components:footnote-reference ()
    (format s "[~d]" (components:target c))))
