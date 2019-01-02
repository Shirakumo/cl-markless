#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless)

(defun output (component &key (target T) (format 'markless))
  (let ((format (etypecase format
                  (output-format format)
                  (symbol (make-instance format)))))
    (typecase component
      (components:component
       (typecase target
         ((eql NIL)
          (with-output-to-string (target)
            (output-component component target format)))
         ((eql T)
          (output-component component *standard-output* format))
         (T
          (output-component component target format))))
      (T
       (output (parse component T) :target target :format format)))))

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

(defmethod output-component (component (target pathname) format)
  (with-open-file (stream target :direction :output
                                 :element-type 'character)
    (output-component component stream format)))

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

(defvar *prefixes* ())
(define-output markless (c s)
  (vector ()
    (when (< 0 (length c))
      (output (aref c 0)))
    (loop for i from 1 below (length c)
          for child = (aref c i)
          do (when (typep child 'components:block-component)
               (format s "~%~{~a~}" (reverse *prefixes*)))
             (output child)))
  
  (string ()
    (loop for char across c
          do (case char
               (#\\
                (format s "\\\\"))
               (#\Linefeed
                (format s "~%~{~a~}" (reverse *prefixes*)))
               (T
                (write-char char s)))))

  (components:unit-component ()
    (format s "~%~{~a~}" (reverse *prefixes*)))

  (components:parent-component ()
    (output (components:children c)))
  
  (components:paragraph ()
    (output-children))

  (components:blockquote-header ()
    (format s "~~ ")
    (output-children))
  
  (components:blockquote ()
    (format s "| ")
    (let ((*prefixes* (list* "| " *prefixes*)))
      (output (components:children c))))

  (components:ordered-list-item ()
    (format s "~d. " (components:number c))
    (output (components:children c)))

  (components:unordered-list-item ()
    (format s "- ")
    (output (components:children c)))

  (components:header ()
    (format s "~v@{#~} " (components:depth c) NIL)
    (output (components:children c)))

  (components:horizontal-rule ()
    (format s "=="))

  (components:code-block ()
    (format s "~v@{:~}~@[ ~a~{, ~a~}~]" (components:depth c) (components:language c) (components:options c))
    (write-string (components:text c) s)
    (format s "~&~v@{:~}" (components:depth c) NIL))

  (components:instruction ()
    (format s "! ~(~a~)" (type-of c)))
  
  (components:message-instruction ()
    (format s "! ~(~a~) ~a" (type-of c) (components:message c)))

  (components:directives-instruction ()
    (format s "! ~(~a~)~{ ~a~}" (type-of c) (components:directives c)))

  (components:set ()
    (format s "! ~(~a~) ~a ~a" (type-of c) (components:variable c) (components:value c)))

  (components:include ()
    (format s "! ~(~a~) ~a" (type-of c) (components:file c)))

  (components:label ()
    (format s "! ~(~a~) ~a" (type-of c) (components:target c)))

  (components:comment ()
    (format s "; ~a" (components:text c)))

  (components:embed ()
    (format s "[ ~(~a~) ~a"
            (type-of c) (components:target c))
    (loop for option in (components:options c)
          do (format s ", ")
             (output option)))

  (components:embed-option ()
    (format s "~(~a~)" (type-of c)))

  (components:width-option ()
    (format s "width ~a~a" (components:size c) (components:unit c)))

  (components:height-option ()
    (format s "height ~a~a" (components:size c) (components:unit c)))

  (components:float-option ()
    (format s "float ~(~a~)" (components:direction c)))

  (components:footnote ()
    (format s "[~d] " (components:target c))
    (output (components:children c)))

  (components:bold ()
    (format s "**")
    (output (components:children c))
    (format s "**"))

  (components:italic ()
    (format s "//")
    (output (components:children c))
    (format s "//"))

  (components:underline ()
    (format s "__")
    (output (components:children c))
    (format s "__"))

  (components:strikethrough ()
    (format s "<-")
    (output (components:children c))
    (format s "->"))

  (components:code ()
    (format s "``~a``" (components:text c)))

  (components:subtext ()
    (format s "v(")
    (output (components:children c))
    (format s ")"))

  (components:supertext ()
    (format s "^(")
    (output (components:children c))
    (format s ")"))

  (components:url ()
    (write-string (components:text c) s))

  (components:compound ()
    (format s "\"")
    (output (components:children c))
    (format s "\"(")
    (when (components:options c)
      (output (first (components:options c)))
      (loop for option in (rest (components:options c))
            do (format s ", ")
               (output option)))
    (format s ")"))

  (components:compound-option ()
    (format s "~(~a~)" (type-of c)))

  (components:font-option ()
    (format s "font ~a" (components:font-family c)))

  (components:color-option ()
    (let ((r (components:red c))
          (g (components:green c))
          (b (components:blue c)))
      (loop for name being the hash-keys of *color-table*
            for option being the hash-values of *color-table*
            do (when (and (= r (components:red option))
                          (= g (components:green option))
                          (= b (components:blue option)))
                 (return (format s "~(~a~)" name)))
            finally (format s "color ~a ~a ~a"  r g b))))
  
  (components:size-option ()
    (let ((size (components:size c))
          (unit (components:unit c)))
      (loop for name being the hash-keys of *size-table*
            for option being the hash-values of *size-table*
            do (when (and (= size (components:size option))
                          (eql unit (components:unit option)))
                 (return (format s "~(~a~)" name)))
            finally (format s "size ~a~(~a~)" size unit))))

  (components:internal-link-option ()
    (format s "#~a" (components:target c)))

  (components:link-option ()
    (if (/= 0 (read-url (components:target c) 0))
        (format s "~a" (components:target c))
        (format s "link ~a" (components:target c))))

  (components:footnote-reference ()
    (format s "[~d]" (components:target c))))
