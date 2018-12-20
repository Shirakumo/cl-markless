#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless)

(defun output (component &key (stream T) (format :markless))
  (etypecase stream
    (stream
     (output-component component stream format))
    ((eql NIL)
     (with-output-to-string (stream)
       (output-component component stream format)))
    ((eql T)
     (output-component component *standard-output* format)))
  component)

(defmacro define-output (format (component stream) &body methods)
  `(progn
     ,@(loop for (class qualifiers . body) in methods
             collect `(defmethod output-component ,@qualifiers ((,component ,class) ,stream (_ (eql ,format)))
                        (labels ((output (,component)
                                   (output-component ,component ,stream _))
                                 (output-children ()
                                   (loop for child across (components:children ,component)
                                         do (output child))))
                          (declare (ignorable #'output #'output-children))
                          ,@body)))))

(defmethod output-component ((root components:parent-component) stream format)
  (loop for child across (components:children root)
        do (output-component child stream format)))

(defmethod output-component ((string string) stream format)
  (write-string string stream))

(defvar *level* 0)
(define-output :debug (c s)
  (components:parent-component ()
   (format s "~&~v@{|  ~}/~a" *level* (type-of c)))

  (components:component ()
   (format s "~&~v@{|  ~} ~a" *level* (type-of c)))

  (string ()
   (format s "~&~v@{|  ~} ~s" *level* (string-trim '(#\Newline) c))))

;; FIXME: stacks on lines
(define-output :markless (c s)
  ;; FIXME: escape string writes
  (components:block-component (:after)
   (fresh-line s))
  
  (components:paragraph ()
   (output-children))
  
  (components:blockquote ()
   (format s "| ")
   (output-children))

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
   (format s "[ ~(~a~) ~a~@[float ~a~]~@[width ~a~]~@[height ~a~] ]"
           (type-of c) (components:target c) (components:float c) (components:width c) (components:height c)))

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
