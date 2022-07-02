#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless)

(defun output (component &rest initargs &key (target T) (format 'markless) &allow-other-keys)
  (let* ((initargs (remf* initargs :target :format))
         (format (etypecase format
                   (output-format
                    (apply #'reinitialize-instance format initargs))
                   (symbol
                    (apply #'make-instance format initargs)))))
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
  (let ((formats ()))
    (labels ((traverse (class)
               (dolist (subclass (subclasses class))
                 (pushnew subclass formats)
                 (traverse subclass))))
      (traverse (find-class 'output-format))
      (sort (mapcar #'class-name formats) #'string<))))

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
  (destructuring-bind (format &optional superclasses slots) (if (listp format) format (list format ()))
    `(progn
       (defclass ,format (,@superclasses output-format) ,slots)
       ,@(loop for (class qualifiers . body) in methods
               collect `(defmethod output-component ,@qualifiers ((,component ,class) (,stream stream) (_ ,format))
                          (labels ((output (,component &optional (,stream ,stream))
                                     (output-component ,component ,stream _))
                                   (output-children (&optional (,stream ,stream))
                                     (loop for child across (components:children ,component)
                                           do (output child ,stream))))
                            (declare (ignorable #'output #'output-children))
                            ,@body))))))

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

(define-output (bbcode () ((supported-tags :initform '(:quote :list :h :hr :code :img :video :b :i :u :s :fixed :sub :super :url :size :color :spoiler) :initarg :supported-tags :accessor supported-tags))) (c s)
  (vector ()
    (when (< 0 (length c))
      (output (aref c 0))
      (loop for i from 1 below (length c)
            for child = (aref c i)
            do (when (typep child 'components:block-component)
                 (format s "~%")
                 (when (and (not (typep child 'components:list-item))
                            (not (typep (aref c (1- i)) 'components:header)))
                   (format s "~%")))
               (output child))))
  
  (string ()
    (write-string c s))
  
  (components:parent-component ()
    (output (components:children c)))

  (components:blockquote-header ())
  
  (components:blockquote ()
    (cond ((find :quote (supported-tags _))
           (if (components:source c)
               (format s "[quote=~s]~%"
                       (with-output-to-string (o)
                         (output (components:source c) o)))
               (format s "[quote]~%"))
           (output (components:children c))
           (format s "[/quote]~%"))
          (T
           (when (components:source c)
             (format s "~a said:~%"
                     (with-output-to-string (o)
                       (output (components:source c) o))))
           (output (components:children c))
           (format s "~%"))))

  (components:ordered-list ()
    (cond ((find :list (supported-tags _))
           (format s "[list=1]~%")
           (output (components:children c))
           (format s "~&[/list]~%"))
          (T
           (output (components:children c)))))

  (components:unordered-list ()
    (cond ((find :list (supported-tags _))
           (format s "[list]~%")
           (output (components:children c))
           (format s "~%[/list]~%"))
          (T
           (output (components:children c)))))

  (components:list-item ()
    (cond ((find :list (supported-tags _))
           (format s "[*] ")
           (output (components:children c)))
          (T
           (format s "- ")
           (output (components:children c)))))

  (components:header ()
    (cond ((find :h (supported-tags _))
           (format s "[h~d]" (components:depth c))
           (output (components:children c))
           (format s "[/h~d]" (components:depth c)))
          ((find :b (supported-tags _))
           (format s "[b]")
           (output (components:children c))
           (format s "[/b]"))
          (T
           (format s "~v@{#~} " (components:depth c) NIL)
           (output (components:children c)))))

  (components:horizontal-rule ()
    (if (find :hr (supported-tags _))
        (format s "[hr]~%")
        (format s "=================~%")))

  (components:code-block ()
    (if (find :code (supported-tags _))
        (format s "[code]~%~a~%[/code]~%" (components:text c))
        (format s "```~%~a~%```~%" (components:text c))))

  (components:instruction ())
  (components:comment ())
  (components:embed ())

  (components:image ()
    (cond ((find :img (supported-tags _))
           (format s "[img]~a[/img]" (components:target c)))
          ((find :url (supported-tags _))
           (format s "[url]~a[/url]" (components:target c)))
          (T
           (format s "image: ~a" (components:target c)))))

  (components:video ()
    (cond ((find :img (supported-tags _))
           (format s "[video]~a[/video]" (components:target c)))
          ((find :url (supported-tags _))
           (format s "[url]~a[/url]" (components:target c)))
          (T
           (format s "video: ~a" (components:target c)))))

  (components:footnote ()
    (format s "[~d] " (components:target c))
    (output (components:children c)))

  (components:bold ()
    (cond ((find :b (supported-tags _))
           (format s "[b]")
           (output (components:children c))
           (format s "[/b]"))
          (T
           (format s "*")
           (output (components:children c))
           (format s "*"))))

  (components:italic ()
    (cond ((find :i (supported-tags _))
           (format s "[i]")
           (output (components:children c))
           (format s "[/i]"))
          (T
           (format s "/")
           (output (components:children c))
           (format s "/"))))

  (components:underline ()
    (cond ((find :u (supported-tags _))
           (format s "[u]")
           (output (components:children c))
           (format s "[/u]"))
          (T
           (format s "_")
           (output (components:children c))
           (format s "_"))))

  (components:strikethrough ()
    (cond ((find :s (supported-tags _))
           (format s "[s]")
           (output (components:children c))
           (format s "[/s]"))
          (T
           (format s "<-")
           (output (components:children c))
           (format s "->"))))

  (components:code ()
    (cond ((find :fixed (supported-tags _))
           (format s "[fixed]~a[/fixed]" (components:text c)))
          ((find :code (supported-tags _))
           (format s "[code]~a[/code]" (components:text c)))
          (T
           (format s "~a" (components:text c)))))

  (components:subtext ()
    (cond ((find :sub (supported-tags _))
           (format s "[sub]")
           (output (components:children c))
           (format s "[sub]"))
          (T
           (output (components:children c)))))

  (components:supertext ()
    (cond ((find :super (supported-tags _))
           (format s "[super]")
           (output (components:children c))
           (format s "[super]"))
          (T
           (output (components:children c)))))

  (components:url ()
    (if (find :url (supported-tags _))
        (format s "[url]~a[/url]" (components:target c))
        (format s "~a" (components:target c))))

  (components:compound ()
    (loop for o in (components:options c)
          do (typecase o
               (components:link-option
                (when (find :url (supported-tags _))
                  (format s "[url=~a]"
                          (components:target o))))
               (components:size-option
                (when (find :size (supported-tags _))
                  (format s "[size=\"~a~a\"]"
                          (components:size o) (components:unit o))))
               (components:color-option
                (when (find :color (supported-tags _))
                  (format s "[color=\"~2,'0x~2,'0x~2,'0x\"]"
                          (components:red o) (components:green o) (components:blue o))))
               (components:spoiler-option
                (when (find :spoiler (supported-tags _))
                  (format s "[spoiler]")))
               (components:bold-option
                (if (find :b (supported-tags _))
                    (format s "[b]")
                    (format s "*")))
               (components:italic-option
                (if (find :i (supported-tags _))
                    (format s "[i]")
                    (format s "/")))
               (components:underline-option
                (if (find :u (supported-tags _))
                    (format s "[u]")
                    (format s "_")))
               (components:strikethrough-option
                (if (find :s (supported-tags _))
                    (format s "[s]")
                    (format s "<-")))))
    (output (components:children c))
    (loop for o in (reverse (components:options c))
          do (typecase o
               (components:link-option
                (if (find :url (supported-tags _))
                    (format s "[/url]")
                    (format s "<~a>" (components:target o))))
               (components:size-option
                (when (find :size (supported-tags _))
                  (format s "[/size]")))
               (components:color-option
                (when (find :color (supported-tags _))
                  (format s "[/color]")))
               (components:spoiler-option
                (when (find :spoiler (supported-tags _))
                  (format s "[/spoiler]")))
               (components:bold-option
                (if (find :b (supported-tags _))
                    (format s "[/b]")
                    (format s "*")))
               (components:italic-option
                (if (find :i (supported-tags _))
                    (format s "[/i]")
                    (format s "/")))
               (components:underline-option
                (if (find :u (supported-tags _))
                    (format s "[/u]")
                    (format s "_")))
               (components:strikethrough-option
                (if (find :s (supported-tags _))
                    (format s "[/s]")
                    (format s "->"))))))

  (components:footnote-reference ()
    (format s "[~d]" (components:target c))))

(defvar *prefixes* ())
(define-output markless (c s)
  (vector ()
    (when (< 0 (length c))
      (output (aref c 0))
      (loop for i from 1 below (length c)
            for child = (aref c i)
            do (when (typep child 'components:block-component)
                 (format s "~%~{~a~}" (reverse *prefixes*))
                 (when (and (not (typep child 'components:list-item))
                            (not (typep (aref c (1- i)) 'components:header)))
                   (format s "~%~{~a~}" (reverse *prefixes*))))
               (output child))))
  
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
    (let* ((prefix (make-string (components:indentation c) :initial-element #\ ))
           (*prefixes* (list* prefix *prefixes*)))
      (write-string prefix s)
      (output (components:children c))))

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
    (format s "~v@{:~}~@[ ~a~{, ~a~}~]" (+ 2 (components:depth c)) (components:language c) (components:options c))
    (format s "~&~a" (components:text c))
    (format s "~&~v@{:~}" (+ 2 (components:depth c)) NIL))

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
             (output option))
    (format s " ]"))

  (components:embed-option ()
    (format s "~(~a~)" (type-of c)))

  (components:width-option ()
    (format s "width ~a~a" (components:size c) (components:unit c)))

  (components:height-option ()
    (format s "height ~a~a" (components:size c) (components:unit c)))

  (components:float-option ()
    (format s "float ~(~a~)" (components:direction c)))

  (components:label-option ()
    (format s "label ~a" (components:target c)))

  (components:caption-option ()
    (format s "caption ")
    (output (components:children c)))

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
    (write-string (components:target c) s))

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
    (if (read-url (components:target c) 0)
        (format s "~a" (components:target c))
        (format s "link ~a" (components:target c))))

  (components:footnote-reference ()
    (format s "[~d]" (components:target c))))
