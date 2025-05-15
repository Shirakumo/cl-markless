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
    (format s " ~a ~s" (type-of c) (components:target c))
    (let ((*level* (1+ *level*)))
      (dolist (option (components:options c))
        (output option))))

  (components:footnote ()
    (format s "/~a (~d)" (type-of c) (components:target c)))

  (components:footnote-reference ()
    (format s " ~a (~d)" (type-of c) (components:target c)))

  (components:compound ()
    (format s "/~a ~{~a~}" (type-of c) (components:options c))
    (let ((*level* (1+ *level*)))
      (dolist (option (components:options c))
        (output option))))

  (components:unit-component ()
    (format s " ~a" (type-of c))))

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

  (components:newline ()
    (write-char #\Linefeed s))
  
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

  (components:align ()
    (format s "[align=~(~a~)]" (components:alignment c))
    (output (components:children c))
    (format s "[/align]"))

  (components:raw ()
    (when (string-equal "bbcode" (components:target c))
      (write-string (components:text c) s)))

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
    (cond ((find :code (supported-tags _))
           (format s "[code]~a[/code]" (components:text c)))
          ((find :fixed (supported-tags _))
           (format s "[fixed]~a[/fixed]" (components:text c)))
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
    (format s "[~d]" (components:target c)))

  (components:en-dash ()
    (format s "–"))

  (components:em-dash ()
    (format s "—"))

  (components:newline ()
    (format s "~%")))

(defvar *prefixes* ())

(defmacro %op (s thing &rest args)
  `(output-operator (format NIL ,thing ,@args) ,s _))

(define-output markless (c s)
  (vector ()
    (when (< 0 (length c))
      (output (aref c 0))
      (loop for i from 1 below (length c)
            for child = (aref c i)
            do (when (typep child 'components:block-component)
                 (fresh-line s)
                 (loop for prefix in (reverse *prefixes*)
                       do (%op s "~a" prefix)))
               (output child))))
  
  (string ()
    (loop for char across c
          do (case char
               (#\\
                (format s "\\\\"))
               (#\Linefeed
                (fresh-line s)
                (loop for prefix in (reverse *prefixes*)
                      do (%op s "~a" prefix)))
               (T
                (write-char char s)))))

  (components:unit-component ()
    (fresh-line s)
    (loop for prefix in (reverse *prefixes*)
          do (%op s "~a" prefix)))

  (components:parent-component ()
    (output (components:children c)))
  
  (components:paragraph ()
    (let* ((prefix (make-string (components:indentation c) :initial-element #\ ))
           (*prefixes* (list* prefix *prefixes*)))
      (%op s "~a" prefix)
      (output (components:children c))))

  (components:blockquote-header ())

  (components:blockquote ()
    (cond ((and (components:source c) (< 0 (components:indentation c)))
           (%op s "~~ ")
           (loop for child across (components:children (components:source c))
                 do (output child))
           (%op s "| "))
          (T
           (%op s "~v@{ ~}| " (components:indentation c) NIL)))
    (let ((*prefixes* (list* (format NIL "~v@{ ~}| " (components:indentation c) NIL) *prefixes*)))
      (output (components:children c))))

  (components:ordered-list-item ()
    (let ((prefix (format NIL "~d. " (components:number c))))
      (%op s "~a" prefix)
      (let ((*prefixes* (list* (make-string (length prefix) :initial-element #\Space) *prefixes*)))
        (output (components:children c)))))

  (components:unordered-list-item ()
    (%op s "- ")
    (let ((*prefixes* (list* "  " *prefixes*)))
      (output (components:children c))))

  (components:header ()
    (%op s "~v@{#~} " (components:depth c) NIL)
    (output (components:children c)))

  (components:horizontal-rule ()
    (%op s "=="))

  (components:code-block ()
    (%op s "~v@{:~}" (+ 2 (components:depth c)) NIL)
    (format s "~@[ ~a~{, ~a~}~]"  (components:language c) (components:options c))
    (format s "~&~v@{ ~}" (components:inset c) NIL)
    (let ((*prefixes* (list* (format NIL "~v@{ ~}" (components:inset c) NIL) *prefixes*)))
      (output (components:text c)))
    (%op s "~%~v@{ ~}~v@{:~}" (components:inset c) (+ 2 (components:depth c)) NIL))

  (components:instruction (:before)
    (%op s "! "))
  
  (components:instruction ()
    (format s "~(~a~)" (type-of c)))
  
  (components:message-instruction ()
    (format s "~(~a~) ~a" (type-of c) (components:message c)))

  (components:directives-instruction ()
    (format s "~(~a~)~{ ~a~}" (type-of c) (components:directives c)))

  (components:set ()
    (format s "~(~a~) ~a ~a" (type-of c) (components:variable c) (components:value c)))

  (components:include ()
    (format s "~(~a~) ~a" (type-of c) (components:file c)))

  (components:label ()
    (format s "~(~a~) ~a" (type-of c) (components:target c)))

  (components:raw ()
    (format s "~(~a~) ~a ~a" (type-of c) (components:target c) (components:text c)))

  (components:comment ()
    (%op s "; ")
    (format s "~a" (components:text c)))

  (components:embed ()
    (%op s "[ ")
    (format s "~(~a~) ~a"
            (type-of c) (components:target c))
    (loop for option in (components:options c)
          do (format s ", ")
             (output option))
    (%op s " ]"))

  (components:embed-option ()
    (format s "~(~a~)" (type-of c)))

  (components:loop-option ()
    (%op s "loop"))

  (components:autoplay-option ()
    (%op s "autoplay"))

  (components:width-option ()
    (%op s "width ~a~(~a~)" (components:size c) (components:unit c)))

  (components:height-option ()
    (%op s "height ~a~(~a~)" (components:size c) (components:unit c)))

  (components:float-option ()
    (%op s "float ~(~a~)" (components:direction c)))

  (components:label-option ()
    (%op s "label ~a" (components:target c)))

  (components:caption-option ()
    (%op s "caption ")
    (output (components:children c)))

  (components:description-option ()
    (%op s "description ~a" (components:text c)))

  (components:options-option ()
    (%op s "options~(~{ ~a~}~)" (components:options c)))

  (components:language-option ()
    (%op s "language ~a" (components:language c)))

  (components:start-option ()
    (%op s "start ~a" (components:start c)))

  (components:end-option ()
    (%op s "end ~:[~;+~]~a" (components:offset-p c) (components:end c)))

  (components:encoding-option ()
    (%op s "encoding ~a" (components:encoding c)))

  (components:embed-link-option ()
    (%op s "link ~a" (components:target c)))

  (components:footnote ()
    (%op s "[~d] " (components:target c))
    (output (components:children c)))

  (components:align ()
    (let ((prefix (ecase (components:alignment c)
                    (:left "|<")
                    (:right "|>")
                    (:center "><")
                    (:justify "||"))))
      (%op s "~a" prefix)
      (let ((*prefixes* (list* "| " *prefixes*)))
        (output (components:children c)))))

  (components:bold ()
    (%op s "**")
    (output (components:children c))
    (%op s "**"))

  (components:italic ()
    (%op s "//")
    (output (components:children c))
    (%op s "//"))

  (components:underline ()
    (%op s "__")
    (output (components:children c))
    (%op s "__"))

  (components:strikethrough ()
    (%op s "<-")
    (output (components:children c))
    (%op s "->"))

  (components:code ()
    (format s "``~a``" (components:text c)))

  (components:subtext ()
    (%op s "v(")
    (output (components:children c))
    (%op s ")"))

  (components:supertext ()
    (%op s "^(")
    (output (components:children c))
    (%op s ")"))

  (components:url ()
    (write-string (components:target c) s))

  (components:compound ()
    (%op s "''")
    (output (components:children c))
    (%op s "''(")
    (when (components:options c)
      (output (first (components:options c)))
      (loop for option in (rest (components:options c))
            do (%op s ", ")
               (output option)))
    (%op s ")"))

  (components:compound-option ()
    (%op s "~(~a~)" (type-of c)))

  (components:bold-option ()
    (%op s "bold"))

  (components:italic-option ()
    (%op s "italic"))

  (components:underline-option ()
    (%op s "underline"))

  (components:strikethrough-option ()
    (%op s "strikethrough"))
  
  (components:spoiler-option ()
    (%op s "spoiler"))

  (components:font-option ()
    (%op s "font ~a" (components:font-family c)))

  (components:color-option ()
    (let ((r (components:red c))
          (g (components:green c))
          (b (components:blue c)))
      (loop for name being the hash-keys of *color-table*
            for option being the hash-values of *color-table*
            do (when (and (= r (components:red option))
                          (= g (components:green option))
                          (= b (components:blue option)))
                 (return (%op s "~(~a~)" name)))
            finally (%op s "color ~a ~a ~a"  r g b))))
  
  (components:size-option ()
    (let ((size (components:size c))
          (unit (components:unit c)))
      (loop for name being the hash-keys of *size-table*
            for option being the hash-values of *size-table*
            do (when (and (= size (components:size option))
                          (eql unit (components:unit option)))
                 (return (%op s "~(~a~)" name)))
            finally (%op s "size ~a~(~a~)" size unit))))

  (components:internal-link-option ()
    (%op s "#~a" (components:target c)))

  (components:link-option ()
    (if (read-url (components:target c) 0)
        (%op s "~a" (components:target c))
        (%op s "link ~a" (components:target c))))

  (components:footnote-reference ()
    (%op s "[~d]" (components:target c)))

  (components:en-dash ()
    (format s "--"))

  (components:em-dash ()
    (format s "---"))

  (components:newline ()
    (fresh-line s)
    (loop for prefix in (reverse *prefixes*)
          do (%op s "~a" prefix))))

(defmethod output-operator (string target (markless markless))
  (write-string string target))

(define-output (highlighted (markless)) (c s)
  (components:component (:around)
    (format s "<span class=\"~(~a~)\">" (type-of c))
    (call-next-method)
    (format s "</span>"))

  (components:root-component (:around)
    (format s "<pre class=\"markless-source\">")
    (call-next-method)
    (format s "</pre>"))

  (string ()
    (loop for char across c
          do (case char
               (#\\
                (format s "\\\\"))
               (#\<
                (format s "&lt;"))
               (#\>
                (format s "&gt;"))
               (#\&
                (format s "&amp;"))
               (#\Linefeed
                (fresh-line s)
                (loop for prefix in (reverse *prefixes*)
                      do (%op s "~a" prefix)))
               (T
                (write-char char s))))))

(defmethod output-operator (string target (thing highlighted))
  (format target "<span class=\"operator\">~a</span>" string))

(define-output (gemtext () ((pending-links :initform NIL :accessor pending-links))) (c s)
  (vector ()
    (loop for child across c 
          do (if (typep child '(and components:block-component
                                   components:parent-component))
                 (output (components:children child))
                 (output child))))

  (string ()
    (loop for char across c
          do (case char
               (#\Linefeed (output char))
               (T (write-char char s)))))

  ((eql #\Linefeed) ()
    (format s "~c~c" #\Return #\Linefeed))

  (components:root-component ()
    (loop for child across (components:children c)
          do (if (typep child 'components:list)
                 (loop for c across (components:children child)
                       do (output c))
                 (output child))))

  (components:parent-component ()
    (output (components:children c)))

  (components:paragraph (:after)
    (output #\Linefeed))

  (components:block-component (:after)
    (dolist (link (reverse (shiftf (pending-links _) ())))
      (format s "=> ~a" link)
      (output #\Linefeed)))

  (components:header ()
    (format s "~v@{#~} " (min (components:depth c) 3) NIL)
    (output (components:children c))
    (output #\Linefeed))

  (components:embed ()
    (format s "=> ~a~@[ ~a~]"
            (components:target c)
            (or (components:find-option 'components:description-option c)
                (components:find-option 'components:label-option c)))
    (output #\Linefeed))

  (components:blockquote-header ())

  (components:blockquote ()
    (format s "> ")
    (output (components:children c))
    (output #\Linefeed))

  (components:list-item ()
    (format s "* ")
    (output (components:children c))
    (output #\Linefeed))

  (components:code-block ()
    (format s "```")(output #\Linefeed)
    (output (components:text c))
    (format s "```")(output #\Linefeed))

  (components:embed-option ())
  (components:compound-option ())

  (components:compound (:after)
    (mapc #'output (components:options c)))

  (components:link-option ()
    (push (components:target c) (pending-links _)))

  (components:raw ()
    (when (string-equal "gemtext" (components:target c))
      (write-string (components:text c) s)))
  
  (components:comment ())
  (components:instruction ())

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

  (components:footnote-reference ()
    (format s "[~d]" (components:target c)))

  (components:footnote ()
    (format s "[~d] " (components:target c))
    (output (components:children c))
    (output #\Linefeed))

  (components:url ()
    (push (components:target c) (pending-links _)))
  
  (components:en-dash ()
    (format s "–"))

  (components:em-dash ()
    (format s "—"))

  (components:newline ()
    (output #\Linefeed)))
