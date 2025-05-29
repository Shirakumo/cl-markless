(defpackage #:cl-markless-latex
  (:nicknames #:org.shirakumo.markless.latex)
  (:use #:cl #:org.shirakumo.markless)
  (:shadow #:output)
  (:local-nicknames
   (#:components #:org.shirakumo.markless.components))
  (:shadowing-import-from #:org.shirakumo.markless #:debug)
  (:export
   #:latex
   #:output))
(in-package #:org.shirakumo.markless.latex)

(defclass latex (output-format)
  ((processor :initarg :processor :initform "lualatex" :accessor processor)
   (documentclass :initarg :documentclass :initform "[a4paper,12pt]{article}" :accessor documentclass)
   (preamble :initarg :preamble :initarg :styling :initform NIL :accessor preamble)
   (verbose :initarg :verbose :initform NIL :accessor verbose)))

(defun output (markless target &rest args)
  (etypecase markless
    (pathname
     (let ((*default-pathname-defaults* (make-pathname :name NIL :type NIL :defaults markless)))
       (apply #'output (cl-markless:parse markless T) target args)))
    (string
     (apply #'output (cl-markless:parse markless T) target args))
    (components:component
     (output-component markless target (apply #'make-instance 'latex args)))))

(defmacro define-tex-output (class &body body)
  (destructuring-bind (class . args) (if (listp class) class (list class))
    `(defmethod output-component ,@args ((component ,class) (stream stream) (format latex))
       (macrolet ((texfun (name &rest args)
                    `(format stream ,(format NIL "\\~~a~{~a~^~}"
                                             (loop for arg in args collect (typecase arg
                                                                             (cons "~a")
                                                                             (symbol (string-downcase arg))
                                                                             (T arg))))
                             ,(etypecase name
                                (symbol (string-downcase name))
                                (string name))
                             ,@(loop for arg in args when (consp arg) collect arg)))
                  (texfun! (name &rest args)
                    `(progn (fresh-line stream)
                            (texfun ,name ,@args)
                            (terpri stream))))
         (flet ((output (component)
                  (output-component component stream format)))
           (declare (ignorable #'output))
           ,@body)))))

(defmethod output-component ((component components:root-component) (path pathname) (format latex))
  (cond ((string= "pdf" (pathname-type path))
         (uiop:with-temporary-file (:pathname tex :stream stream :type "tex")
           (output-component component stream format)
           (close stream)
           (dotimes (i 2)
             (uiop:run-program (list (processor format)
                                     "--shell-escape" "--file-line-error"
                                     "--halt-on-error" "--interaction=nonstopmode"
                                     (format NIL "--jobname=~a" (pathname-name path))
                                     (format NIL "--output-directory=~a" (uiop:native-namestring
                                                                          (truename (make-pathname :name NIL :type NIL :defaults path))))
                                     (uiop:native-namestring tex))
                               :output (when (verbose format) *error-output*)
                               :error-output (when (verbose format) *error-output*)))))
        (T
         (call-next-method))))

(defmacro define-tex-map (component texfun &rest args)
  `(define-tex-output ,component
     (texfun ,texfun ,@args)
     (write-char #\{ stream)
     (call-next-method)
     (write-char #\} stream)))

(define-tex-output string
  (loop for char across component
        do (case char
             ((#\& #\% #\$ #\# #\_ #\{ #\})
              (write-char #\\ stream)
              (write-char char stream))
             (#\~
              (texfun textasciitilde {}))
             (#\^
              (texfun textasciicircum {}))
             (#\\
              (texfun textbackslash {}))
             (T
              (write-char char stream)))))

(defmethod output-component ((component components:component) (stream stream) (format latex)))

(defmethod output-component ((component components:parent-component) (stream stream) (format latex))
  (loop for child across (components:children component)
        do (output-component child stream format)))

(defun scan-type (component type)
  (labels ((scan (component)
             (when (typep component type)
               (return-from scan-type T))
             (when (typep component 'components:parent-component)
               (loop for child across (components:children component)
                     do (scan child)))
             (when (typep component '(or components:compound components:embed))
               (loop for child in (components:options component)
                     do (scan child)))))
    (scan component)))

(define-tex-output components:root-component
  (format stream "\\documentclass~a" (documentclass format))
  (texfun! usepackage [utf8] {inputenc})
  (when (scan-type component 'components:embed)
    (texfun! usepackage {listings}))
  (when (scan-type component 'components:float-option)
    (texfun! usepackage {floatflt}))
  (when (scan-type component 'components:image)
    (texfun! usepackage {graphicx}))
  (when (scan-type component 'components:list)
    (texfun! usepackage {enumitem}))
  (when (scan-type component 'components:footnote)
    (texfun! usepackage {glossaries}))
  (when (scan-type component '(or components:url components:link-option))
    (texfun! usepackage {hyperref}))
  (when (scan-type component 'components:font-option)
    (texfun! usepackage {fontspec}))
  (when (scan-type component 'components:color-option)
    (texfun! usepackage {xcolor}))
  (when (scan-type component 'components:strikethrough)
    (texfun! usepackage {cancel}))
  (when (scan-type component 'components:blockquote)
    (texfun! usepackage {csquotes})
    (texfun! usepackage {tabularx})
    (texfun! usepackage {array}))
  (when (scan-type component 'components:code-block)
    (texfun! usepackage {minted}))
  (when (scan-type component 'components:align)
    (texfun! usepackage {ragged2e}))
  (etypecase (preamble format)
    (null)
    (pathname
     (with-open-file (preamble (preamble format))
       (uiop:copy-stream-to-stream preamble stream)))
    (string
     (write-string (preamble format) stream)))
  (format stream "~&~%")
  (when (components:author component)(write-char #\{ stream)
        (texfun author { (components:author component) }))
  (texfun! begin {document})
  (call-next-method)
  (texfun! end {document}))

(define-tex-map components:italic textit)
(define-tex-map components:bold textbf)
(define-tex-map components:underline underline)
(define-tex-map components:strikethrough cancel)
(define-tex-map components:code texttt)
(define-tex-map components:subtext textsubscript)
(define-tex-map components:supertext textsuperscript)
(define-tex-map components:url url)
(define-tex-map components:label label)
(define-tex-map components:newline newline)
(define-tex-map components:en-dash textendash)
(define-tex-map components:em-dash textemdash)
(define-tex-map components:footnote footnotetext [ (components:target component) ])
(define-tex-map components:footnote-reference footnotemark [ (components:target component) ])
(define-tex-map components:horizontal-rule hrulefill)

(define-tex-output components:comment
  (format stream "~&% ~a~%" (components:text component)))

(define-tex-output components:header
  (format stream "~&~%\\~[chapter~;section~;subsection~;subsubsection~;paragraph~;subparagraph~]{"
          (components:depth component))
  (call-next-method)
  (format stream "}~%"))

(define-tex-output components:paragraph
  (format stream "~&~%")
  (call-next-method)
  (format stream "~&~%"))

(define-tex-output components:blockquote-header)
(define-tex-output components:blockquote
  (cond ((= 0 (components:indentation component))
         (texfun textquote [)
         (when (components:source component)
           (loop for child across (components:children (components:source component))
                 do (output child)))
         (format stream "][]{~%")
         (call-next-method)
         (format stream "~&}~%"))
        (T
         (format stream "~&\\noindent")
         (texfun! begin{tabularx} "{\\textwidth}{m{5em}X}")
         (loop for child across (components:children (components:source component))
               do (output child))
         (format stream " & ")
         (call-next-method)
         (format stream " \\\\")
         (texfun! end{tabularx}))))

(define-tex-output components:unordered-list
  (texfun! begin{itemize})
  (call-next-method)
  (texfun! end{itemize}))

(define-tex-output components:ordered-list
  (texfun! begin{enumerate})
  (call-next-method)
  (texfun! end{enumerate}))

(define-tex-output components:list-item
  (format stream "~&\\item ")
  (call-next-method))

(define-tex-output components:ordered-list-item
  (format stream "\\setcounter{enumi}{~d}"
          (components:number component))
  (call-next-method))

(defun translate-unit (sized)
  (let ((size (components:size sized)))
    (case (components:unit sized)
      (:px (format NIL "~fpt" size))
      (:% (format NIL "~f" (* 0.01 size)))
      (T (format NIL "~f~(~a~)" size (components:unit sized))))))

(define-tex-output (components:embed :around)
  (let ((float (find 'components:float-option (components:options component) :key #'type-of))
        (width (find 'components:width-option (components:options component) :key #'type-of)))
    (if float
        (texfun! begin {floatingfigure} [ (ecase (components:direction float) (:left "l") (:right "r")) ]
                 { (translate-unit width) })
        (texfun! begin {figure} "[H]"))
    (texfun! centering)
    (call-next-method)
    (loop for option in (components:options component)
          do (typecase option
               (components:caption-option
                (texfun caption {)
                (cl-markless:output option)
                (format stream "}"))
               (components:label-option
                (texfun! label { (components:target option) }))))
    (if float
        (texfun! end {floatingfigure})
        (texfun! end {figure}))))

(define-tex-output components:embed
  (texfun! url { (components:target component) }))

(defun file-extension (string)
  (let ((dot (position #\. string :from-end T)))
    (when dot
      (subseq string (1+ dot)))))

(defun maybe-cache (target format)
  (cond ((or (starts-with "http://" target)
             (starts-with "https://" target))
         (let ((temp (with-output-to-string (out)
                       (write-string "/tmp/markless-latex/tmp-" out)
                       (loop for char across target
                             do (if (find char ":;/\\\"*<>|?[]{}&$%#^")
                                    (write-char #\_ out)
                                    (write-char char out))))))
           (ensure-directories-exist temp)
           (unless (probe-file temp)
             (when (verbose format)
               (format *debug-io* "~&; Downloading ~a...~%" target))
             (uiop:run-program (list "curl" "-f" "-o" temp target))
             (unless (find (file-extension target) #(jpg jpeg png eps pdf) :test #'string-equal)
               (let* ((exts (string-right-trim '(#\Linefeed #\Return)
                                               (uiop:run-program (list "file" "-b" "--extension" temp) :output :string)))
                      (ext (first (split-string exts #\/)))
                      (new (format NIL "~a.~a" temp ext)))
                 (uiop:copy-file temp new)
                 (setf temp new))))
           temp))
        (T
         target)))

(define-tex-output components:image
  (handler-case
      (let ((width (find 'components:width-option (components:options component) :key #'type-of))
            (height (find 'components:height-option (components:options component) :key #'type-of))
            (target (maybe-cache (components:target component) format)))
        (texfun! includegraphics
                 [ (if width
                       (format NIL "width=~a" (translate-unit width))
                       (format NIL "width=0.8\textwidth"))
                 (format NIL "~@[,height=~a~]" (when height (translate-unit height))) ]
                 { (identity target) }))
    (error ()
      (call-next-method))))

(define-tex-output components:code-block
  (texfun! begin {minted} [breaklines] { (components:language component) })
  (write-string (components:text component) stream)
  (texfun! end {minted}))

(define-tex-output components:align
  (let ((env (ecase (components:alignment component)
               (:left "flushleft")
               (:right "flushright")
               (:center "center")
               (:justify "justify"))))
    (texfun! begin (identity env))
    (call-next-method)
    (texfun! end (identity env))))

(define-tex-output components:compound
  (loop for option in (components:options component)
        do (output option))
  (call-next-method)
  (loop for option in (components:options component)
        do (format stream "}")))

(define-tex-output components:italic-option
  (texfun textit {))

(define-tex-output components:bold-option
  (texfun textbf {))

(define-tex-output components:underline-option
  (texfun underline {))

(define-tex-output components:strikethrough-option
  (texfun cancel {))

(define-tex-output components:spoiler-option
  (texfun textcolor {black} {))

(define-tex-output components:font-option
  (format stream "{")
  (texfun fontfamily (components:font-family component))
  (texfun selectfont {}))

(define-tex-output components:color-option
  (texfun textcolor [RGB] { (components:red component) "," (components:green component) "," (components:blue component) } {))

(define-tex-output components:size-option
  (format stream "{")
  (texfun fontsize { (translate-unit component) } { (translate-unit component) })
  (texfun selectfont {}))

(define-tex-output components:link-option
  (texfun href { (components:target component) } {))

(define-tex-output components:internal-link-option
  (texfun hyperref [ (components:target component) ] {))

(defmethod output-component ((raw components:raw) (stream stream) (format latex))
  (when (or (string-equal "latex" (components:target raw))
            (string-equal "tex" (components:target raw)))
    (write-string (components:text raw) stream)))
