(defpackage #:cl-markless-plump
  (:nicknames #:org.shirakumo.markless.plump)
  (:use #:cl #:org.shirakumo.markless)
  (:local-nicknames
   (#:components #:org.shirakumo.markless.components))
  (:shadowing-import-from #:org.shirakumo.markless #:debug)
  (:export
   #:plump))
(in-package #:org.shirakumo.markless.plump)

(defvar *root*)

(defun append-style (element format &rest args)
  (setf (plump-dom:attribute element "style")
        (format NIL "~@[~a;~]~?"
                (plump-dom:attribute element "style")
                format args)))

(defclass plump (output-format)
  ((css :initform NIL :initarg :css :accessor css)))

(defmethod output-component (component (target stream) (format plump))
  (let ((dom (output-component component (make-instance 'plump-dom:root) format)))
    (plump:serialize dom target)
    dom))

(defmacro define-plump-output (component tag &body body)
  (let ((type (or (find-symbol (string component) '#:org.shirakumo.markless.components)
                  (error "No such component ~s" component))))
    `(defmethod output-component ((component ,type) (target plump-dom:nesting-node) (format plump))
       (let ((node (plump-dom:make-element target ,tag)))
         (flet ((output (component)
                  (output-component component node format))
                ((setf attribute) (value name &optional target)
                  (setf (plump-dom:attribute (or target node) name) value)))
           (declare (ignorable #'output #'(setf attribute)))
           ,@body)
         node))))

(defmethod output-component ((string string) (target plump-dom:nesting-node) (format plump))
  (plump-dom:make-text-node target string))

(defmethod output-component :around ((component components:root-component) target (format plump))
  (let ((*root* component))
    (call-next-method)))

(define-plump-output root-component "article"
  (when (components:author component)
    (setf (attribute "data-author") (components:author component)))
  (when (components:copyright component)
    (setf (attribute "data-copyright") (components:copyright component)))
  (when (components:language component)
    (setf (attribute "lang") (components:language component)))
  (when (css format)
    (plump-dom:make-fulltext-element node "style" :text (css format)))
  (loop for child across (components:children component)
        do (output child))
  (let ((footnotes (sort (loop for child across (components:children component)
                               when (typep child 'components:footnote)
                               collect child)
                         #'< :key #'components:target)))
    (when footnotes
      (let ((section (plump-dom:make-element node "footer")))
        (setf (attribute "class" section) "footnotes")
        (setf (plump-dom:attribute (plump-dom:make-element section "hr") "style") "clear:both")
        (loop with listing = (plump-dom:make-element section "ol")
              for footnote in footnotes
              for note = (plump-dom:make-element listing "li")
              do (setf (attribute "value" note) (princ-to-string (components:target footnote)))
                 (setf (attribute "id" note) (format NIL "footnote-~d" (components:target footnote)))
                 (loop for child across (components:children footnote)
                       do (output-component child note format)))))))

(defmethod output-component ((component components:component) (target plump-dom:nesting-node) (format plump)))

(defmethod output-component ((component components:parent-component) (target plump-dom:nesting-node) (format plump))
  (loop for child across (components:children component)
        do (output-component child target format)))

(defmethod output-component ((component components:footnote) (target plump-dom:nesting-node) (format plump)))
(defmethod output-component ((component components:blockquote-header) (target plump-dom:nesting-node) (format plump)))

(define-plump-output paragraph "p"
  ;; Not right yet since Markless paragraphs can contain other blocks.
  (loop for child across (components:children component)
        do (output child)))

(define-plump-output blockquote "blockquote"
  (loop for child across (components:children component)
        do (output child))
  (when (components:source component)
    (let ((source (plump-dom:make-element node "cite")))
      (loop for child across (components:children (components:source component))
            do (output-component child source format)))))

(define-plump-output ordered-list "ol"
  (loop for child across (components:children component)
        do (output child)))

(define-plump-output ordered-list-item "li"
  (setf (attribute "value") (princ-to-string (components:number component)))
  (loop for child across (components:children component)
        do (output child)))

(define-plump-output unordered-list "ul"
  (loop for child across (components:children component)
        do (output child)))

(define-plump-output unordered-list-item "li"
  (loop for child across (components:children component)
        do (output child)))

(define-plump-output header "h"
  (setf (plump:tag-name node) (format NIL "h~d" (min 6 (components:depth component))))
  (setf (attribute "id") (string-downcase (components:text component)))
  (loop for child across (components:children component)
        do (output child)))

(define-plump-output horizontal-rule "hr"
  (setf (attribute "style") "clear:both"))

(defmethod output-component ((component components:code-block) (target plump-dom:nesting-node) (format plump))
  (cond ((and (find (components:language component) '("html" "xhtml")  :test #'string-equal)
              (find "inline" (components:options component) :test #'string-equal))
         (let ((plump:*tag-dispatchers*
                 (if (string-equal "xhtml" (components:language component))
                     plump:*xml-tags*
                     plump:*html-tags*)))
           (loop for child across (plump-dom:children (plump:parse (components:text component)))
                 do (plump-dom:append-child target child))))
        (T
         (let ((node (plump-dom:make-element target "code")))
           (flet (((setf attribute) (value name &optional target)
                    (setf (plump-dom:attribute (or target node) name) value)))
             (append-style node "display:block")
             (setf (attribute "class") "code-block")
             (when (and (components:language component)
                        (string/= "" (components:language component)))
               (setf (attribute "data-language") (components:language component)))
             (let ((pre (plump-dom:make-element node "pre")))
               (plump-dom:make-text-node pre (components:text component))))
           node))))

(defmethod output-component ((component components:comment) (target plump-dom:nesting-node) (format plump))
  (plump-dom:make-comment target (components:text component)))

(defun set-plump-embed-options (element options format)
  (loop for option in options
        do (typecase option
             (components:autoplay-option
              (setf (plump-dom:attribute element "autoplay") NIL))
             (components:loop-option
              (setf (plump-dom:attribute element "loop") NIL))
             (components:width-option
              (append-style element "width:~d~(~a~)"
                            (components:size option)
                            (components:unit option))
              (append-style element "object-fit:contain"))
             (components:height-option
              (append-style element "height:~d~(~a~)"
                            (components:size option)
                            (components:unit option)))
             (components:float-option
              (append-style element "float:~(~a~)" (components:direction option)))
             (components:label-option
              (setf (plump-dom:attribute (plump-dom:parent element) "id")
                    (string-downcase (components:target option))))
             (components:caption-option
              (let ((caption (plump-dom:make-element element "figcaption")))
                (output-component option caption format))))))

(defmethod output-component :around ((component components:embed) (target plump-dom:nesting-node) (format plump))
  (let ((figure (plump-dom:make-element target "figure"))
        (options (remove NIL (list (find 'components:float-option (components:options component) :key #'type-of)
                                   (find 'components:caption-option (components:options component) :key #'type-of))))
        (width (find 'components:width-option (components:options component) :key #'type-of)))
    (setf (components:options component) (set-difference (components:options component) options))
    (call-next-method component figure format)
    (set-plump-embed-options figure (list* width options) format)))

(define-plump-output image "a"
  (let ((img (plump-dom:make-element node "img")))
    (set-plump-embed-options img (components:options component) format)
    (setf (plump-dom:attribute img "alt") (components:target component))
    (setf (plump-dom:attribute img "src") (components:target component))
    (append-style img "display:block"))
  (let ((link (find 'components:embed-link-option (components:options component) :key #'type-of)))
    (setf (attribute "href") (if link
                                 (components:target link)
                                 (components:target component)))
    (setf (attribute "target") "_blank")))

(define-plump-output video "video"
  (setf (attribute "src") (components:target component))
  (setf (attribute "controls") NIL)
  (set-plump-embed-options node (components:options component) format)
  (let ((par (plump-dom:make-element node "p")))
    (plump-dom:make-text-node par "HTML5 video is not supported. The video source is here: ")
    (let ((link (plump-dom:make-element par "a")))
      (setf (attribute "src" link) (components:target component))
      (plump-dom:make-text-node link (components:target component)))))

(define-plump-output audio "audio"
  (setf (attribute "src") (components:target component))
  (setf (attribute "controls") NIL)
  (set-plump-embed-options node (components:options component) format)
  (let ((par (plump-dom:make-element node "p")))
    (plump-dom:make-text-node par "HTML5 audio is not supported. The audio source is here: ")
    (let ((link (plump-dom:make-element par "a")))
      (setf (attribute "href" link) (components:target component))
      (plump-dom:make-text-node link (components:target component)))))

(define-plump-output bold "strong"
  (loop for child across (components:children component)
        do (output child)))

(define-plump-output italic "em"
  (loop for child across (components:children component)
        do (output child)))

(define-plump-output underline "u"
  (loop for child across (components:children component)
        do (output child)))

(define-plump-output strikethrough "s"
  (loop for child across (components:children component)
        do (output child)))

(define-plump-output code "code"
  (loop for child across (components:children component)
        do (output child)))

(define-plump-output subtext "sub"
  (loop for child across (components:children component)
        do (output child)))

(define-plump-output supertext "sup"
  (loop for child across (components:children component)
        do (output child)))

(define-plump-output url "a"
  (setf (attribute "href") (components:target component))
  (plump-dom:make-text-node node (components:target component)))

(defmethod label ((component components:header))
  (components:text component))

(defmethod label ((component components:footnote))
  (format NIL "footnote-~d" (components:target component)))

(defmethod output-component ((option components:bold-option) (target plump-dom:nesting-node) (format plump))
  (append-style target "font-weight:bold"))

(defmethod output-component ((option components:italic-option) (target plump-dom:nesting-node) (format plump))
  (append-style target "font-weight:italic"))

(defmethod output-component ((option components:underline-option) (target plump-dom:nesting-node) (format plump))
  (append-style target "text-decoration:underline"))

(defmethod output-component ((option components:strikethrough-option) (target plump-dom:nesting-node) (format plump))
  (append-style target "text-decoration:line-through"))

(defmethod output-component ((option components:spoiler-option) (target plump-dom:nesting-node) (format plump))
  (append-style target "background:black;color:black"))

(defmethod output-component ((option components:font-option) (target plump-dom:nesting-node) (format plump))
  (append-style target "font-family:~a" (components:font-family option)))

(defmethod output-component ((option components:color-option) (target plump-dom:nesting-node) (format plump))
  (append-style target "color:rgb(~d,~d,~d)"
                (components:red option)
                (components:green option)
                (components:blue option)))

(defmethod output-component ((option components:size-option) (target plump-dom:nesting-node) (format plump))
  (append-style target "font-size:~d~(~a~)" (components:size option) (components:unit option)))

(defmethod output-component ((option components:internal-link-option) (target plump-dom:nesting-node) (format plump))
  (setf (plump-dom:tag-name target) "a")
  (setf (plump-dom:attribute target "class") "cross-reference")
  (let ((label-component (components:label (components:target option) *root*)))
    (when label-component
      (setf (plump-dom:attribute target "href") (format NIL "#~a" (string-downcase (components:target option)))))))

(defmethod output-component ((option components:link-option) (target plump-dom:nesting-node) (format plump))
  (setf (plump-dom:tag-name target) "a")
  (setf (plump-dom:attribute target "class") "external-link")
  (setf (plump-dom:attribute target "href") (components:target option)))

(define-plump-output compound "span"
  (loop for option in (components:options component)
        do (output-component option node format))
  (loop for child across (components:children component)
        do (output child)))

(define-plump-output label "a"
  (setf (attribute "id") (string-downcase (components:target component))))

(define-plump-output footnote-reference "sup"
  (setf (attribute "class") "footnote-reference")
  (let ((link (plump-dom:make-element node "a")))
    (let ((target (components:label (princ-to-string (components:target component)) *root*)))
      (when target
        (setf (attribute "href" link) (format NIL "#~a" (label target)))))
    (plump-dom:make-text-node link (format NIL "[~d]" (components:target component)))))

(define-plump-output newline "br")

(defmethod output-component ((dash components:en-dash) (target plump-dom:nesting-node) (format plump))
  (plump-dom:make-text-node target #.(string (code-char #x2013))))

(defmethod output-component ((dash components:em-dash) (target plump-dom:nesting-node) (format plump))
  (plump-dom:make-text-node target #.(string (code-char #x2014))))
