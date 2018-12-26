#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.markless.plump
  (:use #:cl #:org.shirakumo.markless)
  (:local-nicknames
   (#:components #:org.shirakumo.markless.components)))
(in-package #:org.shirakumo.markless.plump)

(defvar *root*)

(defun append-style (element format &rest args)
  (setf (plump-dom:attribute element "style")
        (format NIL "~@[~a;~]~?"
                (plump-dom:attribute element "style")
                format args)))

(defmethod output-component (component (target stream) (format (eql :plump)))
  (let ((dom (output-component component (make-instance 'plump-dom:root) format)))
    (plump:serialize dom target)
    dom))

(defmacro define-plump-output (component tag &body body)
  (let ((type (or (find-symbol (string component) '#:org.shirakumo.markless.components)
                  (error "No such component ~s" component))))
    `(defmethod output-component ((component ,type) (target plump-dom:nesting-node) (format (eql :plump)))
       (let ((node (plump-dom:make-element target ,tag)))
         (flet ((output (component)
                  (output-component component node format))
                ((setf attribute) (value name &optional target)
                  (setf (plump-dom:attribute (or target node) name) value)))
           (declare (ignorable #'output #'(setf attribute)))
           ,@body)
         node))))

(defmethod output-component ((string string) (target plump-dom:nesting-node) (format (eql :plump)))
  (if (string= string #.(string #\Linefeed))
      (plump-dom:make-element target "br")
      (plump-dom:make-text-node target string)))

(defmethod output-component :around ((component components:root-component) target (format (eql :plump)))
  (let ((*root* component))
    (call-next-method)))

(define-plump-output root-component "article"
  (when (components:author component)
    (setf (attribute "data-author") (components:author component)))
  (when (components:copyright component)
    (setf (attribute "data-copyright") (components:copyright component)))
  (loop for child across (components:children component)
        do (output child))
  ;; FIXME: footnotes in footer
  )

(defmethod output-component ((component components:component) (target plump-dom:nesting-node) (format (eql :plump))))

(define-plump-output paragraph "p"
  ;; Not right yet since Markless paragraphs can contain other blocks.
  (loop for child across (components:children component)
        do (output child)))

(define-plump-output blockquote "blockquote"
  (loop for child across (components:children component)
        do (output child))
  (when (components:source component)
    (let ((source (plump-dom:make-element node "source")))
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
  (setf (attribute "id") (components:text component))
  (loop for child across (components:children component)
        do (output child)))

(define-plump-output horizontal-rule "hr")

(define-plump-output code-block "code"
  (append-style node "display:block")
  (setf (attribute "class") "code-block")
  (setf (attribute "data-language") (components:language component))
  (let ((pre (plump-dom:make-element node "pre")))
    (plump-dom:make-text-node pre (components:text component))))

(defmethod output-component ((component components:comment) (target plump-dom:nesting-node) (format (eql :plump)))
  (plump-dom:make-comment target (components:text component)))

(defun set-plump-embed-options (element options)
  (loop for option in options
        do (typecase option
             (components:autoplay-option
              (setf (plump-dom:attribute element "autoplay") NIL))
             (components:loop-option
              (setf (plump-dom:attribute element "loop") NIL))
             (components:width-option
              (setf (plump-dom:attribute element "width")
                    (format NIL "~d~(~a~)"
                            (components:size option)
                            (components:unit option))))
             (components:height-option
              (setf (plump-dom:attribute element "height")
                    (format NIL "~d~(~a~)"
                            (components:size option)
                            (components:unit option))))
             (components:float-option
              (append-style element "float:~(~a~)" (components:direction option))))))

(define-plump-output image "img"
  (setf (attribute "alt") (components:target component))
  (setf (attribute "src") (components:target component))
  (append-style node "display:block")
  (set-plump-embed-options node (components:options component)))

(define-plump-output video "video"
  (setf (attribute "src") (components:target component))
  (setf (attribute "controls") NIL)
  (set-plump-embed-options node (components:options component))
  (let ((par (plump-dom:make-element node "p")))
    (plump-dom:make-text-node par "HTML5 video is not supported. The video source is here: ")
    (let ((link (plump-dom:make-element par "a")))
      (setf (attribute "src" link) (components:target component))
      (plump-dom:make-text-node link (components:target component)))))

(define-plump-output audio "audio"
  (setf (attribute "src") (components:target component))
  (setf (attribute "controls") NIL)
  (set-plump-embed-options node (components:options component))
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

(define-plump-output compound "span"
  (loop for option in (components:options component)
        do (typecase option
             (components:bold-option
              (append-style node "font-weight:bold"))
             (components:italic-option
              (append-style node "font-style:italic"))
             (components:underline-option
              (append-style node "text-decoration:underline"))
             (components:strikethrough-option
              (append-style node "text-decoration:line-through"))
             (components:spoiler-option
              (append-style node "background:black;color:black"))
             (components:font-option
              (append-style node "font-family:~a"
                            (components:font-family option)))
             (components:color-option
              (append-style node "color:rgb(~d,~d,~d)"
                            (components:red option)
                            (components:green option)
                            (components:blue option)))
             (components:size-option
              (append-style node "font-size:~d~(~a~)"
                            (components:size option) (components:unit option)))
             (components:internal-link-option
              (setf (plump-dom:tag-name node) "a")
              (setf (attribute "class") "cross-reference")
              (let ((target (components:label (components:target option) *root*)))
                (when target
                  (setf (attribute "href") (format NIL "#~a" (components:text target))))))
             (components:link-option
              (setf (attribute "class") "external-link")
              (setf (plump-dom:tag-name node) "a")
              (setf (attribute "href") (components:target option)))))
  (loop for child across (components:children component)
        do (output child)))

(define-plump-output footnote-reference "sup"
  (setf (attribute "class") "footnote-reference")
  (let ((link (plump-dom:make-element node "a")))
    (setf (attribute "href" link)
          (format NIL "#footnote-~d" (components:target component)))
    (plump-dom:make-text-node link (format NIL "[~d]" (components:target component)))))
