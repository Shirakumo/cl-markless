#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless)

(defclass directive ()
  ((enabled-p :initarg :enabled-p :initform T :accessor enabled-p)))

(defun ensure-directive (directive-ish)
  (etypecase directive-ish
    (directive directive-ish)
    (symbol (ensure-directive (make-instance directive-ish)))))

(defmethod prefix ((_ directive))
  (error "FIXME: better error"))

(defclass root-directive (directive)
  ())

(defmethod (setf enabled-p) ((value null) (root root-directive))
  (error "FIXME: better error"))

(defmethod invoke ((_ root-directive) line cursor)
  (read-block line cursor))

(defclass block-directive (directive)
  ())

(defclass singular-line-directive (block-directive)
  ())

(defmethod consume-prefix ((_ singular-line-directive) line cursor)
  NIL)

(defclass inline-directive (directive)
  ())

(defclass paragraph (block-directive)
  ())

(defmethod prefix ((_ paragraph))
  #())

(defmethod consume-prefix ((_ paragraph) line cursor)
  (when (< cursor (length line))
    cursor))

(defclass blockquote (block-directive)
  ())

(defmethod prefix ((_ blockquote))
  #("~|" " "))

(defmethod consume-prefix ((_ blockquote) line cursor)
  (match! "| " line cursor))

(defclass unordered-list (block-directive)
  ())

(defmethod prefix ((_ unordered-list))
  #("-" " "))

(defmethod consume-prefix ((_ unordered-list) line cursor)
  (match! "  " line cursor))

(defclass ordered-list (block-directive)
  ())

(defmethod prefix ((_ ordered-list))
  #("1234567890" "1234567890."))

(defmethod consume-prefix ((_ ordered-list) line cursor)
  (match! "  " line cursor))

(defclass header (singular-line-directive)
  ())

(defmethod prefix ((_ header))
  #("#" "# "))

(defclass code-block (block-directive)
  ())

(defmethod prefix ((_ code-block))
  #(":" ":"))

(defclass instruction (singular-line-directive)
  ())

(defmethod prefix ((_ instruction))
  #("!" " "))

(defclass comment (singular-line-directive)
  ())

(defmethod prefix ((_ comment))
  #(";" "; "))

(defclass embed (singular-line-directive)
  ())

(defmethod prefix ((_ embed))
  #("[" " "))

(defclass footnote (singular-line-directive)
  ())

(defmethod prefix ((_ footnote))
  #("[" "1234567890"))

;;;; Inline Directives

(defclass bold (inline-directive)
  ())

(defmethod prefix ((_ bold))
  #("*" "*"))

(defclass italic (inline-directive)
  ())

(defmethod prefix ((_ italic))
  #("/" "/"))

(defclass underline (inline-directive)
  ())

(defmethod prefix ((_ underline))
  #("_" "_"))

(defclass strikethrough (inline-directive)
  ())

(defmethod prefix ((_ strikethrough))
  #("<" "-"))

(defclass code (inline-directive)
  ())

(defmethod prefix ((_ code))
  #("`" "`"))

(defclass dash (inline-directive)
  ())

(defmethod prefix ((_ dash))
  #("-" "-"))

(defclass supertext (inline-directive)
  ())

(defmethod prefix ((_ supertext))
  #("^" "("))

(defclass subtext (inline-directive)
  ())

(defmethod prefix ((_ subtext))
  #("v" "("))

(defclass compound (inline-directive)
  ())

(defmethod prefix ((_ compound))
  #("\""))

(defclass footnote-reference (inline-directive)
  ())

(defmethod prefix ((_ footnote-reference))
  #("[" "1234567890"))

(defclass newline (inline-directive)
  ())

(defmethod prefix ((_ newline))
  #("-" "/" "-"))
