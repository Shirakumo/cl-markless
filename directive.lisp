#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.directives)

(defclass directive ()
  ((enabled-p :initarg :enabled-p :initform T :accessor enabled-p)))

(defun ensure-directive (directive-ish)
  (etypecase directive-ish
    (directive directive-ish)
    (symbol (ensure-directive (make-instance directive-ish)))))

(defmethod dispatch ((_ directive))
  (error "FIXME: better error"))

(defclass block-directive (directive)
  ())

(defclass inline-directive (directive)
  ())

(defclass paragraph (block-directive)
  ())

(defmethod dispatch ((_ paragraph))
  ())

(defclass blockquote (block-directive)
  ())

(defmethod dispatch ((_ blockquote))
  #("~|" " "))

(defclass unordered-list (block-directive)
  ())

(defmethod dispatch ((_ unordered-list))
  #("-" " "))

(defclass ordered-list (block-directive)
  ())

(defmethod dispatch ((_ ordered-list))
  #("1234567890" "1234567890."))

(defclass header (block-directive)
  ())

(defmethod dispatch ((_ header))
  #("#" "# "))

(defclass code-block (block-directive)
  ())

(defmethod dispatch ((_ code-block))
  #(":" ":"))

(defclass instruction (block-directive)
  ())

(defmethod dispatch ((_ instruction))
  #("!" " "))

(defclass comment (block-directive)
  ())

(defmethod dispatch ((_ comment))
  #(";" " "))

(defclass embed (block-directive)
  ())

(defmethod dispatch ((_ embed))
  #("[" " "))

(defclass footnote (block-directive)
  ())

(defmethod dispatch ((_ footnote))
  #("[" "1234567890"))
