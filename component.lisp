#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.components)

(defclass component ()
  ())

(defclass unit-component (component)
  ())

(defclass text-component ()
  ((text :initarg :text :initform (error "TEXT required") :accessor text)))

(defclass parent-component (component)
  ((children :initarg :children :initform (make-array 0 :adjustable T :fill-pointer T) :accessor children)))

(defmethod enter (thing (parent parent-component))
  (cl:vector-push-extend thing (children parent)))

(defclass root-component (parent-component)
  ())

(defclass paragraph (parent-component)
  ())

(defclass blockquote (parent-component)
  ())

(defclass list (parent-component)
  ())

(defclass list-item (parent-component)
  ())

(defclass ordered-list (list)
  ())

(defclass ordered-list-item (list-item)
  ())

(defclass unordered-list (list)
  ())

(defclass unordered-list-item (list-item)
  ())

(defclass header (parent-component)
  ())

(defclass horizontal-rule (unit-component)
  ())

(defclass code-block (text-component)
  ())

(defclass instruction ()
  ())

(defclass message-instruction ()
  ((message :initarg :message :initform (error "MESSAGE required") :accessor message)))

(defclass set (instruction)
  ((variable :initarg :variable :initform (error "VARIABLE required") :accessor variable)
   (value :initarg :value :initform (error "VALUE required") :accessor value)))

(defclass message (message-instruction)
  ())

(defclass warning (message-instruction)
  ())

(defclass error (message-instruction)
  ())

(defclass include (instruction)
  ((file :initarg :file :initform (error "FILE required") :accessor file)))

(defclass directives (instruction)
  ((directives :initarg :directives :initform (error "DIRECTIVES required.") :accessor directives)))

(defclass disable-directives (directives-instruction)
  ())

(defclass enable-directives (directives-instruction)
  ())

(defclass comment (text-component)
  ())

(defclass embed (unit-component)
  ((target :initarg :target :initform (error "TARGET required") :accessor target)
   (float :initarg :float :initform NIL :accessor float)
   (width :initarg :width :initform NIL :accessor width)
   (height :initarg :height :initform NIL :accessor height)))

(defclass image (embed)
  ())

(defclass video (embed)
  ())

(defclass audio (embed)
  ())

(defclass footnote (parent-component)
  ())

(defclass bold (parent-component)
  ())

(defclass italic (parent-component)
  ())

(defclass underline (parent-component)
  ())

(defclass strikethrough (parent-component)
  ())

(defclass code (text-component)
  ())

(defclass subtext (parent-component)
  ())

(defclass supertext (parent-component)
  ())

(defclass url (text-component)
  ())

(defclass compound (parent-component)
  ((options :initarg :options :initform () :accessor options)))

(defclass footnote-reference (unit-component)
  ((target :initarg :target :initform (error "TARGET required") :accessor target)))
