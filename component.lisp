#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.components)

(defvar *instructions*
  '(set info warning error include disable enable))

(defclass component ()
  ())

(defclass unit-component (component)
  ())

(defclass text-component (component)
  ((text :initarg :text :initform (cl:error "TEXT required") :accessor text)))

(defclass parent-component (component)
  ((children :initarg :children :initform (make-array 0 :adjustable T :fill-pointer T) :accessor children)))

(defmethod enter (thing (parent parent-component))
  (vector-push-extend thing (children parent)))

(defclass block-component (component)
  ())

(defclass root-component (parent-component)
  ((labels :initform (make-hash-table :test 'equalp) :accessor labels)
   (author :initform NIL :accessor author)
   (copyright :initform NIL :accessor copyright)))

(defmethod label ((label string) (root root-component))
  (gethash label (labels root)))

(defmethod (setf label) ((value component) (label string) (root root-component))
  (setf (gethash label (labels root)) value))

(defmethod (setf label) ((value null) (label string) (root root-component))
  (remhash label (labels root))
  NIL)

(defclass paragraph (parent-component block-component)
  ((indentation :initarg :indentation :initform 0 :accessor indentation)))

(defclass blockquote (parent-component block-component)
  ((source :initarg :source :initform NIL :accessor source)))

(defclass list (parent-component)
  ())

(defclass list-item (parent-component block-component)
  ())

(defclass ordered-list (list)
  ())

(defclass ordered-list-item (list-item)
  ((number :initarg :number :initform 0 :accessor number)))

(defclass unordered-list (list)
  ())

(defclass unordered-list-item (list-item)
  ())

(defclass header (parent-component block-component)
  ((depth :initarg :depth :initform 0 :accessor depth)))

(defclass horizontal-rule (unit-component block-component)
  ())

(defclass code-block (text-component block-component)
  ((language :initarg :language :initform NIL :accessor language)
   (options :initarg :options :initform () :accessor options)))

(defclass instruction (block-component)
  ())

(defclass message-instruction (instruction)
  ((message :initarg :message :initform (cl:error "MESSAGE required") :accessor message)))

(defclass set (instruction)
  ((variable :initarg :variable :initform (cl:error "VARIABLE required") :accessor variable)
   (value :initarg :value :initform (cl:error "VALUE required") :accessor value)))

(defclass info (message-instruction)
  ())

(defclass warning (message-instruction)
  ())

(defclass error (message-instruction)
  ())

(defclass include (instruction)
  ((file :initarg :file :initform (cl:error "FILE required") :accessor file)))

(defclass directives-instruction (instruction)
  ((directives :initarg :directives :initform (cl:error "DIRECTIVES required.") :accessor directives)))

(defclass disable (directives-instruction)
  ())

(defclass enable (directives-instruction)
  ())

(defclass comment (text-component block-component)
  ())

(defclass embed (unit-component block-component)
  ((target :initarg :target :initform (cl:error "TARGET required") :accessor target)
   (float :initarg :float :initform NIL :accessor float)
   (width :initarg :width :initform NIL :accessor width)
   (height :initarg :height :initform NIL :accessor height)))

(defclass image (embed)
  ())

(defclass video (embed)
  ())

(defclass audio (embed)
  ())

(defclass footnote (parent-component block-component)
  ((target :initarg :target :initform (cl:error "TARGET required") :accessor target)))

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
  ((target :initarg :target :initform (cl:error "TARGET required") :accessor target)))
