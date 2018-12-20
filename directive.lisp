#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless)

(defclass directive ()
  ((enabled-p :initarg :enabled-p :initform T :accessor enabled-p)))

(defmethod print-object ((directive directive) stream)
  (print-unreadable-object (directive stream :type T)
    (unless (enabled-p directive)
      (format stream "INACTIVE"))))

(defun ensure-directive (directive-ish)
  (etypecase directive-ish
    (directive directive-ish)
    (symbol (ensure-directive (make-instance directive-ish)))))

(defmethod prefix ((_ directive))
  (error "FIXME: better error"))

(defmethod begin :before ((directive directive) (parser parser) line cursor)
  (vector-push directive (stack parser)))

(defmethod end ((directive directive) (parser parser))
  (pop (component-stack parser)))

(defmethod end :after ((directive directive) (parser parser))
  (assert (eq directive (vector-pop (stack parser)))))

(defclass root-directive (directive)
  ())

(defmethod (setf enabled-p) ((value null) (root root-directive))
  (error "FIXME: better error"))

(defmethod invoke ((_ root-directive) parser line cursor)
  (read-block parser line cursor))

(defclass block-directive (directive)
  ())

(defclass singular-line-directive (block-directive)
  ())

(defmethod consume-prefix ((_ singular-line-directive) parser line cursor)
  NIL)

(defclass inline-directive (directive)
  ())

(defclass paragraph (block-directive)
  ())

(defmethod prefix ((_ paragraph))
  #())

(defmethod begin ((_ paragraph) parser line cursor)
  (commit (make-instance 'components:paragraph) parser)
  cursor)

(defmethod consume-prefix ((_ paragraph) parser line cursor)
  (when (and (< cursor (length line))
             (eql _ (dispatch (block-dispatch-table parser) line cursor)))
    cursor))

(defmethod invoke ((_ paragraph) parser line cursor)
  (read-inline parser line cursor))

(defclass blockquote (block-directive)
  ())

(defmethod prefix ((_ blockquote))
  #("~|" " "))

(defmethod begin ((_ blockquote) parser line cursor)
  (commit (make-instance 'components:blockquote) parser)
  (+ 2 cursor))

(defmethod consume-prefix ((_ blockquote) parser line cursor)
  (match! "| " line cursor))

(defmethod invoke ((_ blockquote) parser line cursor)
  (read-block parser line cursor))

(defclass unordered-list (block-directive)
  ())

(defmethod prefix ((_ unordered-list))
  #("-" " "))

(defmethod begin ((_ unordered-list) parser line cursor)
  (let* ((children (components:children (car (component-stack parser))))
         (last-child (when (< 0 (length children))
                       (aref children (1- (length children))))))
    (if (typep last-child 'components:unordered-list)
        (push last-child (component-stack parser))
        (commit (make-instance 'components:unordered-list) parser))
    (commit (make-instance 'components:unordered-list-item) parser)
    (+ 2 cursor)))

(defmethod invoke ((_ unordered-list) parser line cursor)
  (read-block parser line cursor))

(defmethod consume-prefix ((_ unordered-list) parser line cursor)
  (match! "  " line cursor))

(defmethod end ((_ unordered-list) (parser parser))
  (setf (component-stack parser) (cddr (component-stack parser))))

(defclass ordered-list (block-directive)
  ())

(defmethod prefix ((_ ordered-list))
  #("1234567890" "1234567890."))

(defmethod begin ((_ ordered-list) parser line cursor)
  (let* ((children (components:children (car (component-stack parser))))
         (last-child (when (< 0 (length children))
                       (aref children (1- (length children))))))
    (if (typep last-child 'components:ordered-list)
        (push last-child (component-stack parser))
        (commit (make-instance 'components:ordered-list) parser))
    (let ((numcnt 0))
      (loop for i from cursor below (length line)
            while (<= (char-code #\0) (char-code (aref line i)) (char-code #\9))
            do (incf numcnt))
      (commit (make-instance 'components:ordered-list-item :number (parse-integer line :start cursor :end (+ cursor numcnt))) parser)
      (+ cursor numcnt 1))))

(defmethod invoke ((_ ordered-list) parser line cursor)
  (read-block parser line cursor))

(defmethod consume-prefix ((_ ordered-list) parser line cursor)
  (let ((numcnt (1+ (ceiling (log GET-COMPONENT 10)))))
    (when (loop for i from cursor
                repeat numcnt
                always (char= #\  (aref line i)))
      (+ cursor numcnt))))

(defclass header (singular-line-directive)
  ())

(defmethod prefix ((_ header))
  #("#" "# "))

(defmethod begin ((_ header) parser line cursor)
  (let ((depth 0))
    (loop for i from cursor below (length line)
          while (char= #\# (aref line i))
          do (incf depth))
    (commit (make-instance 'components:header :depth depth) parser)
    (+ cursor 1 depth)))

(defmethod invoke ((_ header) parser line cursor)
  (read-inline parser line cursor))

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
