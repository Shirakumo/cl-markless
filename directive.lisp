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

(defclass root-directive (directive)
  ())

(defmethod (setf enabled-p) ((value null) (root root-directive))
  (error "FIXME: better error"))

(defmethod invoke ((_ root-directive) parser line cursor)
  (read-block parser line cursor))

(defclass block-directive (directive)
  ())

(defmethod end ((_ block-directive) component parser))

(defclass singular-line-directive (block-directive)
  ())

(defmethod consume-prefix ((_ singular-line-directive) component parser line cursor)
  NIL)

(defclass inline-directive (directive)
  ())

(defmethod consume-prefix ((_ inline-directive) component parser line cursor)
  cursor)

(defmethod end ((_ inline-directive) component parser)
  (change-class component 'components:parent-component))

(defclass paragraph (block-directive)
  ())

(defmethod prefix ((_ paragraph))
  #())

(defmethod begin ((_ paragraph) parser line cursor)
  (let ((end cursor))
    (loop while (and (< end (length line))
                     (char= #\  (aref line end)))
          do (incf end))
    (commit _ (make-instance 'components:paragraph :indentation (- end cursor)) parser)
    end))

(defmethod consume-prefix ((_ paragraph) component parser line cursor)
  (let ((end cursor))
    (loop while (and (< end (length line))
                     (char= #\  (aref line end)))
          do (incf end))
    (when (and (< end (length line))
               (= end (+ cursor (components:indentation component)))
               (not
                (and (= end cursor)
                     (not (eql _ (dispatch (block-dispatch-table parser) line end))))))
      end)))

(defmethod invoke ((_ paragraph) parser line cursor)
  (let ((inner (dispatch (block-dispatch-table parser) line cursor)))
    (if (and inner (not (eq inner _)))
        (begin inner parser line cursor)
        (read-inline parser line cursor))))

(defclass blockquote (block-directive)
  ())

(defmethod prefix ((_ blockquote))
  #("~|" " "))

(defmethod begin ((_ blockquote) parser line cursor)
  (commit _ (make-instance 'components:blockquote) parser)
  (+ 2 cursor))

(defmethod consume-prefix ((_ blockquote) component parser line cursor)
  (match! "| " line cursor))

(defmethod invoke ((_ blockquote) parser line cursor)
  (read-block parser line cursor))

(defclass unordered-list (block-directive)
  ())

(defmethod prefix ((_ unordered-list))
  #("-" " "))

(defmethod begin ((_ unordered-list) parser line cursor)
  (let* ((children (components:children (stack-entry-component (stack-top (stack parser)))))
         (container (when (< 0 (length children))
                       (aref children (1- (length children)))))
         (item (make-instance 'components:unordered-list-item)))
    (unless (typep container 'components:unordered-list)
      (setf container (make-instance 'components:unordered-list))
      (vector-push-extend container children))
    (vector-push-extend item (components:children container))
    (stack-push _ item (stack parser))
    (+ 2 cursor)))

(defmethod invoke ((_ unordered-list) parser line cursor)
  (read-block parser line cursor))

(defmethod consume-prefix ((_ unordered-list) component parser line cursor)
  (match! "  " line cursor))

(defclass ordered-list (block-directive)
  ())

(defmethod prefix ((_ ordered-list))
  #("1234567890" "1234567890."))

(defmethod begin ((_ ordered-list) parser line cursor)
  (let ((end cursor))
    ;; Count length of order number
    (loop while (and (< end (length line))
                     (<= (char-code #\0) (char-code (aref line end)) (char-code #\9)))
          do (incf end))
    (cond ((or (<= (length line) end)
               (char/= #\. (aref line end)))
           ;; We did a bad match, pretend we're a paragraph and skip the match.
           (let ((component (make-instance 'components:paragraph)))
             (commit (directive 'paragraph parser) component parser)
             (vector-push-extend (subseq line cursor (1+ end)) (components:children component))))
          (T
           ;; Construct item, just like for the ordered list.
           (let* ((children (components:children (stack-entry-component (stack-top (stack parser)))))
                  (container (when (< 0 (length children))
                               (aref children (1- (length children)))))
                  (number (parse-integer line :start cursor :end end))
                  (item (make-instance 'components:ordered-list-item :number number)))
             (unless (typep container 'components:ordered-list)
               (setf container (make-instance 'components:ordered-list))
               (vector-push-extend container children))
             (vector-push-extend item (components:children container))
             (stack-push _ item (stack parser)))))
    (+ end 1)))

(defmethod invoke ((_ ordered-list) parser line cursor)
  (read-block parser line cursor))

(defmethod consume-prefix ((_ ordered-list) component parser line cursor)
  (let ((numcnt (1+ (ceiling (log (components:number component) 10)))))
    (when (loop for i from cursor
                repeat numcnt
                always (char= #\  (aref line i)))
      (+ cursor numcnt 1))))

(defclass header (singular-line-directive)
  ())

(defmethod prefix ((_ header))
  #("#" "# "))

(defmethod begin ((_ header) parser line cursor)
  (let ((depth 0))
    (loop for i from cursor below (length line)
          while (char= #\# (aref line i))
          do (incf depth))
    (commit _ (make-instance 'components:header :depth depth) parser)
    (+ cursor 1 depth)))

;; FIXME: label table
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

(defmethod begin ((_ instruction) parser line cursor)
  (multiple-value-bind (typename cursor) (read-space-delimited line (+ cursor 2))
    (let ((type (find-symbol (to-readtable-case typename #.(readtable-case *readtable*))
                             '#:org.shirakumo.markless.components)))
      (unless (and type (subtypep type 'components:instruction))
        (error "FIXME: better error"))
      (commit _ (parse-instruction type line (1+ cursor)) parser))
    cursor))

(defmethod parse-instruction ((type (eql 'components:set)) line cursor)
  (multiple-value-bind (variable cursor) (read-space-delimited line cursor)
    (let ((value (subseq line (1+ cursor))))
      (make-instance type :variable variable :value value))))

(defmethod parse-instruction ((type (eql 'components:info)) line cursor)
  (make-instance type :message (subseq line cursor)))

(defmethod parse-instruction ((type (eql 'components:warning)) line cursor)
  (make-instance type :message (subseq line cursor)))

(defmethod parse-instruction ((type (eql 'components:error)) line cursor)
  (make-instance type :message (subseq line cursor)))

(defmethod parse-instruction ((type (eql 'components:include)) line cursor)
  (make-instance type :file (subseq line cursor)))

(defmethod parse-instruction ((type (eql 'components:enable)) line cursor)
  (make-instance type :directives (split-string line #\  cursor)))

(defmethod parse-instruction ((type (eql 'components:disable)) line cursor)
  (make-instance type :directives (split-string line #\  cursor)))

(defmethod invoke ((_ instruction) parser line cursor)
  (evaluate-instruction (stack-entry-component (stack-top (stack parser))) parser)
  (length line))

(defclass comment (singular-line-directive)
  ())

(defmethod prefix ((_ comment))
  #(";" "; "))

(defmethod begin ((_ comment) parser line cursor)
  (loop while (char= #\; (aref line cursor))
        do (incf cursor))
  (commit _ (make-instance 'components:comment :text (subseq line cursor)) parser)
  (length line))

(defmethod invoke ((_ comment) parser line cursor))

(defclass embed (singular-line-directive)
  ())

(defmethod prefix ((_ embed))
  #("[" " "))

(defmethod begin ((_ embed) parser line cursor)
  (multiple-value-bind (target cursor) (read-space-delimited line (+ cursor 2))
    (let ((options (split-string line #\  cursor))
          (component (make-instance 'components:embed :target target)))
      (loop for (key val) on options by #'cddr
            do (cond ((string-equal key "float")
                      (setf (components:float component)
                            (cond ((string-equal val "left") :left)
                                  ((string-equal val "right") :right)
                                  (T (error "FIXME: better error")))))
                     ((string-equal key "width")
                      (setf (components:width component) val))
                     ((string-equal key "height")
                      (setf (components:height component) val))
                     ((and (string= key "]") (null val)))
                     (T
                      (error "FIXME: better error"))))
      (commit _ component parser)
      (length line))))

(defmethod invoke ((_ embed) parser line cursor))

(defclass footnote (singular-line-directive)
  ())

(defmethod prefix ((_ footnote))
  #("[" "1234567890"))

(defmethod begin ((_ footnote) parser line cursor)
  (incf cursor)
  (let ((end cursor))
    (loop while (and (< end (length line))
                     (<= (char-code #\0) (char-code (aref line end)) (char-code #\9)))
          do (incf end))
    (cond ((or (<= (length line) end)
               (char/= #\] (aref line end)))
           ;; Mismatch. Pretend we're a paragraph.
           (let ((component (make-instance 'components:paragraph)))
             (commit (directive 'paragraph parser) component parser)
             (vector-push-extend (subseq line cursor (1+ end)) (components:children component))))
          (T
           (let ((target (parse-integer line :start cursor :end end)))
             (commit _ (make-instance 'components:footnote :target target) parser))))
    (1+ end)))

(defmethod invoke ((_ footnote) parser line cursor)
  (read-inline parser line cursor))

;;;; Inline Directives

(defclass bold (inline-directive)
  ())

(defmethod prefix ((_ bold))
  #("*" "*"))

(defmethod begin ((_ bold) parser line cursor)
  (commit _ (make-instance 'components:bold) parser)
  (+ 2 cursor))

(defmethod invoke ((_ bold) parser line cursor)
  ;; FIXME: find logic for termination
  (read-inline parser line cursor))

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
