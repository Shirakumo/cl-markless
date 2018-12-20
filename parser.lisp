#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless)

(defparameter *default-directives*
  '(paragraph
    blockquote
    unordered-list
    ordered-list
    header
    code-block
    instruction
    comment
    embed
    footnote

    bold
    italic
    underline
    strikethrough
    code
    dash
    supertext
    subtext
    compound
    footnote-reference
    newline))

(defun compile-dispatch-table (directives)
  ;; FIXME: catch directives that would clash
  (labels ((make-table (candidates i)
             (loop with table = (make-hash-table :test #+sbcl 'eq #-sbcl 'eql
                                                 :size (1+ (length candidates))
                                                 :rehash-threshold 1)
                   for candidate in candidates
                   do (if (<= (length (car candidate)) i)
                          (push candidate (gethash #\Nul table))
                          (loop for char across (aref (car candidate) i)
                                do (push candidate (gethash char table))))
                   finally (return (fill-table table (1+ i)))))
           (fill-table (table i)
             (loop for key being the hash-keys of table
                   for candidates being the hash-values of table
                   do (setf (gethash key table)
                            (if (or (cdr candidates)
                                    (<= i (1- (length (caar candidates)))))
                                (make-table candidates i)
                                (cdr (first candidates))))
                   finally (return table))))
    (make-table (loop for directive in directives
                      collect (cons (prefix directive) directive))
                0)))

(defun dispatch (table string cursor)
  (declare (type hash-table table))
  (declare (type simple-string string))
  (loop with target = table
        for i from cursor below (length string)
        for char of-type character = (aref string i)
        do (setf target (or (gethash char target)
                            (gethash #\Nul target)))
        while (typep target 'hash-table)
        finally (return (when (and (typep target 'directive) (enabled-p target))
                          target))))

(defun print-dispatch-table (table &optional (stream *standard-output*))
  (labels ((p (table level)
             (loop for char being the hash-keys of table using (hash-value target)
                   do (format stream "~&~vt~a" (* 4 level) char)
                      (etypecase target
                        (hash-table (p target (1+ level)))
                        (directive (format stream "  ~a" (type-of target)))))))
    (p table 0)
    table))

;; FIXME: Future: separate out parser state so that parsers can
;;        be re-used

(defclass parser ()
  ((line-break-mode :initarg :line-break-mode :initform :show :accessor line-break-mode)
   (directives :initform () :accessor directives)
   (label-table :initform (make-hash-table :test 'equalp) :accessor label-table)
   (block-dispatch-table :accessor block-dispatch-table)
   (inline-dispatch-table :accessor inline-dispatch-table)
   (input :accessor input)
   (component-stack :accessor component-stack)
   (stack :accessor stack)))

(defmethod initialize-instance :after ((parser parser) &key (directives *default-directives*)
                                                            disabled-directives
                                                            (stack-size-limit 64))
  (setf (directives parser) (mapcar #'ensure-directive directives))
  (setf (stack parser) (make-array stack-size-limit :fill-pointer 0 :initial-element NIL))
  (vector-push (make-instance 'root-directive) (stack parser))
  (dolist (directive disabled-directives)
    (setf (enabled-p (directive directive parser)) NIL))
  (setf (block-dispatch-table parser) (compile-dispatch-table (directives-of 'block-directive parser)))
  (setf (inline-dispatch-table parser) (compile-dispatch-table (directives-of 'inline-directive parser))))

(defmethod label ((label string) (parser parser))
  (gethash label (label-table parser)))

(defmethod (setf label) ((value components:component) (label string) (parser parser))
  (setf (gethash label (label-table parser)) value))

(defmethod (setf label) ((value null) (label string) (parser parser))
  (remhash label (label-table parser))
  NIL)

(defmethod directive ((name string) (parser parser))
  (directive (find-symbol (string-upcase name) '#:org.shirakumo.markless.directives) parser))

(defmethod directive ((name symbol) (parser parser))
  (find name (directives parser) :key #'type-of))

(defmethod directives-of (type (parser parser))
  (remove-if-not (lambda (d) (typep d type)) (directives parser)))

(defmethod disable ((parser parser) test)
  (dolist (directive (directives parser) parser)
    (setf (enabled-p directive) (funcall test directive))))

(defmethod enable ((parser parser) test)
  (dolist (directive (directives parser) parser)
    (setf (enabled-p directive) (funcall test directive))))

(defmethod evaluate-instruction ((parser parser) instruction)
  (error "FIXME: custom condition"))

(defmethod evaluate-instruction ((parser parser) (instruction components:set))
  (case (components:variable instruction)
    (:line-break-mode)
    (T (error "FIXME: custom condition"))))

(defmethod evaluate-instruction ((parser parser) (instruction components:message))
  (format *error-output* "~&[INFO ] ~a~%" (components:message instruction)))

(defmethod evaluate-instruction ((parser parser) (instruction components:warning))
  (format *error-output* "~&[WARN ] ~a~%" (components:message instruction))
  (warn "FIXME: custom condition"))

(defmethod evaluate-instruction ((parser parser) (instruction components:error))
  (format *error-output* "~&[ERROR] ~a~%" (components:message instruction))
  (error "FIXME: custom condition"))

(defmethod evaluate-instruction ((parser parser) (instruction components:include))
  (setf (input parser) (make-concatenated-stream
                        (open (components:file instruction) :element-type 'character)
                        (input parser))))

(defmethod evaluate-instruction ((parser parser) (instruction components:disable))
  (dolist (directive (components:directives instruction))
    (let ((directive (directive directive parser)))
      (when directive
        (setf (enabled-p directive) NIL)))))

(defmethod evaluate-instruction ((parser parser) (instruction components:enable))
  (dolist (directive (components:directives instruction))
    (let ((directive (directive directive parser)))
      (when directive
        (setf (enabled-p directive) T)))))

(defun read-full-line (stream)
  (let ((line (read-line stream)))
    (if (and (< 0 (length line)) (eql #\\ (char line (1- (length line)))))
        (with-output-to-string (out)
          (loop for current = line then (read-line stream)
                do (cond ((eql #\\ (char current (1- (length current))))
                          (write-string current out :end (- (length current) 2)))
                         (T
                          (write-string current out)
                          (return)))))
        line)))

(defmethod parse (thing (parser (eql T)))
  (parse thing (make-instance 'parser)))

(defmethod parse ((pathname pathname) parser)
  (with-open-file (stream pathname :direction :input
                                   :element-type 'character)
    (parse stream parser)))

(defmethod parse ((string string) parser)
  (with-input-from-string (stream string)
    (parse stream parser)))

(defmethod parse ((stream stream) (parser parser))
  (let* ((root (make-instance 'components:root-component))
         (stack (stack parser)))
    (setf (input parser) stream)
    (setf (fill-pointer stack) 1)
    (setf (component-stack parser) (list root))
    (loop while (peek-char NIL (input parser) NIL)
          for line = (read-full-line (input parser))
          do (process-stack parser stack line))
    root))

(defun process-stack (parser stack line)
  (let ((cursor 0)
        (stack-pointer 1))
    (loop while (< stack-pointer (length stack))
          for directive = (aref stack stack-pointer)
          for next-cursor = (consume-prefix directive parser line cursor)
          do (unless next-cursor
               (loop for j downfrom (1- (length stack)) to stack-pointer
                     for directive = (aref stack j)
                     do (end directive parser))
               (return))
             (setf cursor next-cursor)
             (incf stack-pointer))
    (decf stack-pointer)
    (invoke (aref stack stack-pointer) parser line cursor)))

(defun commit (component parser)
  (let ((children (components:children (car (component-stack parser)))))
    (vector-push-extend component children)
    (push component (component-stack parser))
    component))

(defun read-inline (parser line cursor)
  (let ((buffer (make-string-output-stream))
        (table (inline-dispatch-table parser))
        (children (components:children (car (component-stack parser)))))
    (labels ((commit-buffer ()
               (let ((string (get-output-stream-string buffer)))
                 (when (string/= "" string)
                   (vector-push-extend string children))))
             (read-inline-char ()
               (let ((directive (dispatch table line cursor)))
                 (cond (directive
                        (commit-buffer)
                        (setf cursor (begin directive parser line cursor))
                        (invoke directive parser line cursor))
                       (T
                        (write-char (aref line cursor) buffer)
                        (incf cursor))))))
      (loop while (< cursor (length line))
            do (read-inline-char))
      (when (eq :show (line-break-mode parser))
        (write-char #\Newline buffer))
      (commit-buffer))))

(defun read-block (parser line cursor)
  (let* ((table (block-dispatch-table parser))
         (directive (or (dispatch table line cursor)
                        (gethash #\Nul table))))
    (setf cursor (begin directive parser line cursor))
    (invoke directive parser line cursor)))

(defmacro match! (prefix line cursor)
  (let ((lineg (gensym "LINE")) (cursorg (gensym "CURSOR")))
    `(let ((,lineg ,line)
           (,cursorg ,cursor))
       (declare (type simple-string ,lineg))
       (declare (type (unsigned-byte 32) ,cursorg))
       (declare (optimize speed))
       ,(loop for form = `(+ ,cursorg ,(length prefix))
              then `(when (char= ,(aref prefix i) (aref ,lineg (+ ,cursorg ,i)))
                      ,form)
              for i downfrom (1- (length prefix)) to 0
              finally (return form)))))
