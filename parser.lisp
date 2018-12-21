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

(defun to-readtable-case (string case)
  (ecase case
    (:downcase (string-downcase string))
    (:upcase (string-upcase string))
    (:preserve string)
    (:invert (error "FIXME: Implement INVERT read-case."))))

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
   (block-dispatch-table :accessor block-dispatch-table)
   (inline-dispatch-table :accessor inline-dispatch-table)
   (input :accessor input)
   (stack :accessor stack)))

(defstruct stack-entry
  (directive NIL)
  (component NIL))

(defmethod initialize-instance :after ((parser parser) &key (directives *default-directives*)
                                                            disabled-directives
                                                            (stack-size-limit 64))
  (setf (directives parser) (mapcar #'ensure-directive directives))
  (setf (stack parser) (make-array stack-size-limit :fill-pointer 0 :element-type 'stack-entry))
  (loop for i from 0 below stack-size-limit
        do (setf (aref (stack parser) i) (make-stack-entry)))
  (dolist (directive disabled-directives)
    (setf (enabled-p (directive directive parser)) NIL))
  (setf (block-dispatch-table parser) (compile-dispatch-table (directives-of 'block-directive parser)))
  (setf (inline-dispatch-table parser) (compile-dispatch-table (directives-of 'inline-directive parser))))

(defun stack-push (directive component stack)
  (declare (type (vector stack-entry) stack))
  (let ((entry (aref stack (fill-pointer stack))))
    (incf (fill-pointer stack))
    (setf (stack-entry-directive entry) directive)
    (setf (stack-entry-component entry) component)
    entry))

(defun stack-pop (stack)
  (declare (type (vector stack-entry) stack))
  (when (= 0 (fill-pointer stack))
    (error "FIXME: better error"))
  (decf (fill-pointer stack))
  (aref stack (fill-pointer stack)))

(defun stack-top (stack)
  (declare (type (vector stack-entry) stack))
  (aref stack (1- (length stack))))

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

(defmethod evaluate-instruction (instruction (parser parser))
  (error "FIXME: custom condition"))

(defmethod evaluate-instruction ((instruction components:set) (parser parser))
  (let ((v (components:variable instruction)))
    (cond ((string-equal v "line-break-mode")
           (cond ((string= (components:value instruction) "show")
                  (setf (line-break-mode parser) :show))
                 ((string= (components:value instruction) "hide")
                  (setf (line-break-mode parser) :hide))
                 (T
                  (error "FIXME: better error"))))
          ((string-equal v "author")
           (setf (components:author (stack-entry-component (aref (stack parser) 0)))
                 (components:value instruction)))
          ((string-equal v "copyright")
           (setf (components:copyright (stack-entry-component (aref (stack parser) 0)))
                 (components:value instruction)))
          (T
           (error "FIXME: better error")))))

(defmethod evaluate-instruction ((instruction components:info) (parser parser))
  (format *error-output* "~&[INFO ] ~a~%" (components:message instruction)))

(defmethod evaluate-instruction ((instruction components:warning) (parser parser))
  (format *error-output* "~&[WARN ] ~a~%" (components:message instruction))
  (warn "FIXME: custom condition"))

(defmethod evaluate-instruction ((instruction components:error) (parser parser))
  (format *error-output* "~&[ERROR] ~a~%" (components:message instruction))
  (error "FIXME: custom condition"))

(defmethod evaluate-instruction ((instruction components:include) (parser parser))
  (setf (input parser) (make-concatenated-stream
                        (open (components:file instruction) :element-type 'character)
                        (input parser))))

(defmethod evaluate-instruction ((instruction components:disable) (parser parser))
  (dolist (directive (components:directives instruction))
    (let ((directive (directive directive parser)))
      (when directive
        (setf (enabled-p directive) NIL)))))

(defmethod evaluate-instruction ((instruction components:enable) (parser parser))
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
    (stack-push (make-instance 'root-directive) root stack)
    (loop while (peek-char NIL (input parser) NIL)
          for line = (read-full-line (input parser))
          do (process-stack parser stack line))
    root))

(defun process-stack (parser stack line)
  (let ((cursor 0)
        (stack-pointer 1))
    (loop while (< stack-pointer (length stack))
          for entry = (aref stack stack-pointer)
          for next-cursor = (consume-prefix (stack-entry-directive entry)
                                            (stack-entry-component entry)
                                            parser line cursor)
          do (unless next-cursor
               (loop until (= (length stack) stack-pointer)
                     for entry = (stack-pop stack)
                     do (end (stack-entry-directive entry)
                             (stack-entry-component entry)
                             parser))
               (return))
             (setf cursor next-cursor)
             (incf stack-pointer))
    (loop for entry = (aref stack (1- (length stack)))
          do (setf cursor (invoke (stack-entry-directive entry) parser line cursor))
          while (< cursor (length line)))))

(defun commit (directive component parser)
  (let* ((stack (stack parser))
         (children (components:children (stack-entry-component (stack-top stack)))))
    (vector-push-extend component children)
    (stack-push directive component stack)))

(defun read-space-delimited (line cursor)
  (values (with-output-to-string (stream)
            (loop while (< cursor (length line))
                  for char = (aref line cursor)
                  while (char/= #\  char)
                  do (write-char char stream)
                     (incf cursor)))
          cursor))

(defun split-string (string split &optional (start 0))
  (let ((parts ())
        (buffer (make-string-output-stream)))
    (flet ((commit ()
             (let ((string (get-output-stream-string buffer)))
               (when (string/= "" string)
                 (push string parts)))))
      (loop for i from start below (length string)
            for char = (aref string i)
            do (if (char= char split)
                   (commit)
                   (write-char char buffer))
            finally (commit))
      (nreverse parts))))

(defun read-block (parser line cursor)
  (let* ((table (block-dispatch-table parser))
         (directive (or (dispatch table line cursor)
                        (gethash #\Nul table))))
    (setf cursor (begin directive parser line cursor))))

(defun read-inline (parser line cursor)
  (let ((buffer (make-string-output-stream))
        (table (inline-dispatch-table parser))
        (children (components:children (stack-entry-component (stack-top (stack parser))))))
    (labels ((commit-buffer ()
               (let ((string (get-output-stream-string buffer)))
                 (when (string/= "" string)
                   (vector-push-extend string children))))
             (read-inline-char ()
               (let ((directive (dispatch table line cursor)))
                 (cond (directive
                        (commit-buffer)
                        (setf cursor (begin directive parser line cursor))
                        (setf cursor (invoke directive parser line cursor)))
                       (T
                        (write-char (aref line cursor) buffer)
                        (incf cursor))))))
      (loop while (< cursor (length line))
            do (read-inline-char))
      (when (eq :show (line-break-mode parser))
        (write-char #\Newline buffer))
      (commit-buffer)
      cursor)))

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
