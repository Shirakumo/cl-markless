(in-package #:org.shirakumo.markless)

(declaim (ftype (function (directive parser simple-string (unsigned-byte 32)) (unsigned-byte 32)) begin))
(declaim (ftype (function (directive components:component parser simple-string (unsigned-byte 32)) (unsigned-byte 32)) invoke))
(declaim (ftype (function (directive components:component parser) T) end))
(declaim (ftype (function (directive components:component parser simple-string (unsigned-byte 32)) (or null (unsigned-byte 32))) consume-prefix))
(declaim (ftype (function (directive components:component parser simple-string (unsigned-byte 32)) (or null (unsigned-byte 32))) consume-end))

(defparameter *default-directives*
  '(paragraph
    blockquote-header
    blockquote
    unordered-list
    ordered-list
    header
    horizontal-rule
    code-block
    instruction
    comment
    embed
    footnote
    left-align
    right-align
    center
    justify

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
    url
    newline))

(defparameter *default-instruction-types*
  '(components:set
    components:info
    components:warning
    components:error
    components:include
    components:disable
    components:enable
    components:label
    components:raw))

(defparameter *default-compound-options*
  '(components:bold-option
    components:italic-option
    components:underline-option
    components:strikethrough-option
    components:spoiler-option
    components:font-option
    components:color-option
    components:size-option
    components:link-option
    components:internal-link-option))

(defparameter *default-embed-types*
  '(components:image
    components:video
    components:audio
    components:source))

(defparameter *default-embed-options*
  '(components:loop-option
    components:autoplay-option
    components:width-option
    components:height-option
    components:float-option
    components:label-option
    components:caption-option
    components:options-option
    components:language-option
    components:start-option
    components:end-option
    components:encoding-option
    components:embed-link-option))

(defun compile-dispatch-table  (directives)
  (labels ((max-char (candidates i)
             (loop for candidate in candidates
                   maximize (if (<= (length (car candidate)) i)
                                0
                                (loop for char across (aref (car candidate) i)
                                      maximize (char-code char)))))
           (make-table (candidates i)
             (loop with table = (make-array (1+ (max-char candidates i)) :initial-element ())
                   for candidate in candidates
                   do (if (<= (length (car candidate)) i)
                          (push candidate (aref table 0))
                          (loop for char across (aref (car candidate) i)
                                do (push candidate (aref table (char-code char)))))
                   finally (return (fill-table table (1+ i)))))
           (fill-table (table i)
             (loop for key from 0 below (length table)
                   for candidates = (aref table key)
                   do (setf (aref table key)
                            (if (or (cdr candidates)
                                    (<= i (1- (length (caar candidates)))))
                                (make-table candidates i)
                                (cdr (first candidates))))
                   finally (return table))))
    (make-table (loop for directive in directives
                      collect (cons (prefix directive) directive))
                0)))

(defun dispatch (table string cursor)
  (declare (type simple-vector table))
  (declare (type simple-string string))
  (declare (optimize speed (safety 1)))
  (loop with target of-type simple-vector = table
        for i of-type (unsigned-byte 32) from cursor below (length string)
        for char of-type character = (aref string i)
        for code of-type (unsigned-byte 32) = (char-code char)
        do (let ((next (or (when (< code (length target))
                             (aref target code))
                           (aref target 0))))
             (if (arrayp next)
                 (setf target next)
                 (return (if (and (typep next 'directive) (enabled-p next))
                             next
                             (aref table 0)))))
        finally (return (aref table 0))))

(defstruct stack-entry
  (directive NIL)
  (component NIL))

;; FIXME: Future: separate out parser state so that parsers can
;;        be re-used

(defclass parser ()
  ((line-break-mode :initarg :line-break-mode :initform :show :accessor line-break-mode)
   (directives :initform () :accessor directives)
   (embed-types :initform () :accessor embed-types)
   (embed-options :initform () :accessor embed-options)
   (compound-options :initform () :accessor compound-options)
   (instruction-types :initform () :accessor instruction-types)
   (block-dispatch-table :accessor block-dispatch-table)
   (inline-dispatch-table :accessor inline-dispatch-table)
   (input :accessor input)
   (stack :accessor stack)))

(defmethod initialize-instance :after ((parser parser) &key (directives *default-directives*)
                                                            (compound-options *default-compound-options*)
                                                            (embed-types *default-embed-types*)
                                                            (embed-options *default-embed-options*)
                                                            (instruction-types *default-instruction-types*)
                                                            disabled-directives
                                                            (stack-size-limit 64))
  (setf (directives parser) (mapcar #'ensure-directive directives))
  (setf (compound-options parser) (mapcar #'ensure-compound-option compound-options))
  (setf (embed-types parser) (mapcar #'ensure-embed-type embed-types))
  (setf (embed-options parser) (mapcar #'ensure-embed-option embed-options))
  (setf (instruction-types parser) (mapcar #'ensure-instruction-type instruction-types))
  (setf (stack parser) (make-array stack-size-limit :fill-pointer 0 :element-type 'stack-entry))
  (loop for i from 0 below stack-size-limit
        do (setf (aref (stack parser) i) (make-stack-entry)))
  (dolist (directive disabled-directives)
    (setf (enabled-p (directive directive parser)) NIL))
  (setf (block-dispatch-table parser) (compile-dispatch-table (directives-of 'block-directive parser)))
  (setf (inline-dispatch-table parser) (compile-dispatch-table (directives-of 'inline-directive parser))))

(declaim (inline stack-push))
(defun stack-push (directive component stack)
  (declare (type (vector stack-entry) stack))
  (let ((entry (aref stack (fill-pointer stack))))
    (incf (fill-pointer stack))
    (setf (stack-entry-directive entry) directive)
    (setf (stack-entry-component entry) component)
    entry))

(declaim (inline stack-pop))
(defun stack-pop (stack)
  (declare (type (vector stack-entry) stack))
  (when (= 0 (fill-pointer stack))
    (error 'stack-exhausted))
  (decf (fill-pointer stack))
  (aref stack (fill-pointer stack)))

(declaim (inline stack-top))
(defun stack-top (stack)
  (declare (type (vector stack-entry) stack))
  (aref stack (1- (length stack))))

(declaim (inline stack-bottom))
(defun stack-bottom (stack)
  (declare (type (vector stack-entry) stack))
  (aref stack 0))

(defun stack-unwind (stack parser until)
  (loop until (= (length stack) until)
        for entry = (stack-pop stack)
        do (end (stack-entry-directive entry)
                (stack-entry-component entry)
                parser)))

(declaim (inline root))
(defun root (parser)
  (stack-entry-component (stack-bottom (stack parser))))

(defmethod directive ((name symbol) (parser parser))
  (find name (directives parser) :key #'type-of))

(defmethod directive ((name string) parser)
  (directive (find-symbol (to-readtable-case name #.(readtable-case *readtable*))
                          '#:org.shirakumo.markless)
             parser))

(defmethod directives-of (type (parser parser))
  (remove-if-not (lambda (d) (typep d type)) (directives parser)))

(defmethod disable ((parser parser) (test function))
  (dolist (directive (directives parser) parser)
    (setf (enabled-p directive) (funcall test directive))))

(defmethod enable ((parser parser) (test function))
  (dolist (directive (directives parser) parser)
    (setf (enabled-p directive) (funcall test directive))))

(defmethod evaluate-instruction (instruction (parser parser))
  (error 'instruction-evaluation-undefined :instruction instruction))

(defmethod evaluate-instruction ((instruction components:set) (parser parser))
  (let ((v (components:variable instruction)))
    (cond ((string-equal v "line-break-mode")
           (cond ((string= (components:value instruction) "show")
                  (setf (line-break-mode parser) :show))
                 ((string= (components:value instruction) "hide")
                  (setf (line-break-mode parser) :hide))
                 (T
                  (error 'bad-value
                         :variable (components:variable instruction)
                         :value (components:value instruction)))))
          ((string-equal v "author")
           (setf (components:author (root parser))
                 (components:value instruction)))
          ((string-equal v "copyright")
           (setf (components:copyright (root parser))
                 (components:value instruction)))
          ((string-equal v "language")
           (setf (components:language (root parser))
                 (components:value instruction)))
          (T
           (error 'bad-variable
                  :variable (components:variable instruction))))))

(defmethod evaluate-instruction ((instruction components:info) (parser parser))
  (format *error-output* "~&[INFO ] ~a~%" (components:message instruction)))

(defmethod evaluate-instruction ((instruction components:warning) (parser parser))
  (format *error-output* "~&[WARN ] ~a~%" (components:message instruction))
  (warn 'user-warning :message (components:message instruction)))

(defmethod evaluate-instruction ((instruction components:error) (parser parser))
  (format *error-output* "~&[ERROR] ~a~%" (components:message instruction))
  (error 'user-error :message (components:message instruction)))

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

(defmethod evaluate-instruction ((instruction components:label) (parser parser))
  (setf (components:label (components:target instruction) (root parser))
        (stack-entry-component (stack-top (stack parser)))))

(defmethod evaluate-instruction ((instruction components:raw) (parser parser)))

(defun read-full-line (stream)
  (declare (type stream stream))
  (declare (optimize speed))
  (let ((line (read-line stream)))
    (declare (type simple-string line))
    (if (and (< 0 (length line)) (eql #\\ (char line (1- (length line)))))
        (with-output-to-string (out)
          (loop with i of-type fixnum = 0
                with length of-type fixnum = (length line)
                while (< i length)
                for char of-type character = (aref line i)
                do (cond ((char= #\\ char)
                          (incf i)
                          (cond ((= i length)
                                 (setf i -1)
                                 (setf line (read-line stream))
                                 (setf length (length line)))
                                (T
                                 (write-char char out)
                                 (write-char (aref line i) out))))
                         (T
                          (write-char char out)))
                   (incf i)))
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

(defun pop-newline (stack)
  (let ((component (stack-entry-component (stack-top stack))))
    (when (or (typep component 'components:inline-component)
              (typep component 'components:paragraph))
      (let ((children (components:children component)))
        (when (and (< 0 (length children))
                   (typep (aref children (1- (length children))) 'components:newline))
          (vector-pop children))))))

(defmethod parse ((stream stream) (parser parser))
  (let* ((root (make-instance 'components:root-component))
         (stack (stack parser))
         (*current-line-number* 0))
    (setf (input parser) stream)
    (stack-push (make-instance 'root-directive) root stack)
    (loop while (peek-char NIL (input parser) NIL)
          for line = (read-full-line (input parser))
          do (process-stack parser stack line)
             (incf *current-line-number*))
    (when (eq :show (line-break-mode parser))
      (pop-newline stack))
    (stack-unwind stack parser 0)
    root))

(defun commit (directive component parser)
  (let* ((stack (stack parser))
         (children (components:children (stack-entry-component (stack-top stack)))))
    (vector-push-extend component children)
    (stack-push directive component stack)))

(defun process-stack (parser stack line)
  (declare (type simple-string line))
  (declare (type (vector stack-entry) stack))
  (declare (optimize speed))
  (let ((cursor 0)
        (stack-pointer 1))
    (declare (type (unsigned-byte 32) cursor stack-pointer))
    (loop while (< stack-pointer (length stack))
          for entry = (aref stack stack-pointer)
          for next-cursor = (consume-prefix (stack-entry-directive entry)
                                            (stack-entry-component entry)
                                            parser line cursor)
          do (unless next-cursor
               (when (eq :show (line-break-mode parser))
                 (pop-newline stack))
               (stack-unwind stack parser stack-pointer)
               (return))
             (setf cursor next-cursor)
             (incf stack-pointer))
    (loop for entry = (aref stack (1- (length stack)))
          do (setf cursor (invoke (stack-entry-directive entry)
                                  (stack-entry-component entry)
                                  parser line cursor))
          while (< cursor (length line)))
    (when (eq :show (line-break-mode parser))
      (let ((top (stack-entry-component (stack-top stack))))
        (when (or (typep top 'components:inline-component)
                  (typep top 'components:paragraph))
          (vector-push-extend (make-instance 'components:newline) (components:children top)))))))

(defun read-block (parser line cursor)
  (declare (type simple-string line))
  (declare (type (unsigned-byte 32) cursor))
  (declare (optimize speed))
  (let* ((table (block-dispatch-table parser))
         (directive (dispatch table line cursor)))
    (begin directive parser line cursor)))

(defun read-url (line cursor)
  (declare (type simple-string line))
  (declare (type (unsigned-byte 32) cursor))
  (declare (optimize speed (safety 0)))
  (flet ((alpha-p (char)
           (<= (char-code #\A) (char-code char) (char-code #\z)))
         (number-p (char)
           (<= (char-code #\0) (char-code char) (char-code #\9)))
         (drop ()
           (return-from read-url NIL)))
    (declare (inline alpha-p number-p drop))
    (unless (alpha-p (aref line cursor))
      (drop))
    (let ((end cursor)
          (length (length line)))
      (declare (type (unsigned-byte 32) end length))
      ;; Match scheme
      (loop while (< end length)
            for char of-type character = (aref line end)
            do (when (char= #\: char)
                 (return))
               (unless (or (alpha-p char)
                           (number-p char)
                           (char= char #\+)
                           (char= char #\-)
                           (char= char #\.))
                 (drop))
               (incf end))
      (unless (and (< (+ 2 end) length)
                   (char= #\/ (aref line (+ 1 end)))
                   (char= #\/ (aref line (+ 2 end))))
        (drop))
      (incf end 3)
      (let ((start end))
        ;; Match path
        (loop while (< end length)
              for char of-type character = (aref line end)
              do (unless (or (alpha-p char)
                             (number-p char)
                             (find char "$-_.+!*'()&,/:;=?@z%#"))
                   (return))
                 (incf end))
        (unless (< start end)
          (drop))
        end))))

(defun read-inline (parser line cursor end-char)
  (declare (type simple-string line))
  (declare (type (unsigned-byte 32) cursor))
  (declare (type character end-char))
  (declare (optimize speed (safety 1)))
  (let* ((buffer (make-string-output-stream))
         (table (inline-dispatch-table parser))
         (top (stack-top (stack parser))))
    (labels ((commit-buffer ()
               (let ((string (get-output-stream-string buffer)))
                 (when (string/= "" string)
                   (vector-push-extend string (components:children (stack-entry-component top))))))
             (read-inline-char (char)
               (let ((directive (dispatch table line cursor)))
                 (cond ((typep directive 'url)
                        (let ((end (the (unsigned-byte 32) (begin directive parser line cursor))))
                          (cond ((/= cursor end)
                                 (commit-buffer)
                                 (vector-push-extend (make-instance 'components:url :target (subseq line cursor end))
                                                     (components:children (stack-entry-component top)))
                                 (setf cursor end))
                                (T
                                 (write-char char buffer)
                                 (incf cursor)))))
                       (directive
                        (commit-buffer)
                        (return-from read-inline (begin directive parser line cursor)))
                       (T
                        (write-char char buffer)
                        (incf cursor))))))
      (loop while (< cursor (length line))
            for char = (aref line cursor)
            do (when (char= end-char char)
                 (let ((next (consume-end (stack-entry-directive top)
                                          (stack-entry-component top)
                                          parser line cursor)))
                   (when next
                     (commit-buffer)
                     (stack-pop (stack parser))
                     (return-from read-inline next))))
               (cond ((char= char #\\)
                      (write-char (aref line (+ 1 cursor)) buffer)
                      (incf cursor 2))
                     (T
                      (read-inline-char char))))
      (commit-buffer)
      cursor)))
