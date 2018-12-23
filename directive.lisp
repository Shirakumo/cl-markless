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

(defmethod invoke ((_ root-directive) component parser line cursor)
  (read-block parser line cursor))

(defmethod end ((_ root-directive) component parser))

(defclass block-directive (directive)
  ())

(defmethod end ((_ block-directive) component parser))

(defmethod invoke ((_ block-directive) component parser line cursor)
  (read-block parser line cursor))

(defclass singular-line-directive (block-directive)
  ())

(defmethod consume-prefix ((_ singular-line-directive) component parser line cursor)
  NIL)

(defmethod invoke ((_ singular-line-directive) component parser line cursor)
  (read-inline parser line cursor #\Nul))

(defclass inline-directive (directive)
  ())

(defmethod consume-prefix ((_ inline-directive) component parser line cursor)
  cursor)

(defmethod end ((_ inline-directive) component parser)
  (change-class component 'components:parent-component))

(defmethod invoke ((_ inline-directive) component parser line cursor)
  (read-inline parser line cursor #\Nul))

(defclass surrounding-inline-directive (inline-directive)
  ())

(defmethod begin :around ((_ surrounding-inline-directive) parser line cursor)
  (let ((stack (stack parser)))
    (cond ((eq _ (stack-entry-directive (stack-top stack)))
           (stack-pop stack)
           ;; KLUDGE
           (+ cursor 2))
          (T
           (call-next-method)))))

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

(defmethod invoke ((_ paragraph) component parser line cursor)
  (let ((inner (dispatch (block-dispatch-table parser) line cursor)))
    (if (and inner (not (eq inner _)))
        (begin inner parser line cursor)
        (read-inline parser line cursor #\Nul))))

(defclass blockquote-header (singular-line-directive)
  ())

(defmethod prefix ((_ blockquote-header))
  #("~" " "))

(defmethod begin ((_ blockquote-header) parser line cursor)
  (let* ((children (components:children (stack-entry-component (stack-top (stack parser)))))
         (predecessor (when (< 0 (length children))
                      (aref children (1- (length children)))))
         (component (make-instance 'components:blockquote-header)))
    (when (and (typep predecessor 'components:blockquote)
               (null (components:source predecessor)))
      (setf (components:source predecessor) component))
    (commit _ component parser)
    (+ 2 cursor)))

(defclass blockquote (block-directive)
  ())

(defmethod prefix ((_ blockquote))
  #("|" " "))

(defmethod begin ((_ blockquote) parser line cursor)
  (let* ((children (components:children (stack-entry-component (stack-top (stack parser)))))
         (predecessor (when (< 0 (length children))
                      (aref children (1- (length children)))))
         (component (make-instance 'components:blockquote)))
    (when (typep predecessor 'components:blockquote-header)
      (setf (components:source component) predecessor))
    (commit _ component parser)
    (+ 2 cursor)))

(defmethod consume-prefix ((_ blockquote) component parser line cursor)
  (match! "| " line cursor))

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
             (vector-push-extend (subseq line cursor end) (components:children component))
             end))
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
             (stack-push _ item (stack parser))
             (+ end 1))))))

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

(defmethod end :after ((_ header) component parser)
  (let ((target (components:text component)))
    (setf (components:label target (root parser)) component)))

;; FIXME: label table

(defclass code-block (block-directive)
  ())

(defmethod prefix ((_ code-block))
  #(":" ":"))

(defmethod begin ((_ code-block) parser line cursor)
  (multiple-value-bind (language cursor) (read-space-delimited line (+ cursor 2))
    (let ((options (split-string line #\  cursor)))
      (commit _ (make-instance 'components:code-block :language language :options options) parser)
      (length line))))

(defmethod consume-prefix ((_ code-block) component parser line cursor)
  cursor)

(defmethod invoke ((_ code-block) component parser line cursor)
  (if (string= line "::")
      (stack-pop (stack parser))
      (vector-push-extend line (components:children component)))
  (length line))

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

(defmethod invoke ((_ instruction) component parser line cursor)
  (evaluate-instruction component parser)
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
               (= end cursor)
               (char/= #\] (aref line end)))
           ;; Mismatch. Pretend we're a paragraph.
           (let ((component (make-instance 'components:paragraph)))
             (commit (directive 'paragraph parser) component parser)
             (vector-push-extend (subseq line cursor end) (components:children component))
             end))
          (T
           (let* ((target (parse-integer line :start cursor :end end))
                  (component (make-instance 'components:footnote :target target)))
             (setf (components:label target (root parser)) component)
             (commit _ component parser)
             (1+ end))))))

;;;; Inline Directives

(defclass bold (surrounding-inline-directive)
  ())

(defmethod prefix ((_ bold))
  #("*" "*"))

(defmethod begin ((_ bold) parser line cursor)
  (commit _ (make-instance 'components:bold) parser)
  (+ 2 cursor))

(defclass italic (surrounding-inline-directive)
  ())

(defmethod prefix ((_ italic))
  #("/" "/"))

(defmethod begin ((_ italic) parser line cursor)
  (commit _ (make-instance 'components:italic) parser)
  (+ 2 cursor))

(defclass underline (surrounding-inline-directive)
  ())

(defmethod prefix ((_ underline))
  #("_" "_"))

(defmethod begin ((_ underline) parser line cursor)
  (commit _ (make-instance 'components:underline) parser)
  (+ 2 cursor))

(defclass strikethrough (inline-directive)
  ())

(defmethod prefix ((_ strikethrough))
  #("<" "-"))

(defmethod begin ((_ strikethrough) parser line cursor)
  (commit _ (make-instance 'components:strikethrough) parser)
  (+ 2 cursor))

(defmethod invoke ((_ strikethrough) component parser line cursor)
  (read-inline parser line cursor #\-))

(defmethod consume-end ((_ strikethrough) component parser line cursor)
  (match! "->" line cursor))

(defclass code (inline-directive)
  ())

(defmethod prefix ((_ code))
  #("`" "`"))

(defmethod begin ((_ code) parser line cursor)
  (commit _ (make-instance 'components:code) parser)
  (+ 2 cursor))

(defmethod invoke ((_ code) component parser line cursor)
  (vector-push-extend
   (with-output-to-string (output)
     (loop while (< cursor (length line))
           for char = (aref line cursor)
           do (cond ((and (char= #\` char)
                          (< (1+ cursor) (length line))
                          (char= #\` (aref line (1+ cursor))))
                     (incf cursor 2)
                     (stack-pop (stack parser))
                     (return))
                    ((char= #\\ char)
                     (incf cursor)
                     (write-char (aref line cursor) output))
                    (T
                     (write-char char output)))
              (incf cursor)))
   (components:children component))
  cursor)

(defclass supertext (inline-directive)
  ())

(defmethod prefix ((_ supertext))
  #("^" "("))

(defmethod begin ((_ supertext) parser line cursor)
  (commit _ (make-instance 'components:supertext) parser)
  (+ 2 cursor))

(defmethod invoke ((_ supertext) component parser line cursor)
  (read-inline parser line cursor #\)))

(defmethod consume-end ((_ supertext) component parser line cursor)
  (match! ")" line cursor))

(defclass subtext (inline-directive)
  ())

(defmethod prefix ((_ subtext))
  #("v" "("))

(defmethod begin ((_ subtext) parser line cursor)
  (commit _ (make-instance 'components:subtext) parser)
  (+ 2 cursor))

(defmethod invoke ((_ subtext) component parser line cursor)
  (read-inline parser line cursor #\)))

(defmethod consume-end ((_ subtext) component parser line cursor)
  (match! ")" line cursor))

(defclass compound (inline-directive)
  ())

(defmethod prefix ((_ compound))
  #("\""))

(defmethod begin ((_ compound) parser line cursor)
  (commit _ (make-instance 'components:compound) parser)
  (+ 1 cursor))

(defmethod invoke ((_ compound) component parser line cursor)
  (read-inline parser line cursor #\"))

(defmethod consume-end ((_ compound) component parser line cursor)
  (when (and (< (+ 2 cursor) (length line))
             (char= #\( (aref line (+ 1 cursor))))
    (incf cursor 2)
    (let* ((options ())
           (buffer (make-string-output-stream)))
      (loop while (< cursor (length line))
            for char = (aref line cursor)
            do (cond ((char= #\\ char)
                      (incf cursor)
                      (write-char (aref line cursor) buffer))
                     ((char= #\, char)
                      (push (get-output-stream-string buffer) options))
                     ((char= #\) char)
                      (incf cursor)
                      (push (get-output-stream-string buffer) options)
                      (return))
                     (T
                      (write-char char buffer)))
               (incf cursor))
      (setf (components:options component) options)
      cursor)))

(defvar *style-table*
  (let ((table (make-hash-table :test 'equalp)))
    (loop for (name type) in '((bold components:bold-option)
                               (italic components:italic-option)
                               (underline components:underline-option)
                               (strikethrough components:strikethrough-option)
                               (spoiler components:spoiler-option))
          do (setf (gethash (string name) table) (make-instance type)))
    table))

(defun parse-compound-option (option)
  (or (gethash option *color-table*)
      (gethash option *size-table*)
      (gethash option *style-table*)
      (cond ((starts-with "font " option)
             (make-instance 'components:font-option :font-family (subseq option (length "font "))))
            ((starts-with "color #" option)
             (let ((hex (parse-integer option :start (length "color #") :radix 16)))
               (destructuring-bind (r g b) (decompose-rgb hex)
                 (make-instance 'components:color-option :red r :green g :blue b))))
            ((starts-with "color " option)
             (let ((parts (split-string option #\, (length "color "))))
               (destructuring-bind (r g b) (mapcar #'parse-integer parts)
                 (make-instance 'components:color-option :red r :green g :blue b))))
            ((starts-with "size " option)
             (let ((unit (cond ((ends-with "em" option) :em)
                               ((ends-with "pt" option) :pt)
                               (T (error "FIXME: better error"))))
                   (size (parse-float option :start (length "size ") :end (- (length option) 2))))
               (make-instance 'components:size-option :unit unit :size size)))
            ((starts-with "#" option)
             (make-instance 'components:internal-hyperlink-option :target (subseq option 1)))
            ((starts-with "link " option)
             (make-instance 'components:hyperlink-option :target (subseq option (length "link "))))
            (T
             (error "FIXME: better error")))))

(defclass footnote-reference (inline-directive)
  ())

(defmethod prefix ((_ footnote-reference))
  #("[" "1234567890"))

(defmethod begin ((_ footnote-reference) parser line cursor)
  (incf cursor)
  (let* ((end cursor)
         (stack (stack parser))
         (component (stack-entry-component (stack-top stack)))
         (children (components:children component)))
    (loop while (and (< end (length line))
                     (<= (char-code #\0) (char-code (aref line end)) (char-code #\9)))
          do (incf end))
    (cond ((or (<= (length line) end)
               (= end cursor)
               (char/= #\] (aref line end)))
           ;; Mismatch. Pretend we're nothing
           (vector-push-extend "[" children)
           cursor)
          (T
           (let ((target (parse-integer line :start cursor :end end)))
             (vector-push-extend (make-instance 'components:footnote-reference :target target) children)
             (1+ end))))))

(defclass dash (inline-directive)
  ())

(defmethod prefix ((_ dash))
  #("-" "-"))

(defmethod begin ((_ dash) parser line cursor)
  (let* ((stack (stack parser))
         (children (components:children (stack-entry-component (stack-top stack)))))
    (vector-push-extend #.(string (code-char #x2014)) children)
    (+ 2 cursor)))

(defclass newline (inline-directive)
  ())

(defmethod prefix ((_ newline))
  #("-" "/" "-"))

(defmethod begin ((_ newline) parser line cursor)
  (let* ((stack (stack parser))
         (children (components:children (stack-entry-component (stack-top stack)))))
    (vector-push-extend #.(string #\Linefeed) children)
    (+ 3 cursor)))
