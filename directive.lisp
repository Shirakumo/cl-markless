#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless)

(defgeneric prefix (directive))
(defgeneric begin (directive parser line cursor))
(defgeneric invoke (directive component parser line cursor))
(defgeneric end (directive component parser))
(defgeneric consume-prefix (directive component parser line cursor))
(defgeneric consume-end (directive component parser line cursor))

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

(defclass root-directive (directive)
  ())

(defmethod (setf enabled-p) ((value null) (_ root-directive))
  (error 'deactivation-disallowed :directive _))

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
               (or (/= end cursor)
                   (eql _ (or (dispatch (block-dispatch-table parser) line end) _))))
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
           (delegate-paragraph parser line cursor))
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
                always (and (< i (length line))
                            (char= #\  (aref line i))))
      (+ cursor numcnt 1))))

(defclass header (singular-line-directive)
  ())

(defmethod prefix ((_ header))
  #("#" "# "))

(defmethod begin ((_ header) parser line cursor)
  (let ((end cursor))
    (loop while (and (< end (length line))
                     (char= #\# (aref line end)))
          do (incf end))
    (cond ((or (<= (length line) end)
               (char/= #\  (aref line end)))
           (let ((component (make-instance 'components:paragraph)))
             (commit (directive 'paragraph parser) component parser)
             (vector-push-extend (subseq line cursor end) (components:children component))
             end))
          (T
           (commit _ (make-instance 'components:header :depth (- end cursor)) parser)
           (1+ end)))))

(defmethod end :after ((_ header) component parser)
  (let ((target (components:text component)))
    (setf (components:label target (root parser)) component)))

(defclass horizontal-rule (singular-line-directive)
  ())

(defmethod prefix ((_ horizontal-rule))
  #("=" "="))

(defmethod begin ((_ horizontal-rule) parser line cursor)
  (commit _ (make-instance 'components:horizontal-rule) parser)
  (+ cursor 2))

(defmethod invoke ((_ horizontal-rule) component parser line cursor)
  (let ((end cursor))
    (unless (loop while (< end (length line))
                  always (char= #\= (aref line end))
                  do (incf end))
      ;; We did a bad match, pretend we're a paragraph and skip the match.
      (change-class component 'components:paragraph)
      (vector-push-extend (subseq line (- cursor 2) end) (components:children component))
      (setf (stack-entry-directive (stack-top (stack parser)))
            (directive 'paragraph parser)))
    end))

(defclass code-block (block-directive)
  ())

(defmethod prefix ((_ code-block))
  #(":" ":"))

(defmethod begin ((_ code-block) parser line cursor)
  (multiple-value-bind (language cursor) (read-space-delimited line (+ cursor 3))
    (let ((options (split-string line #\  cursor))
          (language (when (string/= "" language) language)))
      (commit _ (make-instance 'components:code-block :language language :options options :text "") parser)
      0)))

(defmethod consume-prefix ((_ code-block) component parser line cursor)
  cursor)

(defmethod invoke ((_ code-block) component parser line cursor)
  (let ((input (input parser))
        (end (length line)))
    (setf line (read-line input))
    (when (string/= line "::")
      (setf (components:text component)
            (with-output-to-string (output)
              (loop (write-string line output)
                    (setf line (read-line input))
                    (if (string= "::" line)
                        (return)
                        (write-char #\Linefeed output))))))
    (stack-pop (stack parser))
    end))

(defclass instruction (singular-line-directive)
  ())

(defmethod prefix ((_ instruction))
  #("!" " "))

(defmethod begin ((_ instruction) parser line cursor)
  (multiple-value-bind (typename cursor) (read-space-delimited line (+ cursor 2))
    (let ((type (find-symbol (to-readtable-case typename #.(readtable-case *readtable*))
                             '#:org.shirakumo.markless.components)))
      (unless (and type (subtypep type 'components:instruction))
        (error 'unknown-instruction :instruction typename))
      (commit _ (parse-instruction (class-prototype type) line (1+ cursor)) parser))
    cursor))

(defmethod parse-instruction ((proto components:set) line cursor)
  (multiple-value-bind (variable cursor) (read-space-delimited line cursor)
    (let ((value (subseq line (1+ cursor))))
      (make-instance (class-of proto) :variable variable :value value))))

(defmethod parse-instruction ((proto components:message-instruction) line cursor)
  (make-instance (class-of proto) :message (subseq line cursor)))

(defmethod parse-instruction ((proto components:include) line cursor)
  (make-instance (class-of proto) :file (subseq line cursor)))

(defmethod parse-instruction ((proto components:directives-instruction) line cursor)
  (make-instance (class-of proto) :directives (split-string line #\  cursor)))

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
  (multiple-value-bind (typename cursor) (read-space-delimited line (+ cursor 2))
    (multiple-value-bind (target cursor) (read-space-delimited line (+ cursor 1))
      (let ((type (find-symbol (to-readtable-case typename #.(readtable-case *readtable*))
                               '#:org.shirakumo.markless.components)))
        (cond ((and type (subtypep type 'components:embed))
               (let ((component (make-instance type :target target)))
                 (multiple-value-bind (options cursor) (split-options line cursor #\])
                   (let ((options (mapcar #'parse-embed-option options)))
                     (loop for option in options
                           do (assert (embed-option-allowed-p option component) ()
                                      'option-disallowed :option option :embed-type type))
                     (setf (components:options component) options))
                   (commit _ component parser)
                   cursor)))
              (T
               (warn 'unknown-embed-type :embed-type typename)
               (let ((paragraph (make-instance 'components:paragraph))
                     (url (make-instance 'components:url :target target)))
                 (vector-push-extend url (components:children paragraph))
                 (commit (directive 'paragraph parser) paragraph parser)
                 (length line))))))))

(defun parse-embed-option (option)
  (let* ((typename (format NIL "~a-option"
                           (subseq option 0 (or (position #\  option)
                                                (length option)))))
         (type (find-symbol (to-readtable-case typename #.(readtable-case *readtable*))
                            '#:org.shirakumo.markless.components)))
    (if (and type (subtypep type 'components:embed-option))
        (parse-embed-option-type (class-prototype type) option)
        (error 'bad-option :option option))))

(defmethod parse-embed-option-type ((type components:embed-option) option)
  (make-instance (class-of type)))

(defmethod parse-embed-option-type ((type components:float-option) option)
  (make-instance (class-of type)
                 :direction (cond ((string-equal "float left" option) :left)
                                  ((string-equal "float right" option) :right)
                                  (T (error 'bad-option :option option)))))

(defmethod parse-embed-option-type ((type components:width-option) option)
  (multiple-value-bind (size unit) (parse-unit option :start (length "width "))
    (make-instance 'components:width-option :size size :unit unit)))

(defmethod parse-embed-option-type ((type components:height-option) option)
  (multiple-value-bind (size unit) (parse-unit option :start (length "height "))
    (make-instance 'components:height-option :size size :unit unit)))

(defmethod embed-option-allowed-p ((option components:embed-option) (embed components:embed)) NIL)
(defmethod embed-option-allowed-p ((option components:width-option) (embed components:embed)) T)
(defmethod embed-option-allowed-p ((option components:height-option) (embed components:embed)) T)
(defmethod embed-option-allowed-p ((option components:float-option) (embed components:embed)) T)
(defmethod embed-option-allowed-p ((option components:autoplay-option) (embed components:video)) T)
(defmethod embed-option-allowed-p ((option components:autoplay-option) (embed components:audio)) T)
(defmethod embed-option-allowed-p ((option components:loop-option) (embed components:video)) T)
(defmethod embed-option-allowed-p ((option components:loop-option) (embed components:audio)) T)

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
    (cond ((or (<= (length line) (1+ end))
               (= end cursor)
               (char/= #\] (aref line end)))
           ;; Mismatch. Pretend we're a paragraph.
           (delegate-paragraph parser line (1- cursor)))
          (T
           (let* ((target (parse-integer line :start cursor :end end))
                  (component (make-instance 'components:footnote :target target)))
             (setf (components:label (princ-to-string target) (root parser)) component)
             (commit _ component parser)
             (+ 2 end))))))

;;;; Inline Directives

(defclass bold (surrounding-inline-directive)
  ())

(defmethod prefix ((_ bold))
  #("*" "*"))

(defmethod begin ((_ bold) parser line cursor)
  (commit _ (make-instance 'components:bold) parser)
  (+ 2 cursor))

(defmethod end :after ((_ bold) component parser)
  (vector-push-front "**" (components:children component)))

(defclass italic (surrounding-inline-directive)
  ())

(defmethod prefix ((_ italic))
  #("/" "/"))

(defmethod begin ((_ italic) parser line cursor)
  (commit _ (make-instance 'components:italic) parser)
  (+ 2 cursor))

(defmethod end :after ((_ italic) component parser)
  (vector-push-front "//" (components:children component)))

(defclass underline (surrounding-inline-directive)
  ())

(defmethod prefix ((_ underline))
  #("_" "_"))

(defmethod begin ((_ underline) parser line cursor)
  (commit _ (make-instance 'components:underline) parser)
  (+ 2 cursor))

(defmethod end :after ((_ underline) component parser)
  (vector-push-front "__" (components:children component)))

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

(defmethod end :after ((_ strikethrough) component parser)
  (vector-push-front "<-" (components:children component)))

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

(defmethod end :after ((_ code) component parser)
  (vector-push-front "``" (components:children component)))

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

(defmethod end :after ((_ supertext) component parser)
  (vector-push-front "^(" (components:children component)))

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

(defmethod end :after ((_ subtext) component parser)
  (vector-push-front "v(" (components:children component)))

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
    (multiple-value-bind (options cursor) (split-options line cursor #\))
      (setf (components:options component)
            (mapcar #'parse-compound-option options))
      cursor)))

(defmethod end :after ((_ compound) component parser)
  (vector-push-front "\"" (components:children component)))

(defun parse-compound-option (option)
  (or (gethash option *color-table*)
      (gethash option *size-table*)
      (cond ((starts-with "#" option)
             (make-instance 'components:internal-link-option :target (subseq option 1)))
            (T
             (let* ((typename (format NIL "~a-option"
                                      (subseq option 0 (or (position #\  option)
                                                           (length option)))))
                    (type (find-symbol (to-readtable-case typename #.(readtable-case *readtable*))
                                       '#:org.shirakumo.markless.components)))
               (if (and type (subtypep type 'components:compound-option))
                   (parse-compound-option-type (class-prototype type) option)
                   (error 'bad-option :option option)))))))

(defmethod parse-compound-option-type ((proto components:compound-option) option)
  (make-instance (class-of proto)))

(defmethod parse-compound-option-type ((proto components:font-option) option)
  (make-instance (class-of proto) :font-family (subseq option (length "font "))))

(defmethod parse-compound-option-type ((proto components:color-option) option)
  (if (starts-with "color #" option)
      (let ((hex (parse-integer option :start (length "color #") :radix 16)))
        (destructuring-bind (r g b) (decompose-rgb hex)
          (make-instance (class-of proto) :red r :green g :blue b)))
      (let ((parts (split-string option #\  (length "color "))))
        (destructuring-bind (r g b) (mapcar #'parse-integer parts)
          (make-instance (class-of proto) :red r :green g :blue b)))))

(defmethod parse-compound-option-type ((proto components:size-option) option)
  (multiple-value-bind (size unit) (parse-unit option :start (length "size "))
    (make-instance (class-of proto) :size size :unit unit)))

(defmethod parse-compound-option-type ((proto components:link-option) option)
  (make-instance (class-of proto) :target (subseq option (length "link "))))

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
