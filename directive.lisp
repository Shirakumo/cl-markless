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

(defun ensure-compound-option (option-ish)
  (etypecase option-ish
    (class (if (subtypep (class-name option-ish) 'components:compound-option)
               option-ish
               (error "~a is not a COMPOUND-OPTION." option-ish)))
    (symbol (ensure-compound-option (find-class option-ish)))))

(defun ensure-embed-type (type-ish)
  (etypecase type-ish
    (class (if (subtypep (class-name type-ish) 'components:embed)
               type-ish
               (error "~a is not an EMBED type." type-ish)))
    (symbol (ensure-embed-type (find-class type-ish)))))

(defun ensure-embed-option (option-ish)
  (etypecase option-ish
    (class (if (subtypep (class-name option-ish) 'components:embed-option)
               option-ish
               (error "~a is not an EMBED-OPTION." option-ish)))
    (symbol (ensure-embed-option (find-class option-ish)))))

(defun ensure-instruction-type (type-ish)
  (etypecase type-ish
    (class (if (subtypep (class-name type-ish) 'components:instruction)
               type-ish
               (error "~a is not an INSTRUCTION type." type-ish)))
    (symbol (ensure-instruction-type (find-class type-ish)))))

(defgeneric prefix (directive))
(defgeneric begin (directive parser line cursor))
(defgeneric invoke (directive component parser line cursor))
(defgeneric end (directive component parser))
(defgeneric consume-prefix (directive component parser line cursor))
(defgeneric consume-end (directive component parser line cursor))

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

(defclass noop (directive)
  ())

(defmethod consume-prefix ((_ noop) component parser line cursor)
  NIL)

(defmethod invoke ((_ noop) component parser line cursor)
  cursor)

(defmethod end ((_ noop) component parser))

(defclass paragraph (block-directive)
  ())

(defmethod (setf enabled-p) ((value null) (_ paragraph))
  (error 'deactivation-disallowed :directive _))

(defmethod prefix ((_ paragraph))
  #())

(defmethod begin ((_ paragraph) parser line cursor)
  (cond ((= cursor (length line))
         (commit (make-instance 'noop) (make-instance 'components:unit-component) parser)
         cursor)
        (T
         (let ((end cursor))
           (loop while (and (< end (length line))
                            (char= #\  (aref line end)))
                 do (incf end))
           (commit _ (make-instance 'components:paragraph :indentation (- end cursor)) parser)
           end))))

(defmethod consume-prefix ((_ paragraph) component parser line cursor)
  (let ((end cursor))
    (loop while (and (< end (length line))
                     (char= #\  (aref line end)))
          do (incf end))
    (when (and (< end (length line))
               (= end (+ cursor (components:indentation component)))
               (or (/= end cursor)
                   (eql _ (dispatch (block-dispatch-table parser) line end))))
      end)))

(defmethod invoke ((_ paragraph) component parser line cursor)
  (read-inline parser line cursor #\nul))

(defclass blockquote-header (block-directive)
  ())

(defmethod prefix ((_ blockquote-header))
  #("~" " "))

(defmethod begin ((_ blockquote-header) parser line cursor)
  (let* ((start cursor)
         (children (components:children (stack-entry-component (stack-top (stack parser)))))
         (predecessor (when (< 0 (length children))
                        (aref children (1- (length children)))))
         (component (make-instance 'components:blockquote-header)))
    (when (and (typep predecessor 'components:blockquote)
               (null (components:source predecessor)))
      (setf (components:source predecessor) component))
    (commit _ component parser)
    (incf cursor 2)
    ;; Special handling for blockquote inline in header
    (let ((pos (search "| " line :start2 cursor))
          (top (fill-pointer (stack parser))))
      (when pos
        (let ((component (make-instance 'components:blockquote :source component :indentation (- pos start))))
          (loop (setf cursor (read-inline parser line cursor #\|))
                (cond ((<= pos cursor)
                       (loop until (< (fill-pointer (stack parser)) top)
                             do (stack-pop (stack parser)))
                       (setf cursor pos)
                       (return))
                      ((char= #\| (char line cursor))
                       (incf (fill-pointer (stack parser)))
                       (vector-push-extend "|" (components:children (stack-entry-component (stack-top (stack parser)))))
                       (incf cursor))))
          ;; Eagerly create our blockquote to set the indentation
          (commit _ component parser)
          (incf cursor 2))))
    cursor))

(defmethod consume-prefix ((_ blockquote-header) (component components:blockquote) parser line cursor)
  (and (loop repeat (components:indentation component)
             always (and (< cursor (length line))
                         (char= #\  (aref line cursor)))
             do (incf cursor))
       (match! "| " line cursor)))

(defmethod consume-prefix ((_ blockquote-header) component parser line cursor)
  NIL)

(defmethod consume-end ((_ blockquote-header) component parser line cursor)
  cursor)

(defmethod invoke ((_ blockquote-header) component parser line cursor)
  (read-inline parser line cursor #\nul))

(defmethod invoke ((_ blockquote-header) (component components:blockquote) parser line cursor)
  (read-block parser line cursor))

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

(defmethod begin ((_ unordered-list) parser line cursor)9
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
  (let ((numcnt (integer-digits (components:number component))))
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
  (let ((start cursor)
        (end cursor))
    (loop while (and (< end (length line))
                     (char= #\: (aref line end)))
          do (incf end))
    (let ((depth (- end cursor)))
      (loop while (and (< end (length line))
                       (char= #\Space (aref line end)))
            do (incf end))
      (multiple-value-bind (language cursor) (read-delimited line end #\,)
        (let ((options (split-options line (1+ cursor) #\Nul))
              (language (when (string/= "" language) language)))
          (commit _ (make-instance 'components:code-block :language language
                                                          :options options
                                                          :text ""
                                                          :depth depth
                                                          :inset start)
                  parser)
          0)))))

(defmethod consume-prefix ((_ code-block) component parser line cursor)
  cursor)

(defmethod invoke ((_ code-block) component parser line cursor)
  (let ((input (input parser))
        (end (length line))
        (suffix (format NIL "~v{ ~}~v{:~}" (components:inset component) 1 (components:depth component) 1)))
    (setf line (read-line input))
    (when (string/= line suffix)
      (setf (components:text component)
            (with-output-to-string (output)
              (loop (write-string line output :start (min (length line) (components:inset component)))
                    (setf line (read-line input))
                    (if (string= suffix line)
                        (return)
                        (write-char #\Linefeed output))))))
    (stack-pop (stack parser))
    end))

(defclass instruction (singular-line-directive)
  ())

(defmethod prefix ((_ instruction))
  #("!" " "))

(defmethod begin ((_ instruction) parser line cursor)
  (multiple-value-bind (typename cursor) (read-delimited line (+ cursor 2) #\ )
    (let ((class (find-instruction-type parser typename)))
      (unless class
        (error 'unknown-instruction :cursor cursor :instruction typename))
      (commit _ (parse-instruction (class-prototype class) line (1+ cursor)) parser))
    cursor))

(defmethod find-instruction-type ((parser parser) (type string))
  (loop for class in (instruction-types parser)
        do (when (string-equal type (class-name class))
             (return class))))

(defmethod parse-instruction ((proto components:set) line cursor)
  (multiple-value-bind (variable cursor) (read-delimited line cursor #\ )
    (let ((value (subseq line (1+ cursor))))
      (make-instance (class-of proto) :variable variable :value value))))

(defmethod parse-instruction ((proto components:message-instruction) line cursor)
  (make-instance (class-of proto) :message (subseq line cursor)))

(defmethod parse-instruction ((proto components:include) line cursor)
  (make-instance (class-of proto) :file (subseq line cursor)))

(defmethod parse-instruction ((proto components:directives-instruction) line cursor)
  (make-instance (class-of proto) :directives (split-string line #\  cursor)))

(defmethod parse-instruction ((proto components:label) line cursor)
  (make-instance (class-of proto) :target (subseq line cursor)))

(defmethod parse-instruction ((proto components:raw) line cursor)
  (multiple-value-bind (target cursor) (read-delimited line cursor #\ )
    (let ((text (subseq line (1+ cursor))))
      (make-instance (class-of proto) :target target :text text))))

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

(defmethod begin ((_ embed) parser line start-cursor)
  (multiple-value-bind (typename cursor) (read-delimited line (+ start-cursor 2) #\ )
    (multiple-value-bind (target cursor) (read-delimited line (+ cursor 1) #\,)
      (let ((class (find-embed-type parser typename))
            ;; KLUDGE: This is bad.
            (target (string-right-trim " ]" target)))
        (cond (class
               (incf cursor)
               (let ((component (make-instance class :target target)))
                 (setf (components:options component)
                       (loop for (string next continue) = (next-option line cursor #\])
                             for option = (when string (parse-embed-option parser cursor string component))
                             when option collect option
                             ;; KLUDGE: Since we can't invoke the parser inside PARSE-EMBED-OPTION
                             ;;         we do things here.
                             do (typecase option
                                  (components:caption-option
                                   (stack-push (directive 'paragraph parser) option (stack parser))
                                   (loop for cursor = (length "caption ")
                                         then (read-inline parser string cursor #\Nul)
                                         while (< cursor (length string)))
                                   (stack-pop (stack parser)))
                                  (components:label-option
                                   (setf (components:label (components:target option) (root parser))
                                         component)))
                                (setf cursor next)
                             while continue))
                 (commit _ component parser)
                 (length line)))
              (T
               (warn 'unknown-embed-type
                     :cursor (+ start-cursor 2)
                     :embed-type typename)
               (let ((paragraph (make-instance 'components:paragraph))
                     (url (make-instance 'components:url :target target)))
                 (vector-push-extend url (components:children paragraph))
                 (commit (directive 'paragraph parser) paragraph parser)
                 (length line))))))))

(defmethod find-embed-type ((parser parser) (type string))
  (loop for class in (embed-types parser)
        do (when (string-equal type (class-name class))
             (return class))))

(defun parse-embed-option (parser cursor option component)
  (let ((class (find-embed-option-type parser option)))
    (if class
        (handler-case
            (let ((option (parse-embed-option-type (class-prototype class) option)))
              (if (embed-option-allowed-p option component)
                  option
                  (warn 'option-disallowed
                        :cursor (+ 2 cursor)
                        :option option
                        :embed-type (class-name class))))
          (invalid-option ()
            (error 'invalid-option :cursor (+ 2 cursor) :option option))
          (error (_)
            (declare (ignore _))
            (warn 'bad-option :cursor (+ 2 cursor) :option option)))
        (warn 'bad-option :cursor (+ 2 cursor) :option option))))

(defmethod find-embed-option-type ((parser parser) (option string))
  (let ((typename (format NIL "embed-~a-option"
                          (subseq option 0 (or (position #\  option)
                                               (length option))))))
    (loop for option in (embed-options parser)
          do (when (or (string-equal typename (class-name option) :start1 (length "embed-"))
                       (string-equal typename (class-name option)))
               (return option)))))

(defmethod parse-embed-option-type ((type components:embed-option) option)
  (make-instance (class-of type)))

(defmethod parse-embed-option-type ((type components:float-option) option)
  (make-instance (class-of type)
                 :direction (cond ((string-equal "float left" option) :left)
                                  ((string-equal "float right" option) :right)
                                  (T (error "FLOAT must be LEFT or RIGHT.")))))

(defmethod parse-embed-option-type ((type components:label-option) option)
  (make-instance (class-of type) :target (subseq option (length "label "))))

(defmethod parse-embed-option-type ((type components:caption-option) option)
  (make-instance (class-of type)))

(defmethod parse-embed-option-type ((type components:description-option) option)
  (make-instance (class-of type) :text (subseq option (length "description "))))

(defmethod parse-embed-option-type ((type components:width-option) option)
  (multiple-value-bind (size unit) (parse-unit option :start (length "width "))
    (when unit
      (make-instance 'components:width-option :size size :unit unit))))

(defmethod parse-embed-option-type ((type components:height-option) option)
  (multiple-value-bind (size unit) (parse-unit option :start (length "height "))
    (when unit
      (make-instance 'components:height-option :size size :unit unit))))

(defmethod parse-embed-option-type ((type components:options-option) option)
  (make-instance (class-of type) :options (split-options option (length "options ") #\Nul)))

(defmethod parse-embed-option-type ((type components:language-option) option)
  (make-instance (class-of type) :language (subseq option (length "language "))))

(defmethod parse-embed-option-type ((type components:start-option) option)
  (make-instance (class-of type) :start (parse-integer option :start (length "start "))))

(defmethod parse-embed-option-type ((type components:end-option) option)
  (if (char= #\+ (aref option (length "end ")))
      (make-instance (class-of type) :end (parse-integer option :start (length "end +")) :offset-p T)
      (make-instance (class-of type) :end (parse-integer option :start (length "end ")))))

(defmethod parse-embed-option-type ((type components:encoding-option) option)
  (let ((encoding (subseq option (length "encoding "))))
    (unless (member encoding '("utf-8" "ascii") :test #'string-equal) (error 'invalid-option))
    (make-instance (class-of type) :encoding encoding)))

(defmethod parse-embed-option-type ((type components:embed-link-option) option)
  (make-instance (class-of type) :target (subseq option (length "link "))))

(defmethod embed-option-allowed-p ((option components:embed-option) (embed components:embed)) NIL)
(defmethod embed-option-allowed-p ((option components:width-option) (embed components:embed)) T)
(defmethod embed-option-allowed-p ((option components:height-option) (embed components:embed)) T)
(defmethod embed-option-allowed-p ((option components:float-option) (embed components:embed)) T)
(defmethod embed-option-allowed-p ((option components:label-option) (embed components:embed)) T)
(defmethod embed-option-allowed-p ((option components:caption-option) (embed components:embed)) T)
(defmethod embed-option-allowed-p ((option components:description-option) (embed components:embed)) T)
(defmethod embed-option-allowed-p ((option components:autoplay-option) (embed components:video)) T)
(defmethod embed-option-allowed-p ((option components:autoplay-option) (embed components:audio)) T)
(defmethod embed-option-allowed-p ((option components:loop-option) (embed components:video)) T)
(defmethod embed-option-allowed-p ((option components:loop-option) (embed components:audio)) T)
(defmethod embed-option-allowed-p ((option components:options-option) (embed components:source)) T)
(defmethod embed-option-allowed-p ((option components:language-option) (embed components:source)) T)
(defmethod embed-option-allowed-p ((option components:start-option) (embed components:source)) T)
(defmethod embed-option-allowed-p ((option components:end-option) (embed components:source)) T)
(defmethod embed-option-allowed-p ((option components:encoding-option) (embed components:source)) T)
(defmethod embed-option-allowed-p ((option components:embed-link-option) (embed components:image)) T)

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

(defclass left-align (block-directive)
  ())

(defmethod prefix ((_ left-align))
  #("|" "<"))

(defmethod begin ((_ left-align) parser line cursor)
  (commit _ (make-instance 'components:align :alignment :left) parser)
  (+ 2 cursor))

(defmethod consume-prefix ((_ left-align) component parser line cursor)
  (match! "|<" line cursor))

(defclass right-align (block-directive)
  ())

(defmethod prefix ((_ right-align))
  #("|" ">"))

(defmethod begin ((_ right-align) parser line cursor)
  (commit _ (make-instance 'components:align :alignment :right) parser)
  (+ 2 cursor))

(defmethod consume-prefix ((_ right-align) component parser line cursor)
  (match! "|>" line cursor))

(defclass center (block-directive)
  ())

(defmethod prefix ((_ center))
  #(">" "<"))

(defmethod begin ((_ center) parser line cursor)
  (commit _ (make-instance 'components:align :alignment :center) parser)
  (+ 2 cursor))

(defmethod consume-prefix ((_ center) component parser line cursor)
  (match! "><" line cursor))

(defclass justify (block-directive)
  ())

(defmethod prefix ((_ justify))
  #("|" "|"))

(defmethod begin ((_ justify) parser line cursor)
  (commit _ (make-instance 'components:align :alignment :justify) parser)
  (+ 2 cursor))

(defmethod consume-prefix ((_ justify) component parser line cursor)
  (match! "||" line cursor))

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
  #("'" "'"))

(defmethod begin ((_ compound) parser line cursor)
  (commit _ (make-instance 'components:compound) parser)
  (+ 2 cursor))

(defmethod invoke ((_ compound) component parser line cursor)
  (read-inline parser line cursor #\'))

(defmethod consume-end ((_ compound) component parser line cursor)
  (when (and (< (+ 3 cursor) (length line))
             (char= #\' (aref line (+ 1 cursor)))
             (char= #\( (aref line (+ 2 cursor))))
    (incf cursor 3)
    (setf (components:options component)
          (loop for (string next continue) = (next-option line cursor #\))
                for option = (when string (parse-compound-option parser cursor string))
                when option collect option
                do (setf cursor next)
                while continue))
    cursor))

(defmethod end :after ((_ compound) component parser)
  (vector-push-front "''" (components:children component)))

(defun parse-compound-option (parser cursor option)
  (or (gethash option *color-table*)
      (gethash option *size-table*)
      (cond ((starts-with "#" option)
             (make-instance 'components:internal-link-option :target (subseq option 1)))
            ((eql (read-url option 0) (length option))
             (make-instance 'components:link-option :target option))
            (T
             (let ((class (find-compound-option-type parser option)))
               (if class
                   (handler-case (parse-compound-option-type (class-prototype class) option)
                     (invalid-option ()
                       (error 'invalid-option :cursor (+ 2 cursor) :option option))
                     (error (e)
                       (declare (ignore e))
                       (warn 'bad-option :cursor (+ 2 cursor) :option option)))
                   (warn 'bad-option :cursor (+ 2 cursor) :option option)))))))

(defmethod find-compound-option-type ((parser parser) option)
  (let ((typename (format NIL "~a-option"
                          (subseq option 0 (or (position #\  option)
                                               (length option))))))
    (loop for option in (compound-options parser)
          do (when (string-equal typename (class-name option))
               (return option)))))

(defmethod parse-compound-option-type ((proto components:compound-option) option)
  (make-instance (class-of proto)))

(defmethod parse-compound-option-type ((proto components:font-option) option)
  (make-instance (class-of proto) :font-family (subseq option (length "font "))))

(defmethod parse-compound-option-type ((proto components:color-option) option)
  (if (starts-with "color #" option)
      (multiple-value-bind (hex end) (parse-integer option :start (length "color #") :radix 16)
        (unless (= end (+ 7 6)) (error 'invalid-option))
        (destructuring-bind (r g b) (decompose-rgb hex)
          (make-instance (class-of proto) :red r :green g :blue b)))
      (let ((parts (split-string option #\  (length "color "))))
        (if (= 1 (length parts))
            (or (gethash (first parts) *color-table*)
                (error 'invalid-option))
            (destructuring-bind (r g b) (mapcar #'parse-integer parts)
              (make-instance (class-of proto) :red r :green g :blue b))))))

(defmethod parse-compound-option-type ((proto components:size-option) option)
  (multiple-value-bind (size unit) (parse-unit option :start (length "size "))
    (when unit
      (make-instance (class-of proto) :size size :unit unit))))

(defmethod parse-compound-option-type ((proto components:link-option) option)
  (let ((target (subseq option (length "link "))))
    (if (starts-with "#" target)
        (make-instance 'components:internal-link-option :target (subseq target 1))
        (make-instance (class-of proto) :target target))))

(defclass deprecated-compound (compound)
  ())

(defmethod prefix ((_ deprecated-compound))
  #("\""))

(defmethod begin ((_ deprecated-compound) parser line cursor)
  (commit _ (make-instance 'components:compound) parser)
  (+ 1 cursor))

(defmethod invoke ((_ deprecated-compound) component parser line cursor)
  (read-inline parser line cursor #\"))

(defmethod end :after ((_ deprecated-compound) component parser)
  (setf (aref (components:children component) 0) "\""))

(defmethod consume-end ((_ deprecated-compound) component parser line cursor)
  (when (and (< (+ 2 cursor) (length line))
             (char= #\( (aref line (+ 1 cursor))))
    (incf cursor 2)
    (setf (components:options component)
          (loop for (string next continue) = (next-option line cursor #\))
                for option = (when string (parse-compound-option parser cursor string))
                when option collect option
                do (setf cursor next)
                while continue))
    cursor))

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

(defclass url (inline-directive)
  ())

(defmethod prefix ((_ url))
  #())

(defmethod begin ((_ url) parser line cursor)
  (or (read-url line cursor)
      cursor))

(defclass dash (inline-directive)
  ())

(defmethod prefix ((_ dash))
  #("-" "-"))

(defmethod begin ((_ dash) parser line cursor)
  (let* ((stack (stack parser))
         (children (components:children (stack-entry-component (stack-top stack)))))
    (incf cursor 2)
    (cond ((and (< cursor (length line))
                (char= #\- (aref line cursor)))
           (vector-push-extend (make-instance 'components:em-dash) children)
           (1+ cursor))
          (T
           (vector-push-extend (make-instance 'components:en-dash) children)
           cursor))))

(defclass newline (inline-directive)
  ())

(defmethod prefix ((_ newline))
  #("-" "/" "-"))

(defmethod begin ((_ newline) parser line cursor)
  (let* ((stack (stack parser))
         (children (components:children (stack-entry-component (stack-top stack)))))
    (vector-push-extend (make-instance 'components:newline) children)
    (+ 3 cursor)))
