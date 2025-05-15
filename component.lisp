(in-package #:org.shirakumo.markless.components)

(defmacro define-printer (class format &rest args)
  `(defmethod print-object ((c ,class) s)
     (print-unreadable-object (c s :type T :identity T)
       (format s ,format ,@args))))

(defclass sized ()
  ((unit :initarg :unit :initform (cl:error "UNIT required") :accessor unit)
   (size :initarg :size :initform (cl:error "SIZE required") :accessor size)))

(defclass targeted ()
  ((target :initarg :target :initform (cl:error "TARGET required") :accessor target)))

(define-printer sized
  "~f~(~a~)" (size c) (unit c))

(defclass component ()
  ())

(defclass unit-component (component)
  ())

(defclass text-component (component)
  ((text :initarg :text :initform (cl:error "TEXT required") :accessor text)))

(defclass parent-component (component)
  ((children :initarg :children :initform (make-array 0 :adjustable T :fill-pointer T) :accessor children)))

(defmethod text ((component parent-component))
  (with-output-to-string (out)
    (cl:labels ((r (component)
                  (loop for child across (children component)
                        do (typecase child
                             (string (write-string child out))
                             (newline (write-char #\Linefeed out))
                             (text-component (write-string (text child) out))
                             (parent-component (r child))))))
      (r component))))

(defclass block-component (component)
  ())

(defclass inline-component (component)
  ())

(defclass root-component (parent-component)
  ((labels :initform (make-hash-table :test 'equalp) :accessor labels)
   (author :initform NIL :accessor author)
   (copyright :initform NIL :accessor copyright)
   (language :initform NIL :accessor language)))

(define-printer root-component
  "~@[~a ~]~@[(c) ~a~]" (author c) (copyright c))

(defmethod label ((label string) (root root-component))
  (gethash label (labels root)))

(defmethod (setf label) ((value component) (label string) (root root-component))
  (setf (gethash label (labels root)) value))

(defmethod (setf label) ((value null) (label string) (root root-component))
  (remhash label (labels root))
  NIL)

(defclass paragraph (parent-component block-component)
  ((indentation :initarg :indentation :initform 0 :accessor indentation)))

(defclass blockquote-header (parent-component block-component)
  ())

(defclass blockquote (parent-component block-component)
  ((source :initarg :source :initform NIL :accessor source)
   (indentation :initarg :indentation :initform 0 :accessor indentation)))

(defclass list (parent-component)
  ())

(defclass list-item (parent-component)
  ())

(defclass ordered-list (list block-component)
  ())

(defclass ordered-list-item (list-item block-component)
  ((number :initarg :number :initform 0 :accessor number)))

(define-printer ordered-list-item
  "(~d)" (number c))

(defclass unordered-list (list block-component)
  ())

(defclass unordered-list-item (list-item block-component)
  ())

(defclass header (parent-component block-component)
  ((depth :initarg :depth :initform 0 :accessor depth)))

(define-printer header
  "(~d)" (depth c))

(defclass horizontal-rule (unit-component block-component)
  ())

(defclass code-block (text-component block-component)
  ((language :initarg :language :initform NIL :accessor language)
   (options :initarg :options :initform () :accessor options)
   (depth :initarg :depth :initform 0 :accessor depth)
   (inset :initarg :inset :initform 0 :accessor inset)))

(define-printer code-block
  "~@[~a~]~{ ~a~}" (language c) (options c))

(defclass instruction (block-component)
  ())

(defclass message-instruction (instruction)
  ((message :initarg :message :initform (cl:error "MESSAGE required") :accessor message)))

(define-printer message-instruction
  "~s" (message c))

(defclass directives-instruction (instruction)
  ((directives :initarg :directives :initform (cl:error "DIRECTIVES required.") :accessor directives)))

(define-printer directives-instruction
  "~{~a~^ ~}" (directives c))

(defclass set (instruction)
  ((variable :initarg :variable :initform (cl:error "VARIABLE required") :accessor variable)
   (value :initarg :value :initform (cl:error "VALUE required") :accessor value)))

(define-printer set
  "~a ~s" (variable c) (value c))

(defclass info (message-instruction)
  ())

(defclass warning (message-instruction)
  ())

(defclass error (message-instruction)
  ())

(defclass include (instruction)
  ((file :initarg :file :initform (cl:error "FILE required") :accessor file)))

(define-printer include
  "~s" (file c))

(defclass disable (directives-instruction)
  ())

(defclass enable (directives-instruction)
  ())

(defclass label (instruction targeted)
  ())

(defclass raw (instruction targeted)
  ((text :initarg :text :initform (cl:error "TEXT required") :accessor text)))

(defclass comment (text-component block-component)
  ())

(defclass embed (unit-component block-component targeted)
  ((options :initarg :options :initform () :accessor options)))

(defun find-option (type container)
  (loop for option in (options container)
        do (when (typep option type)
             (return option))))

(define-printer embed
  "~s" (target c))

(defclass image (embed)
  ())

(defclass video (embed)
  ())

(defclass audio (embed)
  ())

(defclass source (embed)
  ())

(defclass embed-option ()
  ())

(defclass loop-option (embed-option)
  ())

(defclass autoplay-option (embed-option)
  ())

(defclass width-option (embed-option sized)
  ())

(defclass height-option (embed-option sized)
  ())

(defclass float-option (embed-option)
  ((direction :initarg :direction :initform (cl:error "DIRECTION required") :accessor direction)))

(defclass label-option (embed-option targeted)
  ())

(defclass caption-option (embed-option parent-component)
  ())

(defclass description-option (embed-option text-component)
  ())

(defclass options-option (embed-option)
  ((options :initarg :options :initform () :accessor options)))

(defclass language-option (embed-option)
  ((language :initarg :language :initform NIL :accessor language)))

(defclass start-option (embed-option)
  ((start :initarg :start :initform (cl:error "START required") :accessor start)))

(defclass end-option (embed-option)
  ((end :initarg :end :initform (cl:error "END required") :accessor end)
   (offset-p :initarg :offset-p :initform NIL :accessor offset-p)))

(defclass encoding-option (embed-option)
  ((encoding :initarg :encoding :initform (cl:error "ENCODING required") :accessor encoding)))

(defclass embed-link-option (embed-option targeted)
  ())

(defclass footnote (parent-component block-component targeted)
  ())

(define-printer footnote
  "(~d)" (target c))

(defclass align (parent-component block-component)
  ((alignment :initarg :alignment :initform (cl:error "ALIGNMENT required") :accessor alignment)))

(defclass bold (inline-component parent-component)
  ())

(defclass italic (inline-component parent-component)
  ())

(defclass underline (inline-component parent-component)
  ())

(defclass strikethrough (inline-component parent-component)
  ())

(defclass code (inline-component parent-component)
  ())

(defclass subtext (inline-component parent-component)
  ())

(defclass supertext (inline-component parent-component)
  ())

(defclass url (inline-component unit-component targeted)
  ())

(defclass compound (inline-component parent-component)
  ((options :initarg :options :initform () :accessor options)))

(defclass compound-option ()
  ())

(defclass bold-option (compound-option) ())

(defclass italic-option (compound-option) ())

(defclass underline-option (compound-option) ())

(defclass strikethrough-option (compound-option) ())

(defclass spoiler-option (compound-option) ())

(defclass font-option (compound-option)
  ((font-family :initarg :font-family :initform (cl:error "FONT-FAMILY required") :accessor font-family)))

(define-printer font-option
  "~s" (font-family c))

(defclass color-option (compound-option)
  ((red :initarg :red :initform (cl:error "RED required") :accessor red)
   (green :initarg :green :initform (cl:error "GREEN required") :accessor green)
   (blue :initarg :blue :initform (cl:error "BLUE required") :accessor blue)))

(define-printer color-option
  "~d,~d,~d" (red c) (green c) (blue c))

(defclass size-option (compound-option sized)
  ())

(define-printer size-option
  "~f~(~a~)" (size c) (unit c))

(defclass link-option (compound-option targeted)
  ())

(define-printer link-option
  "~s" (target c))

(defclass internal-link-option (link-option)
  ())

(defclass footnote-reference (inline-component unit-component targeted)
  ())

(define-printer footnote-reference
  "(~d)" (target c))

(defclass en-dash (inline-component unit-component) ())
(defclass em-dash (inline-component unit-component) ())
(defclass newline (inline-component unit-component) ())
