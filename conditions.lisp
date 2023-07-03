(in-package #:org.shirakumo.markless)

(defvar *current-line-number*)

(define-condition markless-condition (condition)
  ())

(define-condition implementation-condition (markless-condition)
  ())

(define-condition stack-exhausted (implementation-condition error)
  ()
  (:report (lambda (c s) (format s "The directive stack was exhausted. This means the implementation is seriously screwed up somehow."))))

(define-condition instruction-evaluation-undefined (implementation-condition error)
  ((instruction :initarg :instruction :reader instruction))
  (:report (lambda (c s) (format s "The instruction ~s does not have an evaluation defined."
                                 (instruction c)))))

(define-condition deactivation-disallowed (markless-condition error)
  ((directive :initarg :directive :reader directive-instance))
  (:report (lambda (c s) (format s "Deactivating the ~s directive is not allowed."
                                 (type-of (directive-instance c))))))

(define-condition parser-condition (markless-condition)
  ((line :initarg :line :reader line)
   (cursor :initarg :cursor :reader cursor))
  (:default-initargs :line *current-line-number*
                     :cursor 0))

(define-condition parser-error (parser-condition error)
  ())

(define-condition parser-warning (parser-condition warning)
  ())

(define-condition unknown-instruction (parser-error)
  ((instruction :initarg :instruction :reader instruction))
  (:report (lambda (c s) (format s "The instruction ~s is not known."
                                 (instruction c)))))

(define-condition unknown-embed-type (parser-warning)
  ((embed-type :initarg :embed-type :reader embed-type))
  (:report (lambda (c s) (format s "The embed type ~s is not known."
                                 (embed-type c)))))

(define-condition bad-option (parser-warning)
  ((option :initarg :option :reader option))
  (:report (lambda (c s) (format s "The option ~s is malformed or unrecognised."
                                 (option c)))))

(define-condition bad-unit (bad-option)
  ()
  (:report (lambda (c s) (format s "The unit in ~s is missing or cannot be recognised."
                                 (option c)))))

(define-condition option-disallowed (bad-option)
  ((embed-type :initarg :embed-type :reader embed-type))
  (:report (lambda (c s) (format s "The option ~s is not allowed for the embed type ~a."
                                 (option c) (embed-type c)))))

(define-condition bad-variable (parser-error)
  ((variable :initarg :variable :reader variable-name))
  (:report (lambda (c s) (format s "The variable ~s is not recognised."
                                 (variable-name c)))))

(define-condition bad-value (parser-error)
  ((variable :initarg :variable :reader variable-name)
   (value :initarg :value :reader value))
  (:report (lambda (c s) (format s "The value ~s is not allowed for the variable ~s."
                                 (value c) (variable-name c)))))

(define-condition user-warning (parser-warning)
  ((message :initarg :message :reader message))
  (:report (lambda (c s) (format s "The document warned:~%~a"
                                 (message c)))))

(define-condition user-error (parser-error)
  ((message :initarg :message :reader message))
  (:report (lambda (c s) (format s "The document errored:~%~a"
                                 (message c)))))
