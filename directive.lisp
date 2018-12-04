#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.directives)

(defclass directive ()
  ((enabled-p :initarg :enabled-p :initform T :accessor enabled-p)))

(defun ensure-directive (directive-ish)
  (etypecase directive-ish
    (directive directive-ish)
    (symbol (ensure-directive (make-instance directive-ish)))))

(defclass block-directive (directive)
  ())

(defclass inline-directive (directive)
  ())

(defclass paragraph (directive)
  ())

(defclass header (directive)
  ())
