(in-package #:org.shirakumo.markless)

(defun make-size-table (entries)
  (let ((table (make-hash-table :test 'equalp)))
    (loop for (name (size unit)) on entries by #'cddr
          do (setf (gethash (string name) table)
                   (make-instance 'components:size-option
                                  :size size :unit unit)))
    table))

(defparameter *size-table*
  (make-size-table
   '(microscopic (0.25 :em)
     tiny (0.5 :em)
     small (0.8 :em)
     normal (1.0 :em)
     big (1.5 :em)
     large (2.0 :em)
     huge (2.5 :em)
     gigantic (4.0 :em))))
