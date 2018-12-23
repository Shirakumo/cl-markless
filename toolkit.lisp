#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless)

(defun decompose-rgb (hex)
  (list (ldb (byte 8 16) hex)
        (ldb (byte 8  8) hex)
        (ldb (byte 8  0) hex)))

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
              finally (return `(when (< (+ ,cursorg ,(length prefix)) (length ,lineg))
                                 ,form))))))

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

(defun starts-with (beginning string &optional (start 0))
  (and (<= (length beginning) (- (length string) start))
       (string= beginning string :start2 start :end2 (+ start (length beginning)))))

(defun ends-with (end string)
  (and (<= (length end) (length string))
       (string= end string :start2 (- (length string) (length end)))))

(defun parse-float (string &key (start 0) (end (length string)))
  (let* ((dot (or (position #\. string :start start :end end) end))
         (whole (parse-integer string :start start :end dot)))
    (incf dot)
    (float
     (if (< dot end)
         (let ((fractional (parse-integer string :start dot :end end)))
           (+ whole (/ fractional (expt 10 (- end dot)))))
         whole))))

(defun to-readtable-case (string case)
  (ecase case
    (:downcase (string-downcase string))
    (:upcase (string-upcase string))
    (:preserve string)
    (:invert (error "FIXME: Implement INVERT read-case."))))
