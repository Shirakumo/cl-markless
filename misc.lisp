#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless)

(defmethod count-words ((string string) &optional (method :whitespace))
  (count-words (parse string T) :whitespace))

(defmethod count-words ((pathname pathname) &optional (method :whitespace))
  (count-words (parse pathname T) method))

(defmethod count-words ((component components:component) &optional (method :whitespace))
  (count-words-by method component))

(defgeneric count-words-by (method thing)
  (:method-combination +))

(defmethod count-words-by + (method (component components:component))
  0)

(defmethod count-words-by + (method (component components:parent-component))
  (loop for child across (components:children component)
        sum (count-words-by method child)))

(defmethod count-words-by + (method (component components:text-component))
  (count-words-by method (components:text component)))

(defmethod count-words-by + (method (component components:instruction))
  0)

(defmethod count-words-by + (method (component components:comment))
  0)

(defmethod count-words-by + ((method (eql :whitespace)) (string string))
  (let ((count 0) (word-started-p NIL))
    (flet ((commit ()
             (when word-started-p
               (incf count)
               (setf word-started-p NIL))))
      (loop for i from 0 below (length string)
            for c = (char string i)
            do (cond (#+sb-unicode (not (find (sb-unicode:general-category c) '(:Ll :Lo :Lm :Lt :Lu)))
                      #-sb-unicode (or (find c ".!?-_,;:<>(){}[]&")
                                       (find c '(#\Space #\Tab #\Linefeed #\Return)))
                      (commit))
                     (T
                      (setf word-started-p T)))
            finally (commit)))
    count))

(defmethod count-words-by + ((method (eql :character)) (string string))
  (loop for i from 0 below (length string)
        for c = (char string i)
        count #+sb-unicode (find (sb-unicode:general-category c) '(:Ll :Lo :Lm :Lt :Lu))
        #-sb-unicode (not (or (find c ".!?-_,;:<>(){}[]&0123456789")
                              (find c '(#\Space #\Tab #\Linefeed #\Return))))))
