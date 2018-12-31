#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:cl-markless-epub
  (:nicknames #:org.shirakumo.markless.epub)
  (:use #:cl #:org.shirakumo.markless)
  (:local-nicknames
   (#:components #:org.shirakumo.markless.components))
  (:shadowing-import-from #:org.shirakumo.markless #:debug)
  (:export
   #:epub))
(in-package #:org.shirakumo.markless.epub)

(defclass epub (output-format) ())

(defclass utf-8-input-stream (trivial-gray-streams:fundamental-binary-input-stream)
  ((source :initarg :source :initform (error "SOURCE required.") :accessor source)
   (buffer :initform (make-array 0 :element-type '(unsigned-byte 8)) :accessor buffer)
   (index :initform 0 :accessor index)))

(defun make-utf8-input-stream (streamish)
  (make-instance 'utf-8-input-stream :source (etypecase streamish
                                               (string (make-string-input-stream streamish))
                                               (stream streamish))))

(defun fresh-buffer (stream)
  (let* ((charbuf (make-string 1024 :initial-element #\Nul))
         (read (read-sequence charbuf (source stream)))
         (octs (babel:string-to-octets charbuf :end read :encoding :utf-8)))
    (setf (index stream) 0)
    (setf (buffer stream) octs)))

(defmethod trivial-gray-streams:stream-read-byte ((stream utf-8-input-stream))
  (let ((buffer (buffer stream))
        (index (index stream)))
    (when (<= (length buffer) index)
      (setf index 0)
      (setf buffer (fresh-buffer stream)))
    (cond ((= 0 (length buffer))
           :eof)
          (T
           (setf (index stream) (1+ index))
           (aref buffer index)))))

(defmethod trivial-gray-streams:stream-read-sequence ((stream utf-8-input-stream) sequence start end &key)
  (loop with i = start
        do (let* ((buffer (buffer stream))
                  (index (index stream)))
             (when (<= (length buffer) index)
               (setf index 0)
               (setf buffer (fresh-buffer stream)))
             (when (= 0 (length buffer))
               (return i))
             (let* ((bytes-available (- (length buffer) index))
                    (bytes-to-write (min bytes-available (- end i))))
               (replace sequence buffer :start1 i :end1 (+ i bytes-to-write)
                                        :start2 index)
               (incf (index stream) bytes-to-write)
               (incf i bytes-to-write)
               (when (<= end i) (return i))))))

(defmethod output-component ((component components:root-component) (target pathname) (format epub))
  (create-epub target component))

(defun create-epub (path root)
  (let ((date (get-universal-time)))
    (zip:with-output-to-zipfile (zip path)
      (flet ((file (name streamish)
               (zip:write-zipentry zip name (make-utf8-input-stream streamish)
                                   :file-mode #o640
                                   :file-write-date date))
             (dir (name)
               (zip:write-zipentry zip name (make-concatenated-stream)
                                   :file-mode #o755
                                   :file-write-date date)))
        (file "mimetype" "application/epub+zip")
        (dir "META-INF/")
        (file "META-INF/container.xml" (meta-inf/container))
        (dir "OEBPS/")
        (file "OEBPS/document.opf" (oebps/opf root))
        (file "OEBPS/document.xhtml" (oebps/html root))
        (with-open-file (stream (asdf:system-relative-pathname :cl-markless-epub "stylesheet.css"))
          (file "OEBPS/stylesheet.css" stream))))
    path))

(defun make-element (parent tag attributes)
  (let ((element (plump-dom:make-element parent tag)))
    (loop for (key val) in attributes
          do (setf (plump-dom:attribute element key) val))
    element))

(defmacro with-element (parent &body elements)
  (let ((parentg (gensym "PARENT")))
    `(let ((,parentg ,parent))
       ,@(loop for (tag attributes . children) in elements
               collect (case tag
                         (:text `(plump-dom:make-text-node ,parentg ,attributes))
                         (:extra `(let ((,(first attributes) ,parentg))
                                    ,@children))
                         (T
                          `(with-element (make-element ,parentg ,tag ',attributes)
                             ,@children))))
       ,parentg)))

(defmacro with-xml (&body elements)
  (let ((root (gensym "ROOT")))
    `(let ((plump:*tag-dispatchers* plump:*xml-tags*)
           (,root (plump-dom:make-root)))
       (let ((header (plump-dom:make-xml-header ,root)))
         (setf (plump-dom:attribute header "version") "1.0")
         (setf (plump-dom:attribute header "encoding") "utf-8"))
       (with-element ,root
         ,@elements)
       (plump-dom:serialize ,root NIL))))

(trivial-indent:define-indentation with-element (4 (&whole 2 4 6 (&whole 2 4 6))))
(trivial-indent:define-indentation with-xml ((&whole 2 4 &body)))

(defun meta-inf/container ()
  (with-xml
    ("container" (("version" "1.0")
                  ("xmlns" "urn:oasis:names:tc:opendocument:xmlns:container"))
      ("rootfiles" ()
        ("rootfile" (("full-path" "OEBPS/document.opf")
                     ("media-type" "application/oebps-package+xml")))))))

(defun format-date (&optional (universal-time (get-universal-time)))
  (multiple-value-bind (s m h dd mm yy) (decode-universal-time universal-time 0)
    (format NIL "~4,'0d-~2,'0d-~2,'0d" yy mm dd)))

(defun find-title (root)
  (let ((best (cons most-positive-fixnum "untitled")))
    (labels ((traverse (component)
               (cond ((and (typep component 'components:header)
                           (< (components:depth component) (car best)))
                      (setf (car best) (components:depth component))
                      (setf (cdr best) (components:text component)))
                     ((typep component 'components:parent-component)
                      (loop for child across (components:children component)
                            do (traverse child))))))
      (traverse root)
      (cdr best))))

(defun oebps/opf (root)
  (with-xml
    ("package" (("version" "2.0")
                ("xmlns" "http://www.idpf.org/2007/opf")
                ("unique-identifier" ""))
      ("metadata"
       (("xmlns:dc" "http://purl.org/dc/elements/1.1/")
        ("xmlns:opf" "http://www.idpf.org/2007/opf"))
       ("dc:title" () (:text (find-title root)))
       ("dc:language" () (:text (or (components:language root) "en")))
       ("dc:identifier" (("id" "bookid")) (:text "FIXME"))
       ("dc:creator" (("opf:role" "aut") ("id" "author"))
                     (:text (or (components:author root)
                                (car (last (pathname-directory (user-homedir-pathname))))
                                "Anonymous")))
       ("dc:date" () (:text (format-date)))
       ("dc:rights" () (:text (or (components:copyright root) ""))))
      ("manifest"
       ()
       ("item" (("id" "document") ("href" "document.xhtml") ("media-type" "application/xhtml+xml")))
       ("item" (("id" "stylesheet") ("href" "stylesheet.css") ("media-type" "text/css"))))
      ("spine"
       ()
       ("itemref" (("idref" "document")))))))

(defun oebps/html (root)
  (with-xml
    (:extra (xml)
      (plump-dom:make-doctype xml "html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\""))
    ("html" (("xmlns" "http://www.w3.org/1999/xhtml")
             ("xml:lang" "en"))
      ("head" ()
       ("meta" (("http-equiv" "Content-Type")
                ("content" "application/xhtml+xml; charset=utf-8")))
       ("title" () (:text (find-title root)))
       ("link" (("rel" "stylesheet")
                ("type" "text/css")
                ("href" "stylesheet.css"))))
      ("body" ()
       (:extra (xml)
        (output-component root xml 'cl-markless-plump:plump))))))
