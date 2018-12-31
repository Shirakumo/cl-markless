#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.markless.epub)

(defvar *here* #.(or *compile-file-pathname* *load-pathname* (error "COMPILE-FILE or LOAD this.")))
(defvar *path* NIL)

(defun resource-file (name type)
  (make-pathname :name name :type type :defaults *here*))

(defclass epub (output-format) ())

(defun output (markless target &rest args &key (if-exists :error))
  (etypecase markless
    (pathname
     (let ((*path* markless))
       (apply #'output (cl-markless:parse markless T) target args)))
    (string
     (apply #'output (cl-markless:parse markless T) target args))
    (components:component
     (create-epub target markless :if-exists if-exists))))

(defmethod output-component ((component components:root-component) (target pathname) (format epub))
  (create-epub target component))

(defun create-epub (path root &key (if-exists :error))
  (let ((date (get-universal-time))
        (meta (find-meta root))
        (*path* (or *path* path)))
    (zip:with-output-to-zipfile (zip path :if-exists if-exists)
      (flet ((file (name streamish)
               (zip:write-zipentry zip name (ensure-input-stream streamish)
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
        (file "OEBPS/document.opf" (oebps/opf root meta))
        (file "OEBPS/document.xhtml" (oebps/html root meta))
        (file "OEBPS/stylesheet.css" (resource-file "stylesheet" "css"))
        (dir "OEBPS/embed/")
        (loop for (path target) in (getf meta :embeds)
              do (file (format NIL "OEBPS/~a" target)
                       (merge-pathnames path *path*)))))
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

(defun find-meta (root)
  (let ((title (cons most-positive-fixnum "untitled"))
        (counter 0)
        (embeds ()))
    (labels ((traverse (component)
               (typecase component
                 (components:header
                  (when (< (components:depth component) (car title))
                    (setf (car title) (components:depth component))
                    (setf (cdr title) (components:text component))))
                 (components:embed
                  (let ((target (components:target component)))
                    (cond ((cl-markless:starts-with "#" target))
                          ((cl-markless:read-url target 0)
                           (warn "Embeds to URLs will not work in epubs."))
                          (T
                           (let* ((path (uiop:parse-native-namestring target))
                                  (target (format NIL "embed/~a-~a.~a"
                                                  (pathname-name path) (incf counter) (pathname-type path))))
                             (setf (components:target component) target)
                             (push (list path target) embeds))))))
                 (components:parent-component
                  (loop for child across (components:children component)
                        do (traverse child))))))
      (traverse root)
      (list :title (cdr title)
            :embeds embeds))))

(defun oebps/opf (root meta)
  (with-xml
    ("package" (("version" "2.0")
                ("xmlns" "http://www.idpf.org/2007/opf")
                ("unique-identifier" ""))
      ("metadata"
       (("xmlns:dc" "http://purl.org/dc/elements/1.1/")
        ("xmlns:opf" "http://www.idpf.org/2007/opf"))
       ("dc:title" () (:text (getf meta :title)))
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
       ("item" (("id" "stylesheet") ("href" "stylesheet.css") ("media-type" "text/css")))
       (:extra (xml)
         (loop for (path target) in (getf meta :embeds)
               do (make-element xml "item" `(("id" ,target)
                                             ("href" ,target)
                                             ("media-type" ,(trivial-mimes:mime-lookup path)))))))
      ("spine"
       ()
       ("itemref" (("idref" "document")))))))

(defun oebps/html (root meta)
  (with-xml
    (:extra (xml)
      (plump-dom:make-doctype xml "html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\""))
    ("html" (("xmlns" "http://www.w3.org/1999/xhtml")
             ("xml:lang" "en"))
      ("head" ()
       ("meta" (("http-equiv" "Content-Type")
                ("content" "application/xhtml+xml; charset=utf-8")))
       ("title" () (:text (getf meta :title)))
       ("link" (("rel" "stylesheet")
                ("type" "text/css")
                ("href" "stylesheet.css"))))
      ("body" ()
       (:extra (xml)
        (output-component root xml 'cl-markless-plump:plump))))))
