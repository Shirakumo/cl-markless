(in-package #:org.shirakumo.markless)

(defgeneric generate-component (type &key children recurse))
(defgeneric viable-children (parent))

(defun leaf-classes (class)
  (let ((class (etypecase class
                 (class class)
                 (symbol (find-class class))))
        (leaves ()))
    (labels ((rec (class)
               (if (c2mop:class-direct-subclasses class)
                   (mapc #'rec (c2mop:class-direct-subclasses class))
                   (push (class-name class) leaves))))
      (rec class))
    leaves))

(defun remove-all (list &rest removes)
  (loop for el in list unless (member el removes) collect el))

(defun range (min &optional max)
  (if max
      (+ min (random (- max min)))
      (etypecase min
        (integer min)
        (cons (apply #'range min)))))

(defmethod viable-children ((component components:parent-component))
  (remove-all (leaf-classes 'components:block-component)
              'components:unordered-list-item
              'components:ordered-list-item))

(defmethod viable-children ((component components:unordered-list))
  '(components:unordered-list-item))

(defmethod viable-children ((component components:ordered-list))
  '(components:ordered-list-item))

(defmethod viable-children ((component components:paragraph))
  (list* 'string (leaf-classes 'components:inline-component)))

(defmethod viable-children ((component components:inline-component))
  (list* 'string (leaf-classes 'components:inline-component)))

(defmethod viable-children ((component components:caption-option))
  (list* 'string (leaf-classes 'components:inline-component)))

(defmethod viable-options ((component components:embed))
  (leaf-classes 'components:embed-option))

(defmethod viable-options ((component components:compound))
  (leaf-classes 'components:compound-option))

(defmethod generate-component :around ((component standard-object) &key &allow-other-keys)
  (call-next-method)
  component)

(defmethod generate-component ((component (eql T)) &rest args &key &allow-other-keys)
  (apply #'generate-component (make-instance 'components:root-component) args))

(defmethod generate-component ((component symbol) &rest args &key &allow-other-keys)
  (apply #'generate-component (allocate-instance (find-class component)) args))

(defmethod generate-component ((component (eql 'string)) &key &allow-other-keys)
  (lorem-ipsum:paragraph :word-count (range 2 9) :prologue NIL))

(defmethod generate-component ((component (eql 'link)) &key &allow-other-keys)
  (format NIL "http://~a.com/~a" (lorem-ipsum:word) (lorem-ipsum:word)))

(defmethod generate-component ((component components:sized) &key &allow-other-keys)
  (setf (components:unit component) (alexandria:random-elt '("px" "em" "pt")))
  (setf (components:size component) (alexandria:random-elt '(1.0 0.5 0.25 1.25 2.0))))

(defmethod generate-component ((component components:text-component) &key &allow-other-keys)
  (setf (components:text component) (generate-component 'string)))

(defmethod generate-component :before ((component components:paragraph) &key &allow-other-keys)
  (setf (components:indentation component) (max 0 (- (random 10) 5))))

(defmethod generate-component :before ((component components:blockquote) &key &allow-other-keys)
  (setf (components:indentation component) (max 0 (- (random 10) 5))))

(defmethod generate-component :before ((component components:ordered-list-item) &key &allow-other-keys)
  (setf (components:number component) (range 1 100)))

(defmethod generate-component :before ((component components:header) &key &allow-other-keys)
  (setf (components:depth component) (range 0 5)))

(defmethod generate-component :before ((component components:code-block) &key &allow-other-keys)
  (setf (components:language component) (alexandria:random-elt '("lisp" "tex" "markless" "html")))
  (setf (components:options component) (loop repeat (range 0 5)
                                             collect (lorem-ipsum:word)))
  (setf (components:depth component) (range 0 4))
  (setf (components:inset component) (range 0 10)))

(defmethod generate-component ((component components:message-instruction) &key &allow-other-keys)
  (setf (components:message component) (generate-component 'string)))

(defmethod generate-component ((component components:directives-instruction) &key &allow-other-keys)
  (setf (components:directives component) (loop repeat (range 0 5)
                                                collect (alexandria:random-elt cl-markless:*default-directives*))))

(defmethod generate-component ((component components:label) &key &allow-other-keys)
  (setf (components:target component) (lorem-ipsum:word)))

(defmethod generate-component ((component components:set) &key &allow-other-keys)
  (setf (components:variable component) (lorem-ipsum:word))
  (setf (components:value component) (lorem-ipsum:word)))

(defmethod generate-component ((component components:include) &key &allow-other-keys)
  (setf (components:file component) (lorem-ipsum:word)))

(defmethod generate-component ((component components:raw) &key &allow-other-keys)
  (setf (components:target component) (lorem-ipsum:word))
  (setf (components:text component) (lorem-ipsum:paragraph :word-count (range 1 5))))

(defmethod generate-component ((component components:embed) &key &allow-other-keys)
  (let ((viable (viable-options component)))
    (setf (components:target component) (generate-component 'link))
    (setf (components:options component) (loop repeat (range 0 5)
                                               for type = (alexandria:random-elt viable)
                                               collect (generate-component type)))))

(defmethod generate-component ((component components:embed-option) &key &allow-other-keys))

(defmethod generate-component ((component components:embed-link-option) &key &allow-other-keys)
  (setf (components:target component) (generate-component 'link)))

(defmethod generate-component ((component components:float-option) &key &allow-other-keys)
  (setf (components:direction component) (alexandria:random-elt '(:left :right))))

(defmethod generate-component ((component components:label-option) &key &allow-other-keys)
  (setf (components:target component) (lorem-ipsum:word)))

(defmethod generate-component ((component components:options-option) &key &allow-other-keys)
  (setf (components:options component) (loop repeat (range 0 5)
                                             collect (lorem-ipsum:word))))

(defmethod generate-component ((component components:language-option) &key &allow-other-keys)
  (setf (components:language component) (lorem-ipsum:word)))

(defmethod generate-component ((component components:start-option) &key &allow-other-keys)
  (setf (components:start component) (range 0 100)))

(defmethod generate-component ((component components:end-option) &key &allow-other-keys)
  (setf (components:end component) (range 0 100))
  (setf (components:offset-p component) T))

(defmethod generate-component ((component components:encoding-option) &key &allow-other-keys)
  (setf (components:encoding component) (alexandria:random-elt '("utf-8" "ascii"))))

(defmethod generate-component :before ((component components:footnote) &key &allow-other-keys)
  (setf (components:target component) (range 1 10)))

(defmethod generate-component :before ((component components:align) &key &allow-other-keys)
  (setf (components:alignment component) (alexandria:random-elt '(:left :right :center))))

(defmethod generate-component ((component components:url) &key &allow-other-keys)
  (setf (components:target component) (generate-component 'link)))

(defmethod generate-component :before ((component components:compound) &key &allow-other-keys)
  (let ((viable (viable-options component)))
    (setf (components:options component) (loop repeat (range 0 5)
                                               for type = (alexandria:random-elt viable)
                                               collect (generate-component type)))))

(defmethod generate-component ((component components:compound-option) &key &allow-other-keys))

(defmethod generate-component ((component components:font-option) &key &allow-other-keys)
  (setf (components:font-family component) (lorem-ipsum:word)))

(defmethod generate-component ((component components:color-option) &key &allow-other-keys)
  (setf (components:red component) (range 0 256))
  (setf (components:green component) (range 0 256))
  (setf (components:blue component) (range 0 256)))

(defmethod generate-component ((component components:link-option) &key &allow-other-keys)
  (setf (components:target component) (generate-component 'link)))

(defmethod generate-component ((component components:internal-link-option) &key &allow-other-keys)
  (setf (components:target component) (lorem-ipsum:word)))

(defmethod generate-component ((component components:footnote-reference) &key &allow-other-keys)
  (setf (components:target component) (range 1 10)))

(defmethod generate-component :before ((component components:parent-component) &key &allow-other-keys)
  (setf (components:children component) (make-array 0 :adjustable T :fill-pointer T)))

(defmethod generate-component ((component components:parent-component) &key (children '(0 5)) (recurse 10))
  (when (< 0 recurse)
    (let ((viable (viable-children component)))
      (dotimes (i (range children) component)
        (let ((child (generate-component (alexandria:random-elt viable) :children children :recurse (1- recurse))))
          (vector-push-extend child (components:children component)))))))

(defmethod generate-component ((component components:unit-component) &key &allow-other-keys))
