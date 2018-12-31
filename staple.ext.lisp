(asdf:load-system :staple-markless)

(defmethod staple:subsystems ((system (eql (asdf:find-system :cl-markless))))
  (list (asdf:find-system :cl-markless-plump)
        (asdf:find-system :cl-markless-standalone)))
