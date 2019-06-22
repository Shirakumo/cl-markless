#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-markless
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A parser implementation for Markless"
  :homepage "https://github.com/Shinmera/cl-markless"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "conditions")
               (:file "component")
               (:file "color-table")
               (:file "size-table")
               (:file "printer")
               (:file "parser")
               (:file "directive")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :trivial-indent)
  :in-order-to ((asdf:test-op (asdf:test-op :cl-markless-test))
                (asdf:build-op (asdf:build-op :cl-markless-standalone))))
