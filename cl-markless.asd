(asdf:defsystem cl-markless
  :version "1.1.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
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
               (:file "misc")
               (:file "documentation"))
  :depends-on (:documentation-utils
               :trivial-indent)
  :in-order-to ((asdf:test-op (asdf:test-op :cl-markless-test))
                (asdf:build-op (asdf:build-op :cl-markless-standalone))))
