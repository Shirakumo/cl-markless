(asdf:defsystem cl-markless-plump
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A compiler for Markless to Plump DOM/HTML."
  :homepage "https://shinmera.com/project/cl-markless"
  :serial T
  :components ((:file "plump"))
  :depends-on (:cl-markless
               :plump-dom))
