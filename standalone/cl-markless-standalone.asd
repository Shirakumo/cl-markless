(asdf:defsystem cl-markless-standalone
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Standalone executable of cl-markless"
  :homepage "https://github.com/Shinmera/cl-markless"
  :serial T
  :components ((:file "standalone"))
  :depends-on (:cl-markless
               :cl-markless-plump
               :cl-markless-epub
               :cl-markless-markdown
               :command-line-arguments)
  :build-operation "program-op"
  :build-pathname "cl-markless"
  :entry-point "cl-markless-standalone:start")
