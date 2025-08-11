(asdf:defsystem cl-markless-standalone
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Standalone executable of cl-markless"
  :homepage "https://shinmera.com/project/cl-markless"
  :serial T
  :components ((:file "standalone"))
  :depends-on (:cl-markless
               :cl-markless-plump
               :cl-markless-epub
               :cl-markless-markdown
               :cl-markless-latex
               :pathname-utils
               :command-line-arguments)
  :build-operation "program-op"
  :build-pathname #+win32 "cl-markless"
                  #+linux "cl-markless.run"
                  #-(or win32 linux) "cl-markless.o"
  :entry-point "cl-markless-standalone:start")
