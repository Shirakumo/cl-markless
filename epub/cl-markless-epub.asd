(asdf:defsystem cl-markless-epub
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A compiler for Markless to EPUB."
  :homepage "https://shinmera.com/project/cl-markless"
  :serial T
  :components ((:file "package")
               (:file "toolkit")
               (:file "stream")
               (:file "epub"))
  :depends-on (:cl-markless-plump
               :babel
               :uiop
               :trivial-gray-streams
               :trivial-indent
               :trivial-mimes
               :zip))
