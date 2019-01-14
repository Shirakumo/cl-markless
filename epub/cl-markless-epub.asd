#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-markless-epub
  :version "1.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A compiler for Markless to EPUB."
  :homepage "https://github.com/Shinmera/cl-markless"
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
