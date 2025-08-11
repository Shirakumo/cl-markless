(asdf:defsystem cl-markless-latex
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A compiler for Markless to LaTeX source files."
  :homepage "https://shinmera.com/project/cl-markless"
  :serial T
  :components ((:file "latex"))
  :depends-on (:cl-markless))
