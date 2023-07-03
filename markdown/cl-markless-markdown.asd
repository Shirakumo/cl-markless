(asdf:defsystem cl-markless-markdown
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A compiler for Markdown to Markless"
  :homepage "https://github.com/Shinmera/cl-markless"
  :serial T
  :components ((:file "markdown"))
  :depends-on (:cl-markless
               :3bmd
               :3bmd-ext-code-blocks))
