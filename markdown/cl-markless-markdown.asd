#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-markless-markdown
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A compiler for Markdown to Markless"
  :homepage "https://github.com/Shinmera/cl-markless"
  :serial T
  :components ((:file "markdown"))
  :depends-on (:cl-markless
               :3bmd
               :3bmd-ext-code-blocks))
