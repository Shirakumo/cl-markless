#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-markless-plump
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A compiler for Markless to Plump DOM/HTML."
  :homepage "https://github.com/Shinmera/cl-markless"
  :serial T
  :components ((:file "plump"))
  :depends-on (:cl-markless
               :plump-dom))
