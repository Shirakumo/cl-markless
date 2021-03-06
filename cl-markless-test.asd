#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem cl-markless-test
  :version "1.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A parser implementation for Markless"
  :homepage "https://github.com/Shinmera/cl-markless"
  :serial T
  :components ((:file "tests"))
  :depends-on (:parachute
               :cl-markless)
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :cl-markless-test :compile-all-test-cases)
                         (uiop:symbol-call :parachute :test :cl-markless-test)))
