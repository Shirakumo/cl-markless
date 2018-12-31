#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:cl-markless-epub
  (:nicknames #:org.shirakumo.markless.epub)
  (:use #:cl #:org.shirakumo.markless)
  (:local-nicknames
   (#:components #:org.shirakumo.markless.components))
  (:shadow #:output)
  (:shadowing-import-from #:org.shirakumo.markless #:debug)
  (:export
   #:output
   #:epub))
