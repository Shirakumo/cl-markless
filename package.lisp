#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:cl-markless-components
  (:nicknames #:org.shirakumo.markless.components)
  (:use) (:import-from #:cl #:defclass)
  (:export
   #:component
   #:unit-component
   #:text-component
   #:text
   #:parent-component
   #:children
   #:root-component
   #:paragraph
   #:blockquote
   #:list
   #:list-item
   #:ordered-list
   #:ordered-list-item
   #:unordered-list
   #:unordered-list-item
   #:header
   #:horizontal-rule
   #:code-block
   #:instruction
   #:message-instruction
   #:message
   #:set
   #:variable
   #:value
   #:message
   #:warning
   #:error
   #:include
   #:file
   #:directives
   #:disable-directives
   #:enable-directives
   #:comment
   #:embed
   #:target
   #:float
   #:width
   #:height
   #:image
   #:video
   #:audio
   #:footnote
   #:bold
   #:italic
   #:underline
   #:strikethrough
   #:code
   #:subtext
   #:supertext
   #:url
   #:compound
   #:options
   #:footnote-reference
   #:target))

(defpackage #:cl-markless-directives
  (:nicknames #:org.shirakumo.markless.directives)
  (:use #:cl)
  (:export
   #:directive
   #:enabled-p
   #:ensure-directive
   #:block-directive
   #:inline-directive
   #:paragraph
   #:dispatch))

(defpackage #:cl-markless
  (:nicknames #:org.shirakumo.markless)
  (:use #:cl)
  (:local-nicknames
   (#:components #:org.shirakumo.markless.components)
   (#:directives #:org.shirakumo.markless.directives))
  (:export))
