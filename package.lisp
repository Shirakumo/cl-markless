#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:cl-markless-components
  (:nicknames #:org.shirakumo.markless.components)
  (:use #:cl)
  (:shadow #:list #:number #:set #:variable #:warning #:error #:float #:labels)
  (:export
   #:*instructions*
   #:component
   #:unit-component
   #:text-component
   #:text
   #:block-component
   #:parent-component
   #:children
   #:enter
   #:root-component
   #:labels
   #:author
   #:copyright
   #:label
   #:paragraph
   #:indentation
   #:blockquote-header
   #:blockquote
   #:source
   #:list
   #:list-item
   #:ordered-list
   #:ordered-list-item
   #:number
   #:unordered-list
   #:unordered-list-item
   #:header
   #:depth
   #:horizontal-rule
   #:code-block
   #:language
   #:options
   #:instruction
   #:message-instruction
   #:info
   #:set
   #:variable
   #:value
   #:message
   #:warning
   #:error
   #:include
   #:file
   #:directives-instruction
   #:directives
   #:disable
   #:enable
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
   #:option
   #:bold-option
   #:italic-option
   #:underline-option
   #:strikethrough-option
   #:spoiler-option
   #:font-option
   #:font-family
   #:color-option
   #:red
   #:green
   #:blue
   #:size-option
   #:unit
   #:size
   #:internal-hyperlink-option
   #:hyperlink-option
   #:target
   #:footnote-reference
   #:target))

(defpackage #:cl-markless
  (:nicknames #:org.shirakumo.markless)
  (:use #:cl)
  (:local-nicknames
   (#:components #:org.shirakumo.markless.components))
  (:export))
