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
  ;; color-table.lisp
  (:export
   #:*color-table*)
  ;; conditions.lisp
  (:export)
  ;; directive.lisp
  (:export
   #:prefix
   #:begin
   #:invoke
   #:end
   #:consume-prefix
   #:consume-end
   #:directive
   #:enabled-p
   #:ensure-directive
   #:root-directive
   #:block-directive
   #:singular-line-directive
   #:inline-directive
   #:surrounding-inline-directive
   #:paragraph
   #:blockquote-header
   #:blockquote
   #:unordered-list
   #:ordered-list
   #:header
   #:horizontal-rule
   #:code-block
   #:instruction
   #:comment
   #:embed
   #:footnote
   #:bold
   #:italic
   #:underline
   #:strikethrough
   #:code
   #:supertext
   #:subtext
   #:compound
   #:*style-table*
   #:footnote-reference
   #:dash
   #:newline)
  ;; parser.lisp
  (:export
   #:*default-directives*
   #:compile-dispatch-table
   #:dispatch
   #:parser
   #:line-break-mode
   #:directives
   #:block-dispatch-table
   #:inline-dispatch-table
   #:input
   #:stack
   #:stack-push
   #:stack-pop
   #:stack-top
   #:stack-bottom
   #:root
   #:directive
   #:directives-of
   #:disable
   #:enable
   #:evaluate-instruction
   #:read-full-line
   #:parse
   #:stack-unwind
   #:commit
   #:read-block
   #:read-inline)
  ;; printer.lisp
  (:export
   #:output
   #:define-output
   #:output-component)
  ;; size-table.lisp
  (:export
   #:*size-table*)
  ;; toolkit.lisp
  (:export
   #:match!
   #:read-space-delimited
   #:split-string
   #:starts-with
   #:ends-with
   #:parse-float
   #:to-readtable-case))
