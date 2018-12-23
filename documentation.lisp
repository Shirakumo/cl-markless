#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package org.shirakumo.markless)

;; color-table.lisp
(docs:define-docs
  (variable *color-table*
    ""))

;; component.lisp
(docs:define-docs
  (variable *instructions*
    "")
  (type component
    "")
  (type unit-component
    "")
  (type text-component
    "")
  (function text
    "")
  (type block-component
    "")
  (type parent-component
    "")
  (function children
    "")
  (type root-component
    "")
  (function labels
    "")
  (function author
    "")
  (function copyright
    "")
  (function label
    "")
  (type paragraph
    "")
  (function indentation
    "")
  (type blockquote-header
    "")
  (type blockquote
    "")
  (function source
    "")
  (type list
    "")
  (type list-item
    "")
  (type ordered-list
    "")
  (type ordered-list-item
    "")
  (function number
    "")
  (type unordered-list
    "")
  (type unordered-list-item
    "")
  (type header
    "")
  (function depth
    "")
  (type horizontal-rule
    "")
  (type code-block
    "")
  (function language
    "")
  (function options
    "")
  (type instruction
    "")
  (type set
    "")
  (function variable
    "")
  (function value
    "")
  (type message-instruction
    "")
  (function message
    "")
  (type info
    "")
  (type warning
    "")
  (type error
    "")
  (type include
    "")
  (function file
    "")
  (type directives-instruction
    "")
  (function directives
    "")
  (type disable
    "")
  (type enable
    "")
  (type comment
    "")
  (type embed
    "")
  (function target
    "")
  (function float
    "")
  (function width
    "")
  (function height
    "")
  (type image
    "")
  (type video
    "")
  (type audio
    "")
  (type footnote
    "")
  (type bold
    "")
  (type italic
    "")
  (type underline
    "")
  (type strikethrough
    "")
  (type code
    "")
  (type subtext
    "")
  (type supertext
    "")
  (type url
    "")
  (type compound
    "")
  (type option
    "")
  (type bold-option
    "")
  (type italic-option
    "")
  (type underline-option
    "")
  (type strikethrough-option
    "")
  (type spoiler-option
    "")
  (type font-option
    "")
  (function font-family
    "")
  (type color-option
    "")
  (function red
    "")
  (function green
    "")
  (function blue
    "")
  (type size-option
    "")
  (function unit
    "")
  (function size
    "")
  (type internal-hyperlink-option
    "")
  (type hyperlink-option
    "")
  (type footnote-reference
    ""))

;; conditions.lisp
(docs:define-docs)

;; directive.lisp
(docs:define-docs
  (function prefix
    "")
  (function begin
    "")
  (function invoke
    "")
  (function end
    "")
  (function consume-prefix
    "")
  (function consume-end
    "")
  (type directive
    "")
  (function enabled-p
    "")
  (function ensure-directive
    "")
  (type root-directive
    "")
  (type block-directive
    "")
  (type singular-line-directive
    "")
  (type inline-directive
    "")
  (type surrounding-inline-directive
    "")
  (type paragraph
    "")
  (type blockquote-header
    "")
  (type blockquote
    "")
  (type unordered-list
    "")
  (type ordered-list
    "")
  (type header
    "")
  (type horizontal-rule
    "")
  (type code-block
    "")
  (type instruction
    "")
  (type comment
    "")
  (type embed
    "")
  (type footnote
    "")
  (type bold
    "")
  (type italic
    "")
  (type underline
    "")
  (type strikethrough
    "")
  (type code
    "")
  (type supertext
    "")
  (type subtext
    "")
  (type compound
    "")
  (variable *style-table*
    "")
  (type footnote-reference
    "")
  (type dash
    "")
  (type newline
    ""))

;; parser.lisp
(docs:define-docs
  (variable *default-directives*
    "")
  (function compile-dispatch-table
    "")
  (function dispatch
    "")
  (type parser
    "")
  (function line-break-mode
    "")
  (function directives
    "")
  (function block-dispatch-table
    "")
  (function inline-dispatch-table
    "")
  (function input
    "")
  (function stack
    "")
  (function stack-push
    "")
  (function stack-pop
    "")
  (function stack-top
    "")
  (function stack-bottom
    "")
  (function root
    "")
  (function directive
    "")
  (function directives-of
    "")
  (function disable
    "")
  (function enable
    "")
  (function evaluate-instruction
    "")
  (function read-full-line
    "")
  (function parse
    "")
  (function stack-unwind
    "")
  (function commit
    "")
  (function read-block
    "")
  (function read-inline
    ""))

;; printer.lisp
(docs:define-docs
  (function output
    "")
  (function define-output
    "")
  (function output-component
    ""))

;; size-table.lisp
(docs:define-docs
  (variable *size-table*
    ""))

;; toolkit.lisp
(docs:define-docs
  (function match!
    "")
  (function read-space-delimited
    "")
  (function split-string
    "")
  (function starts-with
    "")
  (function ends-with
    "")
  (function parse-float
    "")
  (function to-readtable-case
    ""))
