#|
 This file is a part of cl-markless
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package org.shirakumo.markless)

;; color-table.lisp
(docs:define-docs
  (variable *color-table*
    "Hash table associating colour names to color-options.

Each entry should be a case-insensitive string as the key and a
CL-MARKLESS-COMPONENTS:COLOR-OPTION as the value. The default table
should include all of the colour names and values defined for
HTML/CSS.

See COLOR-OPTION"))

;; component.lisp
(docs:define-docs
  (variable components:*instructions*
    "List of class names for available instructions.

This list is used by the parser to determine valid instructions. If
you add a new instruction class, you should push its class name onto
this list.

See INSTRUCTION")

  (type components:sized
    "Superclass for classes that represent a size in a particular unit.

See UNIT
See SIZE")
  
  (function components:unit
    "Accesses the unit of the size as a keyword.

The Markless standard defines the following units:
- :EM Applicable for font sizes expressed in relative width.
- :PT Applicable for font sizes expressed in absolute points.
- :PX Applicable for dimensions expressed in absolute pixels.
- :%  Applicable for dimensions expressed in relative percentage to
      its container.

See SIZED")

  (function components:size
    "Accesses the size as a REAL.

This number only makes sense in conjunction with the UNIT.

See SIZED")

  (type components:component
    "Base class for all components that make up the AST of a parse result.")

  (type components:unit-component
    "A component without children that represents a visual part of the document.

See COMPONENT")

  (type components:text-component
    "A component with a text field

See TEXT
See COMPONENT")

  (function components:text
    "Accessor for the text string the component visualises.

See TEXT-COMPONENT")

  (type components:block-component
    "A component that encompasses a block of text.

See COMPONENT")

  (type components:parent-component
    "A component that contains text and subcomponents.

See CHILDREN
See COMPONENT")

  (function components:children
    "Accessor for the vector of child objects.

The vector must be adjustable and have a fill-pointer.

See PARENT-COMPONENT")

  (type components:root-component
    "The base component that makes up a parsed document's AST.

This component also stores cross references and document metadata.

See PARENT-COMPONENT
See LABELS
See AUTHOR
See COPYRIGHT")

  (function components:labels
    "Accesses the label table of the document.

This should return a hash table with EQUALP test. Each key should be
the name of a label, and each value a component with which this label
was associated. This can be used to resolve cross-references, but only
once the document has been fully parsed.

See LABEL
See ROOT-COMPONENT")

  (function components:author
    "Accesses the author metadata of the document.

See ROOT-COMPONENT")

  (function components:copyright
    "Accesses the copyright metadata of the document.

See ROOT-COMPONENT")

  (function components:label
    "Accesses a specific label in the label table of the document.

See LABELS
See ROOT-COMPONENT")

  (type components:paragraph
    "Represents a textual paragraph.

See INDENTATION
See PARENT-COMPONENT
See BLOCK-COMPONENT")

  (function components:indentation
    "Accesses the indentation of the paragraph.

This is retained purely for parsing purposes and has no bearing on the
visual representation in the resulting document.

See PARAGRAPH")

  (type components:blockquote-header
    "Represents the header of a blockquote.

The header should typically not be directly translated into a
resulting component, but instead be represented alongside the
blockquote with which it is associated.

See SOURCE
See PARENT-COMPONENT
See BLOCK-COMPONENT")

  (type components:blockquote
    "Represents a blockquote.

If the blockquote's SOURCE is available, it should be displayed
alongside the blockquote in the resulting document.

See SOURCE
See PARENT-COMPONENT
See BLOCK-COMPONENT")

  (function components:source
    "Accesses the blockquote-header source associated with the blockquote.

See BLOCKQUOTE-HEADER
See BLOCKQUOTE")

  (type components:list
    "Superclass for all list type components.

See PARENT-COMPONENT")

  (type components:list-item
    "Superclass for all list item type components.

See PARENT-COMPONENT")

  (type components:ordered-list
    "Representation of an ordered list.

The child array should only contain ORDERED-LIST-ITEMs.

See LIST
See BLOCK-COMPONENT
See ORDERED-LIST-ITEM")

  (type components:ordered-list-item
    "Representation of an item in an ordered list.

Each item has an associated order number.

See NUMBER
See LIST-ITEM
See BLOCK-COMPONENT")

  (function components:number
    "Accesses the number of the list item as an INTEGER.

See ORDERED-LIST-ITEM")

  (type components:unordered-list
    "Representation of an unordered list.

The child array should only contain UNORDERED-LIST-ITEMs.")

  (type components:unordered-list-item
    "Representation of an item in an unordered list.

See LIST-ITEM
See BLOCK-COMPONENT")

  (type components:header
    "Representation of a section heading.

Each header must have a section depth number.

See DEPTH
See PARENT-COMPONENT
See BLOCK-COMPONENT")

  (function components:depth
    "Accesses the section depth of the header.

This must be a positive integer.

See HEADER")

  (type components:horizontal-rule
    "Representation of a horizontal rule.

See UNIT-COMPONENT
see BLOCK-COMPONENT")

  (type components:code-block
    "Representation of a block of literal text.

See LANGUAGE
See OPTIONS
See TEXT-COMPONENT
See BLOCK-COMPONENT")

  (function components:language
    "Accesses the language of the text in the code-block.

This can be NIL or a string identifying the language.

See CODE-BLOCK")

  (function components:options
    "Accesses the list of options associated with the component.

For a CODE-BLOCK this means a list of opaque options to direct the
representation of the code. For an EMBED this is a list of
EMBED-OPTION instances that dictate visualisation and interaction
options. For a COMPOUND it is a list of COMPOUND-OPTION instances that
convey the various style changes.

See CODE-BLOCK
See EMBED
See COMPOUND")

  (type components:instruction
    "Superclass for all instructions.

See BLOCK-COMPONENT
See *INSTRUCTIONS*")

  (type components:message-instruction
    "Superclass for all instructions that carry a message.

See MESSAGE
See INSTRUCTION")

  (function components:message
    "Accesses the message string of the instruction.

Typically this message should be displayed to the user somehow.

See MESSAGE-INSTRUCTION")

  (type components:directives-instruction
    "Superclass for all instructions that carry a list of directives.

See DIRECTIVES
See INSTRUCTION")

  (function components:directives
    "Accesses the list of directives that this instruction manipulates.

Each entry in the list should be a string representing the name of a
directive.

See DIRECTIVES-INSTRUCTION")

  (type components:set
    "Representation of a SET instruction.

This instruction changes a state variable.

See VARIABLE
See VALUE
See INSTRUCTION")

  (function components:variable
    "Accesses the variable of the set instruction.

This should be a string naming the variable in question.

See SET")

  (function components:value
    "Accesses the value of the set instruction.

This should be a string representing the new value.

See SET")

  (type components:info
    "Represents an INFO instruction.

See MESSAGE-INSTRUCTION")

  (type components:warning
    "Represents a WARNING instruction.

See MESSAGE-INSTRUCTION")

  (type components:error
    "Represents an ERROR instruction.

See MESSAGE-INSTRUCTION")

  (type components:include
    "Represents an INCLUDE instruction

See FILE
See INSTRUCTION")

  (function components:file
    "Accesses the file pathname of the file to be included.

See INCLUDE")

  (type components:disable
    "Represents a DISABLE instruction.

See DIRECTIVES-INSTRUCTION")

  (type components:enable
    "Represents an ENABLE instruction.

See DIRECTIVES-INSTRUCTION")

  (type components:comment
    "Represents a comment.

See TEXT-COMPONENT
See BLOCK-COMPONENT")

  (type components:embed
    "Represents an embed block.

An embed embeds an outside resource into the document. Resolution of
the target is left up to the translator to the resulting document.

See TARGET
See OPTIONS
See UNIT-COMPONENT
See BLOCK-COMPONENT")

  (function components:target
    "Accesses the target of the given object.

The target is a path to an internal or external resource.

See EMBED
See FOOTNOTE
See URL
See LINK-OPTION
See FOOTNOTE-REFERENCE")

  (type components:image
    "Representation of the IMAGE embed type.

See EMBED")

  (type components:video
    "Representation of the VIDEO embed type.

See EMBED")

  (type components:audio
    "Representation of the AUDIO embed type.

See EMBED")

  (type embed-option
    "Superclass for all options for an embed component.")
  
  (type loop-option
    "Represents a loop option.

Causes the embed to loop its content.

See EMBED-OPTION")
  
  (type autoplay-option
    "Represents an autoplay option.

Causes the embed to automatically start playback.

See EMBED-OPTION")
  
  (type width-option
    "Represents a width option.

Causes the embed to restrict its width to the given size.

See EMBED-OPTION
See SIZED")
  
  (type height-option
    "Represents a height option.

Causes the embed to restrict its height to the given size.

See EMBED-OPTION
See SIZED")

  (type components:footnote
    "Representation of a footnote definition.

Footnotes should typically appear at the end of a page or the full
document regardless of their position in the structure.

See TARGET
See PARENT-COMPONENT
See BLOCK-COMPONENT")

  (type components:bold
    "Representation of bold text.

See PARENT-COMPONENT")

  (type components:italic
    "Representation of italic text.

See PARENT-COMPONENT")

  (type components:underline
    "Representation of underlined text.

See PARENT-COMPONENT")

  (type components:strikethrough
    "Representation of struck-through text.

See PARENT-COMPONENT")

  (type components:code
    "Representation of literal text.

See PARENT-COMPONENT")

  (type components:subtext
    "Representation of text sunk below normal text.

See PARENT-COMPONENT")

  (type components:supertext
    "Representation of text raised above normal text.

See PARENT-COMPONENT")

  (type components:url
    "Representation of a literal URL.

See UNIT-COMPONENT
See TARGET")

  (type components:compound
    "Representation of text with a combination of stylistic transforms applied.

See PARENT-COMPONENT
See OPTIONS")

  (type components:compound-option
    "Superclass for all compound options.")

  (type components:bold-option
    "Representation of the bold compound option.

See COMPOUND-OPTION")

  (type components:italic-option
    "Representation of the italic compound option.

See COMPOUND-OPTION")

  (type components:underline-option
    "Representation of the underline compound option.

See COMPOUND-OPTION")

  (type components:strikethrough-option
    "Representation of the strikethrough compound option.

See COMPOUND-OPTION")

  (type components:spoiler-option
    "Representation of the spoiler compound option.

See COMPOUND-OPTION")

  (type components:font-option
    "Representation of the font compound option.

See FONT-FAMILY
See COMPOUND-OPTION")

  (function components:font-family
    "Accesses the font family name of the font option.

See FONT-OPTION")

  (type components:color-option
    "Representation of the color compound option.

See RED
See GREEN
See BLUE
See COMPOUND-OPTION")

  (function components:red
    "Accesses the red component of the color-option.

The value must be an integer in [0,255].

See COLOR-OPTION")

  (function components:green
    "Accesses the green component of the color-option.

The value must be an integer in [0,255].

See COLOR-OPTION")

  (function components:blue
    "Accesses the blue component of the color-option.

The value must be an integer in [0,255].

See COLOR-OPTION")

  (type components:size-option
    "Representation of the size compound option.

See COMPOUND-OPTION
See SIZED")

  (type components:link-option
    "Representation of the link option.

See TARGET
See COMPOUND-OPTION")

  (type components:internal-link-option
    "Representation of an internal link option.

See LINK-OPTION")

  (type components:footnote-reference
    "Representation of a reference to a footnote.

See TARGET
See UNIT-COMPONENT"))

;; conditions.lisp
(docs:define-docs
  (type markless-condition
    "Superclass for all conditions related to markless.

See CL:CONDITION")
  
  (type implementation-condition
    "Superclass for all conditions that indicate an error in the parser implementation.

See MARKLESS-CONDITION")
  
  (type stack-exhausted
    "Error signalled when the directive stack is under- or overflowed.

See IMPLEMENTATION-CONDITION")
  
  (type instruction-evaluation-undefined
    "Error signalled when an instruction is not fully implemented.

See INSTRUCTION
See IMPLEMENTATION-CONDITION")
  
  (function instruction
    "Returns the problematic instruction.

See INSTRUCTION-EVALUATION-UNDEFINED
See UNKNOWN-INSTRUCTION")
  
  (type parser-error
    "Superclass for all conditions that relate to fatal parser errors.

See MARKLESS-CONDITION")
  
  (type parser-warning
    "Superclass for all conditions that relate to recoverable parser errors.

See MARKLESS-CONDITION")
  
  (type deactivation-disallowed
    "Error signalled if an attempt is made to deactivate a directive that cannot be deactivated.

See DIRECTIVE-INSTANCE
See MARKLESS-CONDITION")
  
  (function directive-instance
    "Returns the instance of the directive related to the condition.

See DEACTIVATION-DISALLOWED")
  
  (type unknown-instruction
    "Error signalled if an instruction is encountered that is unknown.

See INSTRUCTION
See PARSER-ERROR")
  
  (type unknown-embed-type
    "Warning signalled if an embed type is encountered that is unknown.

See EMBED-TYPE
See PARSER-WARNING")
  
  (function embed-type
    "Returns the name of the embed type related to the condition.

See UNKNOWN-EMBED-TYPE")
  
  (type bad-option
    "Error signalled if an option is malformed or unknown.

See OPTION
See PARSER-ERROR")
  
  (function option
    "Returns the option string that could not be parsed.

See BAD-OPTION")
  
  (type bad-unit
    "Error signalled if the size contains an unknown unit.

See BAD-OPTION")
  
  (type bad-variable
    "Error signalled if a set instruction refers to an unknown variable.

See VARIABLE-NAME
See PARSER-ERROR")
  
  (function variable-name
    "Returns the name of the variable related to the condition.

See BAD-VARIABLE
See BAD-VALUE")
  
  (type bad-value
    "Error signalled if a set instruction contains an malformed or invalid value for a variable.

See VARIABLE-NAME
See VALUE
See PARSER-ERROR")
  
  (function value
    "Returns the variable value related to the condition.

See BAD-VALUE")
  
  (type user-warning
    "Warning signalled when a warn instruction is encountered.

See MESSAGE
See PARSER-WARNING")
  
  (function message
    "Returns the message the user passed in the warn instruction.

See USER-WARNING
See USER-ERROR")
  
  (type user-error
    "Error signalled when an error instruction is encountered.

See MESSAGE
See PARSER-WARNING"))

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
