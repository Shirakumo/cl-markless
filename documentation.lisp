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

See CL-MARKLESS-COMPONENTS:COLOR-OPTION"))

;; component.lisp
(docs:define-docs
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

  (type components:inline-component
    "A component that encompasses an inline section of text.

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
See LANGUAGE
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
    "Accesses the language.

This can be NIL or a string identifying the language.

See ROOT-COMPONENT
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

See BLOCK-COMPONENT")

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

  (type components:label
    "Represents a LABEL instruction

See TARGET
See INSTRUCTION")

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

  (type components:embed-option
    "Superclass for all options for an embed component.

Every concrete subclass must have the suffix -OPTION.")
  
  (type components:loop-option
    "Represents a loop option.

Causes the embed to loop its content.

See EMBED-OPTION")
  
  (type components:autoplay-option
    "Represents an autoplay option.

Causes the embed to automatically start playback.

See EMBED-OPTION")
  
  (type components:width-option
    "Represents a width option.

Causes the embed to restrict its width to the given size.

See EMBED-OPTION
See SIZED")
  
  (type components:height-option
    "Represents a height option.

Causes the embed to restrict its height to the given size.

See EMBED-OPTION
See SIZED")

  (type components:float-option
    "Represents a float option.

Causes the embed to float within the other blocks.

See DIRECTION
See EMBED-OPTION")

  (function direction
    "The direction in which the float occurs.

Has to be either :LEFT or :RIGHT.

See FLOAT-OPTION")

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
    "Superclass for all compound options.

Every concrete subclass must have the suffix -OPTION.")

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
See UNIT-COMPONENT")

  (type components:en-dash
    "Representation of an en-dash.

See UNIT-COMPONENT")

  (type components:em-dash
    "Representation of an em-dash.

See UNIT-COMPONENT")

  (type components:newline
    "Representation of a line break.

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

  (type parser-condition
    "Superclass for all conditions that relate to the parsing process.

See LINE
See CURSOR
See MARKLESS-CONDITION")

  (function line
    "Returns the line number on which the condition occurred.

The lines are counted from 0.

See PARSER-CONDITION")

  (function cursor
    "Returns the cursor position after which the condition occurred.

The cursor indexes into the line character by character starting from 0.

See PARSER-CONDITION")
  
  (type parser-error
    "Superclass for all conditions that relate to fatal parser errors.

See PARSER-CONDITION")
  
  (type parser-warning
    "Superclass for all conditions that relate to recoverable parser errors.

See PARSER-CONDITION")
  
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
    "Warning signalled if an option is malformed or unknown.

See OPTION
See PARSER-ERROR")
  
  (function option
    "Returns the option string that could not be parsed.

See BAD-OPTION")
  
  (type bad-unit
    "Warning signalled if the size contains an unknown unit.

See BAD-OPTION")

  (type option-disallowed
    "Warning signalled if an option is attempted to be used for an embed that does not allow it.

See EMBED-TYPE
See BAD-OPTION")

  (function embed-type
    "Returns the embed-type that caused the error.

See OPTION-DISALLOWED")
  
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
    "Returns the unique prefix of the directive used to identify its start.

Every directive must provide a method for this function.

The prefix is a vector of strings, where each string denotes a set of
possible characters that can appear at that position in the prefix.

For instance, the following prefix
 #(\"ab\" \"c\")
Matches the following strings
 ac
 bc
 bca
but not the following
 ab
 cc
 abc

See COMPILE-DISPATCH-TABLE
See DIRECTIVE")

  (function begin
    "Function called to match the beginning of a directive.

Every directive must provide a method for this function.

This function is called if the PREFIX of the directive has been
matched. This function should then consume the remainder of the prefix
\(if any) and return the new position of the cursor. The method is
responsible for putting the directive and its component onto the stack
if an actual match is found. This can be done via COMMIT.

On an invalid match, the directive should delegate parsing to another
directive, or in the very least advance the cursor somehow as
otherwise the parser will enter an infinite loop of invoking BEGIN on
the same directive after matching its prefix.

See COMMIT
See DIRECTIVE
See READ-BLOCK
See READ-INLINE")

  (function invoke
    "Function called to invoke parsing of a directive after a successful match.

Every directive must provide a method for this function.

Invoke is called to cause the directive to process its content. It
should return the new cursor position. Invoke will be repeatedly
called on the current directive until the end of the line is reached.

See PROCESS-STACK
See DIRECTIVE")

  (function end
    "Function called when a directive is popped off the stack due to an unsuccessful match.

A method can be added to this to allow the directive to perform some
undoing. This is typically necessary for inline directives as they
might have consumed a prefix that is now invalid due to the incomplete
match.

See PROCESS-STACK
See DIRECTIVE")

  (function consume-prefix
    "Function called to consume the prefix of a directive on the next
    line.

Every block-directive must provide a method for this function.

This function is called on subsequent lines after the directive has
been dispatched and BEGIN has been called. The directive should match
the correct prefix for subsequent lines and return the updated
cursor on a successful match, or NIL if the match has failed.

See PROCESS-STACK
See BLOCK-DIRECTIVE")

  (function consume-end
    "Function called to consume the end of an inline directive.

Every inline-directive must provide a method for this function.

This function is called by READ-INLINE if the supplied end-char has
been encountered. CONSUME-END should then return the updated cursor
position on a successful end match, or NIL on an unsuccessful one.

If it returns successfully, the directive and its component will
automatically be popped off the stack.

See READ-INLINE
See INLINE-DIRECTIVE")

  (type directive
    "Superclass for all directives.

All matching for syntax in Markless is performed by what are called
directives. They control the parsing and compiling behaviour.

A directive will only match if it is currently enabled.

See ENABLED-P")

  (function enabled-p
    "Accesses whether the directive is currently enabled or not.

Some directives may not be disabled, in which case an error of type
DEACTIVATION-DISALLOWED is signalled if an attempt is made to disable
it.

See DIRECTIVE
See DEACTIVATION-DISALLOWED")

  (function ensure-directive
    "Resolves the directive-ish to a directive if possible.

The argument can be a DIRECTIVE instance or a symbol naming the class
of one. In the latter case, a new instance of the named class is
returned if the class is a subclass of DIRECTIVE. Otherwise, an error
is signalled.

See DIRECTIVE")

  (type root-directive
    "Represents the root parser entry.

The root directive is always at the bottom of the stack. It is
responsible for matching and invoking the block directives at the top
level of the document.

The root directive cannot be disabled.

See DIRECTIVE")

  (type block-directive
    "Superclass for all block directives.

Provides default methods for END and INVOKE.

See END
See INVOKE
See DIRECTIVE")

  (type singular-line-directive
    "Superclass for all single-line block directives.

Provides default methods for CONSUME-PREFIX and INVOKE.

See END
See INVOKE
See BLOCK-DIRECTIVE")

  (type inline-directive
    "Superclass for all inline directives.

Provides default methods for CONSUME-PREFIX, END, and INVOKE.

See CONSUME-PREFIX
See END
See INVOKE
See DIRECTIVE")

  (type surrounding-inline-directive
    "Superclass for select surrounding inline directives.

Provides a method for BEGIN that automatically pops the stack and
advances the cursor if the current directive at the top of the stack
is the same as this directive. This is useful for surrounding inline
directives whose postfix is the same as their prefix.

See BEGIN
See INLINE-DIRECTIVE")

  (type paragraph
    "The directive for a paragraph.

This is a very important directive as it acts as the fallback in
pretty much every situation and has the most complex parser logic
associated with it.

See CL-MARKLESS-COMPONENTS:PARAGRAPH
See BLOCK-DIRECTIVE")

  (type blockquote-header
    "The directive for the header of a blockquote.

See CL-MARKLESS-COMPONENTS:BLOCKQUOTE-HEADER
See SINGULAR-LINE-DIRECTIVE")

  (type blockquote
    "The directive for a blockquote.

See CL-MARKLESS-COMPONENTS:BLOCKQUOTE
See BLOCK-DIRECTIVE")

  (type unordered-list
    "The directive for an unordered list and its items.

See CL-MARKLESS-COMPONENTS:UNORDERED-LIST
See CL-MARKLESS-COMPONENTS:UNORDERED-LIST-ITEM
See BLOCK-DIRECTIVE")

  (type ordered-list
    "The directive for an ordered list and its items.

See CL-MARKLESS-COMPONENTS:ORDERED-LIST
See CL-MARKLESS-COMPONENTS:ORDERED-LIST-ITEM
See BLOCK-DIRECTIVE")

  (type header
    "The directive for a section header.

See CL-MARKLESS-COMPONENTS:HEADER
See SINGULAR-LINE-DIRECTIVE")

  (type horizontal-rule
    "The directive for a horizontal rule.

See CL-MARKLESS-COMPONENTS:HORIZONTAL-RULE
See SINGULAR-LINE-DIRECTIVE")

  (type code-block
    "The directive for a code block.

See CL-MARKLESS-COMPONENTS:CODE-BLOCK
See BLOCK-DIRECTIVE")

  (type instruction
    "The directive for an instruction.

The parsing of the actual type of directive is handled by
PARSE-INSTRUCTION. The instruction is parsed fully on BEGIN and then
evaluated on INVOKE.

See PARSE-INSTRUCTION
See EVALUATE-INSTRUCTION
See CL-MARKLESS-COMPONENTS:INSTRUCTION
See SINGULAR-LINE-DIRECTIVE")

  (function parse-instruction
    "Parses an instruction line to the proper instruction instance.

This may signal errors if the parsing is unsuccessful.
Each method should be specialised on the class of the instruction to
parse and should return a fresh instance of that class on a successful
parse. The argument is only a prototype of that class and should not
be used for anything except for the type information and dispatch.

See INSTRUCTION")

  (type comment
    "The directive for a comment.

See CL-MARKLESS-COMPONENTS:COMMENT
See SINGULAR-LINE-DIRECTIVE")

  (type embed
    "The directive for an embed.

The parsing of the embed options is handled by PARSE-EMBED-OPTION.
Whether an option is permitted for a given embed type is determined
by EMBED-OPTION-ALLOWED-P. If NIL is returned for any one option,
an error of type OPTION-DISALLOWED is signalled.

See PARSE-EMBED-OPTION
See EMBED-OPTION-ALLOWED-P
See OPTION-DISALLOWED
See CL-MARKLESS-COMPONENTS:EMBED
See SINGULAR-LINE-DIRECTIVE")

  (function parse-embed-option
    "Parses the given option string into an option if possible.

Takes the first space-delimited token of the string and appends
\"-option\" to it to form the potential name of the option type. It
then searches for a symbol (in the proper case) of that name in the
CL-MARKLESS-COMPONENTS package. If found, and if the symbol names a
subtype of EMBED-OPTION, PARSE-EMBED-OPTION-TYPE is called with a
class prototype of the name. Otherwise an error of type BAD-OPTION is
signalled.

See PARSE-EMBED-OPTION-TYPE
See CL-MARKLESS-COMPONENTS:EMBED-OPTION")

  (function parse-embed-option-type
    "Parses an embed option to the proper option instance.

This may signal errors if the parsing is unsuccessful.
Each method should be specialised on the class of the embed option to
parse and should return a fresh instance of that class on a successful
parse. The argument is only a prototype of that class and should not
be used for anything except for the type information and dispatch.

By default this function simply returns a fresh instance of the class
with no initargs passed.

See EMBED")

  (function embed-option-allowed-p
    "Returns T if the combination of option and embed type are allowed.

The user should add appropriate methods to this function when new
embed types or options are added. Such methods should simply return T,
as the default method will always return NIL.

See CL-MARKLESS-COMPONENTS:EMBED-OPTION
See CL-MARKLESS-COMPONENTS:EMBED")

  (type footnote
    "The directive for a footnote.

See CL-MARKLESS-COMPONENTS:FOOTNOTE
See SINGULAR-LINE-DIRECTIVE")

  (type bold
    "The directive for a bold markup.

See CL-MARKLESS-COMPONENTS:BOLD
See SURROUNDING-INLINE-DIRECTIVE")

  (type italic
    "The directive for an italic markup.

See CL-MARKLESS-COMPONENTS:ITALIC
See SURROUNDING-INLINE-DIRECTIVE")

  (type underline
    "The directive for an underline markup.

See CL-MARKLESS-COMPONENTS:UNDERLINE
See SURROUNDING-INLINE-DIRECTIVE")

  (type strikethrough
    "The directive for a strikethrough markup.

See CL-MARKLESS-COMPONENTS:STRIKETHROUGH
See INLINE-DIRECTIVE")

  (type code
    "The directive for a code markup.

See CL-MARKLESS-COMPONENTS:CODE
See INLINE-DIRECTIVE")

  (type supertext
    "The directive for a supertext markup.

See CL-MARKLESS-COMPONENTS:SUPERTEXT
See INLINE-DIRECTIVE")

  (type subtext
    "The directive for a subtext markup.

See CL-MARKLESS-COMPONENTS:SUBTEXT
See INLINE-DIRECTIVE")

  (type compound
    "The directive for a compound markup.

The parsing of the compound options is handled by
PARSE-COMPOUND-OPTION.

See CL-MARKLESS-COMPONENTS:COMPOUND
See INLINE-DIRECTIVE")

  (function parse-compound-option
    "Parses the given option string into an option if possible.

The following tests are performed:
- If the option is found in *COLOR-TABLE*, its entry is returned.
- If the option is found in *SIZE-TABLE*, its entry is returned.
- If the option starts with a # an INTERNAL-LINK-OPTION is returned.
- If the option matches the URL directive, a LINK-OPTION is returned.
- Otherwise the function proceeds as follows:

Takes the first space-delimited token of the string and appends
\"-option\" to it to form the potential name of the option type. It
then searches for a symbol (in the proper case) of that name in the
CL-MARKLESS-COMPONENTS package. If found, and if the symbol names a
subtype of COMPOUND-OPTION, PARSE-COMPOUND-OPTION-TYPE is called with
a class prototype of the name. Otherwise an error of type BAD-OPTION
is signalled.

See *COLOR-TABLE*
See *SIZE-TABLE*
See PARSE-COMPOUND-OPTION-TYPE
See CL-MARKLESS-COMPONENTS:COMPOUND-OPTION")

  (function parse-compound-option-type
    "Parses a compound option to the proper option instance.

This may signal errors if the parsing is unsuccessful.
Each method should be specialised on the class of the compound option
to parse and should return a fresh instance of that class on a
successful parse. The argument is only a prototype of that class and
should not be used for anything except for the type information and
dispatch. 

By default this function simply returns a fresh instance of the class
with no initargs passed.

See COMPOUND")

  (type footnote-reference
    "The directive for a footnote reference.

See CL-MARKLESS-COMPONENTS:FOOTNOTE-REFERENCE
See INLINE-DIRECTIVE")

  (type url
    "The directive for an inline URL.

The handling of the URL directive is a bit special due to the lack of
a dedicated prefix that can be uniquely matched to initiate the scan
of an URL. Thus, special handling code in READ-INLINE is present to
make this case at least somewhat efficient.

This directive will, unlike all others, return the same cursor on
BEGIN if the complete URL does not match.

See CL-MARKLESS-COMPONENTS:URL
See INLINE-DIRECTIVE")

  (type dash
    "The directive for a dash.

See CL-MARKLESS-COMPONENTS:DASH
See INLINE-DIRECTIVE")

  (type newline
    "The directive for an explicit newline.

See CL-MARKLESS-COMPONENTS:NEWLINE
See INLINE-DIRECTIVE"))

;; parser.lisp
(docs:define-docs
  (variable *default-directives*
    "This variable contains the list of by-default available directives.

This list should only ever contain names of directives, not directive
instances.

See PARSER")

  (function compile-dispatch-table
    "Compiles the given list of directives into a dispatch table.

This table forms a tree of tables with either NIL or a directive as
the leaves. A directive is only ever a leaf if its full prefix has
been matched by traversing the tree of tables. The tree is built using
the PREFIX from the directives.

See DIRECTIVE
See PREFIX
See DISPATCH")

  (function dispatch
    "Dispatches to a directive using the given dispatch table.

Returns the matched directive if a full prefix match has been found
and the directive is enabled.

See ENABLED-P")

  (type stack-entry
    "Representation of an entry on the parse stack.

See STACK-ENTRY-COMPONENT
See STACK-ENTRY-DIRECTIVE
See STACK")

  (function stack-entry-component
    "Accesses the stack entry's component.

It is NOT safe to read this value for stack entries that are outside
the current fill-pointer of the stack.

See STACK-ENTRY")

  (function stack-entry-directive
    "Accesses the stack entry's directive.

It is NOT safe to read this value for stack entries that are outside
the current fill-pointer of the stack.

See STACK-ENTRY")

  (type parser
    "Representation of the parsing state necessary to parse a Markless
    document.

A parser instance is required in order to parse a document. While it
is permitted to re-use a parser instance for subsequent parsing, it is
/NOT/ allowed to use the same parser concurrently. Concurrent parsing
or access of the parser will lead to undefined consequences.

When constructing the parser you may pass a list of directives the
parser should support via the :DIRECTIVES argument. You can also
by-default disable some of those directives by putting their names
into the :DISABLED-DIRECTIVES argument. Note that disabling a
directive is not the same as leaving it out of the :DIRECTIVES list
completely. If it is only disabled it can be enabled via the ENABLE
instruction during parsing. The :STACK-SIZE-LIMIT argument sets the
maximal nesting permitted when parsing. If directives are nested more
deeply than this, parsing of the document will fail. The default is
set to 64.

Note that even though subsequent re-use of the parser is permitted,
changes carried out by an INSTRUCTION during a parse will not be reset
on a subsequent parse, so instructions can poison subsequent
parses. If the INSTRUCTION directive is excluded, subsequent parses
should be clean however. 

See LINE-BREAK-MODE
See DIRECTIVES
See DIRECTIVE
See ENABLE
See DISABLE
See EVALUATE-INSTRUCTION
See BLOCK-DISPATCH-TABLE
See INLINE-DISPATCH-TABLE
See INPUT
See STACK
See PARSE
See COMMIT
See ROOT")

  (function line-break-mode
    "Accesses the line break mode currently active in the parser.

The value must be either :SHOW or :HIDE.

See PARSER")

  (function directives
    "Accesses the list of directive instances the parser supports.

It is NOT safe to modify or set this list after the parser has been
constructed.

See PARSER
See DIRECTIVE")

  (function block-dispatch-table
    "Accesses the dispatch table for block directives.

It is NOT safe to modify or set this table after the parser has been
constructed.

See PARSER
See DISPATCH")

  (function inline-dispatch-table
    "Accesses the dispatch table for inline directives.

It is NOT safe to modify or set this table after the parser has been
constructed.

See PARSER
See DISPATCH")

  (function input
    "Accesses the current input stream from which lines are parsed.

See PARSER")

  (function stack
    "Accesses the stack of components and directives of the parser.

It is NOT safe to modify or set this stack after the parser has been
constructed. This stack must be a vector with element-type STACK-ENTRY
and a fill-pointer.

See PARSER
See STACK-ENTRY
See STACK-PUSH
See STACK-POP
See STACK-TOP
See STACK-BOTTOM")

  (function stack-push
    "Pushes the given directive and component onto the stack.

Returns the top STACK-ENTRY.

See STACK-ENTRY")

  (function stack-pop
    "Pops the top directive and component off the stack.

Returns the popped STACK-ENTRY.

See STACK-ENTRY")

  (function stack-top
    "Returns the stack entry at the top of the stack.

See STACK-ENTRY")

  (function stack-bottom
    "Returns the stack entry at the bottom of the stack.

This must always be an entry with a ROOT-DIRECTIVE and ROOT-COMPONENT
as its contents.

See STACK-ENTRY")

  (function stack-unwind
    "Unwinds the stack to the given index.

Makes sure to call END on all the directives that are popped off the
stack.

See END
See STACK-POP
See PARSER")

  (function root
    "Returns the ROOT-COMPONENT of the current parse operation.

See ROOT
See PARSER")

  (function directive
    "Returns the directive in the parser with the given name, if any.

The name can be either a symbol or a string.

See PARSER
See DIRECTIVE")

  (function directives-of
    "Returns all directives of a given type in the parser.

See PARSER
See DIRECTIVE")

  (function disable
    "Disables all directives in the parser that pass the test function.

See PARSER
See DIRECTIVE
See ENABLED-P")

  (function enable
    "Enables all directives in the parser that pass the test function.

See PARSER
See DIRECTIVE
See ENABLED-P")

  (function evaluate-instruction
    "Evaluates the given instruction, carrying out the changes in parser state.

See INSTRUCTION
See CL-MARKLESS-COMPONENTS:INSTRUCTION
See PARSER")

  (function read-full-line
    "Reads a full line of text from the stream, respecting Markless' line escape syntax.

This means the following
  foo\\
  bar\\
  baz
will be read as
  foobarbaz
")

  (function parse
    "Parses the given input as a Markless document.

If the parser argument is T, a fresh PARSER instance is constructed to
perform the parsing.

The THING may be a PATHNAME, STRING, or a STREAM.

Returns a ROOT-COMPONENT on a successful parse.

Note that the parser is not incremental and will consume the stream
until EOF is encountered.

See CL-MARKLESS-COMPONENTS:ROOT-COMPONENT
See PARSER")

  (function commit
    "Commits the given directive and component.

This pushes the two onto the stack and appends the component to the
child array of the component that was previously at the top of the
stack.

See DIRECTIVE
See CL-MARKLESS-COMPONENTS:COMPONENT
See PARSER")

  (function read-block
    "Attempts to match a block directive.

If the cursor is not already at the end of the line, this will
dispatch a block directive and call BEGIN on it. It will return the
resulting cursor.

See PARSER
See BEGIN
See DISPATCH
See BLOCK-DISPATCH-TABLE")

  (function read-url
    "Attempts to match a URL.

If the match succeeds, a new cursor for the end of the URL is returned
and NIL otherwise.")

  (function read-inline
    "Attempts to match inline content.

This constructs a string of content from the line and appends it to
the current component at the top of the stack until an inline
directive is matched or until the end-char is reached. If an inline
directive is matched, BEGIN is called on it and the result thereof is
returned. If the end-char is matched, CONSUME-END is called on the
directive at the top of the stack. If this returns non-NIL, that value
is returned from READ-INLINE after popping the stack.

See PARSER
See BEGIN
See CONSUME-END
See STACK-POP
See STACK-TOP
See DISPATCH
See INLINE-DISPATCH-TABLE"))

;; printer.lisp
(docs:define-docs
  (function output
    "Outputs the given component to the appropriate stream in the requested format.

If the target is T, it is substituted for *STANDARD-OUTPUT*. If the
target is NIL, it is substituted for a STRING-OUTPUT-STREAM whose
contents are returned in the end. If the target is a PATHNAME, the
denoted file is opened and the target is substituted for a stream to
that file. Otherwise, the target is passed on to OUTPUT-COMPONENT
directly.

If the component is not an instance of COMPONENT, it is replaced by
the return value of calling PARSE on the passed component argument.

By default the MARKLESS and DEBUG outputs are provided.

See PARSE
See OUTPUT-COMPONENT")

  (function define-output
    "Defines a new output format.

This is a shorthand macro that defines methods for
OUTPUT-COMPONENT specialised on streams. Each entry in the body must
follow this structure:

  CLASS QUALIFIERS . BODY

Which will be translated to the appropriate method. Within the body
the following two convenience functions are automatically bound:

- (OUTPUT c)
  Calls OUTPUT-COMPONENT on c with the appropriate extra arguments.
- (OUTPUT-CHILDREN)
  Calls OUTPUT-COMPONENT on each of the children of the current
  component.

See OUTPUT-COMPONENT")

  (type output-format
    "Superclass for all output formats.

If you define a new format, you should define a new subclass to this
and specialise on your new class in your OUTPUT-COMPONENT methods.

See OUTPUT-COMPONENT")

  (function list-output-formats
    "Returns a list of all known output formats.

The list is composed of class names for OUTPUT-FORMAT classes.

See OUTPUT-FORMAT")

  (type markless
    "Output format that prints to valid Markless again.

This should allow you to construct arbitrary component ASTs and
generate valid Markless documents.

See OUTPUT")

  (type debug
    "Output format for debugging and AST visualisation.

This prints all AST contents in an easy to read form, useful for
debugging.")

  (function output-component
    "This function is responsible for formatting the given component to the target in the requested format.

The user should add methods to this function as appropriate, but must
always EQL-specialise on the format argument with a unique symbol to
distinguish their format from others.

Note that the target must not necessarily be a stream unless the
method specialises it to be one.

See OUTPUT"))

;; size-table.lisp
(docs:define-docs
  (variable *size-table*
    "Hash table associating size names to size-options.

Each entry should be a case-insensitive string as the key and a
CL-MARKLESS-COMPONENTS:SIZE-OPTION as the value. The default table
should include all of the size names and values defined by the
Markless standard.

See CL-MARKLESS-COMPONENTS:SIZE-OPTION"))

;; toolkit.lisp
(docs:define-docs
  (function decompose-rgb
    "Decomposes the 24-bit hexadecimal number into three 8-bit numbers.

Returns the RGB channels as a list.")
  
  (function match!
    "Attempts to match the prefix.

Returns the new cursor on success, and NIL on failure.")

  (function read-delimited
    "Reads the next character delimited token.

Returns the read token and the new cursor position as multiple values.

This does properly handle backslash escapes.")

  (function split-string
    "Splits the string by the given split character.

Returns a list of split parts with empty parts removed.

This does NOT handle backslash escapes.")

  (function split-options
    "Splits the given options string into a list of options.

Options are separated via commas. Returns a list of the matched
options and the new cursor position as multiple values.

This does properly handle backslash escaping.")

  (function starts-with
    "Returns true if the given string starts with the given beginning.")

  (function ends-with
    "Returns true if the given string ends with the given ending.")

  (function parse-float
    "Parses the given string as a floating point number.")

  (function parse-unit
    "Parses the given string as a number with a unit at the end.

The following unit types are supported:
- em
- pt
- px
- %
Any other unit, or a missing unit, will signal a warning of type
BAD-UNIT and returns NIL as the unit.

Returns the size and its unit as multiple values.")

  (function to-readtable-case
    "Turns the given string into the case as appropriate for the requested readtable case.

This acts as if the string was parsed into a symbol token by READ but
without actually interning or invoking any of the heavy READ
machinery. This is useful in order to translate names for symbols into
the appropriate case for lookups.")

  (function condense-children
    "Returns a new, condensed child array.

This condenses the child array as follows:
- Consecutive strings are concatenated together
- INSTRUCTION and COMMENT components are filtered out
- PARENT-COMPONENTs that are not subclasses thereof have their
  contents spliced into the new child array.

This does /not/ recursively condense.")

  (function condense-component-tree
    "Condenses the given component destructively and recursively.

See CONDENSE-CHILDREN"))
