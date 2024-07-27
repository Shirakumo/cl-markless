This repository contains test files for a compliant Markless parser implementation. The tests are written in a format that should be easy to parse, and thus easy to use to verify other implementations as well. Each `.test` file contains a number of test cases with the following syntax:

    TEST-FILE ::= TEST-CASE ("\n" TEST-CASE)*
    TEST-CASE ::= INPUT "~~\n" AST "\n"
    INPUT     ::= line*
    AST       ::= "(" name OPTIONS? (" " CHILD)* ")" | error
    OPTIONS   ::= " (" (option (" " option)*)? ")"
    CHILD     ::= AST | string
    error     --- The string "error", signifying a nerror being signalled during parsing
    name      --- The name of a component or option
    string    --- A double-quote enclosed string with backslash escapes
    option    --- An option specific to a component

If this definition does not make it clear, looking at the test files surely will. The AST is an agnostic representation of a parsed Markless document. It should be possible for you to convert your internal structure to this AST one, or directly compare the AST against your own structure to verify the test. A conforming implementation *must* pass all of these tests, no tests are included about Markless features that are implementation dependant or undefined.
    
Note that Markless allows some ambiguity in the parse results, notably inserting additional "empty" components or splitting strings across multiple string "units" within the component. The ASTs in the tests are compact and do not contain any such empty components or superfluously split strings. You should be able to make your AST match fuzzily to bypass these differences if necessary easily enough.

You may include these tests as part of your implementation for automated testing. To do so, we recommend using a git subtree:

    git subtree add --prefix tests https://github.com/shirakumo/markless-tests main --squash

You can similarly update the tests should there be upstream changes like this:

    git subtree pull --prefix tests https://github.com/shirakumo/markless-tests main --squash
