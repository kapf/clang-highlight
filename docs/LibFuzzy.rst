========
LibFuzzy
========

LibFuzzy is a library for heuristically parsing C++ based on Clang's Lexer.
The fuzzy parser is fault-tolerant, works without knowledge of the build system
and on incomplete source files.  As the parser necessarily makes guesses, the
resulting syntax tree may be partially wrong.

This documents describes the LibFuzzy design and interface.

When to use LibFuzzy
--------------------

Use LibFuzzy when you ...:

* need fault-tolerant AST information
* need classification of tokens, but not more
* don't want setup overhead for your tool
* want fast results from a small input

Do not use LibFuzzy when you ...:

* need 100% accuracy
* need the context informations that a full Clang AST provides

Look at the different options for
`Tooling http://clang.llvm.org/docs/Tooling.html` if you are interested in
non-fuzzy approaches.

The Fuzzy AST
-------------

The fuzzy AST is defined in ``Fuzzy/FuzzyAST.h``.  It is designed to be as
similar as possible to the
`Clang AST http://clang.llvm.org/docs/IntroductionToTheClangAST.html`, but
differs because of some design decisions:

* Each AST node contains references to all tokens that belong to it.  This
  implies that by visiting all nodes of the AST of a particular source code, you
  find all the tokens lexed from that code.

  This has led to some hierarchy changes. E.g. ``Expr`` isn't derived from
  ``Stmt`` because as a statement ``Expr`` needs a trailing semicolon, but
  otherwise it doesn't.  Therefore ``ExprLineStmt`` exists to make an ``Expr``
  into a ``Stmt`` and keep track of the semicolon.

* After parsing, each token of the input stream has a reference to the AST node
  that contains it.

  That's why a common base class for all AST nodes exists: ``ASTElement``.  The
  Clang AST doesn't have that.

* The fuzzy parser doesn't go much deeper than classification of tokens.

  There's no canonicalization of qualified identifiers.  Types don't contain a
  reference to the type definition and can't be compared.

How to use the Fuzzy AST
------------------------

The main to call the fuzzy parser is ``fuzzyparse`` which takes a range of
AnnotateToken as input.

.. code-block:: c++

    TranslationUnit fuzzyparse(AnnotatedToken *first, AnnotatedToken *last);

``AnnotatedToken`` is a Clang Lexer token combined with a reference where
in the fuzzy AST it is located.

.. code-block:: c++

    class AnnotatedToken {
      clang::Token Tok_;
      ASTElement *Annot;
      ...
    };

The Clang Tokens can be obtained by the Clang Lexer in raw mode.  The source
code of :program:`clang-highlight` contains sample usage.

Current state
-------------

The fuzzy parser can be tested with :program:`clang-highlight` and the
``-dump-ast`` option.

.. code-block:: bash

    $ cat sample01.cpp
    if (<unparsable>) {
      f(1+1);
    }
    $ clang-highlight -dump-ast sample01.cpp
    If
        Condition
            Unparsable Block:
                <
                unparsable
                >
        Body:
            CompoundStmt:
                ExprLineStmt
                    call expr 'f'
                            1
                        plus
                            1

The parser recognizes the if statement but is unable to parse the condition.
Every unparsable range of source code is put into a ``UnparsableBlock`` which
itself is a subclass of ``ASTElement``.  The fuzzy parser is successfully able
to recover from this error.

C++ does not have a context free grammar.  If in doubt, a fuzzy parser has to
make guesses which may or may not be right.

.. code-block:: bash

    $ cat sample02.cpp
    auto ps = std::make_unique<std::string>();
    std::array<int, 5> a;
    const int SIZE=5;
    std::array<int, SIZE> b;
    $ clang-highlight -dump-ast sample02.cpp
    DeclStmt
        VarDecl 'ps'
            Type 'auto'
            Assignment Type '='
            call expr 'std::make_unique
                <
                    Type 'std::string'
                >'
    DeclStmt
        VarDecl 'a'
            Type 'std::array
                <
                    Type 'int'
                    5
                >'
    DeclStmt
        VarDecl 'SIZE'
            Type 'constint'
            Assignment Type '='
            5
    DeclStmt
        VarDecl 'b'
            Type 'std::array
                <
                    Type 'int'
                    Type 'SIZE'
                >'

There are a number of guesses that need to be made in this code.   Most
importantly:

* Is ``std::make_unique`` a function or a type?
* Is ``std::string`` a constant or a type?
* Is ``SIZE`` a constant or a type?

The first two questions cannot be decided without further context.  The current
strategy is simple:  If something looks like a function call, then it's a
function and not a constructor.  If a template argument is either a type or a
constant, then it's a type.

This strategy may be wrong.  Give that ``SIZE`` is declared inside this code
snippet, it's very certain to assume that ``SIZE`` is a constant.  However, the
fuzzy parser currently does not include context information from the part he
already has parsed.

.. code-block:: bash

    $ cat sample03.cpp
    #if __cplusplus <= 199711L // C++03 or older
      std::tr1::auto_ptr<int> p;
    #else // C++11
      std::unique_ptr<int> p;
    #endif
    $ clang-highlight -dump-ast sample03.cpp
    Preprocessor 'if':
            DeclRefExpr '__cplusplus'
        lessequal
            199711L
    Preprocessor 'else':
    Preprocessor 'endif':
    DeclStmt
        VarDecl 'p'
            Type 'std::tr1::auto_ptr
                <
                    Type 'int'
                >'
    DeclStmt
        VarDecl 'p'
            Type 'std::unique_ptr
                <
                    Type 'int'
                >'

This illustrates why the Clang Parser isn't easily usable for highlighting even
if the code is perfectly fine.  There is no good solution to parse all
preprocessor branches.  If a program depends, say, on 10 macros (``__linux__``,
``__cplusplus``, ``sizeof int``, etc.) then there are 2^10=1024 compilation
passes needed to get all possible results -- which may even lead to different
ASTs in the same places.  If a compiler ignores the conditions the code may
contain syntax errors.  The easiest solution would be to make only one pass and
gray the unused code paths out.

The fuzzy parser parses all preprocessor statements in one pass and the code
without them in another.  Because its fuzziness, this should go reasonably well.

What next
---------

* Add all syntax elements of C++:  Currently, only the most used subset of C++
  is implemented.

* Improve the fuzziness.  Add more sophisticated algorithms to handle unbalanced
  parentheses for example.

* Use context information.  The parser could make use of a symbol table based on
  the code it has seen already.

* Language support for C and Objective C:  Even though these languages share a
  lot of their syntax with C++, they have subtle differences.  It shouldn't be
  hard to add those to the parser.

* Optimize for speed:  Add a memory manager for the AST and improve the parser.
  There hasn't been much focus on speed yet.

* Conversion between Clang's AST and the fuzzy AST.  It there is a way to
  produce a Clang AST, why not make use of it for tools that use the Fuzzy AST?
