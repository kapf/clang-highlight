===============
Clang-Highlight
===============

:program:`clang-highlight` is a syntax highlighting tool for C++ based on a
:doc:`LibFuzzy` framework.

Using clang-highlight
=====================

:program:`clang-highlight` accepts exactly one argument, the file you want to
highlight.

.. code-block:: bash

  $ clang-highlight test.cpp

Or if you want to see it through a pager:

.. code-block:: bash

  $ clang-highlight test.cpp | less -R

Pass ``-`` as file name if you want to highlight the standard input.

Output formats
--------------

.. option:: -stdout

   The default output format.  Uses console colors.

.. option:: -html

   Writes HTML as output with hardcoded colors.  The individual tokens have the
   form ``<span style="color:green">int</span>``.

.. option:: -shtml

   Writes Semantic HTML as output with classes.  The individual tokens have the
   form ``<span class="ch-typename">int</span>``.  The class can be specified in
   a separate style sheet by the user.

Further options
---------------

.. option:: -identifiers-only

   Per default, the star ``*`` in ``type *i;`` is classified as part of the type
   name as is ``<`` and ``>`` in ``unique_ptr<T>``.  To disable this feature,
   use the ``-identifiers-only`` option.

.. option:: -dump-ast

   Only included for testing the fuzzy parser, will be removed later.

Comparison to other highlighters
--------------------------------

Other highlighters exist, but mostly use regular expressions and are therefore
limited by design.  See :doc:`LibFuzzy` for how :program:`clang-highlight`
parses C++.

* `Pygments http://pygments.org/`: "Generic syntax highlighter for general use"
  written in Python.  Lexers are python classes.  The current C++ lexer uses
  regular expressions and only highlights preprocessor and keywords.

* `GNU Source-Highlight http://www.gnu.org/software/src-highlite/`: Generic
  highlighter available for many languages.  Types, keywords, functions etc. can
  be defined by a regular expression in a configuration file.  In C++, only
  keywords, symbols and functions (without templates) are highlighted.  In
  particular, there is no code to highlight other types than the builtin ones.

* Highlighter from Editors (:program:`emacs`, :program:`vim`, etc.): Mostly
  regex-based.  Tightly coupled into the editor, not intended for use on the
  command line.
