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

   Writes semantic HTML with CSS selectors.  The individual tokens have the
   form ``<span class="typename">int</span>``.  The class can be specified in
   a separate style sheet by the user.

.. option:: -latex

   Writes semantic LaTeX for use with the package that is bundled with
   clang-highlight.  See below.

Further options
---------------

.. option:: -identifiers-only

   Per default, the star ``*`` in ``type *i;`` is classified as part of the type
   name as is ``<`` and ``>`` in ``unique_ptr<T>``.  To disable this feature,
   use the ``-identifiers-only`` option.

.. option:: -dump-ast

   Only included for testing the fuzzy parser, will be removed later.

.. option:: -o <file>

   Output to a file instead of standard output.

The LaTeX Package ``clanghighlight``
------------------------------------

:program:`clang-highlight` can be used as a highlighter for LaTeX code.  The
file ``clanghighlight.sty`` that is included in this repository provides a
package for easy usage.  Just put it in the same directory as the ``.tex`` file
you are writing.

.. code-block:: latex

  \usepackage{clanghighlight} % put this into the preamble

  % You might need to specify the full path to clang-highlight
  % \clanghighlightCmd{/path/to/clang-highlight}

  % in the document:
  \begin{cxx}
  // your code goes here
  \end{cxx}

  \begin{cxx}[numbers=left] % the options are directly passed to fancyvrb
  // your code goes here
  \end{cxx}

  \inputcxx{file.cpp} % use code from a file

This package is only in beta status and some more functionality might be added
soon.

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
