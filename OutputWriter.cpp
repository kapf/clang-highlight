//===--- OutputWriter.cpp - clang-highlight ---------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file OutputWriter.cpp
/// \brief Converts the metadata into a given output format.
///
//===----------------------------------------------------------------------===//

#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "OutputWriter.h"

using namespace llvm;

namespace clang {
namespace highlight {

OutputWriter::~OutputWriter() {}

namespace {
struct StdoutFormatInfo {
  StdoutFormatInfo(raw_ostream::Colors Color, bool Bold = false)
      : Color(Color), Bold(Bold) {}
  raw_ostream::Colors Color;
  bool Bold;
};
} // end anonymous namespace

static StdoutFormatInfo getFormatInfo(TokenClass Class) {
  switch (Class) {
  case TokenClass::Type:
    return { raw_ostream::GREEN };
  case TokenClass::Keyword:
    return { raw_ostream::BLUE };
  case TokenClass::Comment:
    return { raw_ostream::RED };
  case TokenClass::Namespace:
    return { raw_ostream::GREEN };
  case TokenClass::Preprocessor:
    return { raw_ostream::CYAN };
  case TokenClass::String:
  case TokenClass::Char:
    return { raw_ostream::MAGENTA };
  case TokenClass::Numeric:
    return { raw_ostream::BLUE, true };
  case TokenClass::Function:
    return { raw_ostream::BLACK, true };
  default:
    return { raw_ostream::BLACK };
  }
}

static const char *getSpanStyle(TokenClass Class) {
  switch (Class) {
  case TokenClass::Namespace:
  case TokenClass::Type:
    return "color:green";
  case TokenClass::Keyword:
    return "color:blue";
  case TokenClass::Comment:
    return "color:darkred";
  case TokenClass::Preprocessor:
    return "color:purple";
  case TokenClass::String:
    return "color:red";
  case TokenClass::Char:
    return "color:magenta";
  case TokenClass::Numeric:
    return "color:DarkSlateGray";
  case TokenClass::Function:
    return "color:black;font-style:italic";
  default:
    return "color:black";
  }
}

static const char *getClassName(TokenClass Class) {
  switch (Class) {
  case TokenClass::Namespace:
    return "namespace";
  case TokenClass::Type:
    return "type";
  case TokenClass::Keyword:
    return "keyword";
  case TokenClass::Comment:
    return "comment";
  case TokenClass::Preprocessor:
    return "preprocessor";
  case TokenClass::String:
    return "string";
  case TokenClass::Char:
    return "char";
  case TokenClass::Function:
    return "function";
  case TokenClass::Numeric:
    return "numeric";
  case TokenClass::Variable:
    return "variable";
  default:
    return "default";
  }
}

namespace {
class XmlEscaper {
  StringRef S;

public:
  XmlEscaper(StringRef S) : S(S) {};

  friend raw_ostream &operator<<(raw_ostream &OS, const XmlEscaper &HE) {
    for (char C : HE.S)
      switch (C) {
      case '&':
        OS << "&amp;";
        break;
      case '\'':
        OS << "&apos;";
        break;
      case '"':
        OS << "&quot;";
        break;
      case '<':
        OS << "&lt;";
        break;
      case '>':
        OS << "&gt;";
        break;
      default:
        OS << C;
        break;
      }
    return OS;
  }
};
} // end anonymous namespace

XmlEscaper xmlEscaped(StringRef S) { return XmlEscaper(S); }

namespace {
class ColorStreamWriter : public OutputWriter {
  raw_ostream &OS;

public:
  ColorStreamWriter(raw_ostream &OS) : OS(OS) {
    OS.changeColor(raw_ostream::BLACK);
  }
  ~ColorStreamWriter() { OS.changeColor(raw_ostream::BLACK); }

  void writeToken(StringRef Text, TokenClass Class) override {
    StdoutFormatInfo Style = getFormatInfo(Class);
    OS.changeColor(Style.Color, Style.Bold);
    OS << Text;
  }
};
} // end anonymous namespace

namespace {
class HtmlWriter : public OutputWriter {
  raw_ostream &OS;

public:
  HtmlWriter(raw_ostream &OS) : OS(OS) {
    OS << "<p style=\"white-space:pre\"><tt>";
  }
  ~HtmlWriter() { OS << "</tt></p>"; }

  void writeToken(StringRef Text, TokenClass Class) override {
    OS << R"(<span style=")" << getSpanStyle(Class) << R"(">)"
       << xmlEscaped(Text) << "</span>";
  }
};
} // end anonymous namespace

namespace {
class SemanticHtmlWriter : public OutputWriter {
  raw_ostream &OS;

public:
  SemanticHtmlWriter(raw_ostream &OS) : OS(OS) {
    OS << R"(<style type="text/css">
.clanghighlight span.namespace { color:green }
.clanghighlight span.type { color:green }
.clanghighlight span.keyword { color:blue }
.clanghighlight span.comment { color:darkred }
.clanghighlight span.preprocessor { color:purple }
.clanghighlight span.string { color:red }
.clanghighlight span.char { color:magenta }
.clanghighlight span.numeric { color:DarkSlateGray }
.clanghighlight span.function { color:black;font-style=italic }
.clanghighlight span.variable { color:black }
.clanghighlight span.default { color:black }
</style>
<p style="white-space:pre" class="clanghighlight"><tt>)";
  }
  ~SemanticHtmlWriter() { OS << "</tt></p>"; }

  void writeToken(StringRef Text, TokenClass Class) override {
    OS << R"(<span class=")" << getClassName(Class) << R"(">)"
       << xmlEscaped(Text) << "</span>";
  }
};
} // end anonymous namespace

namespace {
class LaTeXEscaper {
  StringRef S;

public:
  LaTeXEscaper(StringRef S) : S(S) {};

  friend raw_ostream &operator<<(raw_ostream &OS, const LaTeXEscaper &HE) {
    for (char C : HE.S)
      switch (C) {
      case '{':
      case '}':
      case '_':
      case '&':
      case '#':
      case '%':
      case '$':
        OS << "{\\" << C << "}";
        break;
      case '^':
        OS << "{\\^{}}";
        break;
      case '\\':
        OS << "{\\textbackslash}";
        break;
      case '<':
        OS << "{\\textless}";
        break;
      case '>':
        OS << "{\\textgreater}";
        break;
      case '~':
        OS << "{\\textasciitilde}";
        break;
      default:
        OS << C;
      }
    return OS;
  }
};
} // end anonymous namespace

LaTeXEscaper latexEscaped(StringRef S) { return LaTeXEscaper(S); }

namespace {
class LaTeXWriter : public OutputWriter {
  raw_ostream &OS;

public:
  LaTeXWriter(raw_ostream &OS) : OS(OS) {}
  ~LaTeXWriter() {}

  void writeToken(StringRef Text, TokenClass Class) override {
    if (Class == TokenClass::Whitespace)
      OS << latexEscaped(Text);
    else
      OS << "\\clangHighlightToken{" << getClassName(Class) << "}{"
         << latexEscaped(Text) << "}";
  }
};
} // end anonymous namespace

std::unique_ptr<OutputWriter> makeOutputWriter(OutputFormat Format,
                                               raw_ostream &OS) {
  switch (Format) {
  case OutputFormat::StdoutColored:
    return std::unique_ptr<OutputWriter>(new ColorStreamWriter(OS));
  case OutputFormat::HTML:
    return std::unique_ptr<OutputWriter>(new HtmlWriter(OS));
  case OutputFormat::SemanticHTML:
    return std::unique_ptr<OutputWriter>(new SemanticHtmlWriter(OS));
  case OutputFormat::LaTeX:
    return std::unique_ptr<OutputWriter>(new LaTeXWriter(OS));
  default:
    llvm_unreachable("invalid flag");
  }
}

} // end namespace highlight
} // end namespace clang
