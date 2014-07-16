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
    return { "color:blue" };
  case TokenClass::Comment:
    return { "color:darkred" };
  case TokenClass::Preprocessor:
    return "color:purple";
  case TokenClass::String:
    return "color:red";
  case TokenClass::Char:
    return "color:magenta";
  default:
    return "color:black";
  }
}

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
       << Text << "</span>";
  }
};
} // end anonymous namespace

std::unique_ptr<OutputWriter> makeOutputWriter(OutputFormat Format) {
  switch (Format) {
  case OutputFormat::StdoutColored:
    return std::unique_ptr<OutputWriter>(new ColorStreamWriter(llvm::outs()));
  case OutputFormat::HTML:
    return std::unique_ptr<OutputWriter>(new HtmlWriter(llvm::outs()));
  default:
    llvm_unreachable("invalid flag");
  }
}

} // end namespace highlight
} // end namespace clang
