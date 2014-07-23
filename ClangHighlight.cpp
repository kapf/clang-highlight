//===-- clang-highlight/ClangHighlight.cpp - Clang highlight tool ---------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
///
/// \file ClangHighlight.cpp
/// \brief This file implements a clang-highlight tool that automatically
/// highlights (fragments of) C++ code.
///
//===----------------------------------------------------------------------===//
#include "llvm/Support/Signals.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/MemoryBuffer.h"
#include "clang/Basic/Version.h"
#include "OutputWriter.h"
#include "TokenClassifier.h"

using namespace llvm;
using namespace clang::highlight;

// Mark all our options with this category, everything else (except for -version
// and -help) will be hidden.
static cl::OptionCategory ClangHighlightCategory("Clang-highlight options");
cl::OptionCategory &getClangHighlightCategory() {
  return ClangHighlightCategory;
}

static cl::opt<bool> IdentifiersOnly(
    "identifiers-only",
    cl::desc("Highlight identifiers only.  E.g. don't highlight the '*' "
             "in \"type *i;\""),
    cl::cat(ClangHighlightCategory));

static cl::opt<bool> DumpAST("dump-ast", cl::desc("Print the fuzzy AST."),
                             cl::cat(ClangHighlightCategory));

static cl::opt<OutputFormat> OutputFormatFlag(
    cl::desc("Output format for the highlighted code."),
    cl::values(clEnumValN(OutputFormat::StdoutColored, "stdout",
                          "write colored stdout"),
               clEnumValN(OutputFormat::HTML, "html", "write html"),
               clEnumValEnd),
    cl::cat(ClangHighlightCategory));

static cl::opt<std::string> FileName(cl::Positional, cl::desc("<file>]"),
                                     cl::Required,
                                     cl::cat(ClangHighlightCategory));

static void PrintVersion() {
  raw_ostream &OS = llvm::outs();
  OS << clang::getClangToolFullVersion("clang-highlight") << '\n';
}

static bool parserHighlight(StringRef File, OutputFormat Format,
                            bool IdentifiersOnly, bool DumpAST) {
  auto Source = llvm::MemoryBuffer::getFileOrSTDIN(File);
  if (std::error_code err = Source.getError()) {
    llvm::errs() << err.message() << '\n';
    return true;
  }

  highlight(std::move(*Source), File, makeOutputWriter(Format), IdentifiersOnly,
            DumpAST);
  return false;
}

int main(int argc, const char **argv) {
  llvm::sys::PrintStackTraceOnErrorSignal();

  // Hide unrelated options.
  StringMap<cl::Option *> Options;
  cl::getRegisteredOptions(Options);
  for (auto &Option : Options)
    if (Option.second->Category != &ClangHighlightCategory &&
        Option.first() != "help" && Option.first() != "version")
      Option.second->setHiddenFlag(cl::ReallyHidden);

  cl::SetVersionPrinter(PrintVersion);
  cl::ParseCommandLineOptions(
      argc, argv, "A tool to highlight C and C++ code.\n\n"
                  "If no arguments are specified, it highlights the code from "
                  "standard input\n"
                  "and writes the result to the standard output.\n");

  bool Error = false;

  Error |=
      parserHighlight(FileName, OutputFormatFlag, IdentifiersOnly, DumpAST);

  return Error ? 1 : 0;
}
