//===--- TokenClassifier.h - clang-highlight --------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_TOOLS_CLANG_HIGHLIGHT_TOKEN_CLASSIFIER_H
#define LLVM_CLANG_TOOLS_CLANG_HIGHLIGHT_TOKEN_CLASSIFIER_H

#include "llvm/ADT/StringRef.h"
#include <memory>

namespace llvm {
class MemoryBuffer;
}

namespace clang {
namespace highlight {

class OutputWriter;

struct ParserHints {
  ParserHints() = default;
  ParserHints(llvm::StringRef FileName) : FileName(FileName) {}
  llvm::StringRef FileName;
  std::vector<unsigned> TypeOffsets;
  void normalize() {
    std::sort(TypeOffsets.begin(), TypeOffsets.end());
    TypeOffsets.erase(std::unique(TypeOffsets.begin(), TypeOffsets.end()),
                      TypeOffsets.end());
  }
};

ParserHints collectParserHints(llvm::StringRef SourceFile);
void highlight(std::unique_ptr<llvm::MemoryBuffer> Source,
               std::unique_ptr<OutputWriter> OW,
               ParserHints Hints = ParserHints());

} // end namespace highlight
} // end namespace clang

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_CLANG_TIDY_H
