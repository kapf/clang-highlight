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

void highlight(std::unique_ptr<llvm::MemoryBuffer> Source,
               llvm::StringRef FileName, std::unique_ptr<OutputWriter> OW,
               bool IdentifiersOnly = false, bool DumpAST = false);

} // end namespace highlight
} // end namespace clang

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_CLANG_TIDY_H
