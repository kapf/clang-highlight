#error NOT YET NEEDED
//===--- FuzzyType.h - clang-highlight --------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_TOOLS_CLANG_HIGHLIGHT_FUZZY_TYPE_H
#define LLVM_CLANG_TOOLS_CLANG_HIGHLIGHT_FUZZY_TYPE_H

#include "clang/Basic/SourceManager.h"
#include "AnnotatedToken.h"
#include <memory>

using namespace clang;

namespace clang {
namespace fuzzy {

struct Type {
  struct TypeAnnotation {
    enum AnnotationClass {
      Pointer,
      Reference,
    };
    AnnotationClass Class;
    AnnotatedToken *Tok;
  };
  llvm::SmallVector<TypeAnnotation, 1> Annotations;
  AnnotatedToken *NameToken;
};

} // end namespace fuzzy
} // end namespace clang

#endif // LLVM_CLANG_TOOLS_CLANG_HIGHLIGHT_FUZZY_TYPE_H
