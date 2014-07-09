//===--- AnnotatedToken.h - clang-highlight ---------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_TOOLS_CLANG_HIGHLIGHT_ANNOTATED_TOKEN_H
#define LLVM_CLANG_TOOLS_CLANG_HIGHLIGHT_ANNOTATED_TOKEN_H

#include "clang/Lex/Lexer.h"
#include <memory>

using namespace clang;

namespace clang {
namespace fuzzy {

class ASTElement;

struct AnnotatedToken {
  AnnotatedToken(Token Tok) : Tok(Tok), ASTReference(nullptr) {}

  Token Tok;
  ASTElement *ASTReference;

  StringRef getText(const SourceManager &SourceMgr) const {
    return StringRef(SourceMgr.getCharacterData(Tok.getLocation()),
                     Tok.getLength());
  }

  void setASTReference(ASTElement *ASTReference) {
    this->ASTReference = ASTReference;
  }
};

class AnnotatedTokenRef {
  AnnotatedToken *ATok;

public:
  AnnotatedTokenRef(AnnotatedToken *ATok, ASTElement *AstRef) : ATok(ATok) {
    assert(ATok);
    ATok->setASTReference(AstRef);
  }
  AnnotatedTokenRef(nullptr_t = nullptr) : ATok(nullptr) {}

  AnnotatedToken &operator*() { return *ATok; }
  AnnotatedToken const &operator*() const { return *ATok; }
  AnnotatedToken *operator->() { return ATok; }
  AnnotatedToken const *operator->() const { return ATok; }
};

} // end namespace fuzzy
} // end namespace clang

#endif // LLVM_CLANG_TOOLS_CLANG_HIGHLIGHT_ANNOTATED_TOKEN_H
