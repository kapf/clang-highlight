//===--- FuzzyAST.h - clang-highlight ---------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_TOOLS_CLANG_HIGHLIGHT_FUZZY_AST_H
#define LLVM_CLANG_TOOLS_CLANG_HIGHLIGHT_FUZZY_AST_H

#include "clang/Lex/Lexer.h"
#include "llvm/ADT/StringRef.h"
#include "clang/Basic/SourceManager.h"
#include <memory>

using namespace clang;

namespace clang {
namespace fuzzy {

class Stmt;

struct AnnotatedToken {
  AnnotatedToken(Token Tok) : Tok(Tok), ASTReference(nullptr) {}

  Token Tok;
  Stmt *ASTReference;

  StringRef getText(const SourceManager &SourceMgr) const {
    return StringRef(SourceMgr.getCharacterData(Tok.getLocation()),
                     Tok.getLength());
  }

  void setASTReference(Stmt *ASTReference) {
    this->ASTReference = ASTReference;
  }
};

class Stmt {
public:
  virtual ~Stmt() = 0; // Not optimized

  // TODO: TableGen
  enum StmtClass {
    NoStmtClass = 0,
    DeclRefExprClass,
    LiteralConstantClass,
    BinaryOperatorClass,
  };

  Stmt(StmtClass SC) : sClass(SC) {}

  StmtClass getStmtClass() const { return sClass; }

private:
  StmtClass sClass;
};
inline Stmt::~Stmt() {}

struct Expr : Stmt {
  Expr(StmtClass SC) : Stmt(SC) {}
  ~Expr() = 0;
};
inline Expr::~Expr() {}

class DeclRefExpr : public Expr {
public:
  AnnotatedToken *Tok;
  DeclRefExpr(AnnotatedToken *Tok) : Expr(DeclRefExprClass), Tok(Tok) {
    Tok->setASTReference(this);
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == DeclRefExprClass;
  }
};

class LiteralConstant : public Expr {
public:
  AnnotatedToken *Tok;
  LiteralConstant(AnnotatedToken *Tok) : Expr(LiteralConstantClass), Tok(Tok) {
    Tok->setASTReference(this);
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == LiteralConstantClass;
  }
};

/// Used to store any kind of binary operators, even the overloaded ones
/// (CXXOperatorCallExpr).  As Fuzzy AST doesn't want to store type information
/// for now, this is not needed anyway.
class BinaryOperator : public Expr {
  enum {
    LHS,
    RHS,
    END_EXPR
  };
  std::unique_ptr<Expr> SubExprs[END_EXPR];

public:
  AnnotatedToken *OperatorTok;

  BinaryOperator(std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs,
                 AnnotatedToken *OperatorTok)
      : Expr(BinaryOperatorClass) {
    SubExprs[LHS] = std::move(lhs);
    SubExprs[RHS] = std::move(rhs);
    this->OperatorTok = OperatorTok;
    this->OperatorTok->setASTReference(this);
  }

  static bool classof(const Stmt *T) {
    return T->getStmtClass() == BinaryOperatorClass;
  }

  Expr *getLHS() { return cast<Expr>(SubExprs[LHS].get()); }
  const Expr *getLHS() const { return cast<Expr>(SubExprs[LHS].get()); }
  Expr *getRHS() { return cast<Expr>(SubExprs[RHS].get()); }
  const Expr *getRHS() const { return cast<Expr>(SubExprs[RHS].get()); }
};

std::unique_ptr<Stmt> fuzzyparse(AnnotatedToken *first, AnnotatedToken *last);

void printAST(const Stmt &Root, const SourceManager &SourceMgr);

} // end namespace fuzzy
} // end namespace clang

#endif // LLVM_CLANG_TOOLS_CLANG_HIGHLIGHT_FUZZY_AST_H
