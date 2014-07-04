//===--- FuzzyParser.cpp - clang-highlight ----------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include "llvm/Support/Debug.h"
#include "llvm/ADT/STLExtras.h"
#include "FuzzyAST.h"
#include "clang/Basic/OperatorPrecedence.h"

using namespace clang;

namespace clang {
namespace highlight {

namespace {
struct TokenFilter {
  AnnotatedToken *first, *last;

  AnnotatedToken *next() {
    auto ret = first++;
    while (first != last && first->Tok.getKind() == tok::unknown)
      ++first;
    if (first == last)
      first = last = 0;
    return ret;
  }

  AnnotatedToken *peek() { return first; }
};
} // end anonymous namespace

// static int PrecedenceUnaryOperator = prec::PointerToMember + 1;
static int PrecedenceArrowAndPeriod = prec::PointerToMember + 2;

static std::unique_ptr<Expr> parseExpression(TokenFilter &TF,
                                             int Precedence = 0) {
  assert(TF.peek() && "can't parse empty expression");

  if (Precedence > PrecedenceArrowAndPeriod) {
    if (TF.peek()->Tok.getKind() == tok::identifier)
      return llvm::make_unique<DeclRefExpr>(TF.next());
    if (isLiteral(TF.peek()->Tok.getKind()))
      return llvm::make_unique<LiteralConstant>(TF.next());

    llvm_unreachable("TODO");
  }
  auto LeftExpr = parseExpression(TF, Precedence + 1);

  while (TF.peek()) {
    int CurrentPrecedence =
        getBinOpPrecedence(TF.peek()->Tok.getKind(), true, true);
    assert(CurrentPrecedence <= Precedence);
    if (CurrentPrecedence < Precedence)
      break;
    assert(CurrentPrecedence == Precedence);

    AnnotatedToken *OperatorTok = TF.next();
    auto RightExpr = parseExpression(TF, Precedence + 1);
    LeftExpr = llvm::make_unique<BinaryOperator>(
        std::move(LeftExpr), std::move(RightExpr), OperatorTok);
  }

  return LeftExpr;
}

std::unique_ptr<Stmt> fuzzyparse(AnnotatedToken *first, AnnotatedToken *last) {
  TokenFilter TF{first, last};
  return parseExpression(TF);
}

static void printASTImpl(int Indent, const Stmt &stmt,
                         const SourceManager &SourceMgr) {
  if (auto *BinOp = llvm::dyn_cast<BinaryOperator>(&stmt)) {
    printASTImpl(Indent + 4, *BinOp->getLHS(), SourceMgr);
    llvm::dbgs() << std::string(Indent, ' ')
                 << tok::getTokenName(BinOp->OperatorTok->Tok.getKind())
                 << '\n';
    printASTImpl(Indent + 4, *BinOp->getRHS(), SourceMgr);
  } else if (auto *Decl = llvm::dyn_cast<DeclRefExpr>(&stmt)) {
    llvm::dbgs() << std::string(Indent, ' ') << Decl->Tok->getText(SourceMgr)
                 << '\n';
  } else if (auto *Lit = llvm::dyn_cast<LiteralConstant>(&stmt)) {
    llvm::dbgs() << std::string(Indent, ' ') << Lit->Tok->getText(SourceMgr)
                 << '\n';
  } else {
    llvm_unreachable("TODO");
  }
}

void printAST(const Stmt &Root, const SourceManager &SourceMgr) {
  return printASTImpl(0, Root, SourceMgr);
}

} // end namespace highlight
} // end namespace clang
