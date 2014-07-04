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
namespace fuzzy {

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

static int PrecedenceUnaryOperator = prec::PointerToMember + 1;
static int PrecedenceArrowAndPeriod = prec::PointerToMember + 2;

static std::unique_ptr<Expr> parseExpression(TokenFilter &TF,
                                             int Precedence = 0);

static std::unique_ptr<Expr> parseUnaryOperator(TokenFilter &TF) {
  assert(TF.peek() && "can't parse empty expression");

  if (TF.peek()->Tok.getKind() == tok::star) {  
    AnnotatedToken *Op = TF.next();
    return llvm::make_unique<UnaryOperator>(Op, parseUnaryOperator(TF));
  }

  return parseExpression(TF, PrecedenceArrowAndPeriod);
}

static std::unique_ptr<Expr> parseExpression(TokenFilter &TF,
                                             int Precedence) {
  assert(TF.peek() && "can't parse empty expression");

  if (Precedence == PrecedenceUnaryOperator)
    return parseUnaryOperator(TF);

  if (Precedence > PrecedenceArrowAndPeriod) {
    if (TF.peek()->Tok.getKind() == tok::identifier)
      return llvm::make_unique<DeclRefExpr>(TF.next());
    if (isLiteral(TF.peek()->Tok.getKind()))
      return llvm::make_unique<LiteralConstant>(TF.next());

    llvm_unreachable("expression not separable into operators and operands");
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
  } else if (auto *Unar = llvm::dyn_cast<UnaryOperator>(&stmt)) {
    llvm::dbgs() << std::string(Indent, ' ')
                 << Unar->OperatorTok->getText(SourceMgr)
                 << ": ";
    printASTImpl(0, *Unar->Value, SourceMgr);
  } else {
    llvm_unreachable("TODO: unhandled fuzzy ast node");
  }
}

void printAST(const Stmt &Root, const SourceManager &SourceMgr) {
  return printASTImpl(0, Root, SourceMgr);
}

} // end namespace fuzzy
} // end namespace clang
