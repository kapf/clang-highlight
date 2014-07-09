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
  AnnotatedToken *First, *Last;

  AnnotatedToken *next() {
    auto Ret = First++;
    while (First != Last && First->Tok.getKind() == tok::unknown)
      ++First;
    if (First == Last)
      First = Last = 0;
    return Ret;
  }

  class TokenFilterState {
    friend class TokenFilter;
    TokenFilterState(AnnotatedToken *First, AnnotatedToken *Last)
        : First(First), Last(Last) {}
    AnnotatedToken *First, *Last;
  };

  TokenFilterState mark() const { return TokenFilterState(First, Last); }
  void rewind(TokenFilterState State) {
    First = State.First;
    Last = State.Last;
  }

  class TokenFilterGuard {
    friend class TokenFilter;
    TokenFilterGuard(TokenFilter *TF, TokenFilterState State)
        : TF(TF), State(State) {}

  public:
    ~TokenFilterGuard() {
      if (TF)
        TF->rewind(State);
    }
    void dismiss() { TF = nullptr; }
    TokenFilter *TF;
    TokenFilterState State;
  };
  TokenFilterGuard guard() { return TokenFilterGuard(this, mark()); }

  AnnotatedToken *peek() { return First; }
};
} // end anonymous namespace

static int PrecedenceUnaryOperator = prec::PointerToMember + 1;
static int PrecedenceArrowAndPeriod = prec::PointerToMember + 2;

static std::unique_ptr<Expr> parseExpression(TokenFilter &TF,
                                             int Precedence = 1);

static std::unique_ptr<Expr> parseUnaryOperator(TokenFilter &TF) {
  assert(TF.peek() && "can't parse empty expression");

  if (TF.peek()->Tok.getKind() == tok::star) {
    AnnotatedToken *Op = TF.next();
    return llvm::make_unique<UnaryOperator>(Op, parseUnaryOperator(TF));
  }

  return parseExpression(TF, PrecedenceArrowAndPeriod);
}

static std::unique_ptr<Expr> parseExpression(TokenFilter &TF, int Precedence) {
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
    if (CurrentPrecedence == 0)
      return LeftExpr;

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

std::unique_ptr<Stmt> tryParseDeclStmt(TokenFilter &TF) {
  auto Guard = TF.guard();

  // Form: identifier [any number of '*'] identifier '='
  if (TF.peek() && TF.peek()->Tok.getKind() != tok::identifier)
    return nullptr;
  AnnotatedToken *TypeName = TF.next();
  auto Declaration = llvm::make_unique<DeclStmt>();

  while (TF.peek()) {
    Type VarType(TypeName);

    while (TF.peek() && TF.peek()->Tok.getKind() == tok::star)
      VarType.Decorations.push_back(Type::Decoration(
          Type::Decoration::Pointer, TF.next())); // Skip pointer derefs.

    if (TF.peek() && TF.peek()->Tok.getKind() != tok::identifier)
      return nullptr;

    VarDecl D(VarType, TF.next());

    // TODO: var(init) and var{init} not yet implemented
    if (TF.peek() && TF.peek()->Tok.getKind() == tok::equal) {
      auto *EqualTok = TF.next();
      if (auto Value = parseExpression(TF, prec::Comma + 1))
        D.Value = VarInitialization(VarInitialization::ASSIGNMENT, EqualTok,
                                    std::move(Value));
      else
        return nullptr;
    } else {
      return nullptr;
    }

    Declaration->Decls.push_back(std::move(D));

    if (TF.peek() && TF.next()->Tok.getKind() == tok::semi)
      return std::move(Declaration);
  }

  return nullptr;
}

static std::unique_ptr<Stmt> parseCompoundStmt(TokenFilter &TF) {
  // TODO: Currently only testing driver
  if (auto Decl = tryParseDeclStmt(TF))
    return Decl;
  llvm_unreachable("could not parse decl stmt");
}

std::unique_ptr<Stmt> fuzzyparse(AnnotatedToken *first, AnnotatedToken *last) {
  TokenFilter TF{ first, last };
  return parseCompoundStmt(TF);
}

} // end namespace fuzzy
} // end namespace clang
