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
    assert(Ret->Tok.getKind() != tok::raw_identifier);
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

static bool checkKind(TokenFilter &TF, tok::TokenKind Kind) {
  return TF.peek() && TF.peek()->Tok.getKind() == Kind;
}

static int PrecedenceUnaryOperator = prec::PointerToMember + 1;
static int PrecedenceArrowAndPeriod = prec::PointerToMember + 2;

static std::unique_ptr<Expr> parseExpression(TokenFilter &TF,
                                             int Precedence = 1);

static std::unique_ptr<Expr> parseUnaryOperator(TokenFilter &TF) {
  assert(TF.peek() && "can't parse empty expression");

  if (checkKind(TF, tok::star)) {
    AnnotatedToken *Op = TF.next();
    return llvm::make_unique<UnaryOperator>(Op, parseUnaryOperator(TF));
  }

  return parseExpression(TF, PrecedenceArrowAndPeriod);
}

static std::unique_ptr<Expr>
parseCallExpr(TokenFilter &TF, std::unique_ptr<DeclRefExpr> FunctionName) {
  assert(TF.peek() && TF.peek()->Tok.getKind() == tok::l_paren);
  auto Func = llvm::make_unique<CallExpr>(std::move(FunctionName));
  Func->setLeftParen(TF.next());
  while (!checkKind(TF, tok::r_paren)) {
    Func->Args.push_back(parseExpression(TF, prec::Comma + 1));
    if (TF.peek()->Tok.getKind() == tok::comma)
      Func->append_comma(TF.next());
    else
      break;
  }
  if (checkKind(TF, tok::r_paren)) {
    Func->setRightParen(TF.next());
    return std::move(Func);
  }
  return {};
}

static std::unique_ptr<Expr> parseExpression(TokenFilter &TF, int Precedence) {
  assert(TF.peek() && "can't parse empty expression");

  if (Precedence == PrecedenceUnaryOperator)
    return parseUnaryOperator(TF);

  if (Precedence > PrecedenceArrowAndPeriod) {
    if (isLiteral(TF.peek()->Tok.getKind()))
      return llvm::make_unique<LiteralConstant>(TF.next());

    if (checkKind(TF, tok::identifier)) {
      auto DR = llvm::make_unique<DeclRefExpr>(TF.next());
      if (checkKind(TF, tok::l_paren))
        return parseCallExpr(TF, std::move(DR));
      return std::move(DR);
    }

    return {};
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

static std::unique_ptr<Stmt> tryParseReturnStmt(TokenFilter &TF) {
  auto Guard = TF.guard();
  if (!checkKind(TF, tok::kw_return))
    return {};
  auto *Return = TF.next();
  std::unique_ptr<Expr> Body;
  if (!checkKind(TF, tok::semi)) {
    Body = parseExpression(TF);
    if (!Body || !checkKind(TF, tok::semi))
      return {};
  }
  assert(checkKind(TF, tok::semi));
  auto *Semi = TF.next();
  Guard.dismiss();
  return llvm::make_unique<ReturnStmt>(Return, std::move(Body), Semi);
}
static std::unique_ptr<Stmt> tryParseDeclStmt(TokenFilter &TF) {
  auto Guard = TF.guard();

  // Form: identifier [any number of '*'] identifier '='
  if (!checkKind(TF, tok::identifier))
    return nullptr;
  AnnotatedToken *TypeName = TF.next();
  auto Declaration = llvm::make_unique<DeclStmt>();

  while (TF.peek()) {
    Declaration->Decls.push_back(VarDecl());
    VarDecl &D = Declaration->Decls.back();
    D.VariableType.setName(TypeName);

    while (checkKind(TF, tok::star))
      D.VariableType.Decorations.push_back(Type::Decoration(
          Type::Decoration::Pointer, TF.next())); // Skip pointer derefs.
    for (auto &Dec : D.VariableType.Decorations)
      Dec.fix();

    if (!checkKind(TF, tok::identifier))
      return {};

    D.setName(TF.next());

    if (checkKind(TF, tok::equal)) {
      auto *EqualTok = TF.next();
      if (auto Value = parseExpression(TF, prec::Comma + 1)) {
        D.Value = VarInitialization();
        D.Value->setAssignmentOps(VarInitialization::ASSIGNMENT, EqualTok);
        D.Value->Value = std::move(Value);
      } else {
        return {};
      }
    } else {
      // TODO: var(init) and var{init} not yet implemented
    }

    if (checkKind(TF, tok::semi)) {
      Guard.dismiss();
      return std::move(Declaration);
    }
    if (!checkKind(TF, tok::comma))
      return {};
    TF.next();
  }

  return nullptr;
}

static std::unique_ptr<Stmt> skipUnparsable(TokenFilter &TF) {
  assert(TF.peek());
  auto UB = llvm::make_unique<UnparsableBlock>();
  while (TF.peek()) {
    auto Kind = TF.peek()->Tok.getKind();
    UB->push_back(TF.next());
    if (Kind == tok::semi || Kind == tok::r_brace)
      break;
  }
  return std::move(UB);
}

static std::unique_ptr<Stmt> parseAny(TokenFilter &TF) {
  assert(TF.peek());
  if (auto S = tryParseReturnStmt(TF))
    return S;
  if (auto S = tryParseDeclStmt(TF))
    return S;
  {
    auto Guard = TF.guard();
    if (auto E = parseExpression(TF)) {
      if (checkKind(TF, tok::semi)) {
        Guard.dismiss();
        return llvm::make_unique<ExprLineStmt>(std::move(E), TF.next());
      }
    }
  }
  return skipUnparsable(TF);
}

llvm::SmallVector<std::unique_ptr<Stmt>, 8> fuzzyparse(AnnotatedToken *first,
                                                       AnnotatedToken *last) {
  llvm::SmallVector<std::unique_ptr<Stmt>, 8> Result;
  TokenFilter TF{ first, last };
  while (TF.peek())
    Result.push_back(parseAny(TF));
  return Result;
}

} // end namespace fuzzy
} // end namespace clang
