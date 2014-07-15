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
    while (First != Last && (First->Tok.getKind() == tok::unknown ||
                             First->Tok.getKind() == tok::comment))
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
                                             int Precedence = 1,
                                             bool StopAtGreater = false);

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
  assert(checkKind(TF, tok::l_paren));
  auto Func = llvm::make_unique<CallExpr>(std::move(FunctionName));
  Func->setLeftParen(TF.next());
  while (!checkKind(TF, tok::r_paren)) {
    Func->Args.push_back(parseExpression(TF, prec::Comma + 1));
    if (TF.peek()->Tok.getKind() == tok::comma)
      Func->appendComma(TF.next());
    else
      break;
  }
  if (checkKind(TF, tok::r_paren)) {
    Func->setRightParen(TF.next());
    return std::move(Func);
  }
  return {};
}

static bool isLiteralOrConstant(tok::TokenKind K) {
  if (isLiteral(K))
    return true;

  switch (K) {
  case tok::kw_true:
  case tok::kw_false:
  case tok::kw___objc_yes:
  case tok::kw___objc_no:
  case tok::kw_nullptr:
    return true;
  default:
    return false;
  }
}

static std::unique_ptr<Expr> parseExpression(TokenFilter &TF, int Precedence,
                                             bool StopAtGreater) {
  assert(TF.peek() && "can't parse empty expression");

  if (Precedence == PrecedenceUnaryOperator)
    return parseUnaryOperator(TF);

  if (Precedence > PrecedenceArrowAndPeriod) {
    if (isLiteralOrConstant(TF.peek()->Tok.getKind()))
      return llvm::make_unique<LiteralConstant>(TF.next());

    if (checkKind(TF, tok::identifier) || checkKind(TF, tok::coloncolon)) {
      auto DR = llvm::make_unique<DeclRefExpr>(TF.next());
      while (checkKind(TF, tok::identifier) || checkKind(TF, tok::coloncolon))
        DR->addQualifier(TF.next());
      if (checkKind(TF, tok::l_paren))
        return parseCallExpr(TF, std::move(DR));
      return std::move(DR);
    }

    return {};
    llvm_unreachable("expression not separable into operators and operands");
  }
  auto LeftExpr = parseExpression(TF, Precedence + 1, StopAtGreater);

  while (TF.peek()) {
    if (StopAtGreater && checkKind(TF, tok::greater))
      break;

    int CurrentPrecedence =
        getBinOpPrecedence(TF.peek()->Tok.getKind(), true, true);
    if (checkKind(TF, tok::period) || checkKind(TF, tok::arrow))
      CurrentPrecedence = PrecedenceArrowAndPeriod;
    if (CurrentPrecedence == 0)
      return LeftExpr;

    assert(CurrentPrecedence <= Precedence);
    if (CurrentPrecedence < Precedence)
      break;
    assert(CurrentPrecedence == Precedence);

    AnnotatedToken *OperatorTok = TF.next();

    auto RightExpr = parseExpression(TF, Precedence + 1, StopAtGreater);
    if (!RightExpr)
      return {};
    LeftExpr = llvm::make_unique<BinaryOperator>(
        std::move(LeftExpr), std::move(RightExpr), OperatorTok);
  }

  return LeftExpr;
}

static std::unique_ptr<Stmt> parseReturnStmt(TokenFilter &TF) {
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

static void parseTypeDecorations(TokenFilter &TF, Type &T) {
  while (checkKind(TF, tok::star) || checkKind(TF, tok::amp))
    T.Decorations.push_back(Type::Decoration(checkKind(TF, tok::star)
                                                 ? Type::Decoration::Pointer
                                                 : Type::Decoration::Reference,
                                             TF.next()));
  for (auto &Dec : T.Decorations)
    Dec.fix();
}

static bool isBuiltinType(tok::TokenKind K) {
  switch (K) {
  case tok::kw_short:
  case tok::kw_long:
  case tok::kw___int64:
  case tok::kw___int128:
  case tok::kw_signed:
  case tok::kw_unsigned:
  case tok::kw__Complex:
  case tok::kw__Imaginary:
  case tok::kw_void:
  case tok::kw_char:
  case tok::kw_wchar_t:
  case tok::kw_char16_t:
  case tok::kw_char32_t:
  case tok::kw_int:
  case tok::kw_half:
  case tok::kw_float:
  case tok::kw_double:
  case tok::kw_bool:
  case tok::kw__Bool:
  case tok::kw__Decimal32:
  case tok::kw__Decimal64:
  case tok::kw__Decimal128:
  case tok::kw___vector:
    return true;
  default:
    return false;
  }
}

static std::unique_ptr<Type> parseType(TokenFilter &TF,
                                       bool WithDecorations = true) {
  auto Guard = TF.guard();
  std::unique_ptr<Type> T = llvm::make_unique<Type>();

  if (checkKind(TF, tok::kw_auto)) {
    T->addNameQualifier(nullptr, TF.next());
  } else if (TF.peek() && isBuiltinType(TF.peek()->Tok.getKind())) {
    while (TF.peek() && isBuiltinType(TF.peek()->Tok.getKind()))
      T->addNameQualifier(nullptr, TF.next());
  } else {
    bool GlobalNamespaceColon = true;
    do {
      AnnotatedToken *ColCol = nullptr;
      if (checkKind(TF, tok::coloncolon))
        ColCol = TF.next();
      else if (!GlobalNamespaceColon)
        return {};
      GlobalNamespaceColon = false;
      if (!checkKind(TF, tok::identifier))
        return {};
      T->addNameQualifier(ColCol, TF.next());
    } while (checkKind(TF, tok::coloncolon));
  }

  if (checkKind(TF, tok::less)) {
    T->makeTemplateArgs();
    assert(T->TemplateArgs);
    bool isFirst = true;
    do {
      T->addTemplateSeparator(TF.next());

      if (isFirst && checkKind(TF, tok::greater))
        break;
      isFirst = false;

      if (auto Arg = parseType(TF))
        T->addTemplateArgument(std::move(Arg));
      else if (auto E =
                   parseExpression(TF, prec::Comma + 1, /*StopAtGreater=*/true))
        T->addTemplateArgument(std::move(E));
      else
        return {};
    } while (checkKind(TF, tok::comma));
    if (!checkKind(TF, tok::greater))
      return {};
    T->addTemplateSeparator(TF.next());
  }

  if (WithDecorations)
    parseTypeDecorations(TF, *T);

  Guard.dismiss();
  return T;
}

static std::unique_ptr<VarDecl>
parseVarDecl(TokenFilter &TF, Type *TypeName = 0, bool NameOptional = false) {
  auto Guard = TF.guard();
  auto VD = llvm::make_unique<VarDecl>();
  VarDecl &D = *VD;

  std::unique_ptr<Type> TypeName2;
  if (!TypeName) {
    TypeName2 = parseType(TF);
    if (!TypeName2)
      return {};
    TypeName = TypeName2.get();
  }

  D.VariableType = TypeName->cloneQualifiedID();
  parseTypeDecorations(TF, *D.VariableType);

  if (checkKind(TF, tok::identifier)) {
    D.setName(TF.next());
  } else if (!NameOptional) {
    return {};
  }

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
  Guard.dismiss();
  return VD;
}

static std::unique_ptr<Stmt> parseDeclStmt(TokenFilter &TF) {
  auto Guard = TF.guard();

  auto TypeName = parseType(TF, /*WithDecorations=*/false);
  if (!TypeName)
    return {};
  auto Declaration = llvm::make_unique<DeclStmt>();

  while (TF.peek()) {
    if (checkKind(TF, tok::semi)) {
      Guard.dismiss();
      return std::move(Declaration);
    }
    if (auto D = parseVarDecl(TF, TypeName.get()))
      Declaration->Decls.push_back(std::move(D));
    else
      return {};
    if (checkKind(TF, tok::comma))
      Declaration->appendComma(TF.next());
    else if (!checkKind(TF, tok::semi))
      return {};
  }

  return nullptr;
}

static std::unique_ptr<FunctionDecl> parseFunctionStmt(TokenFilter &TF) {
  auto Guard = TF.guard();
  auto F = llvm::make_unique<FunctionDecl>();
  if (checkKind(TF, tok::kw_static))
    F->setStatic(TF.next());
  if (auto Type = parseType(TF))
    F->ReturnType = std::move(Type);
  else
    return {};

  if (!checkKind(TF, tok::identifier))
    return {};
  F->setName(TF.next());

  if (!checkKind(TF, tok::l_paren))
    return {};

  F->setLeftParen(TF.next());
  while (!checkKind(TF, tok::r_paren)) {
    F->Params.push_back(parseVarDecl(TF, 0, true));
    if (!F->Params.back())
      return {};
    if (checkKind(TF, tok::comma))
      F->appendComma(TF.next());
    else
      break;
  }
  if (checkKind(TF, tok::r_paren)) {
    Guard.dismiss();
    F->setRightParen(TF.next());
    return std::move(F);
  }
  return {};
}

static std::unique_ptr<Stmt> skipUnparsable(TokenFilter &TF) {
  assert(TF.peek());
  auto UB = llvm::make_unique<UnparsableBlock>();
  while (TF.peek()) {
    auto Kind = TF.peek()->Tok.getKind();
    UB->push_back(TF.next());
    if (Kind == tok::semi || Kind == tok::r_brace || Kind == tok::l_brace)
      break;
  }
  return std::move(UB);
}

static std::unique_ptr<Stmt> parseAny(TokenFilter &TF) {
  assert(TF.peek());
  if (auto S = parseReturnStmt(TF))
    return S;
  if (auto S = parseDeclStmt(TF))
    return S;
  if (auto S = parseFunctionStmt(TF))
    return std::move(S);
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
