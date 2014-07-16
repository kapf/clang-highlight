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

#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/STLExtras.h"
#include "clang/Basic/SourceManager.h"
#include "AnnotatedToken.h"
#include <memory>

using namespace clang;

namespace llvm {
class raw_ostream;
}

namespace clang {
namespace fuzzy {

/// ASTElement: Anything inside the AST that may be referenced by an
/// AnnotatedToken must be an ASTElement.  This class is not strictly needed
/// from an AST point of view.
class ASTElement {
protected:
  ~ASTElement() = default; // Not accessible
public:
  // TODO: TableGen
  enum ASTElementClass {
    NoASTElementClass = 0,
    UnparsableBlockClass,
    TypeClass,
    TypeDecorationClass,
    VarInitializationClass,
    VarDeclClass,
    firstExpr,
    ExprLineStmtClass,
    ReturnStmtClass,
    CompoundStmtClass,
    DeclStmtClass,
    DeclRefExprClass,
    LiteralConstantClass,
    UnaryOperatorClass,
    BinaryOperatorClass,
    CallExprClass,
    lastExpr,
    FunctionDeclClass,
  };

  ASTElement(ASTElementClass SC) : sClass(SC) {}

  ASTElementClass getASTClass() const { return sClass; }

private:
  ASTElementClass sClass;
};

/// An expression in it's classical sense.  If an expression is used as a
/// statement, it has to be embedded into a ExprStmt (yet to be implemented).
/// Rationale is that there is otherwise no way to store the semicolon.
class Expr : public ASTElement {
protected:
  Expr(ASTElementClass SC) : ASTElement(SC) {}

public:
  virtual ~Expr() = 0;
  static bool classof(const ASTElement *T) {
    return firstExpr <= T->getASTClass() && T->getASTClass() <= lastExpr;
  }
};
inline Expr::~Expr() {}


class Type;

struct QualifiedID {
  class TypeOrExpression {
    ASTElement *Ptr;

  public:
    TypeOrExpression(std::unique_ptr<Type> T); // : Ptr(T.release()) {}
    TypeOrExpression(std::unique_ptr<Expr> E) : Ptr(E.release()) {}
    TypeOrExpression(const TypeOrExpression &) = delete;
    TypeOrExpression &operator=(const TypeOrExpression &) = delete;
    TypeOrExpression(TypeOrExpression &&O) : Ptr(O.Ptr) { O.Ptr = nullptr; }
    TypeOrExpression &operator=(TypeOrExpression &&O) {
      std::swap(Ptr, O.Ptr);
      return *this;
    }

    ~TypeOrExpression();

    bool isType() const {
      assert(Ptr);
      return isa<Type>(Ptr);
    }
    Type &asType() { return *cast<Type>(Ptr); }
    Expr &asExpr() { return *cast<Expr>(Ptr); }
  };

  struct TemplateArguments {
    llvm::SmallVector<TypeOrExpression, 2> Args;
    llvm::SmallVector<AnnotatedTokenRef, 3> Separators;
  };

  llvm::SmallVector<AnnotatedTokenRef, 1> NameSegments;
  llvm::Optional<std::shared_ptr<TemplateArguments> > TemplateArgs;

  void reown(ASTElement *Ref) {
    for (auto &N : NameSegments)
      N->setASTReference(Ref);
    if (TemplateArgs) {
      for (auto& ATok : (*TemplateArgs)->Separators)
        ATok->setASTReference(Ref);
    }
  }

  void addNameQualifier(AnnotatedToken *NameTok, ASTElement *Ref) {
    NameSegments.push_back(AnnotatedTokenRef(NameTok, Ref));
  }

  void makeTemplateArgs() {
    TemplateArgs = std::make_shared<TemplateArguments>();
  }
  void addTemplateSeparator(AnnotatedToken *ATok, ASTElement *Ref) {
    (*TemplateArgs)->Separators.push_back(AnnotatedTokenRef(ATok, Ref));
  }
  void addTemplateArgument(std::unique_ptr<Type> T) {
    (*TemplateArgs)->Args.push_back(TypeOrExpression(std::move(T)));
  }
  void addTemplateArgument(std::unique_ptr<Expr> E) {
    (*TemplateArgs)->Args.push_back(TypeOrExpression(std::move(E)));
  }
};

// A variable name or function name inside an expression.
class DeclRefExpr : public Expr {
public:
  QualifiedID Qualifier;

  DeclRefExpr() : Expr(DeclRefExprClass) {}

  void addNameQualifier(AnnotatedToken *NameTok) {
    Qualifier.addNameQualifier(NameTok, this);
  }
  void makeTemplateArgs() {
    Qualifier.makeTemplateArgs();
  }
  void addTemplateSeparator(AnnotatedToken *ATok) {
    Qualifier.addTemplateSeparator(ATok, this);
  }
  void addTemplateArgument(std::unique_ptr<Type> T) {
    Qualifier.addTemplateArgument(std::move(T));
  }
  void addTemplateArgument(std::unique_ptr<Expr> E) {
    Qualifier.addTemplateArgument(std::move(E));
  }

  static bool classof(const ASTElement *T) {
    return T->getASTClass() == DeclRefExprClass;
  }
};

/// Int, char or string literals
class LiteralConstant : public Expr {
public:
  AnnotatedTokenRef Tok;
  LiteralConstant(AnnotatedToken *Tok)
      : Expr(LiteralConstantClass), Tok(Tok, this) {
    Tok->setASTReference(this);
  }

  static bool classof(const ASTElement *T) {
    return T->getASTClass() == LiteralConstantClass;
  }
};

/// Any unary operator, even the overloaded ones.
class UnaryOperator : public Expr {
public:
  AnnotatedTokenRef OperatorTok;
  std::unique_ptr<Expr> Value;

  UnaryOperator(AnnotatedToken *OperatorTok, std::unique_ptr<Expr> Value)
      : Expr(UnaryOperatorClass), OperatorTok(OperatorTok, this),
        Value(std::move(Value)) {
    OperatorTok->setASTReference(this);
  }

  static bool classof(const ASTElement *T) {
    return T->getASTClass() == UnaryOperatorClass;
  }
};

/// Used to store any kind of binary operators, even the overloaded ones.
class BinaryOperator : public Expr {
  enum {
    LHS,
    RHS,
    END_EXPR
  };
  std::unique_ptr<Expr> SubExprs[END_EXPR];

public:
  AnnotatedTokenRef OperatorTok;

  BinaryOperator(std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs,
                 AnnotatedToken *OperatorTok)
      : Expr(BinaryOperatorClass), OperatorTok(OperatorTok, this) {
    SubExprs[LHS] = std::move(lhs);
    SubExprs[RHS] = std::move(rhs);
  }

  static bool classof(const ASTElement *T) {
    return T->getASTClass() == BinaryOperatorClass;
  }

  Expr *getLHS() { return cast<Expr>(SubExprs[LHS].get()); }
  const Expr *getLHS() const { return cast<Expr>(SubExprs[LHS].get()); }
  Expr *getRHS() { return cast<Expr>(SubExprs[RHS].get()); }
  const Expr *getRHS() const { return cast<Expr>(SubExprs[RHS].get()); }
};

/// Function calls
class CallExpr : public Expr {
public:
  std::unique_ptr<DeclRefExpr> FunctionName;
  enum {
    LEFT,
    RIGHT,
    END_EXPR
  };
  AnnotatedTokenRef Parens[END_EXPR];
  llvm::SmallVector<std::unique_ptr<Expr>, 4> Args;
  llvm::SmallVector<AnnotatedTokenRef, 3> Commas;

  CallExpr(std::unique_ptr<DeclRefExpr> FunctionName)
      : Expr(CallExprClass), FunctionName(std::move(FunctionName)) {}

  void setParen(int Index, AnnotatedToken *AT) {
    Parens[Index] = AnnotatedTokenRef(AT, this);
  }
  void setLeftParen(AnnotatedToken *AT) { setParen(LEFT, AT); }
  void setRightParen(AnnotatedToken *AT) { setParen(RIGHT, AT); }

  void appendComma(AnnotatedToken *AT) {
    Commas.push_back(AnnotatedTokenRef(AT, this));
  }

  static bool classof(const ASTElement *T) {
    return T->getASTClass() == CallExprClass;
  }
};

/// In contrast to the clang AST, a Stmt is a real statement, that is either a
/// CompoundStmt or a LineStmt.
class Stmt : public ASTElement {
public:
  virtual ~Stmt() = 0; // Not optimized

  Stmt(ASTElementClass SC) : ASTElement(SC) {}
};
inline Stmt::~Stmt() {}

struct UnparsableBlock : Stmt {
  UnparsableBlock() : Stmt(UnparsableBlockClass) {}
  void push_back(AnnotatedToken *Tok) {
    Body.push_back(AnnotatedTokenRef(Tok, this));
  }
  llvm::SmallVector<AnnotatedTokenRef, 8> Body;

  static bool classof(const ASTElement *T) {
    return T->getASTClass() == UnparsableBlockClass;
  }
};

template <typename Iter, typename Value> class IndirectRange {
public:
  IndirectRange(Iter First, Iter Last) : First(First), Last(Last) {}
  struct IndirectIter {
    IndirectIter(Iter Pos) : Pos(Pos) {}
    Iter Pos;
    friend bool operator==(IndirectIter LHS, IndirectIter RHS) {
      return LHS.Pos == RHS.Pos;
    }
    IndirectIter operator++() {
      ++Pos;
      return *this;
    }
    IndirectIter operator++(int) {
      auto Self = *this;
      ++*this;
      return Self;
    }
    Value &operator*() { return **Pos; }
  };

  IndirectIter begin() { return First; }
  IndirectIter end() { return Last; }

private:
  IndirectIter First, Last;
};

class Expr;

/// By a semicolon terminated statement
class LineStmt : public Stmt {
  AnnotatedTokenRef Semi;

protected:
  LineStmt(ASTElementClass SC, AnnotatedToken *Semi)
      : Stmt(SC), Semi(Semi, this) {}
  LineStmt(ASTElementClass SC, nullptr_t) : Stmt(SC), Semi(nullptr) {}
};

/// An expression terminated by a semicolon
struct ExprLineStmt : LineStmt {
  ExprLineStmt(std::unique_ptr<Expr> Body, AnnotatedToken *Semi)
      : LineStmt(ExprLineStmtClass, Semi), Body(std::move(Body)) {}

  std::unique_ptr<Expr> Body;

  static bool classof(const ASTElement *T) {
    return T->getASTClass() == ExprLineStmtClass;
  }
};

struct ReturnStmt : LineStmt {
  ReturnStmt(AnnotatedToken *Return, std::unique_ptr<Expr> Body,
             AnnotatedToken *Semi)
      : LineStmt(ReturnStmtClass, Semi), Body(std::move(Body)),
        Return(Return, this) {}

  std::unique_ptr<Expr> Body;
  AnnotatedTokenRef Return;

  static bool classof(const ASTElement *T) {
    return T->getASTClass() == ReturnStmtClass;
  }
};

/// A {}-Block with Statements inside.
class CompoundStmt : public Stmt {
public:
  llvm::SmallVector<std::unique_ptr<Stmt>, 8> Body;
  using child_range = IndirectRange<
      llvm::SmallVector<std::unique_ptr<Stmt>, 8>::iterator, Stmt>;

  enum {
    LBR,
    RBR,
    END_EXPR
  };
  AnnotatedTokenRef Brackets[END_EXPR];

  CompoundStmt(AnnotatedToken *lbr, AnnotatedToken *rbr)
      : Stmt(CompoundStmtClass) {
    setBracket(LBR, lbr);
    setBracket(RBR, rbr);
  }

  void setBracket(int BracIdx, AnnotatedToken *Tok) {
    assert(0 <= BracIdx && BracIdx < END_EXPR);
    Brackets[BracIdx] = AnnotatedTokenRef(Tok, this);
  }

  void addStmt(std::unique_ptr<Stmt> Statement) {
    Body.push_back(std::move(Statement));
  }

  child_range children() { return child_range(Body.begin(), Body.end()); }

  static bool classof(const ASTElement *T) {
    return T->getASTClass() == CompoundStmtClass;
  }
};

/// A Type with it's decorations.
struct Type : ASTElement {
  Type() : ASTElement(TypeClass) {}

  struct Decoration : ASTElement {
    enum DecorationClass {
      Pointer,
      Reference,
    };
    Decoration(DecorationClass Class, AnnotatedToken *Tok)
        : ASTElement(TypeDecorationClass), Class(Class), Tok(Tok) {}
    DecorationClass Class;
    AnnotatedToken *Tok;

    void fix() { Tok->setASTReference(this); }

    static bool classof(const ASTElement *T) {
      return T->getASTClass() == TypeDecorationClass;
    }
  };

  llvm::SmallVector<Decoration, 1> Decorations;
  QualifiedID Qualifier;

  void addDecoration(Decoration Dec) {
    auto *OldLoc = Decorations.empty() ? nullptr : &Decorations.front();
    Decorations.push_back(Dec);
    if (OldLoc != &Decorations.front())
      for (auto &D : Decorations)
        D.fix();
  }

  void addNameQualifier(AnnotatedToken *NameTok) {
    Qualifier.addNameQualifier(NameTok, this);
  }
  void makeTemplateArgs() {
    Qualifier.makeTemplateArgs();
  }
  void addTemplateSeparator(AnnotatedToken *ATok) {
    Qualifier.addTemplateSeparator(ATok, this);
  }
  void addTemplateArgument(std::unique_ptr<Type> T) {
    Qualifier.addTemplateArgument(std::move(T));
  }
  void addTemplateArgument(std::unique_ptr<Expr> E) {
    Qualifier.addTemplateArgument(std::move(E));
  }
  static bool classof(const ASTElement *T) {
    return T->getASTClass() == TypeClass;
  }

  std::unique_ptr<Type> cloneWithoutDecorations() {
    auto Clone = llvm::make_unique<Type>();
    Clone->Qualifier = Qualifier;
    Clone->Qualifier.reown(Clone.get());
    return Clone;
  }
};

inline QualifiedID::TypeOrExpression::TypeOrExpression(std::unique_ptr<Type> T)
    : Ptr(T.release()) {}

inline QualifiedID::TypeOrExpression::~TypeOrExpression() {
  if (Ptr) {
    if (Type *T = dyn_cast<Type>(Ptr))
      delete T;
    else
      delete cast<Expr>(Ptr);
  }
}

/// Initialization of a variable
struct VarInitialization : ASTElement {
  enum InitializationType {
    NONE = 0,
    ASSIGNMENT,
    CONSTRUCTOR,
    BRACE,
  };
  VarInitialization() : ASTElement(VarInitializationClass), InitType(NONE) {}

  void setAssignmentOps(InitializationType InitType,
                        AnnotatedToken AssignmentOps[2]) {
    this->InitType = ASSIGNMENT;
    if (InitType == ASSIGNMENT) {
      this->AssignmentOps[0] = AnnotatedTokenRef(&AssignmentOps[0], this);
      this->AssignmentOps[1] = AnnotatedTokenRef(nullptr);
    } else {
      this->AssignmentOps[0] = AnnotatedTokenRef(&AssignmentOps[0], this);
      this->AssignmentOps[1] = AnnotatedTokenRef(&AssignmentOps[1], this);
    }
  }

  InitializationType InitType;
  AnnotatedTokenRef AssignmentOps[2]; // '=' or '('+')' or '{'+'}'
  std::unique_ptr<Expr> Value;

  static bool classof(const ASTElement *T) {
    return T->getASTClass() == VarInitializationClass;
  }
};

/// Declaration of a variable with optional initialization
struct VarDecl : ASTElement {
  VarDecl() : ASTElement(VarDeclClass) {}

  void setName(AnnotatedToken *Tok) {
    this->NameTok = AnnotatedTokenRef(Tok, this);
  }

  std::unique_ptr<Type> VariableType;
  AnnotatedTokenRef NameTok;
  llvm::Optional<VarInitialization> Value;
};

/// Only for variable declarations (for now)
struct DeclStmt : LineStmt {
  llvm::SmallVector<std::unique_ptr<VarDecl>, 2> Decls;
  llvm::SmallVector<AnnotatedTokenRef, 1> Commas;

  DeclStmt() : LineStmt(DeclStmtClass, nullptr) {}

  void appendComma(AnnotatedToken *Tok) {
    Commas.push_back(AnnotatedTokenRef(Tok, this));
  }

  static bool classof(const ASTElement *T) {
    return T->getASTClass() == DeclStmtClass;
  }
};

// =============================================================================
// Toplevel Declarations

// struct RecordDecl : Stmt { // can also be a statement inside functions
//   llvm::SmallVector<Stmt> Body;
//   enum { RECORD, NAME, LEFT_BR, RIGHT_BR, END_EXPR };
//   AnnotatedTokenRef Toks[END_EXPR];
// };

struct FunctionDecl : Stmt { // TODO: Not a real statement
  FunctionDecl() : Stmt(FunctionDeclClass) {}
  enum {
    NAME,
    STATIC,
    SEMI,
    LEFT,
    RIGHT,
    END_EXPR
  };
  AnnotatedTokenRef Refs[END_EXPR];
  llvm::SmallVector<std::unique_ptr<VarDecl>, 4> Params;
  llvm::SmallVector<AnnotatedTokenRef, 3> Commas;

  void appendComma(AnnotatedToken *AT) {
    Commas.push_back(AnnotatedTokenRef(AT, this));
  }

  std::unique_ptr<Type> ReturnType;

  void setRef(int Index, AnnotatedToken *Tok) {
    Refs[Index] = AnnotatedTokenRef(Tok, this);
  }
  void setLeftParen(AnnotatedToken *Tok) { setRef(LEFT, Tok); }
  void setRightParen(AnnotatedToken *Tok) { setRef(LEFT, Tok); }
  void setStatic(AnnotatedToken *Tok) { setRef(STATIC, Tok); }
  void setName(AnnotatedToken *Tok) { setRef(NAME, Tok); }
  void setSemicolon(AnnotatedToken *Tok) { setRef(SEMI, Tok); }

  std::unique_ptr<CompoundStmt> Body;

  static bool classof(const ASTElement *T) {
    return T->getASTClass() == FunctionDeclClass;
  }
};

llvm::SmallVector<std::unique_ptr<Stmt>, 8> fuzzyparse(AnnotatedToken *first,
                                                       AnnotatedToken *last);

void printAST(llvm::raw_ostream& OS, const Stmt &Root, const SourceManager &SourceMgr);

} // end namespace fuzzy
} // end namespace clang

#endif // LLVM_CLANG_TOOLS_CLANG_HIGHLIGHT_FUZZY_AST_H
