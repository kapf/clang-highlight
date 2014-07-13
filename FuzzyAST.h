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
#include "clang/Basic/SourceManager.h"
#include "AnnotatedToken.h"
#include <memory>

using namespace clang;

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
    ExprLineStmtClass,
    ReturnStmtClass,
    CompoundStmtClass,
    DeclStmtClass,
    DeclRefExprClass,
    LiteralConstantClass,
    UnaryOperatorClass,
    BinaryOperatorClass,
    CallExprClass,
    FunctionDeclClass,
  };

  ASTElement(ASTElementClass SC) : sClass(SC) {}

  ASTElementClass getASTClass() const { return sClass; }

private:
  ASTElementClass sClass;
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
        : ASTElement(TypeDecorationClass), Class(Class), Tok(Tok, this) {}
    DecorationClass Class;
    AnnotatedTokenRef Tok;

    void fix() { Tok = AnnotatedTokenRef(Tok, this); }

    static bool classof(const ASTElement *T) {
      return T->getASTClass() == TypeDecorationClass;
    }
  };
  llvm::SmallVector<Decoration, 1> Decorations;
  AnnotatedTokenRef NameTok;

  void setName(AnnotatedToken *NameTok) {
    this->NameTok = AnnotatedTokenRef(NameTok, this);
  }

  static bool classof(const ASTElement *T) {
    return T->getASTClass() == TypeClass;
  }
};

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

/// An expression in it's classical sense.  If an expression is used as a
/// statement, it has to be embedded into a ExprStmt (yet to be implemented).
/// Rationale is that there is otherwise no way to store the semicolon.
struct Expr : ASTElement {
  Expr(ASTElementClass SC) : ASTElement(SC) {}
  virtual ~Expr() = 0;
};
inline Expr::~Expr() {}

// A variable name or function name inside an expression.
class DeclRefExpr : public Expr {
public:
  AnnotatedTokenRef Tok;
  DeclRefExpr(AnnotatedToken *Tok) : Expr(DeclRefExprClass), Tok(Tok, this) {
    Tok->setASTReference(this);
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

void printAST(const Stmt &Root, const SourceManager &SourceMgr);

} // end namespace fuzzy
} // end namespace clang

#endif // LLVM_CLANG_TOOLS_CLANG_HIGHLIGHT_FUZZY_AST_H
