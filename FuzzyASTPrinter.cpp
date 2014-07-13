//===--- FuzzyParser.cpp - clang-highlight ----------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
#include "llvm/Support/raw_os_ostream.h"
#include "llvm/Support/Debug.h"
#include "llvm/ADT/STLExtras.h"
#include "FuzzyAST.h"

using namespace clang;

namespace clang {
namespace fuzzy {

namespace {
struct Indented {
  const int Indent;
  explicit Indented(int Indent) : Indent(Indent) {}
  friend raw_ostream &operator<<(raw_ostream &OS, Indented ID) {
    const int Total = 4 * ID.Indent;
    for (int i = 0; i < Total; ++i)
      OS.write(' ');
    return OS;
  }
  Indented next() { return Indented(Indent + 1); }
};
} // end anonymous namespace

namespace {
struct ASTPrinter {
  const SourceManager &SourceMgr;
  llvm::raw_ostream &OS;

  void print(Indented Indent, const Type &T);
  void print(Indented Indent, const VarDecl &DCL);
  void print(Indented Indent, const Expr &EXP);
  void print(Indented Indent, const Stmt &stmt);
};
} // end anonymous namespace

void ASTPrinter::print(Indented Indent, const Type &T) {
  OS << Indent << "Type ";
  for (auto &D : T.Decorations)
    OS << '\'' << D.Tok->getText(SourceMgr) << "' ";
  OS << '\'' << T.NameTok->getText(SourceMgr) << "'\n";
}

void ASTPrinter::print(Indented Indent, const VarDecl &DCL) {
  OS << Indent << "VarDecl '" << DCL.NameTok->getText(SourceMgr) << "'\n";
  print(Indent.next(), *DCL.VariableType);
  if (DCL.Value) {
    const char *InitName[] = { "?", "=", "()", "{}" };
    assert(1 <= DCL.Value->InitType && DCL.Value->InitType < 4);
    OS << Indent.next() << "Assignment Type '" << InitName[DCL.Value->InitType]
       << "'\n";
    assert(DCL.Value->Value);
    print(Indent.next(), *DCL.Value->Value);
  }
}

void ASTPrinter::print(Indented Indent, const Expr &EXP) {
  if (auto *BinOp = llvm::dyn_cast<BinaryOperator>(&EXP)) {
    print(Indent.next(), *BinOp->getLHS());
    OS << Indent << tok::getTokenName(BinOp->OperatorTok->Tok.getKind())
       << '\n';
    print(Indent.next(), *BinOp->getRHS());
  } else if (auto *Decl = llvm::dyn_cast<DeclRefExpr>(&EXP)) {
    OS << Indent << Decl->Tok->getText(SourceMgr) << '\n';
  } else if (auto *Lit = llvm::dyn_cast<LiteralConstant>(&EXP)) {
    OS << Indent << Lit->Tok->getText(SourceMgr) << '\n';
  } else if (auto *Call = llvm::dyn_cast<CallExpr>(&EXP)) {
    OS << Indent << "call expr '" << Call->FunctionName->Tok->getText(SourceMgr)
       << "'\n";
    for (auto &Arg : Call->Args)
      print(Indent.next(), *Arg);
  } else if (auto *Unar = llvm::dyn_cast<UnaryOperator>(&EXP)) {
    OS << Indent << Unar->OperatorTok->getText(SourceMgr) << "\n";
    print(Indent.next(), *Unar->Value);
  } else {
    llvm_unreachable("TODO: unhandled fuzzy ast node of type Expr");
  }
}

void ASTPrinter::print(Indented Indent, const Stmt &stmt) {
  if (auto *DS = llvm::dyn_cast<DeclStmt>(&stmt)) {
    OS << Indent << "DeclStmt\n";
    for (const auto &VD : DS->Decls)
      print(Indent.next(), *VD);
  } else if (auto *UB = llvm::dyn_cast<UnparsableBlock>(&stmt)) {
    (void)UB;
    OS << Indent << "<Unparsable Block>\n";
  } else if (auto *ELS = llvm::dyn_cast<ExprLineStmt>(&stmt)) {
    OS << Indent << "ExprLineStmt\n";
    print(Indent.next(), *ELS->Body);
  } else if (auto *RS = llvm::dyn_cast<ReturnStmt>(&stmt)) {
    OS << Indent << "ReturnStmt\n";
    if (RS->Body)
      print(Indent.next(), *RS->Body);
    else
      OS << Indent.next() << "<void>\n";
  } else if (auto *FD = llvm::dyn_cast<FunctionDecl>(&stmt)) {
    (void)FD;
    OS << Indent << "FunctionDecl\n";
  } else {
    llvm_unreachable("TODO: unhandled fuzzy ast node");
  }
}

void printAST(const Stmt &Root, const SourceManager &SourceMgr) {
  ASTPrinter AP{ SourceMgr, llvm::dbgs() };
  AP.print(Indented(0), Root);
}

} // end namespace fuzzy
} // end namespace clang
