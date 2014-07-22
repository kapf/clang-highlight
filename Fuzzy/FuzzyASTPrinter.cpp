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
  void print(Indented Indent, const QualifiedID &Qual);
  void printScope(Indented Indent, const Scope &Sc);
};
} // end anonymous namespace

void ASTPrinter::printScope(Indented Indent, const Scope &Sc) {
  OS << "{\n";
  for (auto &S : Sc.children())
    print(Indent.next(), S);
  OS << Indent << "}\n";
}

void ASTPrinter::print(Indented Indent, const QualifiedID &Qual) {
  for (auto &N : Qual.NameSegments) {
    OS << N->getText(SourceMgr);
  }
  if (Qual.TemplateArgs) {
    OS << "\n" << Indent << "<\n";
    for (auto &A : (*Qual.TemplateArgs)->Args) {
      if (A.isType())
        print(Indent.next(), A.asType());
      else
        print(Indent.next(), A.asExpr());
    }
    OS << Indent << '>';
  }
}

void ASTPrinter::print(Indented Indent, const Type &T) {
  OS << Indent << "Type ";
  for (auto &D : T.Decorations)
    OS << '\'' << D.Tok->getText(SourceMgr) << "' ";
  OS << '\'';
  print(Indent.next(), T.Qualifier);
  OS << "'\n";
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
    OS << Indent << "DeclRefExpr '";
    print(Indent.next(), Decl->Qualifier);
    OS << "'\n";
  } else if (auto *Lit = llvm::dyn_cast<LiteralConstant>(&EXP)) {
    OS << Indent << Lit->Tok->getText(SourceMgr) << '\n';
  } else if (auto *Call = llvm::dyn_cast<CallExpr>(&EXP)) {
    OS << Indent << "call expr '";
    print(Indent.next(), Call->FunctionName->Qualifier);
    OS << "'\n";
    for (auto &Arg : Call->Args)
      print(Indent.next(), *Arg);
  } else if (auto *Unar = llvm::dyn_cast<UnaryOperator>(&EXP)) {
    OS << Indent << Unar->OperatorTok->getText(SourceMgr) << "\n";
    print(Indent.next(), *Unar->Value);
  } else if (auto *PE = llvm::dyn_cast<ParenExpr>(&EXP)) {
    OS << Indent << "ParenExpr:\n";
    print(Indent.next(), *PE->Value);
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
    OS << Indent << "Unparsable Block:\n";
    for (auto T : UB->Body)
      OS << Indent.next() << T->getText(SourceMgr) << '\n';
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
  } else if (auto *CD = llvm::dyn_cast<ClassDecl>(&stmt)) {
    OS << Indent << '\'' << CD->Refs[ClassDecl::CLASS]->getText(SourceMgr)
       << "' ";
    print(Indent.next(), *CD->Name);
    if (!CD->BaseClasses.empty()) {
      OS << " derived from\n";
      for (auto &BC : CD->BaseClasses) {
        OS << Indent.next()
           << (BC.Accessibility ? BC.Accessibility->getText(SourceMgr)
                                : "<default accessibility>") << ' ';
        print(Indent.next().next(), *BC.T);
      }
    }
    if (!CD->hasScope())
      OS << " (declaration only)\n";
    else
      printScope(Indent, *CD);
  } else if (auto *LBL = llvm::dyn_cast<LabelStmt>(&stmt)) {
    OS << Indent << "Label '" << LBL->LabelName->getText(SourceMgr) << "'\n";
  } else if (auto *NS = llvm::dyn_cast<NamespaceDecl>(&stmt)) {
    OS << Indent << "Namespace '"
       << (NS->Refs[NamespaceDecl::NAME]
               ? NS->Refs[NamespaceDecl::NAME]->getText(SourceMgr)
               : "<anonymous>") << '\'';
    printScope(Indent, *NS);
  } else {
    llvm_unreachable("TODO: unhandled fuzzy ast node");
  }
}

void printAST(llvm::raw_ostream &OS, const Stmt &Root,
              const SourceManager &SourceMgr) {
  ASTPrinter AP{ SourceMgr, OS };
  AP.print(Indented(0), Root);
}

} // end namespace fuzzy
} // end namespace clang
