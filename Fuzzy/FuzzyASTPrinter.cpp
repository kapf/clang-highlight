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

using namespace llvm;

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
  raw_ostream &OS;

  void print(Indented Indent, const Type &T);
  void print(Indented Indent, const VarDecl &DCL);
  void print(Indented Indent, const Expr &EXP);
  void print(Indented Indent, const Stmt &stmt);
  void print(Indented Indent, const QualifiedID &Qual);
  void print(Indented Indent, const PPDirective &Qual);
  void printScope(Indented Indent, const Scope &Sc);
  void printCondition(Indented Indent, const char *Name, ASTElement *E);
};
} // end anonymous namespace

void ASTPrinter::printScope(Indented Indent, const Scope &Sc) {
  OS << "{\n";
  for (auto &S : Sc.children())
    print(Indent.next(), S);
  OS << Indent << "}\n";
}

void ASTPrinter::printCondition(Indented Indent, const char *Name,
                                ASTElement *E) {
  OS << Indent.next() << Name << (E ? "\n" : ": <empty>\n");
  if (E) {
    if (auto *D = dyn_cast<DeclStmt>(E))
      print(Indent.next().next(), *D);
    else if (auto *V = dyn_cast<VarDecl>(E))
      print(Indent.next().next(), *V);
    else if (auto *U = dyn_cast<UnparsableBlock>(E))
      print(Indent.next().next(), *U);
    else
      print(Indent.next().next(), *cast<Expr>(E));
  }
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
  if (auto *BinOp = dyn_cast<BinaryOperator>(&EXP)) {
    print(Indent.next(), *BinOp->getLHS());
    OS << Indent << tok::getTokenName(BinOp->OperatorTok->getTokenKind())
       << '\n';
    print(Indent.next(), *BinOp->getRHS());
  } else if (auto *Decl = dyn_cast<DeclRefExpr>(&EXP)) {
    OS << Indent << "DeclRefExpr '";
    print(Indent.next(), Decl->Qualifier);
    OS << "'\n";
  } else if (auto *Lit = dyn_cast<LiteralConstant>(&EXP)) {
    OS << Indent << Lit->Tok->getText(SourceMgr) << '\n';
  } else if (auto *Call = dyn_cast<CallExpr>(&EXP)) {
    OS << Indent << "call expr '";
    print(Indent.next(), Call->Qualifier);
    OS << "'\n";
    for (auto &Arg : Call->Args)
      print(Indent.next(), *Arg);
  } else if (auto *Unar = dyn_cast<UnaryOperator>(&EXP)) {
    OS << Indent << Unar->OperatorTok->getText(SourceMgr) << "\n";
    print(Indent.next(), *Unar->Value);
  } else if (auto *PE = dyn_cast<ParenExpr>(&EXP)) {
    OS << Indent << "ParenExpr:\n";
    print(Indent.next(), *PE->Value);
  } else {
    llvm_unreachable("TODO: unhandled fuzzy ast node of type Expr");
  }
}

void ASTPrinter::print(Indented Indent, const Stmt &stmt) {
  if (auto *DS = dyn_cast<DeclStmt>(&stmt)) {
    OS << Indent << "DeclStmt\n";
    for (const auto &VD : DS->Decls)
      print(Indent.next(), *VD);
  } else if (auto *UB = dyn_cast<UnparsableBlock>(&stmt)) {
    (void)UB;
    OS << Indent << "Unparsable Block:\n";
    for (auto T : UB->Body)
      OS << Indent.next() << T->getText(SourceMgr) << '\n';
  } else if (auto *ELS = dyn_cast<ExprLineStmt>(&stmt)) {
    OS << Indent << "ExprLineStmt\n";
    print(Indent.next(), *ELS->Body);
  } else if (auto *RS = dyn_cast<ReturnStmt>(&stmt)) {
    OS << Indent << "ReturnStmt\n";
    if (RS->Body)
      print(Indent.next(), *RS->Body);
    else
      OS << Indent.next() << "<void>\n";
  } else if (auto *FD = dyn_cast<FunctionDecl>(&stmt)) {
    OS << Indent << "FunctionDecl '";
    print(Indent.next().next(), FD->Name);
    OS << "'\n" << Indent.next() << "Body:\n";
    if (FD->Body)
      print(Indent.next().next(), *FD->Body);
  } else if (auto *CD = dyn_cast<ClassDecl>(&stmt)) {
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
  } else if (auto *LBL = dyn_cast<LabelStmt>(&stmt)) {
    OS << Indent << "Label '" << LBL->LabelName->getText(SourceMgr) << "'\n";
  } else if (auto *NS = dyn_cast<NamespaceDecl>(&stmt)) {
    OS << Indent << "Namespace '"
       << (NS->Refs[NamespaceDecl::NAME]
               ? NS->Refs[NamespaceDecl::NAME]->getText(SourceMgr)
               : "<anonymous>") << '\'';
    printScope(Indent, *NS);
  } else if (auto TD = dyn_cast<TemplateDecl>(&stmt)) {
    OS << Indent << "Template <'\n";
    for (auto &A : TD->Params) {
      if (auto *E = dyn_cast<Expr>(A.get()))
        print(Indent.next().next(), *E);
      else if (auto *VD = dyn_cast<VarDecl>(A.get()))
        print(Indent.next().next(), *VD);
      else
        print(Indent.next().next(), *static_cast<Stmt *>(A.get()));
    }
    OS << Indent.next() << "> with Body:\n";
    print(Indent.next().next(), *TD->Templated);
  } else if (auto *If = dyn_cast<IfStmt>(&stmt)) {
    OS << Indent << "If\n";
    for (auto &B : If->Branches) {
      printCondition(Indent, "Condition", B.Cond.get());
      OS << Indent.next() << "Body:\n";
      print(Indent.next().next(), *B.Body);
    }
  } else if (auto *CS = dyn_cast<CompoundStmt>(&stmt)) {
    OS << Indent << "CompoundStmt:\n";
    for (auto &S : CS->Body)
      print(Indent.next(), *S);
  } else if (auto *While = dyn_cast<WhileStmt>(&stmt)) {
    OS << Indent << "WhileStmt:\n";
    printCondition(Indent, "Condition", While->Cond.get());
    OS << Indent.next() << "Body:\n";
    print(Indent.next().next(), *While->Body);
  } else if (auto *For = dyn_cast<ForStmt>(&stmt)) {
    OS << Indent << "ForStmt:\n";
    printCondition(Indent, "Init", For->Init.get());
    printCondition(Indent, "Condition", For->Cond.get());
    printCondition(Indent, "Incr", For->Inc.get());
    OS << Indent.next() << "Body:\n";
    print(Indent.next().next(), *For->Body);
  } else {
    llvm_unreachable("TODO: unhandled fuzzy ast node");
  }
}

void ASTPrinter::print(Indented Indent, const PPDirective &PP) {
  if (auto *Inc = dyn_cast<PPInclude>(&PP)) {
    OS << Indent << "Include Directive: '";
    if (Inc->Path)
      for (auto &S : Inc->Path->Refs)
        OS << S->getText(SourceMgr);
    OS << "'\n";
  } else if (auto *If = dyn_cast<PPIf>(&PP)) {
    OS << Indent << "Preprocessor '"
       << If->Refs[PPIf::KEYWORD]->getText(SourceMgr) << "':\n";
    if (If->Cond) {
      if (auto *E = dyn_cast<Expr>(If->Cond.get()))
        print(Indent.next(), *E);
      else
        print(Indent.next(), *cast<UnparsableBlock>(If->Cond.get()));
    }
  } else if (auto *UP = dyn_cast<UnparsablePP>(&PP)) {
    OS << Indent << "Unparsable PP:\n";
    for (auto R : UP->Refs)
      OS << Indent.next() << R->getText(SourceMgr) << '\n';
  } else {
    llvm_unreachable("TODO: unhandled preprocessor directive");
  }
}

void printAST(raw_ostream &OS, const Stmt &Root,
              const SourceManager &SourceMgr) {
  ASTPrinter AP{ SourceMgr, OS };
  AP.print(Indented(0), Root);
}

void printAST(raw_ostream &OS, const TranslationUnit &TU,
              const SourceManager &SourceMgr) {
  ASTPrinter AP{ SourceMgr, OS };
  for (auto &P : TU.PPDirectives) {
    assert(P);
    AP.print(Indented(0), *P);
  }
  for (auto &S : TU.Body) {
    assert(S);
    AP.print(Indented(0), *S);
  }
}

} // end namespace fuzzy
} // end namespace clang
