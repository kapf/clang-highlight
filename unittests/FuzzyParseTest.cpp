//===- unittests/FuzzyParseTest.cpp - fuzzy parsing unit tests ------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/Debug.h"
#include "clang/Lex/Lexer.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceManager.h"
#include "llvm/Config/config.h"
#include "gtest/gtest.h"
#include "../fuzzy/FuzzyAST.h"

#define DEBUG_TYPE "highlight-test"

namespace clang {
namespace fuzzy {

class ClassOfTester {
  bool (*FunPtr)(const ASTElement *);

public:
  ClassOfTester(bool (*FunPtr)(const ASTElement *)) : FunPtr(FunPtr) {}
  bool verify(const ASTElement *AE) { return FunPtr(AE); }
};
template <typename T> ClassOfTester makeClassOfTester() {
  return ClassOfTester(&T::classof);
}

template <typename... T> llvm::SmallVector<ClassOfTester, 8> checkTypeSeq() {
  ClassOfTester Seq[] = { makeClassOfTester<T>()... };
  llvm::SmallVector<ClassOfTester, 8> Ret(Seq, Seq + sizeof...(T));
  return Ret;
}

LangOptions getFormattingLangOpts(bool Cpp03 = false) {
  LangOptions LangOpts;
  LangOpts.CPlusPlus = 1;
  LangOpts.CPlusPlus11 = Cpp03 ? 0 : 1;
  LangOpts.CPlusPlus1y = Cpp03 ? 0 : 1;
  LangOpts.LineComment = 1;
  LangOpts.Bool = 1;
  LangOpts.ObjC1 = 1;
  LangOpts.ObjC2 = 1;
  return LangOpts;
}

class FuzzyParseTest : public ::testing::Test {
protected:
  void checkParse(StringRef Code,
                  llvm::SmallVector<ClassOfTester, 8> TokenTypes) {
    using namespace llvm;
    using namespace clang;

    StringRef FileName = "";
    FileManager Files((FileSystemOptions()));
    DiagnosticsEngine Diagnostics(
        IntrusiveRefCntPtr<DiagnosticIDs>(new DiagnosticIDs),
        new DiagnosticOptions);
    SourceManager SourceMgr(Diagnostics, Files);
    llvm::MemoryBuffer *Buf = llvm::MemoryBuffer::getMemBuffer(Code, FileName);
    FileID ID = SourceMgr.createFileID(Buf);
    auto Langs = getFormattingLangOpts();
    Lexer Lex(ID, SourceMgr.getBuffer(ID), SourceMgr, Langs);
    Lex.SetKeepWhitespaceMode(true);

    IdentifierTable IdentTable(getFormattingLangOpts());

    std::vector<fuzzy::AnnotatedToken> AllTokens;

    for (;;) {
      Token TmpTok;
      Lex.LexFromRawLexer(TmpTok);
      AllTokens.push_back(fuzzy::AnnotatedToken(TmpTok));
      Token &ThisTok = AllTokens.back().Tok;

      StringRef TokenText(SourceMgr.getCharacterData(ThisTok.getLocation()),
                          ThisTok.getLength());

      if (ThisTok.is(tok::raw_identifier)) {
        IdentifierInfo &Info = IdentTable.get(TokenText);
        ThisTok.setIdentifierInfo(&Info);
        ThisTok.setKind(Info.getTokenID());
      }
      AllTokens.back().Text = AllTokens.back().getText(SourceMgr);

      if (ThisTok.is(tok::eof))
        break;
    }

    auto x = fuzzy::fuzzyparse(&*AllTokens.begin(), &*AllTokens.end());

    size_t NonWhitespaceTokens = 0;
    for (auto &Tok : AllTokens)
      if (Tok.Tok.getKind() != tok::comment &&
          Tok.Tok.getKind() != tok::unknown && Tok.Tok.getKind() != tok::eof)
        ++NonWhitespaceTokens;

    EXPECT_EQ(NonWhitespaceTokens, TokenTypes.size());
    for (size_t I = 0, J = 0; I < TokenTypes.size(); ++I, ++J) {
      while (AllTokens[J].Tok.getKind() == tok::comment ||
             AllTokens[J].Tok.getKind() == tok::unknown ||
             AllTokens[J].Tok.getKind() == tok::eof)
        ++J;
      if (!TokenTypes[I].verify(AllTokens[J].ASTReference)) {
        llvm::dbgs() << "Parsed " << Code << " into:\n";
        for (auto &E : x)
          printAST(llvm::dbgs(), *E, SourceMgr);
        ASSERT_TRUE(TokenTypes[I].verify(AllTokens[J].ASTReference));
      }
    }
  }
};

TEST_F(FuzzyParseTest, AssignmentTest) {
  checkParse("int i;", checkTypeSeq<Type, VarDecl, DeclStmt>());
  checkParse("int i=5;", checkTypeSeq<Type, VarDecl, VarInitialization,
                                      LiteralConstant, DeclStmt>());
  checkParse("int i=5,j;",
             checkTypeSeq<Type, VarDecl, VarInitialization, LiteralConstant,
                          DeclStmt, VarDecl, DeclStmt>());
  checkParse(
      "int i=5,j=i;",
      checkTypeSeq<Type, VarDecl, VarInitialization, LiteralConstant, DeclStmt,
                   VarDecl, VarInitialization, DeclRefExpr, DeclStmt>());
  checkParse(
      "int i,j,k,l,m,n,o,p;",
      checkTypeSeq<Type, VarDecl, DeclStmt, VarDecl, DeclStmt, VarDecl,
                   DeclStmt, VarDecl, DeclStmt, VarDecl, DeclStmt, VarDecl,
                   DeclStmt, VarDecl, DeclStmt, VarDecl, DeclStmt>());

  checkParse("int 1=2;",
             checkTypeSeq<UnparsableBlock, UnparsableBlock, UnparsableBlock,
                          UnparsableBlock, UnparsableBlock>());
}

} // end namespace fuzzy
} // end namespace clang
