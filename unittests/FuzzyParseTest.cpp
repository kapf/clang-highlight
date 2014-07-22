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
#include "Fuzzy/FuzzyAST.h"

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
  struct ParseResult {
    TranslationUnit TU;
    std::vector<AnnotatedToken> Tokens;
    static constexpr const char *FileName = "";
    FileManager Files;
    DiagnosticsEngine Diagnostics;
    SourceManager SourceMgr;
    FileID ID;
    Lexer Lex;
    IdentifierTable IdentTable;

    ParseResult(StringRef Code)
        : Files((FileSystemOptions())),
          Diagnostics(IntrusiveRefCntPtr<DiagnosticIDs>(new DiagnosticIDs),
                      new DiagnosticOptions),
          SourceMgr(Diagnostics, Files),
          ID(SourceMgr.createFileID(
              llvm::MemoryBuffer::getMemBuffer(Code, FileName))),
          Lex(ID, SourceMgr.getBuffer(ID), SourceMgr, getFormattingLangOpts()),
          IdentTable(getFormattingLangOpts()) {
      Lex.SetKeepWhitespaceMode(true);

      for (;;) {
        Token TmpTok;
        Lex.LexFromRawLexer(TmpTok);
        Tokens.push_back(fuzzy::AnnotatedToken(TmpTok));
        Token &ThisTok = Tokens.back().Tok;

        StringRef TokenText(SourceMgr.getCharacterData(ThisTok.getLocation()),
                            ThisTok.getLength());

        if (ThisTok.is(tok::raw_identifier)) {
          IdentifierInfo &Info = IdentTable.get(TokenText);
          ThisTok.setIdentifierInfo(&Info);
          ThisTok.setKind(Info.getTokenID());
        }
        Tokens.back().Text = Tokens.back().getText(SourceMgr);

        if (ThisTok.is(tok::eof))
          break;
      }

      TU = fuzzy::fuzzyparse(&*Tokens.begin(), &*Tokens.end());
    }
  };

  void checkParse(StringRef Code,
                  llvm::SmallVector<ClassOfTester, 8> TokenTypes) {
    ParseResult Parsed(Code);
    auto &AllTokens = Parsed.Tokens;

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
        for (auto &S : Parsed.TU.children())
          printAST(llvm::dbgs(), S, Parsed.SourceMgr);
        llvm::dbgs() << "I=" << I << ", J=" << J << '\n';
        EXPECT_TRUE(TokenTypes[I].verify(AllTokens[J].ASTReference));
      }
    }
  }

  void checkUnparsable(StringRef Code) {
    ParseResult Parsed(Code);
    for (auto &Tok : Parsed.Tokens)
      if (Tok.Tok.getKind() != tok::comment &&
          Tok.Tok.getKind() != tok::unknown && Tok.Tok.getKind() != tok::eof)
        EXPECT_TRUE(llvm::isa<UnparsableBlock>(Tok.ASTReference));
  }

  void dump(ParseResult &Parsed, StringRef Code) {
    llvm::dbgs() << Code << '\n';

    llvm::dbgs() << "Parsed " << Code << " into:\n";
    for (auto &S : Parsed.TU.children())
      printAST(llvm::dbgs(), S, Parsed.SourceMgr);
  }

  template <typename T> void checkToplevel(StringRef Code) {
    ParseResult Parsed(Code);
    if (Parsed.TU.children().size() != 1 ||
        !llvm::isa<T>(Parsed.TU.Body[0].get())) {
      dump(Parsed, Code);
    }
    EXPECT_EQ(Parsed.TU.children().size(), size_t(1));
    EXPECT_TRUE(llvm::isa<T>(Parsed.TU.Body[0].get()));
  }

  template <typename F> void checkFirstOn(StringRef Code, F &&f) {
    ParseResult Parsed(Code);
    if (Parsed.TU.children().size() == 0) {
      dump(Parsed, Code);
      EXPECT_TRUE(Parsed.TU.children().size() > 0);
      return;
    }
    if (!f(*Parsed.TU.Body[0], false)) {
      dump(Parsed, Code);
      EXPECT_TRUE(f(*Parsed.TU.Body[0], true));
    }
  }

  template <typename T> void checkFirst(StringRef Code) {
    checkFirstOn(Code, [&](const Stmt &S, bool Abort) {
      if (Abort)
        EXPECT_TRUE(llvm::isa<T>(S));
      else
        return llvm::isa<T>(S);
      return true;
    });
  }
  template <typename T>
  void checkFirst(std::initializer_list<const char *> Codes) {
    for (const char *C : Codes)
      checkFirst<T>(C);
  }
};

TEST_F(FuzzyParseTest, DeclStmtTest) {
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

  checkParse("int *p;",
             checkTypeSeq<Type, Type::Decoration, VarDecl, DeclStmt>());
  checkParse("type &p;",
             checkTypeSeq<Type, Type::Decoration, VarDecl, DeclStmt>());

  checkParse(
      "int* p,* /*comment*/  **  *  *   q;",
      checkTypeSeq<Type, Type::Decoration, VarDecl, DeclStmt, Type::Decoration,
                   Type::Decoration, Type::Decoration, Type::Decoration,
                   Type::Decoration, VarDecl, DeclStmt>());

  checkParse(
      "a b=c,*d=e,********f=****g**h;",
      checkTypeSeq<Type, VarDecl, VarInitialization, DeclRefExpr, DeclStmt,
                   Type::Decoration, VarDecl, VarInitialization, DeclRefExpr,
                   DeclStmt, Type::Decoration, Type::Decoration,
                   Type::Decoration, Type::Decoration, Type::Decoration,
                   Type::Decoration, Type::Decoration, Type::Decoration,
                   VarDecl, VarInitialization, UnaryOperator, UnaryOperator,
                   UnaryOperator, UnaryOperator, DeclRefExpr, BinaryOperator,
                   UnaryOperator, DeclRefExpr, DeclStmt>());

  checkToplevel<DeclStmt>("a b;");
  checkToplevel<DeclStmt>("a b=c(d,e);");
  checkToplevel<DeclStmt>("a b=c(d,e,*g),*h=*i;");

  checkToplevel<DeclStmt>("int a;");
  checkToplevel<DeclStmt>("unsigned long long int a;");
  checkToplevel<DeclStmt>("signed char a;");
  checkToplevel<DeclStmt>("double a;");

  checkParse("register const volatile constexpr int i;",
             checkTypeSeq<Type, Type, Type, Type, Type, VarDecl, DeclStmt>());

  checkUnparsable("int 1=2;");
}

TEST_F(FuzzyParseTest, ExprLineStmtTest) {
  checkToplevel<ExprLineStmt>("a*b*c;");
  checkToplevel<ExprLineStmt>("a*b*c=d;");
  checkToplevel<ExprLineStmt>("a*b*c==d;");
  checkToplevel<ExprLineStmt>("f();");
  checkToplevel<ExprLineStmt>("f(a,b,c);");
  checkToplevel<ExprLineStmt>("f(1,2,3);");
  checkUnparsable("1(a,b);");
  checkToplevel<ExprLineStmt>("f(1)*g;");
  checkToplevel<ExprLineStmt>("n::f(1)*g;");
  checkToplevel<ExprLineStmt>("a+b;");
  checkToplevel<ExprLineStmt>("a-b;");
  checkToplevel<ExprLineStmt>("a*b*c;");
  checkToplevel<ExprLineStmt>("a/b;");
  checkToplevel<ExprLineStmt>("a&b&c;");
  checkToplevel<ExprLineStmt>("a^b;");
  checkToplevel<ExprLineStmt>("a|b;");
  checkToplevel<ExprLineStmt>("a<<b;");
  checkToplevel<ExprLineStmt>("a>>b;");
  checkToplevel<ExprLineStmt>("a<b;");
  checkToplevel<ExprLineStmt>("a>b;");
  checkToplevel<ExprLineStmt>("~a;");
  checkToplevel<ExprLineStmt>("!a;");
  checkToplevel<ExprLineStmt>("-a;");
  checkToplevel<ExprLineStmt>("--a;");
  checkToplevel<ExprLineStmt>("++a;");
  checkToplevel<ExprLineStmt>("++++~~~+~!~++++++!--++++++a;");
  checkToplevel<ExprLineStmt>("\"string literal\";");
  checkToplevel<ExprLineStmt>("nullptr;");
  checkToplevel<ExprLineStmt>("true;");
  checkToplevel<ExprLineStmt>("false;");
  checkToplevel<ExprLineStmt>("-1;");
  checkToplevel<ExprLineStmt>("(1+-1)*(3+5);");
}

TEST_F(FuzzyParseTest, QualifiedIDs) {
  checkToplevel<DeclStmt>("std::vector<int> v;");
  checkToplevel<DeclStmt>("::std::vector v1;");
  checkToplevel<DeclStmt>("std::vector<int> v2;");
  checkToplevel<DeclStmt>("std::vector<int,int> v3;");
  checkToplevel<DeclStmt>("std::vector<> v4;");
  checkToplevel<DeclStmt>("std::vector<1> v5;");
  checkToplevel<DeclStmt>("std::tr1::stl::vector<> v6;");
  checkToplevel<DeclStmt>("::vector<> v7;");
  checkToplevel<DeclStmt>(
      "::std::tr1::stl::vector<std::vector<int>, ::std::pair<int,int> > v8;");
  checkToplevel<DeclStmt>("n::n::n::n::n::a<n::b<c<d<n::n::e,f>,g<h> > > > g;");
  checkToplevel<DeclStmt>("a::b<c::d> ***e=f::g<1>*h::i<2,j>(::k::l);");
  checkToplevel<DeclStmt>("auto x = llvm::make_unique<int>(0);");
  checkParse(
      "auto x = llvm::make_unique<int>(0);",
      checkTypeSeq<Type, VarDecl, VarInitialization, DeclRefExpr, DeclRefExpr,
                   DeclRefExpr, DeclRefExpr, Type, DeclRefExpr, CallExpr,
                   LiteralConstant, CallExpr, DeclStmt>());
  checkToplevel<ExprLineStmt>("n::f(a::b<x>());");
  checkToplevel<ExprLineStmt>("n::f<a,1,2>(a::b<2*3>());");
  checkToplevel<ExprLineStmt>("t<1+b>();");
  checkToplevel<ExprLineStmt>("t< 1<<2 >();");
  checkToplevel<ExprLineStmt>("t< (1>2) >();");
  checkUnparsable("t<1> 2>();");
}

TEST_F(FuzzyParseTest, FunctionDeclStmt) {
  const char *Tests[] = {
    "void f(int,int);", "void g(int i=0);",
    "static std::unique_ptr<VarDecl> parseVarDecl(TokenFilter &TF,"
    "                                             Type *TypeName = 0,"
    "                                             bool NameOptional = false);",
    "void dismiss() { TF = nullptr; }", "type func1();",
    "type func2() { 1+1; }", "type func3(type a) { 1+1; }",
    "type func4(type a, type b) { 1+1; }", "static type func5();",
    "static std::unique_ptr<Expr> parseExpression(TokenFilter &TF,"
    "                                             int Precedence,"
    "                                             bool StopAtGreater);",
    "static bool checkKind(TokenFilter &TF, tok::TokenKind Kind){}",
  };
  for (const char *Code : Tests)
    checkFirst<FunctionDecl>(Code);
}

TEST_F(FuzzyParseTest, ReturnStmt) {
  checkToplevel<ReturnStmt>("return 1;");
  checkToplevel<ReturnStmt>("return a*b;");
  checkUnparsable("return return;");
  checkToplevel<ReturnStmt>("return;");
}

TEST_F(FuzzyParseTest, StructDecl) {
  checkFirst<ClassDecl>(
      { "struct C;", "union C;", "class C{};", "class C{ ><unparsable>< };" });

  auto checkFirstIsFunctionDecl = [&](StringRef Code) {
    checkFirstOn(Code, [](const Stmt &S, bool Abort) {
      if (Abort)
        EXPECT_TRUE(llvm::isa<ClassDecl>(S));
      else if (!llvm::isa<ClassDecl>(S))
        return false;
      const auto &CD = llvm::cast<ClassDecl>(S);
      if (Abort)
        EXPECT_EQ(CD.Body.size(), (size_t)1);
      else if (CD.Body.size() != 1)
        return false;

      if (Abort)
        EXPECT_TRUE(llvm::isa<FunctionDecl>(*CD.Body.front()));
      else if (!llvm::isa<FunctionDecl>(*CD.Body.front()))
        return false;

      return true;
    });
  };

  checkFirstIsFunctionDecl("struct C { C(){} };");
  checkFirstIsFunctionDecl("struct C { ~C(){} };");
  checkFirstIsFunctionDecl("struct C { virtual void f() override =0; };");
  checkFirstIsFunctionDecl(
      "struct C { static constexpr bool g() { return true; } };");
  checkFirstIsFunctionDecl("struct C { C()=default; };");
  checkFirstIsFunctionDecl("struct C { bool operator<(int o); };");
  checkFirstIsFunctionDecl(
      "struct C { friend C operator==(C lhs, C rhs)=default; };");
}

} // end namespace fuzzy
} // end namespace clang
