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

using namespace llvm;

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

template <typename... T> SmallVector<ClassOfTester, 8> checkTypeSeq() {
  ClassOfTester Seq[] = { makeClassOfTester<T>()... };
  SmallVector<ClassOfTester, 8> Ret(Seq, Seq + sizeof...(T));
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
               MemoryBuffer::getMemBuffer(Code, FileName))),
          Lex(ID, SourceMgr.getBuffer(ID), SourceMgr, getFormattingLangOpts()),
          IdentTable(getFormattingLangOpts()) {
      Lex.SetKeepWhitespaceMode(true);

      for (;;) {
        Token TmpTok;
        Lex.LexFromRawLexer(TmpTok);

        if (TmpTok.getKind() == tok::hash && TmpTok.isAtStartOfLine())
          Lex.setParsingPreprocessorDirective(true);
        if (TmpTok.getKind() == tok::eod)
          Lex.setParsingPreprocessorDirective(false);

        Tokens.push_back(fuzzy::AnnotatedToken(TmpTok));
        Token &ThisTok = Tokens.back().Tok();

        StringRef TokenText(SourceMgr.getCharacterData(ThisTok.getLocation()),
                            ThisTok.getLength());

        if (ThisTok.is(tok::raw_identifier)) {
          IdentifierInfo &Info = IdentTable.get(TokenText);
          ThisTok.setIdentifierInfo(&Info);
          ThisTok.setKind(Info.getTokenID());
        }

        if (ThisTok.is(tok::eof))
          break;
      }

      TU = fuzzy::fuzzyparse(&*Tokens.begin(), &*Tokens.end());
    }
  };

  void checkParse(StringRef Code,
                  SmallVector<ClassOfTester, 8> TokenTypes) {
    ParseResult Parsed(Code);
    auto &AllTokens = Parsed.Tokens;

    size_t NonWhitespaceTokens = 0;
    for (auto &Tok : AllTokens)
      if (Tok.getTokenKind() != tok::comment &&
          Tok.getTokenKind() != tok::unknown && Tok.getTokenKind() != tok::eof)
        ++NonWhitespaceTokens;

    EXPECT_EQ(NonWhitespaceTokens, TokenTypes.size());
    for (size_t I = 0, J = 0; I < TokenTypes.size(); ++I, ++J) {
      while (AllTokens[J].getTokenKind() == tok::comment ||
             AllTokens[J].getTokenKind() == tok::unknown ||
             AllTokens[J].getTokenKind() == tok::eof)
        ++J;
      if (!TokenTypes[I].verify(AllTokens[J].getASTReference())) {
        dbgs() << "Parsed " << Code << " into:\n";
        for (auto &S : Parsed.TU.children())
          printAST(dbgs(), S, Parsed.SourceMgr);
        dbgs() << "I=" << I << ", J=" << J << '\n';
        EXPECT_TRUE(TokenTypes[I].verify(AllTokens[J].getASTReference()));
      }
    }
  }

  void checkUnparsable(StringRef Code) {
    ParseResult Parsed(Code);
    for (auto &Tok : Parsed.Tokens)
      if (Tok.getTokenKind() != tok::comment &&
          Tok.getTokenKind() != tok::unknown && Tok.getTokenKind() != tok::eof)
        EXPECT_TRUE(isa<UnparsableBlock>(Tok.getASTReference()));
  }
  void checkUnparsable(std::initializer_list<const char *> Codes) {
    for (const char *C : Codes)
      checkUnparsable(C);
  }

  void dump(ParseResult &Parsed, StringRef Code) {
    dbgs() << Code << '\n';

    dbgs() << "Parsed " << Code << " into:\n";
    for (auto &S : Parsed.TU.children())
      printAST(dbgs(), S, Parsed.SourceMgr);
  }

  template <typename T> void checkToplevel(StringRef Code) {
    ParseResult Parsed(Code);
    if (Parsed.TU.children().size() != 1 ||
        !isa<T>(Parsed.TU.Body[0].get())) {
      dump(Parsed, Code);
    }
    EXPECT_EQ(Parsed.TU.children().size(), size_t(1));
    EXPECT_TRUE(isa<T>(Parsed.TU.Body[0].get()));
  }

  template <typename T>
  void checkToplevel(std::initializer_list<const char *> Codes) {
    for (const char *C : Codes)
      checkToplevel<T>(C);
  }

  template <typename F> void checkFirstPPOn(StringRef Code, F &&f) {
    ParseResult Parsed(Code);
    if (Parsed.TU.PPDirectives.size() == 0) {
      dump(Parsed, Code);
      EXPECT_TRUE(Parsed.TU.PPDirectives.size() > 0);
      return;
    }
    if (!f(*Parsed.TU.PPDirectives[0], false)) {
      dump(Parsed, Code);
      EXPECT_TRUE(f(*Parsed.TU.PPDirectives[0], true));
    }
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
        EXPECT_TRUE(isa<T>(S));
      else
        return isa<T>(S);
      return true;
    });
  }
  template <typename T>
  void checkFirst(std::initializer_list<const char *> Codes) {
    for (const char *C : Codes)
      checkFirst<T>(C);
  }

  template <typename T> void checkFirstPP(StringRef Code) {
    checkFirstPPOn(Code, [&](const PPDirective &P, bool Abort) {
      if (Abort)
        EXPECT_TRUE(isa<T>(P));
      else
        return isa<T>(P);
      return true;
    });
  }
  template <typename T>
  void checkFirstPP(std::initializer_list<const char *> Codes) {
    for (const char *C : Codes)
      checkFirstPP<T>(C);
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

  checkToplevel<DeclStmt>({ "a b;",        //
                            "a b=c(d,e);", //
                            "a b=c(d,e,*g),*h=*i;",
                            //
                            "int a;",                    //
                            "unsigned long long int a;", //
                            "signed char a;",            //
                            "double a;" });

  checkParse("register const volatile constexpr int i;",
             checkTypeSeq<Type, Type, Type, Type, Type, VarDecl, DeclStmt>());

  checkUnparsable({ "int 1=2;", //
                    "1 + !(unparsable!!!);" });
}

TEST_F(FuzzyParseTest, ExprLineStmtTest) {
  checkToplevel<ExprLineStmt>({ "a*b*c;",                       //
                                "a*b*c=d;",                     //
                                "a*b*c==d;",                    //
                                "f();",                         //
                                "f(a,b,c);",                    //
                                "f(1,2,3);",                    //
                                "f(1)*g;",                      //
                                "n::f(1)*g;",                   //
                                "a+b;",                         //
                                "a-b;",                         //
                                "a*b*c;",                       //
                                "a/b;",                         //
                                "a&b&c;",                       //
                                "a^b;",                         //
                                "a|b;",                         //
                                "a<<b;",                        //
                                "a>>b;",                        //
                                "a<b;",                         //
                                "a>b;",                         //
                                "~a;",                          //
                                "!a;",                          //
                                "-a;",                          //
                                "--a;",                         //
                                "++a;",                         //
                                "++++~~~+~!~++++++!--++++++a;", //
                                "\"string literal\";",          //
                                "nullptr;",                     //
                                "this;",                        //
                                "true;",                        //
                                "false;",                       //
                                "-1;",                          //
                                "(1+-1)*(3+5);" });
  checkUnparsable({ "1(a,b);", //
                    "f(",      //
                    "f(," });
}

TEST_F(FuzzyParseTest, QualifiedIDs) {
  checkToplevel<DeclStmt>(
      { "std::vector<int> v;",         //
        "::std::vector v1;",           //
        "std::vector<int> v2;",        //
        "std::vector<int,int> v3;",    //
        "std::vector<> v4;",           //
        "std::vector<1> v5;",          //
        "std::tr1::stl::vector<> v6;", //
        "::vector<> v7;",              //
        "::std::tr1::stl::vector<std::vector<int>, ::std::pair<int,int> > v8;",
        "n::n::n::n::n::a<n::b<c<d<n::n::e,f>,g<h> > > > g;",
        "a::b<c::d> ***e=f::g<1>*h::i<2,j>(::k::l);",
        "auto x = std::make_unique<int>(0);" });

  checkParse("auto x = std::make_unique<int>(0);",
             checkTypeSeq<Type, VarDecl, VarInitialization, CallExpr, CallExpr,
                          CallExpr, CallExpr, Type, CallExpr, CallExpr,
                          LiteralConstant, CallExpr, DeclStmt>());
  checkToplevel<ExprLineStmt>({ "n::f(a::b<x>());",          //
                                "n::f<a,1,2>(a::b<2*3>());", //
                                "t<1+b>();",                 //
                                "t< 1<<2 >();",              //
                                "t< (1>2) >();" });
  checkUnparsable("t<1> 2>();");
}

TEST_F(FuzzyParseTest, FunctionDeclStmt) {
  const char *Tests[] = {
    "void f(int,int);", //
    "void g(int i=0);", //
    "static std::unique_ptr<VarDecl> parseVarDecl(TokenFilter &TF,"
    "                                             Type *TypeName = 0,"
    "                                             bool NameOptional = false);",
    "void dismiss() { TF = nullptr; }",       //
    "type func1();", "type func2() { 1+1; }", //
    "type func3(type a) { 1+1; }", "type func4(type a, type b) { 1+1; }",
    "static type func5();",
    "static std::unique_ptr<Expr> parseExpression(TokenFilter &TF,"
    "                                             int Precedence,"
    "                                             bool StopAtGreater);",
    "static bool checkKind(TokenFilter &TF, tok::TokenKind Kind){}",
  };
  for (const char *Code : Tests)
    checkFirst<FunctionDecl>(Code);
}

TEST_F(FuzzyParseTest, ReturnStmt) {
  checkToplevel<ReturnStmt>({ "return 1;",   //
                              "return a*b;", //
                              "return;" });
  checkUnparsable("return return;");
}

TEST_F(FuzzyParseTest, StructDecl) {
  checkFirst<ClassDecl>({ "struct C;",  //
                          "union C;",   //
                          "class C{};", //
                          "class C{ ><unparsable>< };" });

  auto checkFirstIsFunctionDecl = [&](StringRef Code) {
    checkFirstOn(Code, [](const Stmt &S, bool Abort) {
      if (Abort)
        EXPECT_TRUE(isa<ClassDecl>(S));
      else if (!isa<ClassDecl>(S))
        return false;
      const auto &CD = cast<ClassDecl>(S);
      if (Abort)
        EXPECT_EQ(CD.Body.size(), (size_t)1);
      else if (CD.Body.size() != 1)
        return false;

      if (Abort)
        EXPECT_TRUE(isa<FunctionDecl>(*CD.Body.front()));
      else if (!isa<FunctionDecl>(*CD.Body.front()))
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

TEST_F(FuzzyParseTest, IfStmt) {
  const char *Tests[] = {
    "if (true) {}",     //
    "if (0) do_sth();", //
    "if (int i=0) {}",  //
    "if (int i=0) {} else do_sth_else();",
    "if (int*i=0) {} else if (false) {} else do_sth_else();",
    "if (int*i=0) {} else if (ns::t<4> x=4) {} else do_sth_else();",
    "if (int*i=0) {} else if (ns::t<4> x=4) {} else do_sth_else();",
    "if (1){}else if(1){}else if(1){}else if(1){}else if(1){}else "
    "if(1){}else{}",
  };
  for (const char *Code : Tests)
    checkFirst<IfStmt>(Code);

  checkUnparsable("else if (1);");
}

TEST_F(FuzzyParseTest, IfStmtFuzzy) {
  checkFirst<IfStmt>({ "if (<!unparsable!>) {}", //
                       "if (true {}",            //
                       "if (false)) {}",         //
                       "if (<!unparsable!>);", });
}

TEST_F(FuzzyParseTest, WhileStmt) {
  checkFirst<WhileStmt>({ "while (true) {}",     //
                          "while (0) do_sth();", //
                          "while (int i=0) {}",  //
  });
}

TEST_F(FuzzyParseTest, ForStmt) {
  checkFirst<ForStmt>({ "for (;;) {}",                 //
                        "for (;;);",                   //
                        "for (int i=0;;) {}",          //
                        "for (T x=0,y=3;;) {}",        //
                        "for (T x,y,z;;) {}",          //
                        "for (int i=0;int j=0;) {}",   //
                        "for (int i=0;i<10;i=i+1) {}", //
                        "for (;int j;);",              //
                        "for (;;i=i+1) {}",            //
  });
}

TEST_F(FuzzyParseTest, TemplateDecl) {
  const char *Tests[] = {
    "template <typename T> void f();",
    "template <class T> void f();",
    "template <class T=int> void f();",
    "template <class T=const int****> void f();",
    //"template <class T=int****const> void f();",
    "template <typename T, class X, typename F> void f() {}",
    "template <typename T> struct C;",
    "template <int I> struct C;",
    "template <int I=0> void f();",
    "template <I X=3> void f();",
    "template <int I=0, typename T=int, I X=3> void f();",
  };
  for (const char *Code : Tests)
    checkFirst<TemplateDecl>(Code);
}

TEST_F(FuzzyParseTest, PPIf) {
  checkFirstPP<PPIf>({ "#if 1",             //
                       "#else",             //
                       "#elif 1",           //
                       "#  if unparsable!", //
                       "#else EXPR",        //
                       "#elif 1&1+1*3+f(3)", });
}

TEST_F(FuzzyParseTest, PPInclude) {
  checkFirstPP<PPInclude>({ "#include <algorithm>",  //
                            "#include \"header.h\"", //
                            "#include \"\"",         //
                            "#include <>",           //
                            "# /*comment*/  include <fancy/path!/???.h_>",
                            // " /*comment*/ # /*comment*/ include <x>",
                            "# include \"fancy/path!/???.h_\"", });
}

} // end namespace fuzzy
} // end namespace clang
