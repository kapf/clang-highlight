//===--- TokenClassifier.cpp - clang-highlight ------------------*- C++ -*-===//
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
#include "OutputWriter.h"
#include <unordered_set>
#include "TokenClassifier.h"
#include "Fuzzy/FuzzyAST.h"

using namespace clang;

namespace clang {
namespace highlight {

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

bool isCharLiteral(tok::TokenKind TK) {
  switch (TK) {
  case tok::char_constant:
  case tok::wide_char_constant:
  case tok::utf16_char_constant:
  case tok::utf32_char_constant:
    return true;
  default:
    return false;
  }
}

bool isKeyword(tok::TokenKind TK) {
  switch (TK) {
#define KEYWORD(X, Y) case tok::kw_##X:
#include "clang/Basic/TokenKinds.def"
    return true;
  default:
    return false;
  }
}

TokenClass convertTokenKindToTokenClass(tok::TokenKind TK) {
  if (isCharLiteral(TK))
    return TokenClass::Char;
  if (isStringLiteral(TK))
    return TokenClass::String;
  if (isKeyword(TK))
    return TokenClass::Keyword;
  if (TK == tok::annot_typename)
    return TokenClass::Type;
  if (TK == tok::comment)
    return TokenClass::Comment;
  return TokenClass::Other;
}

void highlight(std::unique_ptr<llvm::MemoryBuffer> Source, StringRef FileName,
               std::unique_ptr<OutputWriter> OW, bool IdentifiersOnly,
               bool DumpAST) {
  using namespace llvm;
  using namespace clang;

  FileManager Files((FileSystemOptions()));
  DiagnosticsEngine Diagnostics(
      IntrusiveRefCntPtr<DiagnosticIDs>(new DiagnosticIDs),
      new DiagnosticOptions);
  SourceManager SourceMgr(Diagnostics, Files);
  llvm::MemoryBuffer *Buf = Source.release(); // SourceMgr owns Buf for us
  const clang::FileEntry *Entry =
      Files.getVirtualFile(FileName, Buf->getBufferSize(), 0);
  SourceMgr.overrideFileContents(Entry, Buf);
  FileID ID =
      SourceMgr.createFileID(Entry, SourceLocation(), clang::SrcMgr::C_User);

  auto Langs = getFormattingLangOpts();
  Lexer Lex(ID, SourceMgr.getBuffer(ID), SourceMgr, Langs);
  Lex.SetKeepWhitespaceMode(true);

  IdentifierTable IdentTable(getFormattingLangOpts());

  std::vector<fuzzy::AnnotatedToken> AllTokens;

  for (;;) {
    Token TmpTok;
    Lex.LexFromRawLexer(TmpTok);

    if (TmpTok.getKind() == tok::hash && TmpTok.isAtStartOfLine())
      Lex.setParsingPreprocessorDirective(true);
    if (TmpTok.getKind() == tok::eod)
      Lex.setParsingPreprocessorDirective(false);

    AllTokens.push_back(fuzzy::AnnotatedToken(TmpTok));
    Token &ThisTok = AllTokens.back().Tok();

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

  auto TU = fuzzy::fuzzyparse(&*AllTokens.begin(), &*AllTokens.end());

  if (DumpAST) {
    fuzzy::printAST(llvm::dbgs(), TU, SourceMgr);
    return;
  }

  const char *LastTokenStart = nullptr, *ThisTokenStart = nullptr;
  Token LastTok;
  TokenClass Class = TokenClass::NONE;
  for (auto &ATok : AllTokens) {
    Token &ThisTok = ATok.Tok();

    ThisTokenStart = SourceMgr.getCharacterData(ThisTok.getLocation());
    if (LastTokenStart) {
      if (Class == TokenClass::NONE)
        Class = convertTokenKindToTokenClass(LastTok.getKind());
      OW->writeToken(StringRef(LastTokenStart, ThisTokenStart - LastTokenStart),
                     Class);
    }

    Class = TokenClass::NONE;

    StringRef TokenText(SourceMgr.getCharacterData(ThisTok.getLocation()),
                        ThisTok.getLength());

    if (ATok.hasASTReference()) {
      if (!IdentifiersOnly || ATok.getTokenKind() == tok::identifier) {
        if (llvm::isa<fuzzy::Type>(ATok.getASTReference()) ||
            llvm::isa<fuzzy::Type::Decoration>(ATok.getASTReference()) ||
            ((llvm::isa<fuzzy::ClassDecl>(ATok.getASTReference()) ||
              llvm::isa<fuzzy::TemplateParameterType>(ATok.getASTReference())) &&
             ATok.getTokenKind() == tok::identifier)) {
          Class = TokenClass::Type;
          ThisTok.setKind(tok::annot_typename);
        }
      }
      if (isa<fuzzy::PPString>(ATok.getASTReference())) {
        Class = TokenClass::String;
      }
      if (isa<fuzzy::PPDirective>(ATok.getASTReference())) {
        Class = TokenClass::Preprocessor;
      }
      if (isa<fuzzy::DeclRefExpr>(ATok.getASTReference())) {
        Class = TokenClass::Variable;
      }
      if (ATok.getTokenKind() == tok::identifier &&
          (isa<fuzzy::CallExpr>(ATok.getASTReference()) ||
           isa<fuzzy::FunctionDecl>(ATok.getASTReference()))) {
        Class = TokenClass::Function;
      }
    }
    LastTok = ThisTok;
    LastTokenStart = ThisTokenStart;
  }
}

} // end namespace highlight
} // end namespace clang
