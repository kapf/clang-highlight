#include "clang/Frontend/CompilerInstance.h"
#include "clang/Tooling/Tooling.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Tooling/CompilationDatabase.h"
#include "clang/Tooling/Tooling.h"
#include "TokenClassifier.h"

using namespace clang::tooling;
using namespace llvm;

namespace clang {
namespace highlight {

namespace {
class ParserHintVisitor : public RecursiveASTVisitor<ParserHintVisitor> {
public:
  explicit ParserHintVisitor(ASTContext *Context, ParserHints *PH)
      : Context(Context), PH(PH) {}

  bool VisitTypeLoc(TypeLoc TL) {
    auto &SM = Context->getSourceManager();
    SourceLocation SpellingLoc = SM.getSpellingLoc(TL.getBeginLoc());
    if (SM.getFilename(SpellingLoc) == PH->FileName)
      PH->TypeOffsets.push_back(SM.getFileOffset(SpellingLoc));
    return true;
  }

private:
  ASTContext *Context;
  ParserHints *PH;
};
} // end anonymous namespace

namespace {
class ParserHintConsumer : public clang::ASTConsumer {
public:
  explicit ParserHintConsumer(ASTContext *Context, ParserHints *PH)
      : Visitor(Context, PH) {}

  virtual void HandleTranslationUnit(clang::ASTContext &Context) {
    Visitor.TraverseDecl(Context.getTranslationUnitDecl());
  }

private:
  ParserHintVisitor Visitor;
};
} // end anonymous namespace

namespace {
class ParserHintAction : public clang::ASTFrontendAction {
  ParserHints *PH;

public:
  ParserHintAction(ParserHints *PH) : PH(PH) {}

  virtual clang::ASTConsumer *
  CreateASTConsumer(clang::CompilerInstance &Compiler, llvm::StringRef InFile) {
    return new ParserHintConsumer(&Compiler.getASTContext(), PH);
  }
};
} // end anonymous namespace

namespace {
class ParserHintActionFactory : public clang::tooling::FrontendActionFactory {
  ParserHints *PH;

public:
  ParserHintActionFactory(ParserHints *PH) : PH(PH) {}

  virtual clang::FrontendAction *create() { return new ParserHintAction(PH); }
};
} // end anonymous namespace

ParserHints collectParserHints(StringRef SourceFile) {
  ParserHints Hints;
  Hints.FileName = SourceFile;

  std::string ErrMsg;
  if (CompilationDatabase *CDB =
          clang::tooling::CompilationDatabase::autoDetectFromSource(SourceFile,
                                                                    ErrMsg)) {
    ClangTool Tool(*CDB, llvm::ArrayRef<std::string>(SourceFile));
    Tool.run(new ParserHintActionFactory(&Hints));
    Hints.normalize();
  } else {
    llvm::errs() << "collectParserHints: " << ErrMsg << '\n';
  }
  return Hints;
}

} // end namespace highlight
} // end namespace clang
