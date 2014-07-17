set(LLVM_LINK_COMPONENTS support)

add_clang_library(clangFuzzy
  FuzzyParser.cpp
  FuzzyASTPrinter.cpp
  )
