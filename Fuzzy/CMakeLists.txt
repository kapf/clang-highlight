set(LLVM_LINK_COMPONENTS support)

clang_tablegen(FuzzyNodes.inc -gen-clang-stmt-nodes
  SOURCE FuzzyNodes.td
  TARGET FuzzyNodes)

add_clang_library(clangFuzzy
  FuzzyParser.cpp
  FuzzyASTPrinter.cpp

  ADDITIONAL_HEADERS
  FuzzyNodes.td
  )
