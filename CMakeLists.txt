set(LLVM_LINK_COMPONENTS support)

set(LLVM_USED_LIBS clangTooling)
add_clang_executable(clang-highlight
  ClangHighlight.cpp
  TokenClassifier.cpp
  OutputWriter.cpp
  ParserHintCollector.cpp
  FuzzyParser.cpp
  )

target_link_libraries(clang-highlight
  clangTooling
  clangAST
  )

install(TARGETS clang-highlight RUNTIME DESTINATION bin)
