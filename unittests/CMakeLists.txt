set(LLVM_LINK_COMPONENTS
  Support
  )

include_directories("..")

add_highlight_unittest(FuzzyParseTests
  FuzzyParseTest.cpp
  )

target_link_libraries(FuzzyParseTests
  clangAST
  clangTooling
  clangFuzzy
  )
