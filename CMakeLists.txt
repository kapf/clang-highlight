set(LLVM_LINK_COMPONENTS support)

set(LLVM_USED_LIBS clangTooling)

add_clang_executable(clang-highlight
  ClangHighlight.cpp
  TokenClassifier.cpp
  OutputWriter.cpp
  ParserHintCollector.cpp
  )

target_link_libraries(clang-highlight
  # clangTooling
  clangAST
  clangFuzzy
  )
include_directories("Fuzzy")

install(TARGETS clang-highlight RUNTIME DESTINATION bin)

add_custom_target(ClangHighlightUnitTests)
set_target_properties(ClangHighlightUnitTests PROPERTIES FOLDER "Clang Highlight Unit Tests")

function(add_highlight_unittest test_dirname)
  add_unittest(ClangHighlightUnitTests ${test_dirname} ${ARGN})
endfunction()

add_subdirectory(Fuzzy)
add_subdirectory(unittests)
