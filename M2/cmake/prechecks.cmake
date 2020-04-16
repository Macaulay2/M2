find_program(CLANG_TIDY		NAMES clang-tidy)
find_program(CLANG_FORMAT	NAMES clang-format)
find_program(CPPCHECK		NAMES cppcheck)
find_program(CPPLINT		NAMES cpplint)
find_program(IWYU		NAMES iwyu)

if(CMAKE_BUILD_TYPE MATCHES Debug)
  set(CMAKE_LINK_WHAT_YOU_USE TRUE)

  if(IWYU)
    set(CMAKE_CXX_INCLUDE_WHAT_YOU_USE ${IWYU} --transitive_includes_only)
  endif()

  if(CPPCHECK)
    set(CMAKE_CXX_CPPCHECK ${CPPCHECK} --std=c++11)
  endif()

  if(CPPLINT)
    set(CMAKE_CXX_CPPLINT  ${CPPLINT}  --linelength=79)
  endif(CPPLINT)

  if(CLANG_TIDY)
    set(CMAKE_CXX_CLANG_TIDY
      ${CLANG_TIDY} -checks=-*,readability-*)
    set(CMAKE_CXX_CLANG_TIDY
      ${CLANG_TIDY} -checks=-*,cppcoreguidelines-*,clang-analyzer-*,modernize-*,performance-*,readability-*)
  endif()
endif()

if(CLANG_FORMAT)
  file(GLOB_RECURSE ENGINE_SOURCES
    Macaulay2/e/*.c
    Macaulay2/e/*.h
    Macaulay2/e/*.cpp
    Macaulay2/e/*.hpp
    )
  # TODO: editing source files from cmake ... kind of a bad idea
  add_custom_target(reformat
    COMMAND ${CLANG_FORMAT} -i ${ENGINE_SOURCES}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )
endif()
