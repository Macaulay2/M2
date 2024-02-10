find_program(CLANG_TIDY		NAMES clang-tidy)
find_program(CLANG_FORMAT	NAMES clang-format)
find_program(CPPCHECK		NAMES cppcheck)
find_program(CPPLINT		NAMES cpplint) # pip install cpplint
find_program(IWYU		NAMES iwyu include-what-you-use) # include-what-you-use.org
find_program(VALGRIND		NAMES valgrind)

# List of available checks: clang-tidy -checks=* --list-checks
# TIP: Starting with "-*" means exclude all
set(CLANG_TIDY_CHECKS
  -*,clang-analyzer-*,cppcoreguidelines-*,performance-*,modernize-*)#,readability-*)

# Note: IWYU options need to be come after -Xiwyu
# Also see:
# https://github.com/include-what-you-use/include-what-you-use/blob/master/docs/IWYUPragmas.md
# https://google.github.io/styleguide/cppguide.html#Names_and_Order_of_Includes
set(IWYU_C_OPTS   "${IWYU};-Xiwyu;--transitive_includes_only")
set(IWYU_CXX_OPTS "${IWYU};-Xiwyu;--transitive_includes_only;-Xiwyu;--cxx17ns")

MACRO (_ADD_PRECHECKS _target _checks _c_sources _cxx_sources)
  message("## Setting up source checks for target ${_target} ...")

  if("${_checks}" MATCHES "LWYU")
    message("     Link What You Use)")
    set_target_properties(${_target} PROPERTIES LINK_WHAT_YOU_USE TRUE)
  endif()

  if(IWYU AND "${_checks}" MATCHES "IWYU")
    message("     Include What You Use")
    # Note: may need to add `llvm-config --includedir` to include directories of target
    set_target_properties(${_target} PROPERTIES C_INCLUDE_WHAT_YOU_USE   "${IWYU_C_OPTS}")
    set_target_properties(${_target} PROPERTIES CXX_INCLUDE_WHAT_YOU_USE "${IWYU_CXX_OPTS}")
  endif()

  if(CPPLINT AND "${_checks}" MATCHES CPPLINT)
    message("     cpplint")
    set_target_properties(${_target} PROPERTIES C_CPPLINT      "${CPPLINT};--linelength=80")
    set_target_properties(${_target} PROPERTIES CXX_CPPLINT    "${CPPLINT};--linelength=80")
  endif()

  if(CPPCHECK AND "${_checks}" MATCHES CPPCHECK)
    message("     cppcheck")
    set_target_properties(${_target} PROPERTIES C_CPPCHECK     "${CPPCHECK};--std=c11}")
    set_target_properties(${_target} PROPERTIES CXX_CPPCHECK   "${CPPCHECK};--std=c++14")
  endif()

  if(CLANG_TIDY AND "${_checks}" MATCHES CLANG_TIDY)
    message("     clang-tidy")
    set_target_properties(${_target} PROPERTIES C_CLANG_TIDY   "${CLANG_TIDY};-checks=${CLANG_TIDY_CHECKS}")
    set_target_properties(${_target} PROPERTIES CXX_CLANG_TIDY "${CLANG_TIDY};-checks=${CLANG_TIDY_CHECKS}")
  endif()
ENDMACRO (_ADD_PRECHECKS)


MACRO (_ADD_CLANG_FORMAT _target _c_sources _cxx_sources)
  if(CLANG_FORMAT)
    message("## Setting up reformat target for running clang-format on ${_target}")
    # TODO: editing source files from cmake ... kind of a bad idea
    # C_CHANGED_FILES = $(git diff --cached --name-only --diff-filter=ACM | grep -Ee "\.[ch]$")
    # CXX_CHANGED_FILES = $(git diff --cached --name-only --diff-filter=ACM | grep -Ee "\.([ch](pp|xx))$")
    add_custom_target(reformat
      COMMAND ${CLANG_FORMAT} -style=file -i ${_c_sources} ${_cxx_sources}
      WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
      )
  endif()
ENDMACRO (_ADD_CLANG_FORMAT)

# use ctest -T memcheck -R unit-tests, for instance, to run tests with Valgrind
if(VALGRIND)
  message("## Setting up Valgrind memory checking through CTest (ctest -T memcheck)")
  set(VALGRIND_COMMAND         "${VALGRIND}")
  set(VALGRIND_COMMAND_OPTIONS "--trace-children=yes --trace-children-skip=/bin/sh,/bin/bash --leak-check=full --error-exitcode=1 --track-origins=yes")
  set(MEMORYCHECK_COMMAND           "${VALGRIND_COMMAND}")
  set(MEMORYCHECK_COMMAND_OPTIONS   "${VALGRIND_COMMAND_OPTIONS}")
  set(MEMORYCHECK_SUPPRESSIONS_FILE "${CMAKE_SOURCE_DIR}/files/M2-suppressions.supp")
endif()
