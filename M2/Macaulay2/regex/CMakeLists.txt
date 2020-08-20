###############################################################################
## This directory contains the rules for building M2-regex, which is a very old
## copy of GNU regex library.

# TODO: refactor d/scclib.c to use POSIX.1 syntax, then drop this folder

add_library(M2-regex STATIC regex.c)

target_include_directories(M2-regex PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<INSTALL_INTERFACE:include/Macaulay2>
  )

target_compile_definitions(M2-regex PUBLIC
  -Dre_compile_fastmap=M2_re_compile_fastmap
  -Dre_compile_pattern=M2_re_compile_pattern
  -Dre_match=M2_re_match
  -Dre_match_2=M2_re_match_2
  -Dre_search=M2_re_search
  -Dre_search_2=M2_re_search_2
  -Dre_set_registers=M2_re_set_registers
  -Dre_set_syntax=M2_re_set_syntax
  -Dregcomp=M2_regcomp
  -Dregerror=M2_regerror
  -Dregexec=M2_regexec
  -Dregfree=M2_regfree
  )

# Compiler warning flags
target_compile_options(M2-regex PRIVATE
  -Wno-cast-qual
  -Wno-sign-compare
  -Wno-unused

  $<$<COMPILE_LANG_AND_ID:C,GNU>:-Wno-misleading-indentation -Wno-old-style-declaration>
  )

# Export the target
install(TARGETS M2-regex EXPORT Macaulay2 COMPONENT devel EXCLUDE_FROM_ALL)
