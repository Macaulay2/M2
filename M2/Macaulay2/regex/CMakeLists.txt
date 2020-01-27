#set(CMAKE_VERBOSE_MAKEFILE on)

add_library(regex OBJECT regex.c)

target_compile_definitions(regex PUBLIC
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

target_include_directories(regex
  PUBLIC
    ${CMAKE_SOURCE_DIR}/Macaulay2/regex
    ${CMAKE_SOURCE_DIR}/include
    ${BOOTSTRAP}/include
    ${BOOTSTRAP}/usr-host/include
    ${BOOTSTRAP}/usr-host/include/cdd
    ${CMAKE_SOURCE_DIR}/submodules/memtailor/src
    ${CMAKE_SOURCE_DIR}/submodules/mathic/src
    ${CMAKE_SOURCE_DIR}/submodules/mathicgb/src
    ${CMAKE_SOURCE_DIR}/submodules/fflas_ffpack/src
    /usr/include/libxml2
    /usr/include/eigen3
  )
