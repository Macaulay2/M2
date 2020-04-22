###############################################################################
# Safe C Compiler (SCC)
# In this directory, we compile the "scc1", the D to C translator, which we want to run
# on the "build" machine, in the sense of autoconf.

find_package(BISON REQUIRED QUIET)
BISON_TARGET(scc-grammar ${CMAKE_CURRENT_SOURCE_DIR}/grammar.y ${CMAKE_CURRENT_BINARY_DIR}/grammar.c
  DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/grammar.h
  COMPILE_FLAGS -yvdlt)

# The header files are only listed so that they show up in IDEs
add_executable(scc1
  scc.h
  scc1.c
  scc1.h
  readfile.c
  readfile.h
  error.c
  error.h
  dictionary.c
  dictionary.h
  list.c
  list.h
  cprint.c
  cprint.h
  type.c
  type.h
  chk.c
  chk.h
  compat.c
  compat.h
  debugging.c
  debugging.h
  grammar.c
  grammar.h
  keywords.h
  )

target_include_directories(scc1 PUBLIC .)

target_compile_options(scc1 PRIVATE
  $<$<COMPILE_LANG_AND_ID:C,GNU>:-Wno-implicit-fallthrough> # FIXME: caused by type.c:175:26
  )

if(BDWGC_FOUND AND GDBM_FOUND)
target_link_libraries(scc1 Threads::Threads ${BDWGC_LIBRARIES} ${GDBM_LIBRARY} ${CMAKE_DL_LIBS})
endif()

# Why do we do this?
# CC scc-core.c
