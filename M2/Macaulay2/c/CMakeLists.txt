# MAHRUD: Does SCC stand for Simple C Compiler?
# In this directory, we compile "scc1", the D to C translator, which we want to run
# on the "build" machine, in the sense of autoconf.

#set(CMAKE_VERBOSE_MAKEFILE on)

find_package(BISON REQUIRED)
BISON_TARGET(scc1 ${CMAKE_CURRENT_SOURCE_DIR}/grammar.y ${CMAKE_CURRENT_BINARY_DIR}/grammar.c
  DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/grammar.h
  COMPILE_FLAGS -yvdlt)

# The header files are only listed so that they show up in IDEs
add_executable(scc1
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
  scc.h
  grammar.h
  keywords.h
  )

target_include_directories(scc1
  PUBLIC
    ${CMAKE_SOURCE_DIR}/Macaulay2/c
    ${CMAKE_SOURCE_DIR}/include
    ${BOOTSTRAP}/include
    /usr/local/include
  PRIVATE
  )

#target_link_directories(scc1 PUBLIC /usr/local/lib)
target_link_libraries(scc1 "-L/usr/local/lib" gdbm m gc)

# Why do we do this?
# CC scc-core.c

# Add this also:
# cd ${CMAKE_SOURCE_DIR}/Macaulay2/c && etags scc1.h readfile.h error.h dictionary.h list.h cprint.h type.h chk.h compat.h debugging.h scc.h grammar.h keywords.h scc1.c readfile.c error.c dictionary.c list.c cprint.c type.c chk.c compat.c debugging.c grammar.y scc-core.h scc-core.c
