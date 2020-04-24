###############################################################################
## This directory contains the rules for building M2-interpreter

include(scc) ## CMake macro for calling the Safe C Compiler
include(startup) ## CMake script for messy substitutions in startup.c

## Configure startup.c
configure_file(startup.c.cmake startup.c @ONLY)

## Configure distributed-packages.h at the destination
string(REPLACE ";" " " DISTRIBUTED_PACKAGES "${DISTRIBUTED_PACKAGES}")
# TODO: configure this header directly in version.dd?
file(GENERATE OUTPUT distributed-packages.h CONTENT "#define DISTRIBUTED_PACKAGES \"${DISTRIBUTED_PACKAGES}\"")

## Configure version.dd at the destination
#configure_file(version.dd.in version.dd #ONLY)

###############################################################################

set(CLIST
  distributed-packages.h
  types.h startup.c
  debug.c debug.h
  memdebug.c memdebug.h
  M2types.c scclib.c M2lib.c M2mem.c
  ../c/scc-core.h

  getpagesize.h M2mem.h
  gmp_aux.c gmp_aux.h # TODO: needed with mpir?
  gdbm_interface.c

  $<$<BOOL:${WITH_XML}>:xml-c.c xml-c.h>
  $<$<BOOL:${WITH_PYTHON}>:python-c.c>
  )

# CAUTION: order is important in this list
# TODO: can there be #ifdefs in D sources?
set(DLIST
  arithmetic.d
  atomic.d
  M2.d
  system.d
  strings.d
  varstrin.d
  strings1.d
  errio.d
  vararray.d
  ctype.d
  nets.d
  varnets.d
  interrupts.d
  pthread0.d
  stdiop0.d
  gmp.d
  engine.dd
  xml.d		# removed unless WITH_XML
  stdio0.d
  parse.d
  expr.d
  stdio.d
  stdiop.d
  err.d
  gmp1.d
  tokens.d
  getline.d
  lex.d
  parser.d
  binding.d
  basic.d
  convertr.d
  common.d
  util.d
  struct.d
  classes.dd
  buckets.dd
  equality.dd
  hashtables.dd
  sets.dd
  version.dd
  evaluate.d
  mysql.d	# removed unless WITH_SQL
  mysqldummy.d	# removed if WITH_SQL
  pthread.d
  actors.d
  actors2.dd
  actors3.d
  actors4.d
  # tasks.d
  xmlactors.d	# removed unless WITH_XML
  actors5.d
  actors6.dd
  threads.dd
  python.d	# removed unless WITH_PYTHON
  interface.dd interface2.d
  texmacs.d
  interp.dd	# this one is last, because it contains the top level interpreter
  )

# Generator expressions conflict with the macro below, so manually remove the ones we don't need
if(NOT WITH_XML)
  list(REMOVE_ITEM DLIST xml.d xmlactors.d)
endif()
if(NOT WITH_SQL)
  list(REMOVE_ITEM DLIST mysql.d)
else()
  list(REMOVE_ITEM DLIST mysqldummy.d)
endif()
if(NOT WITH_PYTHON)
  list(REMOVE_ITEM DLIST python.d)
endif()

###############################################################################
## Generate the interpreter

foreach(_d_source IN LISTS DLIST)
  # See cmake/scc.cmake
  _SCC_GENERATE(${_d_source} _c) # sets _c_source
  list(APPEND CLIST ${_c_source})
endforeach()

###############################################################################
## Compile the interpreter

add_library(M2-interpreter OBJECT ${CLIST})

target_link_libraries(M2-interpreter PUBLIC M2-regex)

target_include_directories(M2-interpreter
  PUBLIC
    .
    ${CMAKE_SOURCE_DIR}/Macaulay2/regex # scclib.c uses regex
    ${CMAKE_SOURCE_DIR}/Macaulay2/e # need newdelete.hpp
    ${CMAKE_SOURCE_DIR}/Macaulay2/c # need scc-core.h
    ${CMAKE_BINARY_DIR}/Macaulay2/d
  PRIVATE
    ${LIBXML2_INCLUDE_DIR}
    ${SQLite3_INCLUDE_DIRS}
    ${Python3_INCLUDE_DIRS}
    ${READLINE_INCLUDE_DIR}
  )

foreach(LIB IN LISTS LIBRARY_LIST)
  if(${LIB}_FOUND)
    target_link_libraries(M2-interpreter PUBLIC ${${LIB}_LIBRARY})
  endif()
endforeach()

# Compiler warning flags
target_compile_options(M2-interpreter PRIVATE
  -Wno-cast-qual # FIXME: caused by M2lib.c:551:43
  -Wno-sign-compare
  -Wno-uninitialized
  -Wno-unused

  # FIXME: mismatched-tags and parentheses-equality errors are caused by scc1
  $<$<COMPILE_LANG_AND_ID:C,AppleClang,Clang>:-Wno-parentheses-equality>
  $<$<COMPILE_LANG_AND_ID:CXX,AppleClang,Clang>:-Wno-mismatched-tags -Wno-parentheses-equality>

  # FIXME: __builtin_frame_address M2lib.c:165 causes this:
  $<$<COMPILE_LANG_AND_ID:C,GNU>:-Wno-frame-address>
  # Givaro and fflas_ffpack cause these:
  $<$<COMPILE_LANG_AND_ID:CXX,GNU>:-Wno-deprecated-copy -Wno-unknown-pragmas>
  )
# add other compilers (eg: Intel, MSVC) here
# for Intel: -diag-disable 981,869,383,2259,444 -debug

###############################################################################

# TODO: generate etags
# cd ${CMAKE_SOURCE_DIR}/Macaulay2/d && etags  ...

# What is this for?
# if [ -f ${CMAKE_SOURCE_DIR}/Macaulay2/d/.gdbinit.mahrud ]; then cp ${CMAKE_SOURCE_DIR}/Macaulay2/d/.gdbinit.mahrud .gdbinit; fi
