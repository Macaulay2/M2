#set(CMAKE_VERBOSE_MAKEFILE on)

# FIXME: for some reason scc1 fails if run in parallel: failed to open typecode.db
# TODO: this isn't working
set(ENV{CMAKE_BUILD_PARALLEL_LEVEL} 1)

################################################################
## Configure startup.c at the destination
include(startup) ## CMake script for messy substitutions in startup.c
configure_file(startup.c.cmake startup.c @ONLY)

## Configure distributed-packages.h at the destination
string(REPLACE ";" " " PACKAGES "${DISTRIBUTED_PACKAGES}")
# TODO: configure this header directly in version.dd?
file(GENERATE OUTPUT distributed-packages.h CONTENT "#define DISTRIBUTED_PACKAGES \"${PACKAGES}\"")

## Configure version.dd at the destination
#configure_file(version.dd.in version.dd #ONLY)

################################################################
## Compile .d and .dd files to .c and .cc files, respectively

# Order matters here
set(CLIST arithmetic-tmp.c atomic-tmp.c M2-tmp.c system-tmp.c strings-tmp.c varstrin-tmp.c strings1-tmp.c errio-tmp.c vararray-tmp.c ctype-tmp.c nets-tmp.c varnets-tmp.c interrupts-tmp.c pthread0-tmp.c stdiop0-tmp.c gmp-tmp.c engine-tmp.cc xml-tmp.c stdio0-tmp.c parse-tmp.c expr-tmp.c stdio-tmp.c stdiop-tmp.c err-tmp.c gmp1-tmp.c tokens-tmp.c getline-tmp.c lex-tmp.c parser-tmp.c binding-tmp.c basic-tmp.c convertr-tmp.c common-tmp.c util-tmp.c struct-tmp.c classes-tmp.cc buckets-tmp.cc equality-tmp.cc hashtables-tmp.cc sets-tmp.cc version-tmp.cc evaluate-tmp.c mysqldummy-tmp.c pthread-tmp.c actors-tmp.c actors2-tmp.cc actors3-tmp.c actors4-tmp.c xmlactors-tmp.c actors5-tmp.c actors6-tmp.cc threads-tmp.cc flint-tmp.c interface-tmp.cc interface2-tmp.c texmacs-tmp.c interp-tmp.cc)

add_custom_target(generate-d ALL DEPENDS ${CLIST})

if(NOT CMAKE_BUILD_TYPE STREQUAL "Debug")
  set(SCCFLAGS -O)
endif()

foreach(FILENAME IN LISTS CLIST)
  string(REGEX MATCH "^([a-zA-Z0-9_]+)-tmp\.(c|cc)$" NULL ${FILENAME})
  set(NAME ${CMAKE_MATCH_1})
  string(REPLACE "c" "d" DEXT ${CMAKE_MATCH_2})

  add_custom_command(OUTPUT ${NAME}.sig
    COMMAND
      scc1 ${SCCFLAGS} -dep ${CMAKE_CURRENT_SOURCE_DIR}/${NAME}.${DEXT}
    COMMAND
      mv ${NAME}.dep.tmp ${NAME}.dep
    COMMAND
      test -e ${NAME}.sig
        && cmp -s ${NAME}.sig.tmp ${NAME}.sig
        || cp ${NAME}.sig.tmp ${NAME}.sig
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${PREVSIG}
    )

  add_custom_command(OUTPUT ${FILENAME}
    COMMAND
      scc1 ${SCCFLAGS} ${CMAKE_CURRENT_SOURCE_DIR}/${NAME}.${DEXT}
    COMMAND
      test -e ${NAME}-exports.h
        && cmp -s ${NAME}-exports.h.tmp ${NAME}-exports.h
        || cp ${NAME}-exports.h.tmp ${NAME}-exports.h
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${NAME}.sig ${PREVTMP}
    )

  set(PREVSIG ${NAME}.sig)
  set(PREVTMP ${FILENAME})
endforeach()

################################################################

# TODO
# cd ${CMAKE_SOURCE_DIR}/Macaulay2/d && etags  mysql.d python-c.c python.d  debug.c  gmp_aux.c  main.c  getpagesize.h types.h M2mem.h debug.h startup-header.h startup-trailer.h  arithmetic.d atomic.d M2.d system.d strings.d varstrin.d strings1.d errio.d vararray.d ctype.d nets.d varnets.d interrupts.d pthread0.d stdiop0.d  gmp.d engine.dd xml.d stdio0.d parse.d expr.d stdio.d stdiop.d err.d gmp1.d tokens.d getline.d lex.d parser.d binding.d basic.d convertr.d common.d util.d struct.d classes.dd buckets.dd equality.dd hashtables.dd sets.dd version.dd evaluate.d mysqldummy.d pthread.d actors.d actors2.dd actors3.d actors4.d xmlactors.d actors5.d actors6.dd threads.dd flint.d interface.dd interface2.d texmacs.d interp.dd  M2types.c scclib.c M2lib.c M2mem.c gdbm_interface.c xml-c.c

# What is this for?
# if [ -f ${CMAKE_SOURCE_DIR}/Macaulay2/d/.gdbinit.mahrud ]; then cp ${CMAKE_SOURCE_DIR}/Macaulay2/d/.gdbinit.mahrud .gdbinit; fi

################################################################
## Compile the interpreter

add_library(interpreter OBJECT
  ${CLIST}
  gmp_aux.c
  main.c
  M2types.c
  scclib.c
  M2lib.c
  M2mem.c
  gdbm_interface.c
  xml-c.c
  debug.c # Where is this done normally?
  ${CMAKE_CURRENT_BINARY_DIR}/startup.c
  )

add_dependencies(interpreter generate-d e-includes)

target_include_directories(interpreter
  PUBLIC
    .
    ${CMAKE_SOURCE_DIR}/include
    ${CMAKE_BINARY_DIR}/include
    ${CMAKE_SOURCE_DIR}/Macaulay2/regex
    ${CMAKE_SOURCE_DIR}/Macaulay2/e
    ${CMAKE_SOURCE_DIR}/Macaulay2/c
    ${CMAKE_BINARY_DIR}/Macaulay2/d
    ${CMAKE_BINARY_DIR}/Macaulay2/e-includes
    /usr/local/include
  PRIVATE
    ${EIGEN3_INCLUDE_DIR}
    ${GIVARO_INCLUDE_DIRS}
    ${LIBXML2_INCLUDE_DIR}
    ${READLINE_INCLUDE_DIR}
    ${CMAKE_SOURCE_DIR}/submodules/memtailor/src
    ${CMAKE_SOURCE_DIR}/submodules/mathic/src
    ${CMAKE_SOURCE_DIR}/submodules/mathicgb/src
  )

# common warning flags
target_compile_options(interpreter PRIVATE
  -Wall
  #-Wextra
  -Wfatal-errors
  -Wno-cast-qual
  -Wno-parentheses
  -Wno-shadow
  -Wno-sign-compare
  -Wno-sign-conversion
  -Wno-uninitialized
  -Wno-unused
  -Wno-unused-parameter
  # temporary flags
  -Wno-unknown-pragmas
  -Wno-frame-address
  )
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  # using Clang or AppleClang
  target_compile_options(interpreter PRIVATE
    -Wno-mismatched-tags # needed currently, but if we clean up includes, should not be needed?
    -Wno-parentheses-equality
    )
endif()
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  # using GCC
endif()
# add other compilers (eg: Intel, MSVC) here
# for Intel: -diag-disable 981,869,383,2259,444 -debug
