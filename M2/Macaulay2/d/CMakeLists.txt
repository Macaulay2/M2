#set(CMAKE_VERBOSE_MAKEFILE on)
# TODO: use PRE_BUILD targets for parsing d files

################################################################
## Configure startup.c at the destination
include(startup) ## CMake script for messy substitutions in startup.c
configure_file(startup.c.cmake startup.c @ONLY)

## Configure distributed-packages.h at the destination
string(REPLACE ";" " " DISTRIBUTED_PACKAGES "${DISTRIBUTED_PACKAGES}")
# TODO: configure this header directly in version.dd?
file(GENERATE OUTPUT distributed-packages.h CONTENT "#define DISTRIBUTED_PACKAGES \"${DISTRIBUTED_PACKAGES}\"")

## Configure version.dd at the destination
#configure_file(version.dd.in version.dd #ONLY)

################################################################

set(CLIST
  M2lib.c M2mem.c M2types.c scclib.c
  debug.c # Where is this done normally?
  gdbm_interface.c
  gmp_aux.c
  xml-c.c
  startup.c
  )

set(DLIST arithmetic.d atomic.d M2.d system.d strings.d varstrin.d strings1.d errio.d vararray.d ctype.d nets.d varnets.d interrupts.d pthread0.d stdiop0.d gmp.d engine.dd xml.d stdio0.d parse.d expr.d stdio.d stdiop.d err.d gmp1.d tokens.d getline.d lex.d parser.d binding.d basic.d convertr.d common.d util.d struct.d classes.dd buckets.dd equality.dd hashtables.dd sets.dd version.dd evaluate.d mysqldummy.d pthread.d actors.d actors2.dd actors3.d actors4.d xmlactors.d actors5.d actors6.dd threads.dd interface.dd interface2.d texmacs.d interp.dd)

################################################################
## Macro for compiling .d and .dd files to .c and .cc files, respectively

if(CMAKE_BUILD_TYPE MATCHES "Rel")
  set(SCCFLAGS -O)
else()
  # TODO: set(SCCFLAGS -debug)
endif()

MACRO (_SCC_GENERATE _source _prev)
  string(REGEX MATCH "^([a-zA-Z0-9_]+)\.(d|dd)$" NULL ${_source})
  set(_name ${CMAKE_MATCH_1})
  string(REPLACE "d" "c" _ext ${CMAKE_MATCH_2})

  # TODO: use the {_name}.dep file to check for dependencies
  # TODO: use DEPFILE option, but only works with ninja
  add_custom_command(OUTPUT ${_name}-tmp.${_ext}
    COMMENT "Generating ${_name}.${_ext}"
    COMMAND
      scc1 ${SCCFLAGS} -dep ${CMAKE_CURRENT_SOURCE_DIR}/${_source}
    COMMAND
      mv ${_name}.sig.tmp ${_name}.sig && mv ${_name}.dep.tmp ${_name}.dep
    COMMAND
      scc1 ${SCCFLAGS} ${CMAKE_CURRENT_SOURCE_DIR}/${_source}
    COMMAND
      mv ${_name}-exports.h.tmp ${_name}-exports.h
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${_source} ${${_prev}_source}
    )

  set(${_prev}_name.sig ${_name}.sig)
  set(${_prev}_source   ${_name}-tmp.${_ext})
ENDMACRO (_SCC_GENERATE)

################################################################

# TODO
# cd ${CMAKE_SOURCE_DIR}/Macaulay2/d && etags  ...

# What is this for?
# if [ -f ${CMAKE_SOURCE_DIR}/Macaulay2/d/.gdbinit.mahrud ]; then cp ${CMAKE_SOURCE_DIR}/Macaulay2/d/.gdbinit.mahrud .gdbinit; fi

################################################################
## Compile the interpreter

foreach(d_file IN LISTS DLIST)
  _SCC_GENERATE(${d_file} c_file)
  list(APPEND CLIST ${c_file_source})
endforeach()

# TODO: are all of these needed?
add_library(M2-interpreter OBJECT ${CLIST})


target_include_directories(M2-interpreter
  PUBLIC
    .
    ${CMAKE_SOURCE_DIR}/Macaulay2/e
    ${CMAKE_SOURCE_DIR}/Macaulay2/c
    ${CMAKE_BINARY_DIR}/Macaulay2/d
  PRIVATE
    ${LIBXML2_INCLUDE_DIR}
    ${READLINE_INCLUDE_DIR} # TODO: needed?
  )

# common warning flags
target_compile_options(M2-interpreter PRIVATE
  -Wall
  #-Wextra
  -Wfatal-errors
  -Wno-sign-compare
  -Wno-uninitialized
  -Wno-unused
  # temporary flags
  -Wno-unknown-pragmas
  )
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
  # using Clang or AppleClang
  target_compile_options(M2-interpreter PRIVATE
    -Wno-mismatched-tags # needed currently, but if we clean up includes, should not be needed?
    -Wno-parentheses-equality
    )
endif()
if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  # using GCC
  target_compile_options(M2-interpreter PRIVATE
    # temporary flags
    -Wno-frame-address # many errors about __builtin_frame_address in M2lib.c:165
    )
endif()
# add other compilers (eg: Intel, MSVC) here
# for Intel: -diag-disable 981,869,383,2259,444 -debug
