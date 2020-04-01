# In this directory we link the object files compiled in the other directories to
# make the executable, "M2-binary".  We also make and install the shell script wrapper,
# "M2", whose function is to set LD_LIBRARY_PATH appropriately.

set(CMAKE_VERBOSE_MAKEFILE OFF)

# TODO: do we still want to use M2${EXE} defined in cmake/config.cmake?
# TODO: move main.c here
add_executable(M2-binary timestamp.cpp ../d/main.c)

set_target_properties(M2-binary PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${M2_INSTALL_PREFIX}/${M2_INSTALL_BINDIR})

# NOTE: is d/debug.o extra?
target_link_libraries(M2-binary
  M2-regex M2-interpreter M2-engine M2-system M2-kernel
  Threads::Threads ${LIBGDBM} ${CMAKE_DL_LIBS}) # TODO: handle the latter two differently

set(PKGLIB_LIST    FACTORY FFLAS_FFPACK GIVARO)
set(LIBRARY_LIST   HISTORY READLINE MPSOLVE)
set(LIBRARIES_LIST LAPACK LIBXML2 ${MP_LIBRARY} BDWGC MPFR NTL FLINT MATHICGB MATHIC MEMTAILOR TBB FROBBY)

foreach(LIB IN LISTS LIBRARY_LIST)
  if(${LIB}_FOUND)
    target_link_libraries(M2-binary ${${LIB}_LIBRARY})
  endif()
endforeach()

foreach(LIB IN LISTS LIBRARIES_LIST)
  if(${LIB}_FOUND)
    target_link_libraries(M2-binary ${${LIB}_LIBRARIES})
  endif()
endforeach()

foreach(LIB IN LISTS PKGLIB_LIST)
  if(${LIB}_FOUND)
    target_link_libraries(M2-binary PkgConfig::${LIB})
  endif()
endforeach()

###############################################################################

find_program(LDD ldd)
find_program(OTOOL otool)
if(LDD)
  add_custom_command(TARGET M2-binary POST_BUILD
    COMMENT "## Linked libraries:"
    COMMAND ${SET_LD_LIBRARY_PATH} ${LDD} $<TARGET_FILE:M2-binary>)
elseif(OTOOL)
  add_custom_command(TARGET M2-binary POST_BUILD
    COMMENT "## Linked libraries:"
    COMMAND ${SET_LD_LIBRARY_PATH} ${OTOOL} -L $<TARGET_FILE:M2-binary>)
endif()

###############################################################################

# TODO: strip args

# objcopy --only-keep-debug Macaulay2/bin/M2-binary M2.debug-info
# objdump -x Macaulay2/bin/M2-binary | egrep RPATH || [ $? -lt 2 ]
# objdump -x Macaulay2/bin/M2-binary | egrep "RPATH +/usr/lib64/Macaulay2/lib$" || [ $? -lt 2 ]

if(APPLE)
  set(EXPORT_STRING [[DYLD_LIBRARY_PATH=`dirname "$0"`/../@CMAKE_INSTALL_LIBDIR@/Macaulay2/lib:$DYLD_LIBRARY_PATH]])
elseif(UNIX)
  set(EXPORT_STRING   [[LD_LIBRARY_PATH=`dirname "$0"`/../@CMAKE_INSTALL_LIBDIR@/Macaulay2/lib:$LD_LIBRARY_PATH]])
endif()

set(M2_TEMPLATE "#!/bin/sh\n${EXPORT_STRING} `dirname \"$0\"`/M2@EXE@ \"$@\"\n")
string(CONFIGURE "${M2_TEMPLATE}" M2_TEMPLATE @ONLY)
file(GENERATE OUTPUT ${M2_INSTALL_PREFIX}/${M2_INSTALL_BINDIR}/M2 CONTENT "${M2_TEMPLATE}")
