# In this directory we link the object files compiled in the other directories to
# make the executable, "M2-binary".  We also make and install the shell script wrapper,
# "M2", whose function is to set LD_LIBRARY_PATH appropriately.

set(CMAKE_VERBOSE_MAKEFILE OFF)

# Any other sources?
# TODO: do we still want to use M2${EXE} defined in cmake/config.cmake?
add_executable(M2-binary timestamp.cpp)
target_link_directories(M2-binary PRIVATE ${CMAKE_BINARY_DIR}/usr-host/lib)
target_include_directories(M2-binary PRIVATE ${CMAKE_BINARY_DIR}/usr-host/include)
# TODO: make sure this is okay on multi-configuration editors
set_target_properties(M2-binary PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_INSTALL_FULL_BINDIR})

# Link the big integer package
if(USING_MPIR)
  target_link_libraries(M2-binary ${MPIR_LIBRARIES})
else()
  target_link_libraries(M2-binary gmp gmpxx)
endif()

# NOTE: is d/debug.o extra?
target_link_libraries(M2-binary
  regex interpreter M2-engine system kernel
  Threads::Threads ${LAPACK_LIBRARIES}
  ${LIBXML2_LIBRARIES} ${LIBGDBM} ${CMAKE_DL_LIBS}
  )

if(HISTORY_FOUND AND READLINE_FOUND)
target_link_libraries(M2-binary
  # Link commandline related libraries
  ${HISTORY_LIBRARY} ${READLINE_LIBRARY}
  )
endif()

if(MATHICGB_FOUND AND MATHIC_FOUND AND MEMTAILOR_FOUND AND FROBBY_FOUND)
target_link_libraries(M2-binary
  # mathicgb depends on mathic, memtailor, and TBB
  ${MATHICGB_LIBRARIES} ${MATHIC_LIBRARIES} ${MEMTAILOR_LIBRARIES} ${TBB_LIBRARIES} ${FROBBY_LIBRARIES}
  )
endif()

if(BDW_GC_FOUND AND FACTORY_FOUND AND FFLAS_FFPACK_FOUND AND GIVARO_FOUND)
target_link_libraries(M2-binary
  PkgConfig::BDW_GC
  PkgConfig::FACTORY # factory depends on flint mpfr ntl gmp
  PkgConfig::FFLAS_FFPACK # fflas-ffpack depends on openmp, givaro, gmp, and gmpxx
  PkgConfig::GIVARO # givaro depends on gmpxx and gmp (or mpir and mpirxx)
  )
endif()

###############################################################################

find_program(LDD ldd)
find_program(OTOOL otool)
if(LDD)
  add_custom_command(TARGET M2-binary POST_BUILD
    COMMENT "## Linked libraries:"
    COMMAND ${LDD} $<TARGET_FILE:M2-binary>
    )
elseif(OTOOL)
  add_custom_command(TARGET M2-binary POST_BUILD
    COMMENT "## Linked libraries:"
    COMMAND ${OTOOL} -L $<TARGET_FILE:M2-binary>
    )
endif()

###############################################################################

# TODO: strip args

# objcopy --only-keep-debug Macaulay2/bin/M2-binary M2.debug-info
# objdump -x Macaulay2/bin/M2-binary | egrep RPATH || [ $? -lt 2 ]
# objdump -x Macaulay2/bin/M2-binary | egrep "RPATH +/usr/lib64/Macaulay2/lib$" || [ $? -lt 2 ]

if(APPLE)
  set(EXPORT_STRING [[DYLD_LIBRARY_PATH=`dirname "$0"`/../${CMAKE_INSTALL_LIBDIR}/Macaulay2/lib:$DYLD_LIBRARY_PATH]])
elseif(UNIX)
  set(EXPORT_STRING [[  LD_LIBRARY_PATH=`dirname "$0"`/../${CMAKE_INSTALL_LIBDIR}/Macaulay2/lib:$LD_LIBRARY_PATH]])
endif()

set(M2_TEMPLATE "#!/bin/sh\n@EXPORT_STRING@ `dirname \"$0\"`/M2@EXE@ \"$@\"\n")
string(CONFIGURE "${M2_TEMPLATE}" M2_TEMPLATE @ONLY)
file(GENERATE
  OUTPUT M2
  CONTENT "${M2_TEMPLATE}"
  )

install(FILES M2
  TYPE BIN
  PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
  )
#install(TARGETS M2-binary RUNTIME
#  FILE_PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
#  )

# TODO
# LD_LIBRARY_PATH="${M2_HOST_PREFIX}/lib:$LD_LIBRARY_PATH" ldd M2-binary.tmp
