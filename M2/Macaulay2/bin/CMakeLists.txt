# In this directory we link the object files compiled in the other directories to
# make the executable, "M2-binary".  We also make and install the shell script wrapper,
# "M2", whose function is to set LD_LIBRARY_PATH appropriately.

set(CMAKE_VERBOSE_MAKEFILE OFF)

# Any other sources?
# TODO: do we still want to use M2${EXE} defined in cmake/config.cmake?
add_executable(M2-binary timestamp.cpp)
target_link_directories(M2-binary PRIVATE ${CMAKE_BINARY_DIR}/usr-host/lib)
target_include_directories(M2-binary PRIVATE ${CMAKE_BINARY_DIR}/usr-host/include)
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
  Threads::Threads ${LAPACK_LIBRARIES} PkgConfig::BDWGC
  ${LIBXML2_LIBRARIES} ${LZMA_LIBRARIES} ${LIBGDBM} ${CMAKE_DL_LIBS}
  )

if(MATHICGB_FOUND AND MATHIC_FOUND AND MEMTAILOR_FOUND)
target_link_libraries(M2-binary
  # mathicgb depends on mathic, memtailor, and TBB
  ${MATHICGB_LIBRARIES} ${MATHIC_LIBRARIES} ${MEMTAILOR_LIBRARIES} ${TBB_LIBRARIES} frobby
  )
endif()

if(FACTORY_FOUND AND FFLAS_FFPACK_FOUND AND GIVARO_FOUND)
target_link_libraries(M2-binary
  PkgConfig::FACTORY # factory depends on flint mpfr ntl gmp
  PkgConfig::FFLAS_FFPACK # fflas-ffpack depends on openmp, givaro, gmp, and gmpxx
  PkgConfig::GIVARO # givaro depends on gmpxx and gmp (or mpir and mpirxx)
  )
endif()

# Link commandline related libraries
if(UNIX)
  target_link_libraries(M2-binary ${LIBHISTORY} PkgConfig::READLINE)
elseif(APPLE)
  target_link_libraries(M2-binary history readline termcap)
endif()

get_target_property(LINK_LIBRARIES M2-binary LINK_LIBRARIES)
message("## Library information: ${LINK_LIBRAREIS}")

# TODO
# LD_LIBRARY_PATH="/home/mahrud/Projects/M2/M2/M2/BUILD/mahrud/build/usr-host/lib:$LD_LIBRARY_PATH" ldd /home/mahrud/Projects/M2/M2/M2/BUILD/mahrud/build/usr-dist/x86_64-Linux-Fedora-30/bin/M2-binary.tmp

# objcopy --only-keep-debug Macaulay2/bin/M2-binary M2.debug-info
# objdump -x Macaulay2/bin/M2-binary | egrep RPATH || [ $? -lt 2 ]
# objdump -x Macaulay2/bin/M2-binary | egrep "RPATH +/usr/lib64/Macaulay2/lib$" || [ $? -lt 2 ]

#AC_OUTPUT() # files configured in this folder
#Macaulay2/bin/M2

#if(APPLE)
#  set(EXPORT_STRING [[export DYLD_LIBRARY_PATH=`dirname "$0"`/../@tail_librariesdir@:$DYLD_LIBRARY_PATH ;;]])
#elseif(UNIX)
#  set(EXPORT_STRING [[export   LD_LIBRARY_PATH=`dirname "$0"`/../@tail_librariesdir@:$LD_LIBRARY_PATH ;;]])
#endif()
# TEMPLATE:
##! /bin/sh
## We assume that @ pre_bindir @/.. is ${pre_exec_prefix} (see configure.ac)
#@EXPORT_STRING@ `dirname "$0"`/M2@EXE@ "$@"
