# In this directory we link the object files compiled in the other directories to
# make the executable, "M2-binary".  We also make and install the shell script wrapper,
# "M2", whose function is to set LD_LIBRARY_PATH appropriately.

# set(CMAKE_VERBOSE_MAKEFILE on)

# Any other sources?
add_executable(M2-binary timestamp.c)

# NOTE: is d/debug.o extra?
target_link_libraries(M2-binary
  regex interpreter engine system kernel
  Threads::Threads
  ${LAPACK_LIBRARIES} # lapack depends on blas
  ${LIBXML2_LIBRARIES}
  ${LZMA_LIBRARIES}
  ${MPIR_LIBRARIES}
  PkgConfig::MATHICGB # mathicgb depends on mathic and memtailor
  PkgConfig::FACTORY # factory depends on flint mpfr ntl gmp
  # PkgConfig::FFLAS # fflas-ffpack depends on openmp, givaro, gmpxx, and gmp
  PkgConfig::GIVARO # givaro depends on gmpxx and gmp
  PkgConfig::READLINE
  PkgConfig::GC
  # ${LIBCDD}
  # TODO: should we check and link only when each of these are present?
  ${LIBHISTORY} ${LIBFROBBY} ${LIBGDBM} ${LIBM} ${LIBC} ${LIBGDBM} ${LIBM} ${LIBC} ${CMAKE_DL_LIBS} # dl is needed on linux
  # ${CURSES_LIBRARIES} ${GLPK_LIBRARIES} ${LIBMPC} ${LIBZ} tbb quadmath tinfo # are these not needed?
  )

target_link_directories(M2-binary PUBLIC
  ${BOOTSTRAP}/usr-host/lib
  ${BOOTSTRAP}/submodules/memtailor/.libs
  ${BOOTSTRAP}/submodules/mathic/.libs
  ${BOOTSTRAP}/submodules/mathicgb/.libs
  ${MATHICGB_LIBRARIES}
  /lib64
  /usr/lib64
  /usr/lib
  /usr/local/lib
  )

# LD_LIBRARY_PATH="/home/mahrud/Projects/M2/M2/M2/BUILD/mahrud/build/usr-host/lib:$LD_LIBRARY_PATH" ldd /home/mahrud/Projects/M2/M2/M2/BUILD/mahrud/build/usr-dist/x86_64-Linux-Fedora-30/bin/M2-binary.tmp

# objcopy --only-keep-debug Macaulay2/bin/M2-binary M2.debug-info
# objdump -x Macaulay2/bin/M2-binary | egrep RPATH || [ $? -lt 2 ]
# objdump -x Macaulay2/bin/M2-binary | egrep "RPATH +/usr/lib64/Macaulay2/lib$" || [ $? -lt 2 ]

# mv Macaulay2/bin/M2-binary Macaulay2/bin/M2
