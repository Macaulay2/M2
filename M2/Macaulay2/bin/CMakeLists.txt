# In this directory we link the object files compiled in the other directories to
# make the executable, "M2-binary".  We also make and install the shell script wrapper,
# "M2", whose function is to set LD_LIBRARY_PATH appropriately.

# set(CMAKE_VERBOSE_MAKEFILE on)

# Any other sources?
add_executable(M2-binary timestamp.c)

add_dependencies(M2-binary factory)

# NOTE: is d/debug.o extra?
target_link_libraries(M2-binary
  regex interpreter engine system kernel
  # Order matters here!
  "-L/usr/local/lib"
  stdc++
  factory   mathicgb mathic memtailor readline history frobby flint cdd givaro
  "-framework Accelerate"
  xml2 lzma z
  "-framework Accelerate"
  readline ncurses ntl glpk
  mpc mpfr gmpxx  gmp  gc gdbm tbb
  m c  memtailor  mathic  mathicgb
  # factory mathicgb mathic memtailor frobby cdd givaro lapack blas xml2 lzma z
  # tbb dl m quadmath lapack blas readline tinfo ntl flint gmp glpk mpc mpfr gmpxx gmp gc gdbm pthread tbb dl m c
  )

target_include_directories(M2-binary
  PUBLIC
    ${CMAKE_SOURCE_DIR}include
    ${BOOTSTRAP}/include
    ${BOOTSTRAP}/usr-host/include
    ${BOOTSTRAP}/usr-host/include/cdd
    ${CMAKE_SOURCE_DIR}/submodules/memtailor/src
    ${CMAKE_SOURCE_DIR}/submodules/mathic/src
    ${CMAKE_SOURCE_DIR}/submodules/mathicgb/src
    ${CMAKE_SOURCE_DIR}/submodules/fflas_ffpack/src
    /usr/include/libxml2
    /usr/include/eigen3
    /usr/local/include
  )

target_link_directories(M2-binary PUBLIC
  ${BOOTSTRAP}/usr-host/lib
  ${BOOTSTRAP}/submodules/memtailor/.libs
  ${BOOTSTRAP}/submodules/mathic/.libs
  ${BOOTSTRAP}/submodules/mathicgb/.libs
  /usr/lib/gcc/x86_64-redhat-linux/9
  /lib64
  /usr/lib64
  /usr/lib
  /usr/local/lib
  )
