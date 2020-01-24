# In this directory we link the object files compiled in the other directories to
# make the executable, "M2-binary".  We also make and install the shell script wrapper,
# "M2", whose function is to set LD_LIBRARY_PATH appropriately.

# set(CMAKE_VERBOSE_MAKEFILE on)

# Any other sources?
add_executable(M2-binary timestamp.c)

# NOTE: is d/debug.o extra?
target_link_libraries(M2-binary
  regex interpreter engine system kernel
  # Order matters here!
  factory mathicgb mathic memtailor frobby cdd givaro lapack blas xml2 lzma z
  tbb dl gfortran m quadmath lapack blas readline tinfo ntl flint gmp glpk mpc mpfr gmpxx gmp gc gdbm pthread tbb dl m c
  )

target_include_directories(M2-binary
  PUBLIC
    ${SOURCE}include
    ${BOOTSTRAP}/include
    ${BOOTSTRAP}/usr-host/include
    ${BOOTSTRAP}/usr-host/include/cdd
    ${SOURCE}/submodules/memtailor/src
    ${SOURCE}/submodules/mathic/src
    ${SOURCE}/submodules/mathicgb/src
    ${SOURCE}/submodules/fflas_ffpack/src
    /usr/include/libxml2
    /usr/include/eigen3
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
  )
