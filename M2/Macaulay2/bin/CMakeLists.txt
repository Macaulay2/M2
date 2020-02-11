# In this directory we link the object files compiled in the other directories to
# make the executable, "M2-binary".  We also make and install the shell script wrapper,
# "M2", whose function is to set LD_LIBRARY_PATH appropriately.

# set(CMAKE_VERBOSE_MAKEFILE on)

# Any other sources?
add_executable(M2-binary timestamp.c)

add_dependencies(M2-binary factory-target)

# NOTE: is d/debug.o extra?
target_link_libraries(M2-binary
  regex interpreter engine system kernel
  blas lapack
  # Order matters here!
  factory mathicgb mathic memtailor readline history frobby flint cdd givaro
  xml2 lzma z ncurses ntl glpk mpc mpfr gmpxx gmp gc gdbm m c
  pthread dl # needed on linux
  # tbb quadmath tinfo # are these needed?
  )

target_link_directories(M2-binary PUBLIC
  ${BOOTSTRAP}/usr-host/lib
  ${BOOTSTRAP}/submodules/memtailor/.libs
  ${BOOTSTRAP}/submodules/mathic/.libs
  ${BOOTSTRAP}/submodules/mathicgb/.libs
  /lib64
  /usr/lib64
  /usr/lib
  /usr/local/lib
  )
