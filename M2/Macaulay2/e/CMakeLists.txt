set(CMAKE_VERBOSE_MAKEFILE OFF)

# TODO: add -DNDEBUG compiler definition

# Use globbing to find source files.
# Any source files added to include/ or src/ will automatically be added to the project.
#file(GLOB_RECURSE INCLUDE_FILES ${CMAKE_CURRENT_SOURCE_DIR}/*.h)
#file(GLOB_RECURSE SOURCE_FILES_NO_MAIN ${CMAKE_CURRENT_SOURCE_DIR}/*.cpp)
#list(REMOVE_ITEM SOURCE_FILES_NO_MAIN ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp)
#set(SOURCE_FILES ${SOURCE_FILES_NO_MAIN} ${CMAKE_CURRENT_SOURCE_DIR}/src/main.cpp)

#add_library(engine OBJECT  ${INCLUDE_FILES} ${SOURCE_FILES_NO_MAIN} ${SOURCE_FILES})

# TODO: consider using:
# source_group(TREE "${CMAKE_CURRENT_SOURCE_DIR}/base/dir" PREFIX "Header Files" FILES ${FILE_LIST})

set(UNDETERMINED
  dmat-ffpack.cpp
  franzi-brp-test.cpp
  gb-trace.hpp
  overflow-test.cpp
  test.dd
  aring-wrap # both cpp and hpp
  points # both cpp and hpp
  )

# these files are include only.
set(HPPONLYFILES
  timing.hpp
  SLP-defs.hpp
  SLP-imp.hpp
  aring-glue.hpp
  aring-qq.hpp
  aring-translate.hpp
  array.hpp
  dmat-gf-flint-big.hpp
  dmat-gf-flint.hpp
  dmat-lu-inplace.hpp
  dmat-lu-qq.hpp
  dmat-lu-zzp-ffpack.hpp
  dmat-lu-zzp-flint.hpp
  dmat-lu.hpp
  dmat-qq-flint.hpp
  dmat-qq-interface-flint.hpp
  dmat-zz-flint.hpp
  dmat-zzp-flint.hpp
  engine-includes.hpp
  exceptions.hpp
  geopoly.hpp
  geovec.hpp
  hash.hpp
  index.hpp
  mat-arith.hpp
  mat-elem-ops.hpp
  mat-linalg.hpp
  mat-util.hpp
  mutablemat-defs.hpp
  mutablemat-imp.hpp
  queue.hpp
  res-a0-pair.hpp
  ringelem.hpp
  schur-poly-heap.hpp
  smat.hpp
  stdinc-m2.hpp
  stop.hpp
  style.hpp
  f4/moninfo2.hpp
  f4/res-memblock.hpp
  f4/res-monomial-types.hpp
  f4/res-schreyer-order.hpp
  )

# c files, and some h files
set(CFILES
  complex.c
  complex.h
  error.c
  error.h
  exptable.c
  exptable.h
  monordering.c
  monordering.h
  rand.h
  table.c
  table.h
  gmp-util.h
  engine.h
  defgroups.h
  mpreal.h
  )

# these files have .cpp file, but NOT .hpp file
set(CPPONLYFILES
  engine.cpp
  factor.cpp
  flint.cpp
  franzi-gb.cpp
  franzi-interface.cpp
  matrix-kbasis.cpp
  matrix-sort.cpp
  matrix-symm.cpp
  ntl-debugio.cpp
  ntl-internal.cpp
  rand.cpp
  res-a2-gb.cpp
  ring-vecs.cpp
  x-aring.cpp
  x-factor.cpp
  x-factor.hpp
  x-free.cpp
  x-gb.cpp
  x-mat.cpp
  x-monideal.cpp
  x-monoid.cpp
  x-monom.cpp
  x-mutablemat.cpp
  x-relem.cpp
  x-ringmap.cpp
  )

# these files all have .hpp and .cpp files
set(SRCLIST
  Eschreyer
  GF
  LLL
  NAG
  SLP
  ZZ
  ZZp
  aring-CC
  aring-CCC
  aring-RR
  aring-RRR
  aring-gf-flint-big
  aring-gf-flint
  aring-gf-givaro
  aring-m2-gf
  aring-qq-flint
  aring-qq-gmp
  aring-tower
  aring-zz-flint
  aring-zz-gmp
  aring-zzp-ffpack
  aring-zzp-flint
  aring-zzp
  aring
  assprime
  betti
  buffer
  coeffrings
  comb
  comp-gb-declared
  comp-gb-proxy
  comp-gb
  comp-res
  comp
  cra
  debug
  det
  dmat
  dpoly
  eigen
  finalize
  fplll-interface
  frac
  fractionfreeLU
  franzi-brp
  freemod
  gauss
  gb-default
  gb-homog2
  gb-sugarless
  gb-test1
  gb-toric
  gb-walk
  gbring
  gbweight
  hermite
  hilb
  imonorder
  int-bag
  intarray
  interreduce
  interrupted
  lapack
  localring
  mat
  matrix-con
  matrix-stream
  matrix
  mem
  memory-status
  monideal-minprimes
  monideal
  monoid
  monomial-sets
  monomial
  montable
  montableZZ
  mutablecomplex
  mutablemat
  newdelete
  ntl-interface
  ntuple
  overflow
  pfaff
  poly
  polyquotient
  polyring
  polyroots
  qring
  reader
  reducedgb-ZZ
  reducedgb-field-local
  reducedgb-field
  reducedgb-marked
  reducedgb
  relem
  res-a0-poly
  res-a0
  res-a1-poly
  res-a1
  res-a2
  ring
  ringmap
  sagbi
  schorder
  schur
  schur2
  schurSn
  skew
  skewpoly
  solvable
  spair
  text-io
  tower
  util
  varpower
  weylalg
  f4/f4-computation
  f4/f4-m2-interface
  f4/f4-mem
  f4/f4-monlookup
  f4/f4-spairs
  f4/f4-syz
  f4/f4-types
  f4/f4
  f4/gausser
  f4/hilb-fcn
  f4/memblock
  f4/monhashtable
  f4/moninfo
  f4/monsort
  f4/ntuple-monomial
  f4/res-f4-computation
  f4/res-f4-m2-interface
  f4/res-f4-monlookup
  f4/res-f4
  f4/res-gausser-QQ-hybrid
  f4/res-gausser-QQ
  f4/res-gausser-ZZp
  f4/res-gausser
  f4/res-moninfo-dense
  f4/res-moninfo-sparse
  f4/res-moninfo
  f4/res-monomial-sorter
  f4/res-poly-ring
  f4/res-schreyer-frame
  f4/res-varpower-monomial
  f4/varpower-monomial
  )

list(TRANSFORM SRCLIST APPEND .cpp OUTPUT_VARIABLE CPPFILES)
list(TRANSFORM SRCLIST APPEND .hpp OUTPUT_VARIABLE HPPFILES)

set(SOURCES
  ${CFILES}
  ${HPPONLYFILES}
  ${CPPONLYFILES}
  ${CPPFILES}
  ${HPPFILES}
  )

# TODO: write the M2Config.cmake module for users
add_library(M2-engine SHARED ${SOURCES})

# TODO: install the unstripped library with debug_info in the appropriate place.
# On Fedora: /usr/lib/debug/usr/lib64/

add_dependencies(M2-engine interpreter e-includes)

target_link_libraries(M2-engine Eigen3::Eigen)

#TODO: look into this pattern
#    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
#    $<INSTALL_INTERFACE:include>
target_include_directories(M2-engine
  PUBLIC
    ${BOOTSTRAP}/usr-host/include
    ${CMAKE_SOURCE_DIR}/Macaulay2/e
    ${CMAKE_BINARY_DIR}/Macaulay2/e-includes
    ${CMAKE_SOURCE_DIR}/include
    ${BOOTSTRAP}/include
    ${BOOTSTRAP}/usr-host/include
    ${BOOTSTRAP}/usr-host/include/cdd
  PRIVATE
    ${LIBXML2_INCLUDE_DIR}
    ${EIGEN3_INCLUDE_DIR}
    ${CMAKE_SOURCE_DIR}/submodules/memtailor/src
    ${CMAKE_SOURCE_DIR}/submodules/mathic/src
    ${CMAKE_SOURCE_DIR}/submodules/mathicgb/src
    SYSTEM ${CMAKE_SOURCE_DIR}/submodules/fflas_ffpack/src
    ${CMAKE_BINARY_DIR}/Macaulay2/d
    ${CMAKE_SOURCE_DIR}/Macaulay2/c
    SYSTEM /usr/local/include
  )

# TODO: enable INTERPROCEDURAL_OPTIMIZATION?
if(SUPPRESS_COMPILER_WARNINGS)
  if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    # using Clang or AppleClang
    target_compile_options(M2-engine PRIVATE
      -Wall -Wuninitialized
      -Wextra -Wno-unused-parameter
      #  -pedantic-errors -Wno-extra-semi # we cannot put this in since fflas_ffpack appears to use gnu extensions...
      #  -Wno-shadow # not needed for appleclang
      #  -Wno-parentheses # not needed for appleclang
      -Wno-sign-conversion
      -Wno-sign-compare
      #  -Wno-error=pragmas # not needed for appleclang
      -Wno-unknown-pragmas
      -Wno-mismatched-tags # needed currently, but if we clean up includes, should not be needed?
      #  -Wno-unused-local-typedefs # one warning, in mathic.h from x-aring.cpp
      #  -Wno-cast-qual # not needed for appleclang
      )
  endif()
  if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    # using GCC
  endif()
  if(CMAKE_CXX_COMPILER_ID STREQUAL "Intel")
    # using Intel C++
  endif()
  if(CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    # using Visual Studio C++
  endif()
endif()

# TODO
if(FALSE)
enable_testing()
add_executable(macaulay2-test
  # unit-tests
  unit-tests/ARingCCCTest.cpp
  unit-tests/ARingCCTest.cpp
  unit-tests/ARingGFTest.cpp
  unit-tests/ARingQQFlintTest.cpp
  unit-tests/ARingQQGmpTest.cpp
  unit-tests/ARingRRRTest.cpp
  unit-tests/ARingRRTest.cpp
  unit-tests/ARingTest.hpp
  unit-tests/ARingZZTest.cpp
  unit-tests/ARingZZpTest.cpp
  unit-tests/DMatTest.hpp
  unit-tests/DMatZZpTest.cpp
  unit-tests/GivaroTest.cpp
  unit-tests/M2-cpp-replacement.cpp
  unit-tests/M2-replacement.c
  unit-tests/M2mem-replacement.c
  unit-tests/M2mem-replacement.h
  unit-tests/PointArray.cpp
  unit-tests/ResTest.cpp
  unit-tests/RingCCCTest.cpp
  unit-tests/RingQQTest.cpp
  unit-tests/RingRRRTest.cpp
  unit-tests/RingTest.hpp
  unit-tests/RingTowerTest.cpp
  unit-tests/RingZZTest.cpp
  unit-tests/RingZZpTest.cpp
  unit-tests/SubsetTest.cpp
  unit-tests/basics-test.cpp
  unit-tests/fromStream.cpp
#  unit-tests/gtest-include.cpp
  unit-tests/testMain.cpp
)

# find_package(GTest REQUIRED)
# include_directories(${GTEST_INCLUDE_DIRS})
# target_link_libraries(macaulay2-test ${GTEST_BOTH_LIBRARIES})
# add_test(AllTestsInFoo macaulay2-test)


# add_executable(proj-test
#   ${source_files}
#   ${test_files}
#   )

# set(GTEST_INCLUDE_DIRS
#   gtest-1.7.0/include
#   gtest-1.7.0
#   )

# target_include_directories(proj-test PUBLIC
#   ${GTEST_INCLUDE_DIRS}
#   include
#   )
endif()
