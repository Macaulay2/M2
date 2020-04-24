###############################################################################
## This directory contains the rules for building M2-engine and M2-unit-tests
## - run unit tests:  ctest -R unit-tests

# TODO: determine the fate of these files
set(UNDETERMINED
  dmat-ffpack.cpp
  franzi-brp-test.cpp
  gb-trace.hpp
  overflow-test.cpp
  test.dd
  aring-wrap # both cpp and hpp
  points # both cpp and hpp
  )

# c files, and some h files
# TODO: convert to cpp or simplify if not needed
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

# these files are include only.
set(HPPONLYFILES
  newdelete.hpp
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
  geobucket.hpp
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
  mutablemat.hpp
  newdelete.hpp
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

# these files have .cpp file, but NOT .hpp file
set(CPPONLYFILES
  engine.cpp
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
  # Faugère's F4 Algorithm
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
  # Boolean Involutive Gröbner Bases
  bibasis/bibasis
  bibasis/allocator
  bibasis/launcher
  bibasis/monom
  bibasis/monomDL
  bibasis/monomDRL
  bibasis/monomLex
  bibasis/settings-manager
  )

list(TRANSFORM SRCLIST APPEND .cpp OUTPUT_VARIABLE CPPFILES)
list(TRANSFORM SRCLIST APPEND .hpp OUTPUT_VARIABLE HPPFILES)

###############################################################################
## Compile the engine
# TODO: enable INTERPROCEDURAL_OPTIMIZATION?

# set -DBUILD_SHARED_LIBS=ON to generate a shared library
add_library(M2-engine STATIC
  ${HPPONLYFILES}
  ${CPPONLYFILES}
  ${HPPFILES}
  ${CPPFILES}
  ${CFILES}
  )

target_include_directories(M2-engine
  PUBLIC . ${CMAKE_BINARY_DIR}/Macaulay2/d)

target_link_libraries(M2-engine
  PUBLIC M2-interpreter M2-regex)

if(EIGEN3_FOUND)
  target_link_libraries(M2-engine PUBLIC Eigen3::Eigen)
endif()

foreach(LIB IN LISTS LIBRARIES_LIST)
  if(${LIB}_FOUND)
    target_link_libraries(M2-engine PUBLIC ${${LIB}_LIBRARIES})
  endif()
endforeach()

foreach(LIB IN LISTS PKGLIB_LIST)
  if(${LIB}_FOUND)
    target_link_libraries(M2-engine PUBLIC PkgConfig::${LIB})
    target_link_libraries(M2-engine PUBLIC PkgConfig::${LIB})
  endif()
endforeach()

# Compiler warning flags
target_compile_options(M2-engine PRIVATE
  -Wno-cast-qual # FIXME
  -Wno-sign-compare
  -Wno-unused-local-typedefs # FIXME: caused by mathic/Geobucket.h:510:49

  $<$<COMPILE_LANG_AND_ID:CXX,AppleClang,Clang>:
  -Wno-deprecated-register # caused by mps/mpc.h:114:37
  -Wno-mismatched-tags # FIXME: caused by scc1
  -Wno-unused-variable # FIXME: caused by schur2.cpp:597:31
  >

  $<$<COMPILE_LANG_AND_ID:CXX,GNU>:
  -Wno-deprecated-copy -Wno-unknown-pragmas # caused by Givaro and fflas_ffpack
  -Wno-ignored-qualifiers # FIXME: caused by bibasis/tset.hpp:44:9
  -Wno-implicit-fallthrough # FIXME: caused by gb-default.cpp, gb-test1.cpp, gb-walk.cpp, hermite.cpp, etc.
  >

  # Intel TODO: -diag-disable 981,869,383,2259,444 -debug -Wimplicit-function-declaration
  )

###############################################################################
# Add source linting and formatting checks for this target, see cmake/prechecks.cmake

if(LINTING)
  _ADD_PRECHECKS(M2-engine "${CFILES}" "${HPPONLYFILES} ${CPPONLYFILES} ${HPPFILES} ${CPPFILES}")
endif()

_ADD_CLANG_FORMAT(M2-engine "${CFILES}" "${HPPONLYFILES} ${CPPONLYFILES} ${HPPFILES} ${CPPFILES}")

###############################################################################
## To try, run ctest or ./M2-unit-tests in Macaulay2/e

if(BUILD_TESTING)
  include(GoogleTest)

  # TODO: overflow tests
  # TODO: break up into multiple branches
  add_executable(M2-unit-tests
    unit-tests/M2-cpp-replacement.cpp
    unit-tests/M2-replacement.c
    unit-tests/M2mem-replacement.c
    unit-tests/M2mem-replacement.h

    unit-tests/ARingTest.hpp
    unit-tests/ARingZZTest.cpp
    unit-tests/ARingZZpTest.cpp
    unit-tests/ARingGFTest.cpp
    unit-tests/ARingQQFlintTest.cpp
    unit-tests/ARingQQGmpTest.cpp
    unit-tests/ARingRRTest.cpp
    unit-tests/ARingCCTest.cpp
    unit-tests/ARingRRRTest.cpp
    unit-tests/ARingCCCTest.cpp

    unit-tests/RingTest.hpp
    unit-tests/RingZZTest.cpp
    unit-tests/RingZZpTest.cpp
    unit-tests/RingQQTest.cpp
    unit-tests/RingCCCTest.cpp
    unit-tests/RingRRRTest.cpp
    unit-tests/RingTowerTest.cpp

    unit-tests/DMatTest.hpp
    unit-tests/DMatZZpTest.cpp

    unit-tests/ResTest.cpp
    unit-tests/PointArray.cpp
    unit-tests/SubsetTest.cpp
    unit-tests/basics-test.cpp

    unit-tests/fromStream.cpp
    unit-tests/testMain.cpp # not needed, except for GC_INIT
    )
  ## Separate main(), separate executable
  # TODO: why does this run main on compilation?
  add_executable(M2-unit-tests-givaro unit-tests/GivaroTest.cpp) # FIXME: line 155

  target_link_libraries(M2-unit-tests M2-engine)
  target_link_libraries(M2-unit-tests-givaro M2-engine)

  ## Link with Googletest
  if(GTEST_FOUND)
  target_link_libraries(M2-unit-tests GTest::GTest GTest::Main)
  target_include_directories(M2-unit-tests PRIVATE ${GTEST_INCLUDE_DIR})

  target_link_libraries(M2-unit-tests-givaro GTest::GTest GTest::Main)
  target_include_directories(M2-unit-tests-givaro PRIVATE ${GTEST_INCLUDE_DIR})
  endif()

  target_compile_options(M2-unit-tests PRIVATE
    -Wno-cast-qual # FIXME: caused by NAG.hpp:559:37 and NAG.hpp:603:37
    -Wno-sign-compare
    $<$<COMPILE_LANG_AND_ID:CXX,AppleClang,Clang>:-Wno-mismatched-tags> # FIXME
    )

  # TODO: valgrind --track-origins=yes ...
  # add_test(NAME M2-unit-tests WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND M2-unit-tests)

  ## Add the tests
  gtest_discover_tests(M2-unit-tests TEST_PREFIX unit-tests:)
  gtest_discover_tests(M2-unit-tests-givaro TEST_PREFIX unit-tests:Givaro:)
endif()
