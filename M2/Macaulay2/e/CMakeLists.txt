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

# c files
# TODO: convert to cpp or simplify if not needed
# MES TODO:   monordering.c removed this, is that correct?
set(CFILES
  complex.c
  error.c
  exptable.c
  table.c
  )
# h files
set(HFILES
  complex.h
  error.h
  exptable.h
  table.h
  engine.h
  defgroups.h
  mpreal.h
  )

# these files are include only.
set(HPPONLYFILES
  MemoryBlock.hpp
  NCAlgebras/Range.hpp
  VectorArithmetic.hpp
  ARingElem.hpp
  newdelete.hpp
  timing.hpp
  SLP-defs.hpp
  SLP-imp.hpp
  aring-glue.hpp
  aring-qq.hpp
  aring-translate.hpp
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
  style.hpp
  f4/f4-types.hpp
  f4/memblock.hpp
  schreyer-resolution/res-memblock.hpp
  schreyer-resolution/res-monomial-types.hpp
  schreyer-resolution/res-schreyer-order.hpp
  )

# these files have .cpp file, but NOT .hpp file
set(CPPONLYFILES
  engine.cpp # TODO: find a new home for the contents
  franzi-gb.cpp
  franzi-interface.cpp
  matrix-kbasis.cpp
  matrix-sort.cpp
  matrix-symm.cpp
  ntl-debugio.cpp
  ntl-internal.cpp
  res-a2-gb.cpp
  ring-vecs.cpp

  interface/aring.cpp
  interface/aring.h
  interface/computation.h # no .cpp yet
  interface/cra.cpp
  interface/cra.h
  interface/factory.cpp
  interface/factory.h
  interface/flint.cpp
  interface/flint.h
  interface/freemodule.cpp # TODO: .h
  interface/gmp-util.h # only has inline definitions
  interface/groebner.cpp
  interface/groebner.h
  interface/matrix.cpp
  interface/matrix.h
  interface/monoid.cpp
  interface/monoid.h
  interface/monomial-ideal.cpp
  interface/monomial-ideal.h
  interface/monomial-ordering.cpp
  interface/monomial-ordering.h
  interface/mutable-matrix.cpp
  interface/mutable-matrix.h
  interface/polyroots.cpp # the declarations are in factory.h
  interface/random.cpp
  interface/random.h
  interface/ring.cpp
  interface/ring.h
  interface/ringelement.cpp
  interface/ringelement.h
  interface/ringmap.cpp
  interface/ringmap.h
  )

# these files all have .hpp and .cpp files
# MES TODO: put these NCAlgebra files (and friends) into alpha order in list
set(SRCLIST
  myalloc
  matrix-ncbasis
  M2FreeAlgebraQuotient
  M2FreeAlgebra
  NCAlgebras/Word
  NCAlgebras/NCReduction
  NCAlgebras/FreeAlgebraQuotient
  NCAlgebras/FreeAlgebra
  NCAlgebras/FreeMonoid
  NCAlgebras/WordTable
  NCAlgebras/SuffixTree
  NCAlgebras/NCGroebner
  NCAlgebras/NCF4
  NCAlgebras/OverlapTable
  NCResolutions/nc-res-computation
  Polynomial
  monomial-collection
  Eschreyer
  GF
  LLL
  NAG # TODO: move raw* functions to e/interface
  SLP
  ZZ
  ZZp
  aring-RRi
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
  monsort
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
  tower # TODO: move rawTowerTranslatePoly to e/interface
  util
  varpower
  weylalg
  # Faugère's F4 Algorithm
  f4/f4-computation
  f4/f4-m2-interface
  f4/f4-mem
  f4/f4-monlookup
  f4/f4-spairs
  f4/f4
  f4/gausser
  f4/hilb-fcn
  f4/monhashtable
  f4/moninfo
  f4/ntuple-monomial
  f4/varpower-monomial
  # Schreyer resolution a la Linear algebra (F4 Faugere style)
  schreyer-resolution/res-f4-computation
  schreyer-resolution/res-f4-m2-interface
  schreyer-resolution/res-f4-monlookup
  schreyer-resolution/res-f4
  schreyer-resolution/res-gausser-QQ-hybrid
  schreyer-resolution/res-gausser-QQ
  schreyer-resolution/res-gausser-ZZp
  schreyer-resolution/res-gausser
  schreyer-resolution/res-moninfo-dense
  schreyer-resolution/res-moninfo-sparse
  schreyer-resolution/res-moninfo
  schreyer-resolution/res-monomial-sorter
  schreyer-resolution/res-poly-ring
  schreyer-resolution/res-schreyer-frame
  schreyer-resolution/res-varpower-monomial
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

set(SOURCES ${CPPONLYFILES} ${CPPFILES} ${CFILES})
set(HEADERS ${HPPONLYFILES} ${HPPFILES} ${HFILES})
list(APPEND HEADERS ${CMAKE_BINARY_DIR}/include/M2/config.h)

###############################################################################
## Generate TAGS file
# TODO: the order is somewhat different, is it a problem?

if(ETAGS)
  set(TAGS TAGS)
  set_source_files_properties(TAGS PROPERTIES GENERATED true)
  add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/TAGS
    COMMENT "Generating e/TAGS file"
    COMMAND ${ETAGS} -o TAGS ${HEADERS} ${SOURCES}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endif()

###############################################################################
## Compile the engine
# TODO: enable INTERPROCEDURAL_OPTIMIZATION?

# TODO: remove STATIC once M2-engine can be used independently, then we can
# set -DBUILD_SHARED_LIBS=ON to generate a shared library
add_library(M2-engine STATIC ${SOURCES} ${HEADERS} ${TAGS})

# TODO: Which headers should be included in the development build?
set_target_properties(M2-engine PROPERTIES PUBLIC_HEADER "${HEADERS}")

target_include_directories(M2-engine PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<INSTALL_INTERFACE:include/Macaulay2>)

# TODO: make engine independent of interpreter headers
target_link_libraries(M2-engine PUBLIC M2-interpreter)

if(EIGEN3_FOUND)
  target_link_libraries(M2-engine PUBLIC Eigen3::Eigen)
endif()

if(OpenMP_FOUND)
  target_link_libraries(M2-engine PUBLIC OpenMP::OpenMP_CXX)
endif()

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
## Export the target

# TODO: change ARCHIVE to LIBRARY once M2-engine can be used independently
install(TARGETS M2-engine EXPORT Macaulay2
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/Macaulay2/lib
  COMPONENT devel EXCLUDE_FROM_ALL
  PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/Macaulay2
  COMPONENT devel EXCLUDE_FROM_ALL)

###############################################################################
## Add source linting and formatting checks for this target, see cmake/prechecks.cmake

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
    unit-tests/ARingGFTest.cpp # FIXME: see aring-gf-givaro.cpp:199
    unit-tests/ARingQQFlintTest.cpp
    unit-tests/ARingQQGmpTest.cpp
    unit-tests/ARingRRTest.cpp
    unit-tests/ARingCCTest.cpp
    unit-tests/ARingRRRTest.cpp
    unit-tests/ARingCCCTest.cpp
    unit-tests/NCGroebnerTest.cpp

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

  if(BDWGC_FOUND)
  target_link_libraries(M2-unit-tests M2-engine ${BDWGC_LIBRARIES} ${CMAKE_DL_LIB})
  endif()

  ## Add the tests
  # TODO: valgrind --track-origins=yes ...
  gtest_discover_tests(M2-unit-tests TEST_PREFIX unit-tests:)

  ## Link with Googletest
  if(GTEST_FOUND)
  target_link_libraries(M2-unit-tests GTest::GTest GTest::Main)
  endif()

  target_compile_options(M2-unit-tests PRIVATE
    -Wno-cast-qual # FIXME: caused by NAG.hpp:559:37 and NAG.hpp:603:37
    -Wno-sign-compare
    $<$<COMPILE_LANG_AND_ID:CXX,AppleClang,Clang>:-Wno-mismatched-tags> # FIXME
    )

  ###############################################################################
  ## GivaroTest.cpp has a main function and does not use gtest
  #add_executable(M2-unit-tests-givaro unit-tests/GivaroTest.cpp) # FIXME: line 155
  #target_link_libraries(M2-unit-tests-givaro M2-engine)
  #add_test(NAME unit-tests:GivaroTest WORKING_DIRECTORY ${CMAKE_BINARY_DIR} COMMAND M2-unit-tests-givaro)
endif()
