###############################################################################
## This directory contains the rules for building M2-engine and M2-unit-tests
## memtailor, mathic, and mathicgb are also imported as subdirectories here
## - run unit tests:  ctest -R unit-tests (includes mathicgb and friends)

set(with_tbb ${WITH_TBB}) # for mathicgb

add_subdirectory(memtailor)
add_subdirectory(mathic)
add_subdirectory(mathicgb)

target_compile_options(mathicgb PRIVATE
  -Wall -Wextra -Wno-unused-parameter -Wno-unused-value -Wno-unused-variable -Wno-sign-compare
  -Wno-deprecated-declarations
  )

if(CMAKE_BUILD_TYPE MATCHES "Debug") # Debugging
  add_compile_definitions(MEMT_DEBUG MATHIC_DEBUG MATHICGB_DEBUG)
endif()

###############################################################################
## M2-engine sources

# TODO: determine the fate of these files
set(UNDETERMINED
  basic-mutable-matrices/dmat-ffpack.cpp
  overflow-test.cpp
  unused/test.dd
  )

# c files
# TODO: convert to cpp or simplify if not needed
# MES TODO:   monordering.c removed this, is that correct?
set(CFILES
  error.c
  exptable.c
  table.c
  cytools/box_enum.c
  )
# h files
set(HFILES
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
  basic-rings/vector-arithmetic.hpp
  timing.hpp
  SLP/SLP-defs.hpp
  SLP/SLP-imp.hpp
  basic-mutable-matrices/dmat-gf-flint-big.hpp
  basic-mutable-matrices/dmat-gf-flint.hpp
  basic-mutable-matrices/dmat-lu-inplace.hpp
  basic-mutable-matrices/dmat-lu-qq.hpp
  basic-mutable-matrices/dmat-lu-zzp-ffpack.hpp
  basic-mutable-matrices/dmat-lu-zzp-flint.hpp
  basic-mutable-matrices/dmat-lu.hpp
  basic-mutable-matrices/dmat-qq-flint.hpp
  basic-mutable-matrices/dmat-qq-interface-flint.hpp
  basic-mutable-matrices/dmat-zz-flint.hpp
  basic-mutable-matrices/dmat-zzp-flint.hpp
  engine-includes.hpp
  exceptions.hpp
  rings/geobucket.hpp
  rings/geopoly.hpp
  rings/geovec.hpp
  hash.hpp
  basic-mutable-matrices/mat-arith.hpp
  basic-mutable-matrices/mat-elem-ops.hpp
  basic-mutable-matrices/mat-linalg.hpp
  basic-mutable-matrices/mat-util.hpp
  mutable-matrices/mutablemat-defs.hpp
  mutable-matrices/mutablemat-imp.hpp
  mutable-matrices/mutablemat.hpp
  newdelete.hpp
  monomials/ExponentVector.hpp
  resolutions/res-a0-pair.hpp
  schur-rings/schur-poly-heap.hpp
  basic-mutable-matrices/smat.hpp
  util.hpp
  style.hpp
  f4/f4-types.hpp
  f4/memblock.hpp
  f4/ntuple-monomial.hpp
  f4/varpower-monomial.hpp
  gb-f4/MonomialTypes.hpp
  schreyer-resolutions/res-memblock.hpp
  schreyer-resolutions/res-monomial-types.hpp
  schreyer-resolutions/res-schreyer-order.hpp
  unused/PolynomialStream.hpp
  basic-rings/aring-glue.hpp
  basic-rings/aring-QQ.hpp
  basic-rings/aring-translate.hpp
  )

# these files have .cpp file, but NOT .hpp file
set(CPPONLYFILES
  engine.cpp # TODO: find a new home for the contents
  computations/BRP-gb.cpp
  computations/BRP-interface.cpp
  matrices/matrix-kbasis.cpp
  matrices/matrix-sort.cpp
  matrices/matrix-symm.cpp
  computations/ntl-debugio.cpp
  computations/ntl-internal.cpp
  resolutions/res-a2-gb.cpp
  interface/aring.cpp
  interface/aring.h
  interface/computation.h # no .cpp yet
  interface/cra.cpp
  interface/cra.h
  interface/cone.cpp
  interface/cone.h
  interface/factory.cpp
  interface/factory.h
  interface/flint.cpp
  interface/flint.h
  interface/freemodule.cpp # TODO: .h
  interface/gmp-util.h # only has inline definitions
  interface/groebner.cpp
  interface/groebner.h
  interface/m2-mem.cpp
  interface/m2-mem.h
  interface/m2-types.cpp
  interface/m2-types.h
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
  rings/ring-vecs.cpp
  )

# these files all have .hpp and .cpp files
# MES TODO: put these NCAlgebra files (and friends) into alpha order in list
set(SRCLIST
  computeGV
  cytools/lattice_points
  cytools/lattice-points-normaliz
  cytools/cone-interior-point
  BasicPoly
  BasicPolyList
  BasicPolyListParser
  myalloc
  matrices/matrix-ncbasis
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
  monomials/monomial-collection
  computations/Eschreyer
  computations/LLL
  NAG/NAG # TODO: move raw* functions to e/interface
  SLP/SLP
  computations/assprime
  resolution/betti
  buffer
  coeffrings
  comb
  groebner-computations/comp-gb-declared
  groebner-computations/comp-gb-proxy
  groebner-computations/comp-gb
  resolution/comp-res
  computations/comp
  cra
  debug
  rings/dpoly
  computations/det
  eigen
  finalize
  fplll-interface
  fractionfreeLU
  computations/BRP
  free-modules/freemod
  gauss
  groebner-computations/gb-default
  groebner-computations/gb-homog2
  groebner-computations/gb-sugarless
  groebner-computations/gb-toric
  groebner-computations/gb-walk
  groebner-computations/gbring
  groebner-computations/gbweight
  hermite
  hilb
  imonorder
  int-bag
  interreduce
  interrupted
  basic-mutable-matrices/lapack
  basic-mutable-matrices/mat
  basic-mutable-matrices/dmat
  matrices/matrix-con
  matrices/matrix-stream
  matrices/matrix
  mem
  memory-status
  monomials/monideal-minprimes
  monomials/monideal
  monoid
  monomials/monomial-sets
  monomials/monomial
  monomials/monsort
  monomials/montable
  monomials/montableZZ
  mutablecomplex
  monomials/overflow
  computations/ntl-interface
  computations/pfaff
  basic-rings/reader
  groebner-computations/reducedgb-ZZ
  groebner-computations/reducedgb-field-local
  groebner-computations/reducedgb-field
  groebner-computations/reducedgb-marked
  groebner-computations/reducedgb
  ring-elements/ring-element
  resolutions/res-a0-poly
  resolutions/res-a0
  resolutions/res-a1-poly
  resolutions/res-a1
  resolutions/res-a2
  ringmap
  groebner-computations/sagbi
  free-modules/schreyer-orders
  schur-rings/schur
  schur-rings/schur2
  schur-rings/schurSn
  groebner-computations/spair
  text-io
  monomials/ExponentList
  # Faugère's F4 Algorithm
  gb-f4/Basis
  gb-f4/GBF4Computation
  gb-f4/GBF4Interface
  gb-f4/MacaulayMatrix
  gb-f4/MonomialHashTable
  gb-f4/MonomialLookupTable
  gb-f4/MonomialView
  gb-f4/PolynomialList
  gb-f4/SPairs
  f4/f4-computation
  f4/f4-m2-interface
  f4/f4-monlookup
  f4/f4-spairs
  f4/f4
  f4/hilb-fcn
  f4/monhashtable
  f4/moninfo
  # Schreyer resolution a la Linear algebra (F4 Faugere style)
  schreyer-resolutions/res-f4-computation
  schreyer-resolutions/res-f4-m2-interface
  schreyer-resolutions/res-f4-monlookup
  schreyer-resolutions/res-f4
  schreyer-resolutions/res-moninfo-dense
  schreyer-resolutions/res-moninfo-sparse
  schreyer-resolutions/res-moninfo
  schreyer-resolutions/res-monomial-sorter
  schreyer-resolutions/res-poly-ring
  schreyer-resolutions/res-schreyer-frame
  schreyer-resolutions/res-dep-graph
  # Boolean Involutive Gröbner Bases
  bibasis/bibasis
  bibasis/allocator
  bibasis/launcher
  bibasis/monom
  bibasis/monomDL
  bibasis/monomDRL
  bibasis/monomLex
  bibasis/settings-manager
  basic-rings/aring-CC
  basic-rings/aring-CCC
  basic-rings/aring-RR
  basic-rings/aring-RRR
  basic-rings/aring-RRi
  basic-rings/aring-CCi
  basic-rings/aring-GF-flint-big
  basic-rings/aring-GF-flint
  basic-rings/aring-m2-GF
  basic-rings/aring-QQ-flint
  basic-rings/aring-QQ-gmp
  basic-rings/aring-tower
  basic-rings/aring-ZZ-flint
  basic-rings/aring-ZZ-gmp
  basic-rings/aring-ZZp-ffpack
  basic-rings/aring-ZZp-flint
  basic-rings/aring-ZZp
  basic-rings/aring
  rings/frac
  rings/GF
  rings/localring
  rings/poly
  rings/polyquotient
  rings/polyring
  rings/qring
  rings/ring
  rings/ringelem
  rings/skew
  rings/skewpoly
  rings/solvable
  rings/tower
  rings/weylalg
  rings/ZZ
  rings/ZZp
  schur-rings/schur
  schur-rings/schur2
  schur-rings/schurSn
)

list(TRANSFORM SRCLIST APPEND .cpp OUTPUT_VARIABLE CPPFILES)
list(TRANSFORM SRCLIST APPEND .hpp OUTPUT_VARIABLE HPPFILES)

set(SOURCES ${CPPONLYFILES} ${CPPFILES} ${CFILES})
set(HEADERS ${HPPONLYFILES} ${HPPFILES} ${HFILES})

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
target_link_libraries(M2-engine PUBLIC memtailor mathic mathicgb)

if(EIGEN3_FOUND)
  target_link_libraries(M2-engine PUBLIC Eigen3::Eigen)
endif()

if(OpenMP_FOUND)
  target_link_libraries(M2-engine PUBLIC OpenMP::OpenMP_CXX)
endif()

foreach(LIB IN LISTS PKGLIB_LIST)
  if(${LIB}_FOUND)
    target_link_libraries(M2-engine PUBLIC PkgConfig::${LIB})
  endif()
endforeach()

foreach(LIB IN LISTS LIBRARIES_LIST)
  if(${LIB}_FOUND)
    target_link_libraries(M2-engine PUBLIC ${${LIB}_LIBRARIES})
    target_include_directories(M2-engine PUBLIC
        "$<BUILD_INTERFACE:${${LIB}_INCLUDE_DIR}>")
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
  -Wno-deprecated-copy #-Wno-unknown-pragmas # caused by Givaro and fflas_ffpack
  -Wno-ignored-qualifiers # FIXME: caused by bibasis/tset.hpp:44:9
  -Wno-implicit-fallthrough # FIXME: caused by gb-default.cpp, gb-walk.cpp, hermite.cpp, etc.
  >

  # Intel TODO: -diag-disable 981,869,383,2259,444 -debug -Wimplicit-function-declaration
  )

###############################################################################
## Export the target

# TODO: change ARCHIVE to LIBRARY once M2-engine can be used independently
install(TARGETS M2-engine mathicgb mathic memtailor EXPORT Macaulay2
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}/Macaulay2/lib
  COMPONENT devel EXCLUDE_FROM_ALL
  PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/Macaulay2
  COMPONENT devel EXCLUDE_FROM_ALL)

###############################################################################
## Add source linting and formatting checks for this target, see cmake/prechecks.cmake

if(LINTING)
  _ADD_PRECHECKS(M2-engine "IWYU" "${CFILES}" "${HPPONLYFILES} ${CPPONLYFILES} ${HPPFILES} ${CPPFILES}")
endif()

_ADD_CLANG_FORMAT(M2-engine "${CFILES}" "${HPPONLYFILES} ${CPPONLYFILES} ${HPPFILES} ${CPPFILES}")

###############################################################################
## To try, run ctest or ./M2-unit-tests in Macaulay2/e

if(BUILD_TESTING)
  include(GoogleTest)

  # TODO: overflow tests
  # TODO: break up into multiple branches
  add_executable(M2-unit-tests EXCLUDE_FROM_ALL
    unit-tests/M2-cpp-replacement.cpp

    unit-tests/util-polyring-creation.hpp
    unit-tests/util-polyring-creation.cpp
    unit-tests/RingElem.hpp
    unit-tests/RingElem.cpp
    unit-tests/NewF4Test.cpp
    unit-tests/MonoidTest.cpp
    unit-tests/PolyRingTest.cpp

    unit-tests/ARingTest.hpp
    unit-tests/ARingZZTest.cpp
    unit-tests/ARingZZpTest.cpp
#    unit-tests/ARingGFTest.cpp # TODO: needs rework - ARingGFFlint API has changed
    unit-tests/ARingQQFlintTest.cpp
    unit-tests/ARingQQGmpTest.cpp
    unit-tests/ARingRRTest.cpp
    unit-tests/ARingCCTest.cpp
    unit-tests/ARingRRRTest.cpp
    unit-tests/ARingRRiTest.cpp
    unit-tests/ARingCCCTest.cpp
    unit-tests/NCGroebnerTest.cpp
    unit-tests/WeylAlgebraTest.cpp
    unit-tests/QuotientRingTest.cpp

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

    unit-tests/MatrixIOTest.cpp
    unit-tests/fromStream.cpp
    unit-tests/LatticePointsTest.cpp
    unit-tests/testMain.cpp # not needed, except for GC_INIT

    unit-tests/OverflowTest.cpp
    unit-tests/BasicPolyListParserTest.cpp
    unit-tests/BRP-test.cpp
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
    -Wno-cast-qual # FIXME: caused by NAG/NAG.hpp:559:37 and NAG/NAG.hpp:603:37
    -Wno-sign-compare
    $<$<COMPILE_LANG_AND_ID:CXX,AppleClang,Clang>:-Wno-mismatched-tags> # FIXME
    )

endif()
