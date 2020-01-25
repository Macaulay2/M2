#set(CMAKE_VERBOSE_MAKEFILE on)

# TODO: add -DNDEBUG compiler definition

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

# set(SOURCES
#   error.c table.c exptable.c monordering.c complex.c
#   eigen.cpp monomial-sets.cpp mutablecomplex.cpp localring.cpp
#   f4/res-monomial-sorter.cpp f4/res-varpower-monomial.cpp f4/res-f4-monlookup.cpp f4/res-moninfo-dense.cpp f4/res-moninfo-sparse.cpp f4/res-moninfo.cpp f4/res-poly-ring.cpp f4/res-f4-m2-interface.cpp f4/res-schreyer-frame.cpp f4/res-f4.cpp f4/res-f4-computation.cpp f4/res-gausser.cpp f4/res-gausser-ZZp.cpp f4/res-gausser-QQ.cpp f4/res-gausser-QQ-hybrid.cpp f4/f4.cpp f4/f4-syz.cpp f4/f4-mem.cpp f4/f4-monlookup.cpp f4/f4-types.cpp f4/f4-computation.cpp f4/f4-spairs.cpp f4/f4-m2-interface.cpp f4/hilb-fcn.cpp f4/monhashtable.cpp f4/monsort.cpp f4/moninfo.cpp f4/ntuple-monomial.cpp f4/varpower-monomial.cpp f4/gausser.cpp
#   betti.cpp util.cpp GF.cpp ZZ.cpp ZZp.cpp reader.cpp matrix-stream.cpp cra.cpp aring-zz-gmp.cpp aring-qq-gmp.cpp aring-zz-flint.cpp aring-qq-flint.cpp aring-zzp-flint.cpp aring-RR.cpp aring-CC.cpp aring-RRR.cpp aring-CCC.cpp aring-tower.cpp aring-gf-flint-big.cpp aring-gf-flint.cpp aring-m2-gf.cpp aring-zzp.cpp aring-gf-givaro.cpp aring-zzp-ffpack.cpp aring.cpp schurSn.cpp schur2.cpp dpoly.cpp tower.cpp interreduce.cpp gb-test1.cpp finalize.cpp NAG.cpp SLP.cpp overflow.cpp memory-status.cpp imonorder.cpp sagbi.cpp monideal-minprimes.cpp lapack.cpp LLL.cpp fractionfreeLU.cpp ntl-interface.cpp fplll-interface.cpp mat.cpp dmat.cpp coeffrings.cpp Eschreyer.cpp gbweight.cpp qring.cpp polyquotient.cpp poly.cpp debug.cpp comp.cpp comp-gb.cpp comp-gb-proxy.cpp comp-gb-declared.cpp comp-res.cpp res-a0.cpp res-a0-poly.cpp res-a1.cpp res-a1-poly.cpp res-a2.cpp reducedgb.cpp reducedgb-field.cpp reducedgb-marked.cpp reducedgb-field-local.cpp reducedgb-ZZ.cpp montable.cpp montableZZ.cpp schorder.cpp assprime.cpp buffer.cpp comb.cpp det.cpp frac.cpp franzi-brp.cpp freemod.cpp gb-default.cpp gb-homog2.cpp gb-sugarless.cpp gb-toric.cpp gb-walk.cpp gbring.cpp gauss.cpp hermite.cpp hilb.cpp int-bag.cpp intarray.cpp matrix.cpp matrix-con.cpp mem.cpp monideal.cpp monoid.cpp monomial.cpp ntuple.cpp polyring.cpp pfaff.cpp relem.cpp ring.cpp ringmap.cpp schur.cpp skew.cpp skewpoly.cpp solvable.cpp spair.cpp text-io.cpp varpower.cpp weylalg.cpp interrupted.cpp newdelete.cpp flint.cpp rand.cpp ntl-internal.cpp ntl-debugio.cpp matrix-kbasis.cpp matrix-symm.cpp matrix-sort.cpp ring-vecs.cpp res-a2-gb.cpp engine.cpp x-aring.cpp x-monom.cpp x-monoid.cpp x-relem.cpp x-free.cpp x-mat.cpp x-gb.cpp x-ringmap.cpp x-mutablemat.cpp franzi-gb.cpp franzi-interface.cpp x-monideal.cpp x-factor.cpp polyroots.cpp
#   )

add_library(engine OBJECT ${SOURCES})

add_dependencies(engine interpreter)

target_include_directories(engine
  PUBLIC
    ${BOOTSTRAP}/usr-host/include
    ${SOURCE}/Macaulay2/e
    # ../e-includes # FIXME
    ${BOOTSTRAP}/Macaulay2/e-includes
    ${SOURCE}/include
    ${BOOTSTRAP}/include
    ${BOOTSTRAP}/usr-host/include
    ${BOOTSTRAP}/usr-host/include/cdd
    ${SOURCE}/submodules/memtailor/src
    ${SOURCE}/submodules/mathic/src
    ${SOURCE}/submodules/mathicgb/src
    SYSTEM ${SOURCE}/submodules/fflas_ffpack/src
    /usr/include/libxml2
    /usr/local/include/eigen3
    ${CMAKE_CURRENT_BINARY_DIR}/../d
    ${SOURCE}/Macaulay2/c
    SYSTEM /usr/local/include
  )

target_compile_options(engine PRIVATE
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
