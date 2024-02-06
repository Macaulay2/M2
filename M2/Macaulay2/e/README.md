## Engine Notes

### Fall 2020 Work in Progress

1. Parallel directory structure in e, d, m2

- Main header: `e/engine.h`
  - should be short, mostly include other headers
  - organize interface functions in `e/interface`; eg:

        e/interface/matrix.h   // defining types currently in engine.h
        e/interface/matrix.cpp // previously x-mat.cpp

  - each should be self contained, include minimal dependencies (specifically, not `engine.h`)
  - associated interpreter and top level code should be placed in appropriate files; eg:

        d/matrix.dd
        m2/matrix.m2

- unit tests should be provided for all interface routines; eg:

        e/unit-test/matrix.cpp

- Internal routines
  - should be in respective directories, filename based on the classes; eg:

        e/matrix/matrix.hpp
        e/matrix/dense.hpp

2. GC barrier between engine and front end

- issue: ringelem, our_new_delete vs our_new_gc
- goal: ability to hotswap the GC backend by editing only one file
- benefit: allow easy comparison and benchmarking

3. Computations to be written or rewritten

- e.g. gb, smith normal form, etc.


### Engine
- engine.h and x-*
- newdelete hash
- Arithmetic
 - Flint
 - GMP
 - MPFR
 - Arb, etc.
- Monoids
- Rings
- RingElements
- RingMaps
- Matrices
- FreeModules
- Computations
 - LLL
 - GB
 - Resolution
 - Hilbert*
- NAG
- Util
- Interface


--------------------------------------------------------
-- 12/26/2011 MES
Cleaning up code todo:
1. get gtest working
    i.e. a make file target
2. create a gtest file
    linking test file
3. DONE tabs --> spaces
   DONE put in tab-mode-null into each file
    change copyrights
4. remove as many includes as possible
5. maybe make a set of subdirectories of 'e':
  rings
  matrices
  computations
  util
  commands
  tests (gtest stuff)
6. buffer --> use ostringstream?
7. text-io --> maybe keep these except for bignum_text_out?
8. DONE at some point, merge back in the stuff with Jakob, preferably soon
9.

------------------------------------------------------

these files really use overflow facilities:

    gbring.o
    imonorder.o
    matrix-kbasis.o
    matrix.o
    monoid.o
    monorder.o
    overflow.o
    polyring.o
    varpower.o

these files depends on overflow.hpp

    CC.o
    CCC.o
    Eschreyer.o
    GF.o
    QQ.o
    RR.o
    RRR.o
    ZZ.o
    ZZp.o
    comp-gb-declared.o
    comp-gb.o
    comp-res.o
    debug.o
    frac.o
    freemod.o
    gb-default.o
    gb-homog2.o
    gb-sugarless.o
    gb-toric.o
    gbring.o
    gbweight.o
    imonorder.o
    matrix-kbasis.o
    matrix.o
    monoid.o
    monorder.o
    montable.o
    ntuple.o
    overflow.o
    polyring.o
    qring.o
    reducedgb-ZZ.o
    reducedgb-field-local.o
    reducedgb-field.o
    reducedgb.o
    res-a2-gb.o
    res-a2.o
    schorder.o
    skewpoly.o
    solvable.o
    spair.o
    varpower.o
    weylalg.o
    x-gb.o
    x-mat.o
    x-relem.o
