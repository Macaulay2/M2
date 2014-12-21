/* Copyright 2014, Michael E. Stillman */

#ifndef _res_f4_types_hpp_
#define _res_f4_types_hpp_

#include "varpower-monomial.hpp"
#include "ntuple-monomial.hpp"
#include "moninfo.hpp"
#include "res-gausser.hpp"

struct poly {
  int len;
  ResGausser::F4CoefficientArray coeffs;
  monomial_word *monoms; // This is all of the monomials written contiguously
};

#if 0
typedef unsigned int MonomialWord;
typedef unsigned int ComponentIndex;

// Coefficients.  The implementation of arrays of coeffs
// is done as a private array.  Note that the length is
// not encoded: keep that length separately.
typedef void *F4CoefficientArray;

typedef std::vector<MonomialWord> MonomialArray;

struct Polynomial {
  int len;
  F4CoefficientArray coeffs;
  monomial_word *monoms; // This is all of the monomials written contiguously
};

struct FrameElement {
  long mDegree; // Degree of what??
  unsigned long mLength; // number of monomials/coefficients present.
  F4CoefficientArray mCoefficients; // if null, then lead coefficient is 1
  MonomialArray mMonomials; // lead Monomial is the first monomial in this vector
};

struct Level {
  std::vector<FrameElement> mLevel; // Could be large
};

struct Frame {
  std::vector<Level> mFrame; // This one has small size.
};

// Data types for the matrices over the coefficient ring:
struct RowElement {
  packed_monomial mMultiplier; // points into a memory block.  Unowned memory
  ComponentIndex mColumn;  // This row represents mMultiplier * g[mColumn]
    // where mColumn indexes into a Level

  unsigned int mRowLength; // length of both of the following arrays
  F4CoefficientArray mCoefficients; 
  ComponentIndex* mComponentIndices; // points into a memory block
};

struct ColumnElement {
  packed_monomial mMonomial;
  ComponentIndex mRow;  // Which row is being used as a pivot for this column
                        // -1: means none.  -1: not set yet.
};

class CoefficientMatrix {
private:
  std::vector <RowElement> mRows;
  std::vector <ColumnElement> mColumns;
  // Also include the hash table here?

public:
  // method to construct the matrix
  //  process row
  //  

  // method to reduce matrix (places answers directly into the frame?)
};


/*
Algorithm:
1. translate M2 GB into the 0,1 levels of the frame
2. construct the rest of the frame
  this entails:
    a. compute ideal quotient
    b. sort these (in Schreyer order)
    c. resort all of them at this new level, by degree.
    d. make sure that we know which monomials are "above" a given one (DescendentRange: but now
       they are all over the place)
  all of the above information can be in a memory arena.   This will last the lifetime of
  the computation.
3. For each (level,degree):
  create memory blocks (perhaps arenas?)
  create the coefficient matrix
    need hash table for the columns
    really, have two matrices (or, set of rows, same columnset): reducers, spairs
    (remember: we ignore all columns which are not in the initial submodule).
  reduce matrix
    when reducing an spair row: keep track of the reduction row process.
    after that, translate this to a polynomial in the frame
4. betti numbers
  create matrix (over coeff ring) of (level,deg) --> (level-1,deg).
  compute its rank
  create a Betti display for this
5. Minimalization of the matrices
  to be determined: how best to do this
 */
#endif
#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
