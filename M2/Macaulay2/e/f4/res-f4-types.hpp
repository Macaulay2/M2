/* Copyright 2014, Michael E. Stillman */

#ifndef _res_f4_types_hpp_
#define _res_f4_types_hpp_

#include "varpower-monomial.hpp"
#include "ntuple-monomial.hpp"
#include "moninfo.hpp"
#include "res-gausser.hpp"

class poly_iter;

typedef int FieldElement;

struct poly {
  int len; // in monomials?  This only determines both sizes below
           // in the case of fixed length monomials
  ResGausser::CoefficientArray coeffs;
  monomial_word *monoms; // This is all of the monomials written contiguously
  poly() : len(0), coeffs(nullptr), monoms(nullptr) {}
};

class poly_constructor {
private:
  std::vector<packed_monomial> monoms;
  std::vector<FieldElement> coeffs;
  const MonomialInfo& mMonoid;
public:
  poly_constructor(const MonomialInfo& M) : mMonoid(M) { }
  
  void appendTerm(packed_monomial monom, FieldElement coeff)
  {
    monoms.push_back(monom); // a pointer
    coeffs.push_back(coeff);
  }

  void setPoly(poly& result)
  {
    result.len = coeffs.size();
    result.coeffs = new FieldElement[result.len];
    result.monoms = new monomial_word[mMonoid.max_monomial_size()*result.len];
    // copy coeffs
    for (long i=0; i<result.len; i++)
      result.coeffs[i] = coeffs[i];
    // copy monoms: not pointers, actual monoms
    monomial_word* monomptr = result.monoms;
    for (long i=0; i<result.len; i++)
      {
        mMonoid.copy(monoms[i], monomptr);
        monomptr += mMonoid.max_monomial_size();
      }
  }
};

class poly_iter {
  const ResGausser& G;
  const MonomialInfo& M;
  const poly& elem;
  long coeff_index;
  long monom_index;
public:
  friend bool operator==(const poly_iter& a, const poly_iter& b);
  friend bool operator!=(const poly_iter& a, const poly_iter& b);

  poly_iter(const ResGausser& G0, const MonomialInfo& M0, const poly& elem0)
    : G(G0),
      M(M0),
      elem(elem0),
      coeff_index(0),
      monom_index(0)
  {}

  poly_iter(const ResGausser& G0, const MonomialInfo& M0, const poly& elem0, int) // end
    : G(G0),
      M(M0),
      elem(elem0),
      coeff_index(elem.len),
      monom_index(0)
  {}
  
  int coefficient() const { return elem.coeffs[coeff_index]; }
  packed_monomial monomial() const { return elem.monoms + monom_index; }
  void operator++() { coeff_index++; monom_index += M.max_monomial_size(); }
};

inline bool operator==(const poly_iter& a, const poly_iter& b) { return a.coeff_index == b.coeff_index; }
inline bool operator!=(const poly_iter& a, const poly_iter& b) { return a.coeff_index != b.coeff_index; }

inline void display_poly(FILE* fil, const ResGausser& G, const MonomialInfo& M, const poly& f)
{
  bool first_term = true;
  auto end = poly_iter(G, M, f, 1); // end
  for (auto it = poly_iter(G, M, f); it != end; ++it)
    {
      FieldElement c = G.coeff_to_int(it.coefficient());
      packed_monomial mon = it.monomial();
      if (c != 1) fprintf(fil, "%d", c);
      M.showAlpha(mon);
      first_term = false;
    }
}

#if 0
typedef unsigned int MonomialWord;
typedef unsigned int ComponentIndex;

// Coefficients.  The implementation of arrays of coeffs
// is done as a private array.  Note that the length is
// not encoded: keep that length separately.
typedef void *CoefficientArray;

typedef std::vector<MonomialWord> MonomialArray;

struct Polynomial {
  int len;
  CoefficientArray coeffs;
  monomial_word *monoms; // This is all of the monomials written contiguously
};

struct FrameElement {
  long mDegree; // Degree of what??
  unsigned long mLength; // number of monomials/coefficients present.
  CoefficientArray mCoefficients; // if null, then lead coefficient is 1
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
  CoefficientArray mCoefficients; 
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
