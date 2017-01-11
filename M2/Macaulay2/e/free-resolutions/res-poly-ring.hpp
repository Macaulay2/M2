/* Copyright 2015, Michael E. Stillman */

#ifndef _res_poly_ring_hpp_
#define _res_poly_ring_hpp_

#include "moninfo.hpp"
#include "res-gausser.hpp"

#include <cstdio>
typedef int FieldElement;

struct poly {
  int len; // in monomials?  This only determines both sizes below
           // in the case of fixed length monomials
  ResGausser::CoefficientArray coeffs;
  monomial_word *monoms; // This is all of the monomials written contiguously
  poly() : len(0), coeffs(nullptr), monoms(nullptr) {}
};

class ResPolyRing
{
public:
  ResPolyRing(const ResGausser& G, const MonomialInfo& M) : mResGausser(G), mMonoid(M) {}

  const ResGausser& resGausser() const { return mResGausser; }
  const MonomialInfo& monoid() const { return mMonoid; }

  void memUsage(const poly& f, long& nterms, long& bytes_used, long& bytes_alloc) const;
private:
  ResGausser mResGausser;
  MonomialInfo mMonoid;
};

class poly_constructor {
private:
  std::vector<packed_monomial> monoms;
  std::vector<FieldElement> coeffs;
  const ResPolyRing& mRing;
public:
  poly_constructor(const ResPolyRing& R) : mRing(R) { }
  
  void appendTerm(packed_monomial monom, FieldElement coeff)
  {
    monoms.push_back(monom); // a pointer
    coeffs.push_back(coeff);
  }

  void setPoly(poly& result)
  {
    result.len = static_cast<int>(coeffs.size());
    result.coeffs = new FieldElement[result.len];
    result.monoms = new monomial_word[mRing.monoid().max_monomial_size()*result.len];
    // copy coeffs
    for (int i=0; i<result.len; i++)
      result.coeffs[i] = coeffs[i];
    // copy monoms: not pointers, actual monoms
    monomial_word* monomptr = result.monoms;
    for (int i=0; i<result.len; i++)
      {
        mRing.monoid().copy(monoms[i], monomptr);
        monomptr += mRing.monoid().max_monomial_size();
      }
  }
};

class poly_iter {
  const ResPolyRing& mRing;
  const poly& elem;
  long coeff_index;
  long monom_index;
public:
  friend bool operator==(const poly_iter& a, const poly_iter& b);
  friend bool operator!=(const poly_iter& a, const poly_iter& b);

  poly_iter(const ResPolyRing& R, const poly& elem0)
    : mRing(R),
      elem(elem0),
      coeff_index(0),
      monom_index(0)
  {}

  poly_iter(const ResPolyRing& R, const poly& elem0, int) // end
    : mRing(R),
      elem(elem0),
      coeff_index(elem.len),
      monom_index(0)
  {}
  
  int coefficient() const { return elem.coeffs[coeff_index]; }
  packed_monomial monomial() const { return elem.monoms + monom_index; }
  void operator++() { coeff_index++; monom_index += mRing.monoid().max_monomial_size(); }
};

inline bool operator==(const poly_iter& a, const poly_iter& b) { return a.coeff_index == b.coeff_index; }
inline bool operator!=(const poly_iter& a, const poly_iter& b) { return a.coeff_index != b.coeff_index; }

inline void display_poly(FILE* fil, const ResPolyRing& R, const poly& f)
{
  auto end = poly_iter(R, f, 1); // end
  for (auto it = poly_iter(R, f); it != end; ++it)
    {
      FieldElement c = R.resGausser().coeff_to_int(it.coefficient());
      packed_monomial mon = it.monomial();
      if (c != 1) fprintf(fil, "%d", c);
      R.monoid().showAlpha(mon);
    }
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/free-resolutions "
// indent-tabs-mode: nil
// End:
