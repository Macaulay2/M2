/* Copyright 2015-2016, Michael E. Stillman */

#ifndef _res_poly_ring_hpp_
#define _res_poly_ring_hpp_

#include "../skew.hpp"
#include "res-varpower-monomial.hpp"
//#include "ntuple-monomial.hpp"
#include "res-moninfo.hpp"
#include "res-gausser.hpp"
#include "res-schreyer-order.hpp"

#include <memory>  // For std::unique_ptr
#include <iostream>

typedef int FieldElement;

class poly {
  friend class ResPolyRing;
  friend class poly_constructor;
  friend class poly_iter;
public:
  static long npoly_destructor;
  int len; // in monomials?  This only determines both sizes below
           // in the case of fixed length monomials
  std::unique_ptr<FieldElement[]> coeffs;
  std::unique_ptr<res_monomial_word[]> monoms;

public:  
  poly() : len(0), coeffs(nullptr), monoms(nullptr) {}

  ~poly()
  {
    if (coeffs) npoly_destructor++;
    //    std::cout << "Calling ~poly()" << std::endl << std::flush;
  }
  
  poly(const poly& other) = default;
  poly(poly&& other) = default;
  poly& operator=(const poly& other) = default;
  poly& operator=(poly&& other) = default;
};

class ResPolyRing
{
public:
  ResPolyRing(const ResGausser* G, const ResMonoid* M) : mResGausser(G), mMonoid(M), mSkew(nullptr) {}
  ResPolyRing(const ResGausser* G, const ResMonoid* M, const SkewMultiplication* skewInfo) : mResGausser(G), mMonoid(M), mSkew(skewInfo) {}  
  
  const ResGausser& resGausser() const { return *mResGausser; }
  const ResMonoid& monoid() const { return *mMonoid; }

  bool isSkewCommutative() const { return mSkew != nullptr; }
  const SkewMultiplication* skewInfo() const { return mSkew; }

  void memUsage(const poly& f, long& nterms, long& bytes_used, long& bytes_alloc) const;
private:
  std::unique_ptr<const ResGausser> mResGausser;
  std::unique_ptr<const ResMonoid> mMonoid;
  const SkewMultiplication* mSkew;
};

class poly_constructor {
private:
  std::vector<res_packed_monomial> monoms;
  std::vector<FieldElement> coeffs;
  const ResPolyRing& mRing;
public:
  static long ncalls;
  static long ncalls_fromarray;

  poly_constructor(const ResPolyRing& R) : mRing(R) { }
  
  void appendTerm(res_packed_monomial monom, FieldElement coeff)
  {
    monoms.push_back(monom); // a pointer
    coeffs.push_back(coeff);
  }

  void setPoly(poly& result)
  {
    ncalls++;
    result.len = static_cast<int>(coeffs.size());
    result.coeffs.reset(new FieldElement[result.len]);
    result.monoms.reset(new res_monomial_word[mRing.monoid().max_monomial_size()*result.len]);

    // copy coeffs
    for (int i=0; i<result.len; i++)
      result.coeffs[i] = coeffs[i];
    // copy monoms: not pointers, actual monoms
    res_monomial_word* monomptr = result.monoms.get();
    for (int i=0; i<result.len; i++)
      {
        mRing.monoid().copy(monoms[i], monomptr);
        monomptr += mRing.monoid().max_monomial_size();
      }
  }

  static void setPolyFromArrays(poly& result,
                                int len,
                                std::unique_ptr<FieldElement[]>& coeffs,
                                std::unique_ptr<res_monomial_word[]>& monoms)
  {
    ncalls_fromarray++;
    result.len = len;
    result.coeffs.swap(coeffs);
    result.monoms.swap(monoms);
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
  res_packed_monomial monomial() const { return elem.monoms.get() + monom_index; }
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
      res_packed_monomial mon = it.monomial();
      if (c != 1) fprintf(fil, "%d", c);
      R.monoid().showAlpha(mon);
    }
}

bool check_poly(const ResPolyRing& R, const poly&f, const ResSchreyerOrder& O);
#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
