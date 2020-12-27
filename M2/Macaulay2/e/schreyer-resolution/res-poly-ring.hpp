/* Copyright 2015-2016, Michael E. Stillman */

#ifndef _res_poly_ring_hpp_
#define _res_poly_ring_hpp_

#include "newdelete.hpp"                               // for our_new_delete
#include "schreyer-resolution/res-gausser.hpp"         // for CoefficientVector
#include "schreyer-resolution/res-moninfo.hpp"         // for ResMonoid
#include "schreyer-resolution/res-monomial-types.hpp"  // for res_monomial_word

#include <iostream>                                    // for ostream
#include <memory>                                      // for unique_ptr
#include <type_traits>                                 // for swap
#include <vector>                                      // for vector, swap

class Monoid;
class SkewMultiplication;
struct ResSchreyerOrder;

class poly
{
  friend class ResPolyRing;
  friend class poly_constructor;
  friend class poly_iter;

 public:
  static long npoly_destructor;
  int len;  // in monomials?  This only determines both sizes below
            // in the case of fixed length monomials
  CoefficientVector coeffs;
  //  std::unique_ptr<FieldElement[]> coeffs;
  std::vector<res_monomial_word> monoms;
  //  std::unique_ptr<res_monomial_word[]> monoms;

 public:
  poly() : len(0), coeffs() {}
  ~poly()
  {
    if (!coeffs.isNull()) npoly_destructor++;
    //    std::cout << "Calling ~poly()" << std::endl << std::flush;
  }

  poly(const poly& other) = default;
  poly(poly&& other) = default;
  poly& operator=(const poly& other) = default;
  poly& operator=(poly&& other) = default;
};

class ResPolyRing : public our_new_delete
{
 public:
  ResPolyRing(const ResGausser* G, const ResMonoid* M, const Monoid* origM)
    : mResGausser(G), mMonoid(M), mOriginalMonoid(origM), mSkew(nullptr)
  {
  }
  ResPolyRing(const ResGausser* G,
              const ResMonoid* M,
              const Monoid* origM,
              const SkewMultiplication* skewInfo)
    : mResGausser(G), mMonoid(M), mOriginalMonoid(origM), mSkew(skewInfo)
  {
  }

  const ResGausser& resGausser() const { return *mResGausser; }
  const ResMonoid& monoid() const { return *mMonoid; }
  const Monoid& originalMonoid() const { return *mOriginalMonoid; }
  bool isSkewCommutative() const { return mSkew != nullptr; }
  const SkewMultiplication* skewInfo() const { return mSkew; }
  void memUsage(const poly& f,
                long& nterms,
                long& bytes_used,
                long& bytes_alloc) const;

 private:
  std::unique_ptr<const ResGausser> mResGausser;
  std::unique_ptr<const ResMonoid> mMonoid;
  const Monoid* mOriginalMonoid;
  const SkewMultiplication* mSkew;
};

class poly_constructor
{
 private:
  std::vector<res_packed_monomial> monoms;
  CoefficientVector coeffs;
  //  std::vector<FieldElement> coeffs;
  const ResPolyRing& mRing;

 public:
  static long ncalls;
  static long ncalls_fromarray;

  poly_constructor(const ResPolyRing& R) : mRing(R)
  {
    coeffs = R.resGausser().allocateCoefficientVector();
  }

  ~poly_constructor() { mRing.resGausser().deallocate(coeffs); }
  void appendMonicTerm(res_packed_monomial monom)
  {
    monoms.push_back(monom);  // a pointer
    mRing.resGausser().pushBackOne(coeffs);
  }

  void pushBackTerm(res_packed_monomial monom)
  {
    monoms.push_back(monom);  // a pointer
  }

  CoefficientVector& coefficientInserter() { return coeffs; }
  void setPoly(poly& result)
  {
    ncalls++;
    result.len = static_cast<int>(mRing.resGausser().size(coeffs));
    std::swap(result.coeffs, coeffs);
    result.monoms.resize(result.len * mRing.monoid().max_monomial_size());

    // copy monoms: not pointers, actual monoms
    res_monomial_word* monomptr = result.monoms.data();
    for (int i = 0; i < result.len; i++)
      {
        mRing.monoid().copy(monoms[i], monomptr);
        monomptr += mRing.monoid().monomial_size(monoms[i]);
      }
  }

  static void setPolyFromArrays(poly& result,
                                int len,
                                CoefficientVector& coeffs,
                                std::vector<res_monomial_word>& monoms)
  {
    ncalls_fromarray++;
    result.len = len;
    result.coeffs.swap(coeffs);
    std::swap(result.monoms, monoms);
  }
};

class poly_iter
{
  const ResPolyRing& mRing;
  const poly& elem;
  long coeff_index;
  long monom_index;

 public:
  friend bool operator==(const poly_iter& a, const poly_iter& b);
  friend bool operator!=(const poly_iter& a, const poly_iter& b);

  poly_iter(const ResPolyRing& R, const poly& elem0)
      : mRing(R), elem(elem0), coeff_index(0), monom_index(0)
  {
  }

  poly_iter(const ResPolyRing& R, const poly& elem0, int)  // end
      : mRing(R),
        elem(elem0),
        coeff_index(elem.len),
        monom_index(0)
  {
  }

  int coefficient_index() const { return static_cast<int>(coeff_index); }
  //  int coefficient() const { return elem.coeffs[coeff_index]; }
  res_const_packed_monomial monomial() const
  {
    return elem.monoms.data() + monom_index;
  }
  void operator++()
  {
    coeff_index++;
    monom_index +=
        mRing.monoid().monomial_size(elem.monoms.data() + monom_index);
  }
};

inline bool operator==(const poly_iter& a, const poly_iter& b)
{
  return a.coeff_index == b.coeff_index;
}
inline bool operator!=(const poly_iter& a, const poly_iter& b)
{
  return a.coeff_index != b.coeff_index;
}

inline void display_poly(std::ostream& o, const ResPolyRing& R, const poly& f)
{
  auto end = poly_iter(R, f, 1);  // end
  int i = 0;
  for (auto it = poly_iter(R, f); it != end; ++it, ++i)
    {
      R.resGausser().out(o, f.coeffs, i);
      res_const_packed_monomial mon = it.monomial();
      R.monoid().showAlpha(mon);
    }
}

bool check_poly(const ResPolyRing& R, const poly& f, const ResSchreyerOrder& O);
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
