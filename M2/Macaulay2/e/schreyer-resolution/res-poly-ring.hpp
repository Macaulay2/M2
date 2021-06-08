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

class ResPolynomial
{
  friend class ResPolyRing;
  friend class ResPolynomialConstructor;
  friend class ResPolynomialIterator;

 public:
  static long npoly_destructor;
  int len;  // in monomials?  This only determines both sizes below
            // in the case of fixed length monomials
  CoefficientVector coeffs;
  //  std::unique_ptr<FieldElement[]> coeffs;
  std::vector<res_monomial_word> monoms;
  //  std::unique_ptr<res_monomial_word[]> monoms;

 public:
  ResPolynomial() : len(0), coeffs() {}
  ~ResPolynomial()
  {
    if (!coeffs.isNull()) npoly_destructor++;
    //    std::cout << "Calling ~poly()" << std::endl << std::flush;
  }

  ResPolynomial(const ResPolynomial& other) = default;
  ResPolynomial(ResPolynomial&& other) = default;
  ResPolynomial& operator=(const ResPolynomial& other) = default;
  ResPolynomial& operator=(ResPolynomial&& other) = default;
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
  void memUsage(const ResPolynomial& f,
                long& nterms,
                long& bytes_used,
                long& bytes_alloc) const;

 private:
  std::unique_ptr<const ResGausser> mResGausser;
  std::unique_ptr<const ResMonoid> mMonoid;
  const Monoid* mOriginalMonoid;
  const SkewMultiplication* mSkew;
};

class ResPolynomialConstructor
{
 private:
  std::vector<res_packed_monomial> monoms;
  CoefficientVector coeffs;
  //  std::vector<FieldElement> coeffs;
  const ResPolyRing& mRing;

 public:
  static long ncalls;
  static long ncalls_fromarray;

  ResPolynomialConstructor(const ResPolyRing& R) : mRing(R)
  {
    coeffs = R.resGausser().allocateCoefficientVector();
  }

  ~ResPolynomialConstructor() { mRing.resGausser().deallocate(coeffs); }
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
  void setPoly(ResPolynomial& result)
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

  static void setPolyFromArrays(ResPolynomial& result,
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

class ResPolynomialIterator
{
  const ResPolyRing& mRing;
  const ResPolynomial& elem;
  long coeff_index;
  long monom_index;

 public:
  friend bool operator==(const ResPolynomialIterator& a, const ResPolynomialIterator& b);
  friend bool operator!=(const ResPolynomialIterator& a, const ResPolynomialIterator& b);

  ResPolynomialIterator(const ResPolyRing& R, const ResPolynomial& elem0)
      : mRing(R), elem(elem0), coeff_index(0), monom_index(0)
  {
  }

  ResPolynomialIterator(const ResPolyRing& R, const ResPolynomial& elem0, int)  // end
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

inline bool operator==(const ResPolynomialIterator& a, const ResPolynomialIterator& b)
{
  return a.coeff_index == b.coeff_index;
}
inline bool operator!=(const ResPolynomialIterator& a, const ResPolynomialIterator& b)
{
  return a.coeff_index != b.coeff_index;
}

inline void display_poly(std::ostream& o, const ResPolyRing& R, const ResPolynomial& f)
{
  auto end = ResPolynomialIterator(R, f, 1);  // end
  int i = 0;
  for (auto it = ResPolynomialIterator(R, f); it != end; ++it, ++i)
    {
      R.resGausser().out(o, f.coeffs, i);
      res_const_packed_monomial mon = it.monomial();
      R.monoid().showAlpha(mon);
    }
}

bool check_poly(const ResPolyRing& R, const ResPolynomial& f, const ResSchreyerOrder& O);
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
