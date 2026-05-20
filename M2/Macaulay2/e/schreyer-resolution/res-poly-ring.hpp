/* Copyright 2015-2016, Michael E. Stillman */

#ifndef _res_poly_ring_hpp_
#define _res_poly_ring_hpp_

/**
 * @file schreyer-resolution/res-poly-ring.hpp
 * @brief `ResPolyRing` and `ResPolynomial` --- resolution-tuned polynomial-ring view and value type.
 *
 * Declares the F4 resolution's stripped-down counterpart of
 * `GBRing` / `gbvector`. `ResPolynomial` stores a polynomial as
 * a length counter, a flat `ElementArray` of coefficients
 * (decoded through `VectorArithmetic`), and a flat
 * `std::vector<res_monomial_word>` of packed monomials whose
 * layout is controlled by `ResMonoid`; the destructor counts
 * itself in the static `npoly_destructor` debug tally. The
 * three friends --- `ResPolyRing`, `ResPolynomialConstructor`,
 * and `ResPolynomialIterator` --- are the only entry points
 * allowed to peek inside the layout, which lets future reshapes
 * stay invisible to callers. `ResPolyRing` ties together the
 * coefficient ring (via `VectorArithmetic`), the resolution
 * monoid `M`, the original engine monoid `origM` (kept for
 * promotion / lifting paths), and an optional
 * `SkewMultiplication` so resolutions over skew-commutative
 * rings get the right sign in `mult`.
 *
 * The split from `PolynomialRing` exists because resolutions do
 * vastly more monomial arithmetic per second than ordinary
 * polynomial ops, and the standard ring's virtual dispatch and
 * full degree-monoid recursion would dominate. Here the
 * `VectorArithmetic` indirection inlines, and the
 * `ResMonoid` choice between dense (`res-moninfo-dense.hpp`)
 * and sparse (`res-moninfo-sparse.hpp`) layouts can be
 * benchmarked without touching call sites.
 *
 * @see VectorArithmetic.hpp
 * @see res-moninfo.hpp
 * @see res-monomial-types.hpp
 * @see res-f4-computation.hpp
 * @see res-schreyer-frame.hpp
 */

#include "VectorArithmetic.hpp"                        // for VectorArithmetic
#include "newdelete.hpp"                               // for our_new_delete
#include "schreyer-resolution/res-moninfo.hpp"         // for ResMonoid
#include "schreyer-resolution/res-monomial-types.hpp"  // for res_monomial_word

#include <iostream>                                    // for ostream
#include <memory>                                      // for unique_ptr
#include <type_traits>                                 // for swap
#include <vector>                                      // for vector, swap

class Monoid;
class SkewMultiplication;
struct ResSchreyerOrder;

/**
 * @brief Polynomial type used by the F4 resolution engine: parallel
 * coefficient vector and concatenated monomial buffer.
 *
 * @details `coeffs` is an `ElementArray` of `len` coefficients managed
 * through `VectorArithmetic`. `monoms` is a flat
 * `std::vector<res_monomial_word>` holding all `len` monomials laid
 * out back-to-back (variable-size in the general case --- iterate
 * with `ResPolynomialIterator`, which advances by
 * `ResMonoid::monomial_size`). The static `npoly_destructor` counter
 * tracks how many destructors actually freed a coefficient array,
 * useful for leak / churn diagnostics. Copy and move are default
 * because both members own their data.
 */
class ResPolynomial
{
  friend class ResPolyRing;
  friend class ResPolynomialConstructor;
  friend class ResPolynomialIterator;

 public:
  static long npoly_destructor;
  int len;  // in monomials?  This only determines both sizes below
            // in the case of fixed length monomials
  ElementArray coeffs;
  std::vector<res_monomial_word> monoms;

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

/**
 * @brief The polynomial-ring view the F4 resolution engine reduces against:
 * coefficient arithmetic plus the engine-specific `ResMonoid`.
 *
 * @details Bundles a `VectorArithmetic` (built from `baseRing`, usually the
 * base field but not required to be for non-minimal complexes), the
 * `ResMonoid` that owns monomial layout and order, and the
 * original `Monoid` for translating back to engine-side polynomials.
 * Optional `SkewMultiplication*` enables the skew-commutative
 * resolution path; `isSkewCommutative()` reports whether it's set.
 */
class ResPolyRing : public our_new_delete
{
 public:
  ResPolyRing(const Ring* baseRing,
              const ResMonoid* M,
              const Monoid* origM)
    : mVectorArithmetic(baseRing),
      mMonoid(M),
      mOriginalMonoid(origM),
      mSkew(nullptr)
  {
  }
  ResPolyRing(const Ring* baseRing, // usually the underlying base field, although for non-minimal complexes, it does not need to be a field.
              const ResMonoid* M,
              const Monoid* origM,
              const SkewMultiplication* skewInfo)
    : mVectorArithmetic(baseRing),
      mMonoid(M),
      mOriginalMonoid(origM),
      mSkew(skewInfo)
  {
  }

  const VectorArithmetic& vectorArithmetic() const { return mVectorArithmetic; }
  const ResMonoid& monoid() const { return *mMonoid; }
  const Monoid& originalMonoid() const { return *mOriginalMonoid; }
  bool isSkewCommutative() const { return mSkew != nullptr; }
  const SkewMultiplication* skewInfo() const { return mSkew; }
  void memUsage(const ResPolynomial& f,
                long& nterms,
                long& bytes_used,
                long& bytes_alloc) const;

 private:
  VectorArithmetic mVectorArithmetic;
  std::unique_ptr<const ResMonoid> mMonoid;
  const Monoid* mOriginalMonoid;
  const SkewMultiplication* mSkew;
};

/**
 * @brief Builder that accumulates terms into a `ResPolynomial` and finalises
 * the layout in one shot via `setPoly`.
 *
 * @details Stages monomials as `res_packed_monomial` pointers in `monoms`
 * and grows a parallel `ElementArray` in `coeffs`. `appendMonicTerm`
 * pushes a monomial and an implicit `1` coefficient; `pushBackTerm`
 * pushes just the monomial (the caller is expected to write the
 * coefficient through `coefficientInserter`). `setPoly` allocates
 * the final flat `monoms` buffer in the target `ResPolynomial`,
 * copies each monomial in, and swaps the coefficient array over.
 * Maintains two static counters (`ncalls`, `ncalls_fromarray`) for
 * tracking how often each construction path is used.
 */
class ResPolynomialConstructor
{
 private:
  std::vector<res_packed_monomial> monoms;
  ElementArray coeffs;
  const ResPolyRing& mRing;

 public:
  static long ncalls;
  static long ncalls_fromarray;

  ResPolynomialConstructor(const ResPolyRing& R) : mRing(R)
  {
    coeffs = R.vectorArithmetic().allocateElementArray();
  }

  ~ResPolynomialConstructor() {
    mRing.vectorArithmetic().deallocateElementArray(coeffs);
  }
  void appendMonicTerm(res_packed_monomial monom)
  {
    monoms.push_back(monom);  // a pointer
    mRing.vectorArithmetic().pushBackOne(coeffs);
  }

  void pushBackTerm(res_packed_monomial monom)
  {
    monoms.push_back(monom);  // a pointer
  }

  ElementArray& coefficientInserter() { return coeffs; }
  void setPoly(ResPolynomial& result)
  {
    ncalls++;
    result.len = static_cast<int>(mRing.vectorArithmetic().size(coeffs));
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
                                ElementArray& coeffs,
                                std::vector<res_monomial_word>& monoms)
  {
    ncalls_fromarray++;
    result.len = len;
    result.coeffs.swap(coeffs);
    std::swap(result.monoms, monoms);
  }
};

/**
 * @brief Forward iterator over the terms of a `ResPolynomial`.
 *
 * @details Carries two cursors: `coeff_index` ticks through `elem.coeffs`
 * one per term, and `monom_index` walks the flat `elem.monoms`
 * buffer in monomial-size strides (`ResMonoid::monomial_size`),
 * which is variable when monomials are not fixed-width. Equality
 * compares the coefficient index only --- the "end" iterator pins
 * it to `elem.len`.
 */
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
      R.vectorArithmetic().displayElement(o, f.coeffs, i);
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
