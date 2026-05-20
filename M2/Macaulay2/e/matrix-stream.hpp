// Copyright 2013 Michael E. Stillman

#ifndef __matrix_stream_hhp__
#define __matrix_stream_hhp__

/**
 * @file matrix-stream.hpp
 * @brief `MatrixStream` --- term-by-term streaming construction of a `Matrix`.
 *
 * Declares the engine's streaming matrix-builder API. A caller
 * constructs `MatrixStream(target_FreeModule)` and drives a
 * hand-rolled state machine:
 * `idealBegin(polyCount)` ->
 * `appendPolynomialBegin(nTerms)` ->
 * `appendTermBegin(component)` ->
 * `appendExponent(var, exp)` (zero or more) ->
 * `appendTermDone(coefficient)` ->
 * `appendPolynomialDone()` ->
 * `idealDone()`. `value()` then returns the finished `Matrix*`
 * (and is `nullptr` until `idealDone`). The class also exposes
 * `modulus()` / `varCount()` / `comCount()` so the same protocol
 * the `mathicgb` stream interface uses can interrogate the
 * target ring. Coefficients are accepted as `mpz_class` and land
 * in the target's coefficient ring; `modulus()` returns the
 * characteristic (the prime when the ring is `Z/p`, `0` for
 * characteristic-zero rings).
 *
 * The companion template `matrixToStream<T>(M, stream)` is the
 * reverse direction --- it walks an existing `Matrix` column by
 * column and emits the same event sequence into any `T` that
 * implements the protocol. The streaming shape is the right
 * choice when a parser emits one int at a time and the full
 * polynomial does not fit in memory ahead of time (msolve and
 * `BasicPolyList` text formats, F4-style streaming input from
 * disk). `matrix-con.hpp` is the by-construction sibling that
 * wants whole columns up front.
 *
 * @see matrix.hpp
 * @see matrix-con.hpp
 * @see BasicPolyListParser.hpp
 */

#include "poly.hpp"
#include "matrix.hpp"
#include "matrix-con.hpp"
#include <gmpxx.h>

#if 0
// Example of the calls used to create an ideal (one rowed matrix), over a poly ring
// over a prime finite field.
// After this. s.value() returns the Matrix with these elements as columns.
    s.idealBegin(2);
      s.appendPolynomialBegin(2); // x^2 - y
        s.appendTermBegin(0);
          s.appendExponent(0,2);
        s.appendTermDone(1);
        s.appendTermBegin(0);
          s.appendExponent(1,1);
        s.appendTermDone(s.modulus() - 1);
      s.appendPolynomialDone();
      s.appendPolynomialBegin(2); // x^3-z
        s.appendTermBegin(0);
          s.appendExponent(0,3);
        s.appendTermDone(1);
        s.appendTermBegin(0);
          s.appendExponent(2,1);
        s.appendTermDone(s.modulus() - 1);
      s.appendPolynomialDone();
    s.idealDone();
#endif

/**
 * @brief Streaming consumer that builds an engine `Matrix` from the
 * mathicgb-style stream callbacks (`idealBegin /
 * appendPolynomialBegin / appendTermBegin / appendExponent /
 * appendTermDone / appendPolynomialDone / idealDone`).
 *
 * @details Mirrors the `BasicPolyListStreamCollector` shape but produces a
 * proper engine `Matrix*` over a `PolyRing` instead of a portable
 * `BasicPolyList`. Used as the bridge between mathicgb's stream
 * input and the engine's typed matrices --- `value()` returns
 * `nullptr` until `idealDone()` is called.
 */
class MatrixStream
{
 public:
  MatrixStream(const FreeModule* F);
  ~MatrixStream();

  const PolyRing& ring() const { return *mPolyRing; }
  const Matrix* value() const
  {
    return mValue;
  }  // This will return null before idealDone() is called.

  // Fields required for the general stream interface (see mathicgb::mathicgb.h)
  using Coefficient = mpz_class;
  // typedef long Coefficient;
  // typedef size_t VarIndex;
  typedef int VarIndex;
  typedef int Exponent;
  //typedef unsigned int Component;
  typedef int Component;

  Coefficient modulus() const
  {
    return static_cast<int>(mPolyRing->characteristic());
  }
  VarIndex varCount() const { return mPolyRing->n_vars(); }
  Component comCount() const { return mFreeModule->rank(); }
  
  void idealBegin(size_t polyCount);
  void appendPolynomialBegin(size_t termCount);
  void appendTermBegin(Component com);

  void appendExponent(VarIndex index, Exponent exponent);
  void appendTermDone(Coefficient coefficient);
  void appendPolynomialDone();
  void idealDone();

 private:
  const PolyRing* mPolyRing;
  const FreeModule* mFreeModule;

  MatrixConstructor mMatrixConstructor;
  const Matrix* mValue;
  Exponent* mCurrentExponents;
  Component mCurrentComponent;

  Nterm** mCurrentColumn;  // array 0..numcomps-1
  Nterm** mLastTerms;
};

template <typename T>
void matrixToStream(const Matrix* M, T& stream)
{
  const Ring* R = M->get_ring();
  const PolyRing* P = R->cast_to_PolyRing();
  assert(P != 0);
  const Ring* KK = P->getCoefficientRing();
  size_t nvars = P->n_vars();
  size_t ncols = M->n_cols();
  int charac = static_cast<int>(P->characteristic());
  assert(charac > 0);
  exponents_t exp =
      ALLOCATE_EXPONENTS(EXPONENT_BYTE_SIZE(nvars));  // allocated on stack
  stream.idealBegin(ncols);
  Matrix::iterator i(M);
  for (int c = 0; c < ncols; c++)
    {
      i.set(c);
      // We need the length of this column, in number of monomials
      size_t nterms = 0;
      for (; i.valid(); i.next())
        {
          Nterm* t = i.entry();
          for ([[maybe_unused]] Nterm& s : t) nterms++;
        }
      stream.appendPolynomialBegin(nterms);

      i.set(c);
      // Now we process each column, sending it to the stream
      for (; i.valid(); i.next())
        {
          Nterm* t = i.entry();
          for (Nterm& s : t)
            {
              P->getMonoid()->to_expvector(s.monom, exp);
              stream.appendTermBegin(i.row());
              for (size_t j = 0; j < nvars; j++)
                if (exp[j] != 0) stream.appendExponent(j, exp[j]);
              std::pair<bool, long> b = KK->coerceToLongInteger(s.coeff);
              assert(b.first);
              int a = static_cast<int>(
                  b.second);  // This will fit, as the charac fits into an int
              if (a < 0) a += charac;
              stream.appendTermDone(a);
            }
        }
      stream.appendPolynomialDone();
    }
  stream.idealDone();
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
