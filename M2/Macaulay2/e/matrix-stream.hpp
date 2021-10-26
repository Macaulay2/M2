// Copyright 2013 Michael E. Stillman

#ifndef __matrix_stream_hhp__
#define __matrix_stream_hhp__

#include "poly.hpp"
#include "matrix.hpp"
#include "matrix-con.hpp"

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
  typedef int Coefficient;
  // typedef long Coefficient;
  typedef size_t VarIndex;
  typedef int Exponent;
  typedef unsigned int Component;

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
  exponents exp =
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
          for (Nterm* s = t; s != 0; s = s->next) nterms++;
        }
      stream.appendPolynomialBegin(nterms);

      i.set(c);
      // Now we process each column, sending it to the stream
      for (; i.valid(); i.next())
        {
          Nterm* t = i.entry();
          for (Nterm* s = t; s != 0; s = s->next)
            {
              P->getMonoid()->to_expvector(s->monom, exp);
              stream.appendTermBegin(i.row());
              for (size_t j = 0; j < nvars; j++)
                if (exp[j] != 0) stream.appendExponent(j, exp[j]);
              std::pair<bool, long> b = KK->coerceToLongInteger(s->coeff);
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
