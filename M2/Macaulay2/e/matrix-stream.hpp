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
        s.appendTermBegin();
          s.appendExponent(0,2);
        s.appendTermDone(1);
        s.appendTermBegin();
          s.appendExponent(1,1);
        s.appendTermDone(s.modulus() - 1);
      s.appendPolynomialDone();
      s.appendPolynomialBegin(2); // x^3-z
        s.appendTermBegin();
          s.appendExponent(0,3);
        s.appendTermDone(1);
        s.appendTermBegin();
          s.appendExponent(2,1);
        s.appendTermDone(s.modulus() - 1);
      s.appendPolynomialDone();
    s.idealDone();
#endif

class MatrixStream {
public:
  MatrixStream(const PolyRing* R);
  ~MatrixStream();

  const PolyRing& ring() const { return *mPolyRing; }
  const Matrix* value() const { return mValue; }  // This will return null before idealDone() is called.

  // Fields required for the general stream interface (see mathicgb::mathicgb.h)
  typedef int Coefficient;
  typedef size_t VarIndex;
  typedef int Exponent;
  
  Coefficient modulus() const { return mPolyRing->charac(); }
  VarIndex varCount() const { return mPolyRing->n_vars(); }
  
  void idealBegin(size_t polyCount);
  void appendPolynomialBegin(size_t termCount);
  void appendTermBegin();
  
  void appendExponent(VarIndex index, Exponent exponent);
  void appendTermDone(Coefficient coefficient);
  void appendPolynomialDone();
  void idealDone();
  
private:
  const PolyRing* mPolyRing;
  MatrixConstructor mMatrixConstructor;
  const Matrix* mValue;
  Nterm* mCurrentPoly;
  Nterm* mLastTerm;
  Exponent* const mCurrentExponents;

};

template<typename T>
void matrixToStream(const Matrix* M, T& stream)
{
  const Ring *R = M->get_ring();
  const PolyRing *P = R->cast_to_PolyRing();
  M2_ASSERT(P != 0);
  const Ring *KK = P->getCoefficientRing();
  size_t nvars = P->n_vars();
  size_t npolys = M->n_cols();
  int charac = static_cast<int>(P->charac());
  M2_ASSERT(charac > 0);
  exponents exp = ALLOCATE_EXPONENTS(EXPONENT_BYTE_SIZE(nvars)); // allocated on stack
  stream.idealBegin(npolys);
  for (int i=0; i<npolys; i++)
    {
      Nterm *t = M->elem(0,i);

      size_t nterms = 0;
      for (Nterm *s = t; s != 0; s=s->next) nterms++;
      stream.appendPolynomialBegin(nterms);

      for (Nterm *s = t; s != 0; s=s->next)
        {
          P->getMonoid()->to_expvector(s->monom, exp);
          stream.appendTermBegin();
          for (size_t i=0; i<nvars; i++)
            if (exp[i] != 0)
              stream.appendExponent(i,exp[i]);
          int c = KK->coerce_to_int(s->coeff);
          if (c < 0) c += charac;
          stream.appendTermDone(c);
        }
      stream.appendPolynomialDone();
    }
  stream.idealDone();
  return;
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:


