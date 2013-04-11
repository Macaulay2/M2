#include "matrix-stream.hpp"

MatrixStream::MatrixStream(const PolyRing* R)
  : mPolyRing(R),
    mMatrixConstructor(R->make_FreeModule(1), 0),
    mValue(0),
    mCurrentPoly(0),
    mLastTerm(0),
    mCurrentExponents(newarray_clear(Exponent,R->n_vars()))
{
  // Nothing further to do here

}

MatrixStream::~MatrixStream()
{
  deletearray(mCurrentExponents);
}

void MatrixStream::idealBegin(size_t polyCount)
{
  // We ignore polyCount
  // Nothing to do
}
void MatrixStream::appendPolynomialBegin(size_t termCount)
{
  // we ignore termCount
  // Nothing to do
}
void MatrixStream::appendTermBegin()
{
  // Nothing to do
}

void MatrixStream::appendExponent(VarIndex index, Exponent exponent)
{
  M2_ASSERT(index >= 0);
  M2_ASSERT(index < mPolyRing->n_vars());
  mCurrentExponents[index] = exponent;
}
void MatrixStream::appendTermDone(Coefficient coefficient)
{
  Nterm* t = ring().new_term();
  ring().getMonoid()->from_expvector(mCurrentExponents, t->monom);
  t->coeff = ring().getCoefficients()->from_int(coefficient);
  if (mLastTerm == 0)
    {
      mCurrentPoly = t;
      mLastTerm = t;
      t->next = 0;
    }
  else
    {
      mLastTerm->next = t;
      mLastTerm = t;
    }
  for (size_t i=0; i<ring().n_vars(); i++)
    mCurrentExponents[i] = 0;
}
void MatrixStream::appendPolynomialDone()
{
  // Time to append a polynomial
  //  ring().sort(mCurrentPoly);  // This is just in case the elements come in out of order
  vec v = ring().make_vec(0, mCurrentPoly);
  mMatrixConstructor.append(v);
  mCurrentPoly = 0;
  mLastTerm = 0;
}
void MatrixStream::idealDone()
{
  mValue = mMatrixConstructor.to_matrix();
  mMatrixConstructor = MatrixConstructor();
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
