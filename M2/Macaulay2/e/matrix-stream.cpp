#include "matrix-stream.hpp"

MatrixStream::MatrixStream(const FreeModule* F)
    : mFreeModule(F), mMatrixConstructor(F, 0), mValue(0), mCurrentComponent(0)
{
  mPolyRing = F->get_ring()->cast_to_PolyRing();
  assert(mPolyRing != 0);
  mCurrentExponents = newarray_clear(Exponent, mPolyRing->n_vars());
  mCurrentColumn = newarray_clear(Nterm*, F->rank());
  mLastTerms = newarray_clear(Nterm*, F->rank());
}

MatrixStream::~MatrixStream()
{
  freemem(mCurrentExponents);
  freemem(mCurrentColumn);
  freemem(mLastTerms);
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
void MatrixStream::appendTermBegin(Component com)
{
  mCurrentComponent = com;
  // Nothing else to do
}

void MatrixStream::appendExponent(VarIndex index, Exponent exponent)
{
  assert(index < mPolyRing->n_vars());
  mCurrentExponents[index] = exponent;
}
void MatrixStream::appendTermDone(Coefficient coefficient)
{
  // Now we need to create an Nterm, and attach it at mCurrentComponent
  Nterm* t = ring().new_term();
  ring().getMonoid()->from_expvector(mCurrentExponents, t->monom);
  t->coeff = ring().getCoefficients()->from_long(coefficient);
  t->next = 0;
  if (mLastTerms[mCurrentComponent] == 0)
    {
      mCurrentColumn[mCurrentComponent] = t;
      mLastTerms[mCurrentComponent] = t;
    }
  else
    {
      mLastTerms[mCurrentComponent]->next = t;
      mLastTerms[mCurrentComponent] = t;
    }
  for (size_t i = 0; i < ring().n_vars(); i++) mCurrentExponents[i] = 0;
}
void MatrixStream::appendPolynomialDone()
{
  // Time to append a polynomial
  //  ring().sort(mCurrentPoly);  // This is just in case the elements come in
  //  out of order
  vec v = ring().make_vec_from_array(mFreeModule->rank(), mCurrentColumn);

  mMatrixConstructor.append(v);
  for (int i = 0; i < mFreeModule->rank(); i++)
    {
      mCurrentColumn[i] = 0;
      mLastTerms[i] = 0;
    }
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
