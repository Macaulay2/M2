/* This code written by Franziska Hinkelmann is in the public domain */

#include "franzi-brp.hpp"
#include "matrix-con.hpp"
#include "matrix.hpp"

extern void testSPolynomial();
extern void gb(IntermediateBasis &F, int n);

brMonomial exponentsToLong(int nvars, const exponents exp)
{
  brMonomial result = 0;
  for (int i = 0; i < nvars; i++)
    if (exp[i] != 0)
      {
        result += 1ul << (nvars - 1 - i);
        // cout << "result " << result;
      }
  // cout << endl;
  return result;
}

void longToExponents(int nvars, brMonomial mono, exponents exp)
{
  //    cout << "longToExponents " << mono << endl;
  for (int i = 0; i < nvars; i++)
    {
      exp[nvars - i - 1] = ((mono & 1ul) ? 1 : 0);
      mono >>= 1;
      //      cout << "after shift longToExponents " << mono << endl;
    }
  //  for( int i=0; i < nvars; i++ )
  //  {
  //    cout << exp[i] << " ";
  //  }
  //  cout << endl;
  //
}

IntermediateBasis BRPSfromMatrix(const Matrix *m)
{
  const Ring *R = m->get_ring();
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  assert(P != 0);  // TODO: change this.
  const Monoid *M = P->getMonoid();
  int n = P->n_vars();
  exponents exp = newarray_atomic(int, n);
  IntermediateBasis F;
  for (int i = 0; i < m->n_cols(); i++)
    {
      vec v = m->elem(i);
      if (v == 0) continue;
      BRP temp;
      for (Nterm *f = v->coeff; f != 0; f = f->next)
        {
          M->to_expvector(f->monom, exp);
          brMonomial mono = exponentsToLong(n, exp);
          temp = temp + BRP(mono);
        }
      F[i] = temp;
    }
  freemem(exp);
  return F;
}

const Matrix *BRPStoMatrix(const PolynomialRing *P, const IntermediateBasis &F)
{
  MatrixConstructor C(P->make_FreeModule(1), 0);
  const Monoid *M = P->getMonoid();
  const Ring *K = P->getCoefficients();
  int n = P->n_vars();
  exponents exp = newarray_atomic(int, n);
  monomial mon = M->make_one();
  for (IntermediateBasis::const_iterator it = F.begin(); it != F.end(); ++it)
    {
      ring_elem f;
      for (monomials::const_iterator it2 = it->second.m.begin();
           it2 != it->second.m.end();
           ++it2)
        {
          brMonomial mono = *it2;
          // cout << "a single mono in brpstoMatrix " << mono << endl;
          longToExponents(n, mono, exp);
          //          for( int i=0; i < n; i++ )
          //          {
          //            cout << exp[i] << " ";
          //          }
          //          cout << endl;
          M->from_expvector(exp, mon);
          ring_elem g = P->make_flat_term(K->one(), mon);
          P->add_to(f, g);
        }
      C.append(P->make_vec(0, f));
    }

  freemem(exp);
  return C.to_matrix();
}

extern "C" const Matrix *rawGbBoolean(const Matrix *m)
{
  IntermediateBasis F = BRPSfromMatrix(m);
  // cout << "first BRP in F " << F.begin()->second << endl;
  const Ring *R = m->get_ring();
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected a polynomial ring");
      return 0;
    }
  if (P->characteristic() != 2)
    {
      ERROR("expected coefficient ring ZZ/2");
      return 0;
    }
  if (not P->getCoefficientRing()->isFinitePrimeField())
    {
      ERROR("expected coefficient ring ZZ/2");
      return 0;
    }
  int n = P->n_vars();
  if (n > 64)
    {
      ERROR("Cannot handle more than 64 variables yet");
      return 0;
    }
  gb(F, n);
  // cout << "first BRP in F after gb() " << F.begin()->second << endl;
  return BRPStoMatrix(P, F);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
