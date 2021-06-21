// Copyright 1998  Michael E. Stillman

#include "LLL.hpp"

#include "relem.hpp"
#include "matrix.hpp"
#include "text-io.hpp"
#include "interrupted.hpp"

bool LLLoperations::checkThreshold(ring_elem num, ring_elem den)
{
  // Makes sure that 1/4 < num/den <= 1
  // Assumed: num, den are elements of ZZ.
  mpz_srcptr a = num.get_mpz();
  mpz_srcptr b = den.get_mpz();
  if (mpz_sgn(a) < 0) return false;
  if (mpz_sgn(b) < 0) return false;
  if (mpz_cmp(a, b) > 0) return false;  // return false if a>b.
  mpz_t c;
  mpz_init(c);
  mpz_mul_2exp(c, a, 2);  // c = 4*a
  int cmp = mpz_cmp(b, c);
  mpz_clear(c);
  if (cmp >= 0) return false;
  return true;
}

bool LLLoperations::initializeLLL(const MutableMatrix *A,
                                  gmp_QQ threshold,
                                  MutableMatrix *&LLLstate)
{
  // First check m: should be a matrix over globalZZ.
  if (A == 0 || A->get_ring() != globalZZ)
    {
      ERROR("LLL only defined for matrices over ZZ");
      return false;
    }

  ring_elem num = globalZZ->from_int(mpq_numref(threshold));
  ring_elem den = globalZZ->from_int(mpq_denref(threshold));
  // Check that 'threshold' is in range, and set the numerator, denom
  if (!checkThreshold(num, den))
    {
      ERROR("LLL threshold should be in the range (1/4, 1]");
      globalZZ->remove(num);
      globalZZ->remove(den);
      return false;
    }

  // LLLstate has n+4 columns, and n rows.
  // First n columns: LLLstate(i,i) = D#i
  //                  LLLstate(i,j) = lambda#(j,i) for
  // Last four columns just have entries in row 0:
  // The entries are: k, kmax, alphaTop, alphaBottom: all are ZZ values.

  size_t n = A->n_cols();
  LLLstate = MutableMatrix::zero_matrix(globalZZ, n, n + 4, A->is_dense());
  if (n > 0)
    {
      LLLstate->set_entry(0, n, globalZZ->from_long(1));  // k := 2
      // LLLstate->set_entry(0,n+1,globalZZ->from_long(0)); // kmax := 1
      LLLstate->set_entry(
          0, n + 2, num);  // Set threshold numerator, denominator
      LLLstate->set_entry(0, n + 3, den);
      ring_elem dot;
      A->dot_product(0, 0, dot);
      LLLstate->set_entry(0, 0, dot);  // D#1 := dot(A,0,0)
    }
  return true;
}

bool LLLoperations::Lovasz(MutableMatrix *lambda,
                           int k,
                           ring_elem alphaTop,
                           ring_elem alphaBottom)
{
  // Test:alphaBottom * (D#(k-2) * D#k + lambda#(k,k-1)^2) <
  //              alphaTop * D#(k-1)^2
  ring_elem D2, D1, D, L;
  mpz_t a, b;
  lambda->get_entry(k - 1, k - 1, D1);
  lambda->get_entry(k, k, D);
  bool Lnotzero = lambda->get_entry(k - 1, k, L);
  if (k == 1)
    mpz_init_set(a, D.get_mpz());
  else
    {
      mpz_init(a);
      lambda->get_entry(k - 2, k - 2, D2);
      mpz_mul(a, D2.get_mpz(), D.get_mpz());
    }
  mpz_init(b);
  if (Lnotzero)
    {
      mpz_mul(b, L.get_mpz(), L.get_mpz());
      mpz_add(a, a, b);
    }
  mpz_mul(a, a, alphaBottom.get_mpz());  // This is the LHS.

  mpz_mul(b, D1.get_mpz(), D1.get_mpz());
  mpz_mul(b, alphaTop.get_mpz(), b);  // RHS
  int cmp = mpz_cmp(a, b);
  mpz_clear(a);
  mpz_clear(b);
  return (cmp < 0);
}

void LLLoperations::REDI(int k,
                         int ell,
                         MutableMatrix *A,
                         MutableMatrix *Achange,  // can be NULL
                         MutableMatrix *lambda)
{
  // set q = ...
  // negate q.
  ring_elem Dl, mkl, q;
  if (!lambda->get_entry(ell, k, mkl)) return;
  lambda->get_entry(ell, ell, Dl);
  mpz_srcptr a = mkl.get_mpz();
  mpz_srcptr b = Dl.get_mpz();  // b = D#ell
  mpz_t c, d;
  mpz_init(c);
  mpz_init(d);
  mpz_mul_2exp(c, a, 1);  // c = 2*lambda#(k,ell)
  mpz_abs(d, c);          // d = abs(2*lambda#(k,ell)
  mpz_add(c, c, b);       // c = 2*lambda#(k,ell) + D#ell
  mpz_mul_2exp(d, b, 1);  // d = 2*D#ell
  mpz_fdiv_q(c, c, d);    // c = (almost) final q
  mpz_neg(c, c);
  q = ring_elem(c);

  // A->addColumnMultiple(ell,q,k);
  // lambda->addColumnMultiple(ell,q,k);

  A->column_op(k, q, ell);
  if (Achange) Achange->column_op(k, q, ell);
  lambda->column_op(k, q, ell);

  mpz_clear(c);
  mpz_clear(d);
}

void LLLoperations::SWAPI(int k,
                          int kmax,
                          MutableMatrix *A,
                          MutableMatrix *Achange,  // can be NULL
                          MutableMatrix *lambda)
{
  int i;
  mpz_t a, b, B, C1, C2, D, D1, lam;
  ring_elem rD1, rD, rlam;

  A->interchange_columns(k, k - 1);
  if (Achange) Achange->interchange_columns(k, k - 1);

  mpz_init(a);
  mpz_init(b);
  mpz_init(B);
  mpz_init(C1);
  mpz_init(C2);

  lambda->get_entry(k - 1, k - 1, rD1);
  lambda->get_entry(k, k, rD);
  mpz_init_set(D1, rD1.get_mpz());
  mpz_init_set(D, rD.get_mpz());

  if (lambda->get_entry(k - 1, k, rlam))
    mpz_init_set(lam, rlam.get_mpz());
  else
    mpz_init(lam);

  // Interchange both of these columns, except for these three terms:
  if (k >= 2)
    {
      lambda->interchange_columns(k, k - 1);
      lambda->set_entry(k - 1, k, globalZZ->from_int(lam));
      lambda->set_entry(k, k, globalZZ->from_int(D));
      lambda->set_entry(k, k - 1, globalZZ->from_long(0));
      // (k-1,k-1) is set below.
    }

  // B := (D#(k-2) * D#k + lam^2) // D#(k-1);
  if (k == 1)
    mpz_set(a, D);
  else
    {
      ring_elem rD2;
      lambda->get_entry(k - 2, k - 2, rD2);
      mpz_mul(a, rD2.get_mpz(), D);
    }
  mpz_mul(b, lam, lam);
  mpz_add(a, a, b);
  mpz_fdiv_q(B, a, D1);
  lambda->set_entry(k - 1, k - 1, globalZZ->from_int(B));

  // scan(k+1..C.kmax, i-> (
  //     t := lambda#(i,k);
  //     lambda#(i,k) = (D#k * lambda#(i,k-1) - lam * t) // D#(k-1);
  //     lambda#(i,k-1) = (B*t + lam*lambda#(i,k))//(D#k);));
  for (i = k + 1; i <= kmax; i++)
    {
      ring_elem s, t;
      bool s_notzero = lambda->get_entry(k - 1, i, s);
      bool t_notzero = lambda->get_entry(k, i, t);
      if (s_notzero)
        mpz_mul(a, D, s.get_mpz());
      else
        mpz_set_ui(a, 0);
      // lambda#(i,k) = (D#k * lambda#(i,k-1) - lam * t) // D#(k-1);
      if (t_notzero)
        mpz_mul(b, lam, t.get_mpz());
      else
        mpz_set_ui(b, 0);
      mpz_sub(a, a, b);
      mpz_fdiv_q(C1, a, D1);

      // lambda#(i,k-1) = (B*t + lam*lambda#(i,k))//(D#k);));
      mpz_mul(b, lam, C1);
      if (t_notzero)
        mpz_mul(a, B, t.get_mpz());
      else
        mpz_set_ui(a, 0);

      mpz_add(a, a, b);
      mpz_fdiv_q(C2, a, D);

      lambda->set_entry(
          k, i, globalZZ->from_int(C1));  // These two lines will remove t,s.
      lambda->set_entry(k - 1, i, globalZZ->from_int(C2));
    }
  mpz_clear(a);
  mpz_clear(b);
  mpz_clear(B);
  mpz_clear(C1);
  mpz_clear(C2);
  mpz_clear(D1);
  mpz_clear(D);
  mpz_clear(lam);
}

int LLLoperations::doLLL(MutableMatrix *A,
                         MutableMatrix *Achange,
                         MutableMatrix *LLLstate,
                         int nsteps)
{
  size_t n = A->n_cols();
  if (n == 0) return COMP_DONE;

  // Extract the state from LLLstate:
  int k, kmax;
  ring_elem a, alphaTop, alphaBottom;
  buffer o;

  if (LLLstate->get_entry(0, n, a))
    {
      std::pair<bool, long> res = globalZZ->coerceToLongInteger(a);
      assert(res.first);
      k = static_cast<int>(res.second);
    }
  else
    k = 0;

  if (LLLstate->get_entry(0, n + 1, a))
    {
      std::pair<bool, long> res = globalZZ->coerceToLongInteger(a);
      assert(res.first);
      kmax = static_cast<int>(res.second);
    }
  else
    kmax = 0;

  LLLstate->get_entry(0, n + 2, alphaTop);  // Don't free alphaTop!
  LLLstate->get_entry(0, n + 3, alphaBottom);

  while (k < n && nsteps != 0 && !system_interrupted())
    {
      if (M2_gbTrace >= 1)
        {
          o.reset();
          o << ".";
          if (M2_gbTrace >= 2) o << k;
          if (nsteps % 20 == 0) o << newline;
          emit(o.str());
        }
      nsteps--;

      if (k > kmax)
        {
          if (M2_gbTrace == 1)
            {
              o.reset();
              o << "." << k;
              if (nsteps % 20 == 0) o << newline;
              emit(o.str());
            }

          kmax = k;
          for (int j = 0; j <= k; j++)
            {
              ring_elem u;
              A->dot_product(k, j, u);
              for (int i = 0; i <= j - 1; i++)
                {
                  // u = (D#i * u - lambda#(k,i) * lambda#(j,i)) // D#(i-1)
                  ring_elem Di, mki, mji, Di1;
                  LLLstate->get_entry(i, i, Di);
                  globalZZ->mult_to(u, Di);
                  if (LLLstate->get_entry(i, k, mki) &&
                      LLLstate->get_entry(i, j, mji))
                    {
                      ring_elem t1 = globalZZ->mult(mki, mji);
                      globalZZ->subtract_to(u, t1);
                    }
                  if (i > 0)
                    {
                      LLLstate->get_entry(
                          i - 1, i - 1, Di1);  // Cannot be zero!!
                      ring_elem t1 = globalZZ->divide(u, Di1);
                      globalZZ->remove(u);
                      u = t1;
                    }
                }
              // At this point we have our element:
              LLLstate->set_entry(j, k, u);
              if (j == k && globalZZ->is_zero(u))
                {
                  ERROR("LLL vectors not independent");
                  return COMP_ERROR;
                }
            }
        }  // end of the k>kmax initialization
      REDI(k, k - 1, A, Achange, LLLstate);
      if (Lovasz(LLLstate, k, alphaTop, alphaBottom))
        {
          SWAPI(k, kmax, A, Achange, LLLstate);
          k--;
          if (k == 0) k = 1;
        }
      else
        {
          for (int ell = k - 2; ell >= 0; ell--)
            REDI(k, ell, A, Achange, LLLstate);
          k++;
        }
    }

  // Before returning, reset k,kmax:
  LLLstate->set_entry(0, n, globalZZ->from_long(k));
  LLLstate->set_entry(0, n + 1, globalZZ->from_long(kmax));

  if (k >= n) return COMP_DONE;
  if (nsteps == 0) return COMP_DONE_STEPS;
  return COMP_INTERRUPTED;
}

bool LLLoperations::LLL(MutableMatrix *A,
                        MutableMatrix *Achange,  // can be NULL
                        gmp_QQ threshold)
{
  MutableMatrix *LLLstate;
  if (!initializeLLL(A, threshold, LLLstate)) return false;
  int ret = doLLL(A, Achange, LLLstate);
  if (ret != COMP_DONE)
    {
      deleteitem(LLLstate);
      return false;
    }
  return true;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
