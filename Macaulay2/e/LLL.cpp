// Copyright 1998  Michael E. Stillman

#include "LLL.hpp"
#include "relem.hpp"
#include "matrix.hpp"
#include "frac.hpp"
#include "text_io.hpp"

bool LLLoperations::checkThreshold(ring_elem num, ring_elem den)
{
  // Makes sure that 1/4 < num/den <= 1
  // Assumed: num, den are elements of ZZ.
  mpz_ptr a = MPZ_VAL(num);
  mpz_ptr b = MPZ_VAL(den);
  if (mpz_sgn(a) < 0) return false;
  if (mpz_sgn(b) < 0) return false;
  if (mpz_cmp(a,b) > 0) return false;  // return false if a>b.
  mpz_t c;
  mpz_init(c);
  mpz_mul_2exp(c,a,2);  // c = 4*a
  int cmp = mpz_cmp(b,c);
  mpz_clear(c);
  if (cmp >= 0) return false;
  return true;
}

bool LLLoperations::setThreshold(const RingElement *threshold, ring_elem& num, ring_elem &den)
{
  const Ring *R = threshold->get_ring();
  if (R == globalZZ)
    {
      num = globalZZ->copy(threshold->get_value());
      den = globalZZ->from_int(1);
      return true;
    }
  const FractionField *KK = R->cast_to_FractionField();
  if (KK != 0)
    {
      const Ring *K = KK->get_ring();
      if (K == globalZZ)
	{
	  num = globalZZ->copy(KK->numerator(threshold->get_value()));
	  den = globalZZ->copy(KK->denominator(threshold->get_value()));
	  return true;
	}
    }
  return false;
}

bool LLLoperations::initializeLLL(const SparseMutableMatrix *A,
			    const RingElement *threshold,
			    SparseMutableMatrix *& LLLstate)
{
  // First check m: should be a matrix over globalZZ.
  if (A == 0 || A->getRing() != globalZZ)
    {
      ERROR("LLL only defined for matrices over ZZ");
      return false;
    }

  // Check that 'threshold' is in range, and set the numerator, denom
  ring_elem num;
  ring_elem den;
  if (!setThreshold(threshold, num, den))
    return false;
  if (!checkThreshold(num,den))
    {
      globalZZ->remove(num);
      globalZZ->remove(den);
      return false;
    }

  // LLLstate has n+4 columns, and n rows.
  // First n columns: LLLstate(i,i) = D#i
  //                  LLLstate(i,j) = lambda#(j,i) for 
  // Last four columns just have entries in row 0:
  // The entries are: k, kmax, alphaTop, alphaBottom: all are ZZ values.

  int n = A->n_cols();
  LLLstate = SparseMutableMatrix::make(globalZZ,n,n+4);
  if (n > 0)
    {
      LLLstate->setEntry(0,n,globalZZ->from_int(1));  // k := 2
      //LLLstate->setEntry(0,n+1,globalZZ->from_int(0)); // kmax := 1
      LLLstate->setEntry(0,n+2,num); // Set threshold numerator, denominator
      LLLstate->setEntry(0,n+3,den);
      LLLstate->setEntry(0,0,A->dotProduct(0,0));  // D#1 := dot(A,0,0)
    }
  return true;
}

bool LLLoperations::initializeLLL(const Matrix *m,
				  const RingElement *threshold,
				  bool useChangeOfBasisMatrix,
				  SparseMutableMatrix *& A,
				  SparseMutableMatrix *& LLLstate)
{
  // First check m: should be a matrix over globalZZ.
  if (m->get_ring() != globalZZ)
    {
      ERROR("LLL only defined for matrices over ZZ");
      return false;
    }

  // Set A and A's change of basis, if useChangeOfBasisMatrix is set.
  A = SparseMutableMatrix::make(m);
  int n = m->n_cols();
  if (useChangeOfBasisMatrix)
    {
      SparseMutableMatrix *B = SparseMutableMatrix::identity(globalZZ,n);
      A->setColumnChangeMatrix(B);
    }

  bool ret = initializeLLL(A,threshold,LLLstate);
  if (!ret)
    {
      delete A->getColumnChangeMatrix();
      delete A;
    }
  return ret;
}

bool LLLoperations::Lovasz(SparseMutableMatrix *lambda,
			   int k,
			   ring_elem alphaTop,
			   ring_elem alphaBottom)
{
  // Test:alphaBottom * (D#(k-2) * D#k + lambda#(k,k-1)^2) < 
  //	          alphaTop * D#(k-1)^2
  ring_elem D2,D1,D,L;
  mpz_t a,b;
  lambda->getEntry(k-1,k-1,D1);
  lambda->getEntry(k,k,D);
  bool Lnotzero = lambda->getEntry(k-1,k,L);
  if (k == 1)
    mpz_init_set(a,MPZ_VAL(D));
  else
    {
      mpz_init(a);
      lambda->getEntry(k-2,k-2,D2);
      mpz_mul(a,MPZ_VAL(D2),MPZ_VAL(D));
    }
  mpz_init(b);
  if (Lnotzero)
    {
      mpz_mul(b,MPZ_VAL(L),MPZ_VAL(L));
      mpz_add(a,a,b);
    }
  mpz_mul(a,a,MPZ_VAL(alphaBottom));  // This is the LHS.
  
  mpz_mul(b,MPZ_VAL(D1),MPZ_VAL(D1));
  mpz_mul(b,MPZ_VAL(alphaTop),b); // RHS
  int cmp = mpz_cmp(a,b);
  mpz_clear(a);
  mpz_clear(b);
  return (cmp < 0);
}

void LLLoperations::REDI(int k, int ell,
			 SparseMutableMatrix *A,
			 SparseMutableMatrix *lambda)
{
  // set q = ...
  // negate q.
  ring_elem Dl, mkl, q;
  if (!lambda->getEntry(ell,k,mkl)) return;
  lambda->getEntry(ell,ell,Dl);
  mpz_ptr a = MPZ_VAL(mkl);
  mpz_ptr b = MPZ_VAL(Dl);  // b = D#ell
  mpz_t c, d;
  mpz_init(c);
  mpz_init(d);
  mpz_mul_2exp(c,a,1); // c = 2*lambda#(k,ell)
  mpz_abs(d,c);        // d = abs(2*lambda#(k,ell)
  mpz_add(c,c,b);      // c = 2*lambda#(k,ell) + D#ell
  mpz_mul_2exp(d,b,1); // d = 2*D#ell
  mpz_fdiv_q(c,c,d);   // c = (almost) final q
  mpz_neg(c,c);
  q = MPZ_RINGELEM(c);

  A->addColumnMultiple(ell,q,k);
  lambda->addColumnMultiple(ell,q,k);

  mpz_clear(c);
  mpz_clear(d);
}

void LLLoperations::SWAPI(int k, int kmax,
			  SparseMutableMatrix *A,
			  SparseMutableMatrix *lambda)
{
  int i;
  mpz_t a,b,B,C1,C2,D,D1,lam;
  ring_elem rD1,rD,rlam;

  A->interchangeColumns(k,k-1);

  mpz_init(a);
  mpz_init(b);
  mpz_init(B);
  mpz_init(C1);
  mpz_init(C2);

  lambda->getEntry(k-1,k-1,rD1);
  lambda->getEntry(k,k,rD);
  mpz_init_set(D1,MPZ_VAL(rD1));
  mpz_init_set(D,MPZ_VAL(rD));

  if (lambda->getEntry(k-1,k,rlam))
    mpz_init_set(lam,MPZ_VAL(rlam));
  else
    mpz_init(lam);

  // Interchange both of these columns, except for these three terms:
  if (k >= 2)
    {
      lambda->interchangeColumns(k,k-1);
      lambda->setEntry(k-1,k,globalZZ->from_int(lam));
      lambda->setEntry(k,k,globalZZ->from_int(D));
      lambda->setEntry(k,k-1,globalZZ->from_int(0));
      // (k-1,k-1) is set below.
    }
  
  // B := (D#(k-2) * D#k + lam^2) // D#(k-1);
  if (k == 1)
    mpz_set(a,D);
  else
    {
      ring_elem rD2;
      lambda->getEntry(k-2,k-2,rD2);
      mpz_mul(a,MPZ_VAL(rD2),D);
    }
  mpz_mul(b,lam,lam);
  mpz_add(a,a,b);
  mpz_fdiv_q(B,a,D1);
  lambda->setEntry(k-1,k-1,globalZZ->from_int(B));
  
  // scan(k+1..C.kmax, i-> (
  //	 t := lambda#(i,k);
  //	 lambda#(i,k) = (D#k * lambda#(i,k-1) - lam * t) // D#(k-1);
  //	 lambda#(i,k-1) = (B*t + lam*lambda#(i,k))//(D#k);));
  for (i=k+1; i<=kmax; i++)
    {
      ring_elem s,t;
      bool s_notzero = lambda->getEntry(k-1,i,s);
      bool t_notzero = lambda->getEntry(k,i,t);
      if (s_notzero)
	mpz_mul(a,D, MPZ_VAL(s));
      else
	mpz_set_ui(a,0);
      // lambda#(i,k) = (D#k * lambda#(i,k-1) - lam * t) // D#(k-1);
      if (t_notzero)
	mpz_mul(b,lam,MPZ_VAL(t));
      else
	mpz_set_ui(b,0);
      mpz_sub(a,a,b);
      mpz_fdiv_q(C1,a,D1);

      // lambda#(i,k-1) = (B*t + lam*lambda#(i,k))//(D#k);));
      mpz_mul(b,lam,C1);
      if (t_notzero)
	mpz_mul(a,B,MPZ_VAL(t));
      else
	mpz_set_ui(a,0);

      mpz_add(a,a,b);
      mpz_fdiv_q(C2,a,D);

      lambda->setEntry(k,i,globalZZ->from_int(C1));  // These two lines will remove t,s.
      lambda->setEntry(k-1,i,globalZZ->from_int(C2));
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

int LLLoperations::doLLL(SparseMutableMatrix *A,
			 SparseMutableMatrix *LLLstate,
			 int nsteps)
{
  int n = A->n_cols();
  if (n == 0) return COMP_DONE;

  // Extract the state from LLLstate:
  int k, kmax;
  ring_elem a, alphaTop, alphaBottom;
  buffer o;

  if (LLLstate->getEntry(0,n,a))
    k = globalZZ->coerce_to_int(a);
  else
    k = 0;

  if (LLLstate->getEntry(0,n+1,a))
    kmax = globalZZ->coerce_to_int(a);
  else
    kmax = 0;

  LLLstate->getEntry(0,n+2,alphaTop);  // Don't free alphaTop!
  LLLstate->getEntry(0,n+3,alphaBottom);

  while (k < n && nsteps != 0 && !system_interrupted)
    {
      if (gbTrace >= 1)
	{
	  o.reset();
	  o << ".";
	  if (gbTrace >= 2)
	    o << k;
	  if (nsteps % 20 == 0)
	    o << newline;
	  emit(o.str());
	}
      nsteps--;

      if (k > kmax)
	{
	  if (gbTrace == 1)
	    {
	      o.reset();
	      o << "." << k;
	      if (nsteps % 20 == 0)
		o << newline;
	      emit(o.str());
	    }

	  kmax = k;
	  for (int j=0; j<=k; j++)
	    {
	      ring_elem u = A->dotProduct(k,j);
	      for (int i=0; i<=j-1; i++)
		{
		  // u = (D#i * u - lambda#(k,i) * lambda#(j,i)) // D#(i-1)
		  ring_elem Di, mki, mji, Di1;
		  LLLstate->getEntry(i,i,Di);
		  globalZZ->mult_to(u, Di);
		  if (LLLstate->getEntry(i,k,mki) &&  LLLstate->getEntry(i,j,mji))
		    {
		      ring_elem t1 = globalZZ->mult(mki,mji);
		      globalZZ->subtract_to(u,t1);
		    }
		  if (i > 0)
		    {
		      LLLstate->getEntry(i-1,i-1,Di1);  // Cannot be zero!!
		      ring_elem t1 = globalZZ->divide(u,Di1);
		      globalZZ->remove(u);
		      u = t1;
		    }
		}
	      // At this point we have our element:
	      LLLstate->setEntry(j,k,u);
	      if (j == k && globalZZ->is_zero(u))
		{
		  ERROR("LLL vectors not independent");
		  return COMP_ERROR;
		}
	    }
	} // end of the k>kmax initialization
      REDI(k,k-1,A,LLLstate);
      if (Lovasz(LLLstate,k,alphaTop,alphaBottom))
	{
	  SWAPI(k,kmax,A,LLLstate);
	  k--;
	  if (k == 0) k = 1;
	}
      else
	{
	  for (int ell=k-2; ell>=0; ell--)
	    REDI(k,ell,A,LLLstate);
	  k++;
	}
    }

  // Before returning, reset k,kmax:
  LLLstate->setEntry(0,n,globalZZ->from_int(k));
  LLLstate->setEntry(0,n+1,globalZZ->from_int(kmax));

  if (k > n) return COMP_DONE;
  if (nsteps == 0) return COMP_DONE_STEPS;
  return COMP_INTERRUPTED;
}

bool LLLoperations::LLL(const Matrix *m, const RingElement *threshold, Matrix *&LLLbasis)
{
  SparseMutableMatrix *A;
  SparseMutableMatrix *LLLstate;
  if (!initializeLLL(m,threshold,false,A,LLLstate))
    return false;
  int ret = doLLL(A,LLLstate);
  if (ret != COMP_DONE)
    {
      delete A;
      delete LLLstate;
      return false;
    }
  LLLbasis = A->toMatrix();
  return true;
}

bool LLLoperations::LLL(const Matrix *m, const RingElement *threshold, Matrix *&LLLbasis, Matrix *&ChangeOfBasis)
{
  SparseMutableMatrix *A;
  SparseMutableMatrix *LLLstate;
  if (!initializeLLL(m,threshold,true,A,LLLstate))
    return false;
  int ret = doLLL(A,LLLstate);
  if (ret != COMP_DONE)
    {
      delete A;
      delete LLLstate;
      return false;
    }
  LLLbasis = A->toMatrix();
  ChangeOfBasis = A->getColumnChangeMatrix()->toMatrix();
  return true;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
