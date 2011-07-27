#include "cra.hpp"
#include "relem.hpp"
#include "matrix.hpp"
#include "poly.hpp"

void ChineseRemainder::CRA0(mpz_t a, mpz_t b, mpz_t um, mpz_t vn, mpz_t mn, mpz_t result)
{
  mpz_mul(result,um,b);
  mpz_addmul(result,vn,a);
  mpz_mod(result,result,mn);
}

bool ChineseRemainder::computeMultipliers(mpz_t m, mpz_t n, mpz_t result_um, mpz_t result_vn, mpz_t result_mn)
{
  mpz_t g;
  mpz_init(g);
  mpz_gcdext(g, result_um, result_vn, m, n);
  if (0 != mpz_cmp_si(g,1)) return false;
  mpz_mul(result_mn, m,n);
  mpz_mul(result_um,result_um,m);
  mpz_mul(result_vn,result_vn,n);
  mpz_clear(g);
  return true;
}

void ChineseRemainder::CRA(mpz_t a, mpz_t b, mpz_t m, mpz_t n, mpz_t result)
{
  mpz_t um, vn, mn;
  mpz_init(um);
  mpz_init(vn);
  mpz_init(mn);
  computeMultipliers(m, n, um, vn, mn);
  CRA0(a,b,um,vn,mn,result);
  mpz_clear(um);
  mpz_clear(vn);
  mpz_clear(mn);
}


ring_elem ChineseRemainder::combine(const PolyRing *R, ring_elem ff, ring_elem gg, mpz_t m, mpz_t n)
{
  // Assumption: f and g are in a poly ring whose coeff ring is ZZ

  mpz_t um, vn, mn;
  mpz_init(um);
  mpz_init(vn);
  mpz_init(mn);
  computeMultipliers(m, n, um, vn, mn);

  mpz_t result_coeff;
  mpz_init(result_coeff);

  const Monoid *M = R->getMonoid();
  const Ring *K = R->getCoefficientRing();

  Nterm *f = ff;
  Nterm *g = gg;
  Nterm head;
  Nterm *result = &head;

  while (1)
    {
      if (g == NULL) 
	{
	  // mult each term of g by n.
	  break;
	}
      if (f == NULL) 
	{
	  // return 0
	  break;
	}
      switch (M->compare(f->monom, g->monom)) {
      case -1:
	result->next = R->new_term();
	result = result->next;
	result->next = 0;
	M->copy(g->monom, result->monom);
	mpz_mul(result_coeff, g->coeff.get_mpz(), um);
	result->coeff = K->from_int(result_coeff);
	g = g->next;
	break;
      case 1:
	result->next = R->new_term();
	result = result->next;
	result->next = 0;
	M->copy(f->monom, result->monom);
	mpz_mul(result_coeff, f->coeff.get_mpz(), um);
	result->coeff = K->from_int(result_coeff);
	f = f->next;
	break;
      case 0:
	Nterm *tmf = f;
	Nterm *tmg = g;
	f = f->next;
	g = g->next;
	CRA0(tmf->coeff.get_mpz(), tmg->coeff.get_mpz(),
	     um, vn, mn, result_coeff);
	Nterm *t = R->new_term();
	M->copy(tmf->monom, t->monom);
	t->coeff = K->from_int(result_coeff);
	t->next = 0;
	result->next = t;
	result = t;
	break;
      }
    }
  
  mpz_clear(um);
  mpz_clear(vn);
  mpz_clear(mn);
  mpz_clear(result_coeff);
  return head.next;
}

const RingElement * rawRingElementCRA(const RingElement *f, 
				      const RingElement *g,
				      mpz_t m,
				      mpz_t n)
{
  // Assumption: f and g are either in ZZ, or in a polynomial ring whose coeff
  // ring is ZZ.  The output is a ring element in the same ring.

  const Ring *Rf = f->get_ring();
  const Ring *Rg = g->get_ring();
  if (Rf != Rg)
    {
      ERROR("expected same ring");
      return 0;
    }
  const PolyRing *P = Rf->cast_to_PolyRing();
  if (P == 0)
    {
      // check whether Rf is ZZ.  If not, error.
      if (!Rf->is_ZZ())
	{
	  ERROR("expected ZZ, or polynomial ring over ZZ");
	  return 0;
	}
      ERROR("not implemented yet");
      return 0;
    }
  else 
    {
      const Ring *K = P->getCoefficientRing();
      if (K->is_ZZ())
	{
	  ring_elem rf = f->get_value();
	  ring_elem rg = g->get_value();
	  ring_elem result = ChineseRemainder::combine(P,rf,rg,m,n);
	  return RingElement::make_raw(Rf, result);
	}
      else
	{
	  ERROR("expected coefficient ring to be ZZ");
	  return 0;
	}
    }
  ERROR("not written yet");
  return 0;
}

const Matrix * rawMatrixCRA(const Matrix *f, 
			    const Matrix *g,
			    mpz_t m,
			    mpz_t n)
{
  // Assumption: f and g are either matrices over ZZ, or over a polynomial ring whose coeff
  // ring is ZZ.  The output is a matrix in the same ring.
  ERROR("not written yet, d'oh");
  return 0;
}



ring_elem ChineseRemainder::reconstruct(const PolynomialRing *RQ, const Ring *R, ring_elem f, mpz_t m)
{
  // f should be an element in the polynomial ring R (over ZZ).
  // RQ should be the same ring as R, but with rational coefficients
  return 0;
}

RingElement * ChineseRemainder::reconstruct(const Ring *RQ, const RingElement *f, mpz_t m)
{
  // f should be an element in the polynomial ring R (over ZZ).
  // RQ should be the same ring as R, but with rational coefficients
  return 0;
}

Matrix * ChineseRemainder::reconstruct(const Ring *RQ, const Matrix *f, mpz_t m)
{
  // f should be an element in the polynomial ring R (over ZZ).
  // RQ should be the same ring as R, but with rational coefficients
  return 0;
}

