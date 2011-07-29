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

ring_elem ChineseRemainder::CRA(const PolyRing *R, ring_elem ff, ring_elem gg, mpz_t um, mpz_t vn, mpz_t mn)
{
  mpz_t result_coeff;
  mpz_init(result_coeff);
  
  Nterm *f = ff;
  Nterm *g = gg;
  Nterm head;
  Nterm *result = &head;
  
  const Monoid *M = R->getMonoid();
  const Ring *K = R->getCoefficientRing();
  
  while (1)
  {
    if (g == NULL) 
    {
      // mult each term of f by n:
      for ( ;f!=0; f=f->next)
      {
        result->next = R->new_term();
        result = result->next;
        result->next = 0;
        M->copy(f->monom, result->monom);
        mpz_mul(result_coeff, f->coeff.get_mpz(), um);
        result->coeff = K->from_int(result_coeff);
      }
      break;
    }
    if (f == NULL) 
    {
      // mult each term of g by n:
      for ( ;g!=0; g=g->next)
      {
        result->next = R->new_term();
        result = result->next;
        result->next = 0;
        M->copy(g->monom, result->monom);
        mpz_mul(result_coeff, g->coeff.get_mpz(), um);
        result->coeff = K->from_int(result_coeff);
      }
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

  mpz_clear(result_coeff);
  result->next = 0;
  return head.next;
}

ring_elem ChineseRemainder::CRA(const PolyRing *R, ring_elem ff, ring_elem gg, mpz_t m, mpz_t n)
{
  // compute the multipliers
  mpz_t um, vn, mn;
  mpz_init(um);
  mpz_init(vn);
  mpz_init(mn);
  computeMultipliers(m, n, um, vn, mn);  
  // compute the chinese remainder with precomputed multipliers
  ring_elem result = CRA(R, ff, gg, um, vn, mn);
  mpz_clear(um);
  mpz_clear(vn);
  mpz_clear(mn);
  return result;
}

vec ChineseRemainder::CRA(const PolyRing *R, vec f, vec g, mpz_t um, mpz_t vn, mpz_t mn)
{  
  vecterm head;
  vec result = &head;
  
  while (1)
  {
    if (g == NULL) 
    {
      // mult each term of f by n:
      for ( ;f!=0; f=f->next)
      {
        result->next = R->new_vec();
        result = result->next;
        result->next = 0;
        result->coeff=CRA(R,f->coeff,0,um,vn,mn);
        result->comp=f->comp;
      }
      break;
    }
    if (f == NULL) 
    {
      // mult each term of g by n:
      for ( ;g!=0; g=g->next)
      {
        result->next = R->new_vec();
        result = result->next;
        result->next = 0;
        result->coeff=CRA(R,0,g->coeff,um,vn,mn);
        result->comp=g->comp;      
      }
      break;
    }
    if (f->comp < g->comp)
    {
	result->next = R->new_vec();
	result = result->next;
	result->next = 0;
        result->coeff=CRA(R,0,g->coeff,um,vn,mn);
        result->comp=g->comp; 	
        g = g->next;
    } else if (f->comp > g->comp)
    {
      result->next = R->new_vec();
      result = result->next;
      result->next = 0;
      result->coeff=CRA(R,f->coeff,0,um,vn,mn);
      result->comp=f->comp; 	
      f = f->next;
    } else 
    {
      result->next = R->new_vec();
      result = result->next;
      result->next = 0;
      result->coeff=CRA(R,f->coeff,g->coeff,um,vn,mn);
      result->comp=f->comp; 	
      f = f->next;
      g = g->next;
    }
  }
  result->next = 0;
  return head.next;
}



Matrix * ChineseRemainder::CRA(const Matrix *f, const Matrix *g, mpz_t um, mpz_t vn, mpz_t mn)
{
  if (f->get_ring() != g->get_ring())
  {
    ERROR("matrices have different base rings");
    return 0;
  }
  if (f->rows()->rank() != g->rows()->rank()
      || f->cols()->rank() != g->cols()->rank())
  {
    ERROR("matrices have different shapes");
    return 0;
  }
  
  const PolyRing *R = f->get_ring()->cast_to_PolyRing();
  if (R == 0)
  {
    ERROR("expected polynomial ring over ZZ");
    return 0;
  }
  
  const FreeModule *F = f->rows();
  const FreeModule *G = f->cols();
  const int *deg;
  
  if (!f->rows()->is_equal(g->rows()))
    F = R->make_FreeModule(f->n_rows());
  
  if (!f->cols()->is_equal(g->cols()))
    G = R->make_FreeModule(f->n_cols());
  
  if (EQ == f->degree_monoid()->compare(f->degree_shift(), g->degree_shift()))
    deg = f->degree_shift();
  else
    deg = f->degree_monoid()->make_one();
  
  MatrixConstructor mat(F,G,deg);
  for (int i=0; i<f->n_cols(); i++)
  {
    vec u = CRA(R,f->elem(i),g->elem(i),um,vn,mn);
    mat.set_column(i,u);
  }
  return mat.to_matrix();
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
	  ring_elem result = ChineseRemainder::CRA(P,rf,rg,m,n);
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
  
  const Ring *R = f->get_ring();

  // Error handling:
  if (f->get_ring() != g->get_ring())
  {
    ERROR("matrtices have different base rings");
    return 0;
  }
  if (f->rows()->rank() != g->rows()->rank() 
      || f->cols()->rank() != g->cols()->rank())
  {
    ERROR("matrixces have different shapes");
    return 0;
  }
  
  // Assumption: f and g are either matrices over ZZ, or over a polynomial ring whose coeff
  // ring is ZZ.  The output is a matrix in the same ring.
  
  mpz_t um, vn, mn;
  mpz_init(um);
  mpz_init(vn);
  mpz_init(mn);
  ChineseRemainder::computeMultipliers(m, n, um, vn, mn);
  mpz_t result_coeff;
  mpz_init(result_coeff);
  Matrix * result=ChineseRemainder::CRA(f,g,um,vn,mn);
  mpz_clear(um);
  mpz_clear(vn);
  mpz_clear(mn);
  return result;
  
}




#if 0
Matrix *Matrix::operator+(const Matrix &m) const
{
  if (get_ring() != m.get_ring())
  {
    ERROR("matrices have different base rings");
    return 0;
  }
  if (rows()->rank() != m.rows()->rank()
      || cols()->rank() != m.cols()->rank())
  {
    ERROR("matrices have different shapes");
    return 0;
  }
  
  const Ring *R = get_ring();
  const FreeModule *F = rows();
  const FreeModule *G = cols();
  const int *deg;
  
  if (!rows()->is_equal(m.rows()))
    F = R->make_FreeModule(n_rows());
  
  if (!cols()->is_equal(m.cols()))
    G = R->make_FreeModule(n_cols());
  
  if (EQ == degree_monoid()->compare(degree_shift(), m.degree_shift()))
    deg = degree_shift();
  else
    deg = degree_monoid()->make_one();
  
  MatrixConstructor mat(F,G,deg);
  for (int i=0; i<n_cols(); i++)
  {
    vec v = R->copy_vec(elem(i));
    vec w = R->copy_vec(m[i]);
    R->add_vec_to(v,w);
    mat.set_column(i, v);
  }
  return mat.to_matrix();
}
#endif


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

