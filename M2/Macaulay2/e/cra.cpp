#include "cra.hpp"
#include "relem.hpp"
#include "matrix.hpp"
#include "poly.hpp"

void ChineseRemainder::CRA0(mpz_t a, mpz_t b, mpz_t um, mpz_t vn, mpz_t mn, mpz_t result)
{
  mpz_t mn_half;
  mpz_t zerompz;
  mpz_init_set_si(zerompz,0);
  mpz_init(mn_half);
  mpz_mul(result,um,b);
  mpz_addmul(result,vn,a);
  mpz_mod(result,result,mn);
  mpz_tdiv_q_2exp(mn_half,mn,1);
  // get canonical representative
  if (mpz_cmp(result,mn_half)>0)
  {
    mpz_sub(result,result,mn);
  }
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
  mpz_t mn_half;
  mpz_init(result_coeff);
  mpz_init(mn_half);
  Nterm *f = ff;
  Nterm *g = gg;
  Nterm head;
  Nterm *result = &head;

  mpz_tdiv_q_2exp(mn_half,mn,1);

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
        mpz_mul(result_coeff, f->coeff.get_mpz(), vn);
        mpz_mod(result_coeff,result_coeff,mn);
        if (mpz_cmp(result_coeff,mn_half)>0)
        {
          mpz_sub(result_coeff,result_coeff,mn);
        }
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
        mpz_mod(result_coeff,result_coeff,mn);
        if (mpz_cmp(result_coeff,mn_half)>0)
        {
          mpz_sub(result_coeff,result_coeff,mn);
        }
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
  // Error handling:
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


void showint(mpz_t a)
{
  char s[1000];
  mpz_get_str(s,10,a);
  fprintf(stderr," %s ",s);
}


bool ChineseRemainder::ratConversion(mpz_t c, mpz_t m, mpq_t result)
{
  mpz_t a1,a2,u1,u2,q,h,mhalf,u2sqr,a2sqr;
  bool retVal=true;
  mpz_init_set(a1,m);
  mpz_init_set(a2,c);
  mpz_init_set_si(u1,0);
  mpz_init_set_si(u2,1);
  mpz_init_set_si(q,0);
  mpz_init_set_si(h,0);
  mpz_init(u2sqr);
  mpz_init(a2sqr);
  mpz_init(mhalf);

  mpz_tdiv_q_2exp(mhalf,m,1);

  for (;;)
  {
    mpz_mul(u2sqr,u2,u2);

    if (mpz_cmp(u2sqr,mhalf)>=0) // u2sqr >= mhalf
    {
      retVal=false;
      mpq_set_z(result,c);
      break;
    }

    mpz_mul(a2sqr,a2,a2);

    if (mpz_cmp(a2sqr,mhalf)<0) // a2sqr < half
    {
      retVal=true;
      mpq_set_num(result,a2);
      mpq_set_den(result,u2);
      mpq_canonicalize(result);
      break;
    }

    mpz_fdiv_q(q,a1,a2);
    mpz_submul(a1,q,a2);
    mpz_submul(u1,q,u2);
    mpz_swap(a1,a2);
    mpz_swap(u1,u2);

  }
  // clean up
  // mpz_clears(a1,a2,u1,u2,q,h,mhalf,u2sqr,a2sqr,(void *)0);
  mpz_clear(a1);
  mpz_clear(a2);
  mpz_clear(u1);
  mpz_clear(u2);
  mpz_clear(q);
  mpz_clear(h);
  mpz_clear(mhalf);
  mpz_clear(u2sqr);
  mpz_clear(a2sqr);


  return retVal;
}


ring_elem ChineseRemainder::ratConversion(const ring_elem ff,
                        mpz_t m,
                        const PolyRing *RQ)
{
  mpq_t result_coeff;
  mpq_init(result_coeff);
  Nterm *f = ff;
  Nterm head;
  Nterm *result = &head;

  const Monoid *M = RQ->getMonoid();
  const Ring *K = RQ->getCoefficientRing();

  for (;f!=NULL;f=f->next)
  {
    result->next=RQ->new_term();
    result=result->next;
    result->next=0;
    M->copy(f->monom,result->monom);
    ratConversion(f->coeff.get_mpz(),m,result_coeff);
    result->coeff=K->from_rational(result_coeff);
  }

  mpq_clear(result_coeff);
  result->next = 0;
  return head.next;
}

vec ChineseRemainder::ratConversion(vec f,
                  mpz_t m,
                  const PolyRing *RQ)
{
  vecterm head;
  vec result = &head;
  for (;f!=NULL;f=f->next)
  {
    result->next = RQ->new_vec();
    result = result->next;
    result->next = 0;
    result->comp=f->comp;
    result->coeff=ratConversion(f->coeff,m,RQ);
  }

  result->next = 0;
  return head.next;
}


const RingElement * rawRingElementRatConversion(const RingElement *f,
                                                mpz_t m,
                                                const Ring *RQ)
{
  const Ring *Rf = f->get_ring();
  const PolyRing *P = Rf->cast_to_PolyRing();
  const PolyRing *PQ = RQ->cast_to_PolyRing();

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
      ring_elem result = ChineseRemainder::ratConversion(rf,m,PQ);
      return RingElement::make_raw(PQ,result); // debug this line!
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

// f should be an element in the polynomial ring R (over ZZ).
// RQ should be the same ring as R, but with rational coefficients

const Matrix * rawMatrixRatConversion(const Matrix *f,
                                      mpz_t m,
                                      const Ring *RQ)
{

  const PolyRing *R = f->get_ring()->cast_to_PolyRing();
  const PolyRing *PQ = RQ->cast_to_PolyRing();

  if (R == 0)
  {
    ERROR("expected polynomial ring over ZZ");
    return 0;
  }

  const FreeModule *F = f->rows();
  const FreeModule *G = f->cols();
  const FreeModule *FQ = PQ->make_FreeModule(F->rank());
  const FreeModule *GQ = PQ->make_FreeModule(G->rank());

  const int *deg;

  deg = f->degree_monoid()->make_one();

  MatrixConstructor mat(FQ,GQ,deg);
  for (int i=0; i<f->n_cols(); i++)
  {
    vec u = ChineseRemainder::ratConversion(f->elem(i),m,PQ);
    mat.set_column(i,u);
  }
  return mat.to_matrix();
}
