// Copyright 2005 Michael E. Stillman

#include "polyQQ.hpp"
#include "relem.hpp"
#include "QQ.hpp"

#define FRAC_VAL(f) (reinterpret_cast<PolyQQ::elem *>((f).poly_val))
#define FRAC_RINGELEM(a) (ring_elem(reinterpret_cast<Nterm *>(a)))

PolyQQ::~PolyQQ()
{
}
PolyQQ *PolyQQ::create(const PolyRing *P)
{
  PolyQQ *R = new PolyQQ;

  R->initialize_ring(P->charac(),
		     P->get_degree_ring());

  R->initialize_PolynomialRing(P->getCoefficients(),
			       P->getMonoid(),
			       P,
			       R,
			       globalZZ);

  R->poly_size_ = sizeof(elem);

  R->zeroV = R->from_int(0);
  R->oneV = R->from_int(1);
  R->minus_oneV = R->from_int(-1);

  R->gb_ring_ = P->get_gb_ring();

  return R;
}

#if 0
PolyQQ *PolyQQ::create_quotient(const PolyQQ *P, const Matrix *I)
  // I should be be a one row matrix, and a GB in P.
  // These facts are NOT checked.
  // Any quotient elements of P are ignored?
{
}

PolyQQ *PolyQQ::create_quotient(const PolyQQ *P, const PolyQQ *B)
  // B should be a logical coeff ring of P.
  // All quotient elements of B are extended up to P.
  // and a new ring is made.
{
}
#endif

void PolyQQ::text_out(buffer &o) const
{
  o << "polyring(QQ,";
  getMonoid()->text_out(o);
  o << ")\n";
#ifdef DEVELOPMENT
#warning "display quotient polynomials"
#endif
}

PolyQQ::elem *PolyQQ::new_elem() const
{
  elem *result = GETMEM(elem *, poly_size_);
  result->numer = 0;
  mpz_init(result->denom);
  return result;
}

PolyQQ::elem *PolyQQ::make_fraction(Nterm *top, mpz_ptr bottom) const
{
  elem *result = GETMEM(elem *, poly_size_);
  result->numer = top;
  mpz_init_set(result->denom,bottom);
  return result;
}

PolyQQ::elem *PolyQQ::make_fraction(Nterm *top) const
{
  elem *result = GETMEM(elem *, poly_size_);
  result->numer = top;
  mpz_init_set_si(result->denom,1);
  return result;
}

Nterm * PolyQQ::numerator(ring_elem f) const 
{
  return FRAC_VAL(f)->numer;
}

mpz_ptr PolyQQ::denom(ring_elem f) const
{
  return FRAC_VAL(f)->denom;
}

void PolyQQ::simplify(elem *f) const
{
  // Find and remove the content between denominator, numerator.
#ifdef DEVELOPMENT
#warning "write this!"
#endif
}

Nterm *PolyQQ::mult_by_coeff(mpz_ptr c, Nterm *f) const
  // Copies f first!
{
  Nterm *g = getNumeratorRing()->copy(f);
  ring_elem cr = MPZ_RINGELEM(c);
  for (Nterm *t = g; t != 0; t = t->next)
    {
      // Multiply by c
      t->coeff = globalZZ->RingZZ::mult(cr,t->coeff);
    }
  return g;
}

ring_elem PolyQQ::from_double(double n) const 
{
  elem *result = make_fraction(numerR_->from_double(n));
  return FRAC_RINGELEM(result);
}

ring_elem PolyQQ::from_int(int n) const 
{
  elem *result = make_fraction(numerR_->from_int(n));
  return FRAC_RINGELEM(result);
}

ring_elem PolyQQ::from_int(mpz_ptr n) const 
{
  elem *result = make_fraction(numerR_->from_int(n));
  return FRAC_RINGELEM(result);  
}

ring_elem PolyQQ::from_rational(mpq_ptr n) const
{
  elem *result = make_fraction(numerR_->from_int(mpq_numref(n)),
			       mpq_denref(n));
  return FRAC_RINGELEM(result);
}

ring_elem PolyQQ::var(int v) const 
{
  elem *result = make_fraction(numerR_->var(v));
  return FRAC_RINGELEM(result);
}

bool PolyQQ::promote(const Ring *R, const ring_elem f, ring_elem &result) const 
{
  int *exp = newarray(int,n_vars()); // Actually, possibly smaller number is needed.
  result = make_logical_term(R,f,exp);
  return true;
}

bool PolyQQ::lift(const Ring *R, const ring_elem f, ring_elem &result) const 
{
  // R may be QQ, or a ring of the form QQ[M'],for some possibly smaller monoid
  // R cannot be ZZ?
#ifdef DEVELOPMENT
#warning "write lift"
#endif
  return false;
}

ring_elem PolyQQ::preferred_associate(ring_elem f) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return ZERO_RINGELEM;
}

bool PolyQQ::is_unit(const ring_elem f) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return false;
}

bool PolyQQ::is_zero(const ring_elem f) const 
{
  return numerR_->is_zero(numerator(f));
}

bool PolyQQ::is_equal(const ring_elem f, const ring_elem g) const 
{
  bool bot = (mpz_cmp(denom(f),denom(g)) == 0);
  if (!bot) return false;
  return numerR_->is_equal(numerator(f), numerator(g));
}

int PolyQQ::compare_elems(const ring_elem f, const ring_elem g) const
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return 0;
}

ring_elem PolyQQ::copy(const ring_elem f) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return ZERO_RINGELEM;
}

void PolyQQ::remove(ring_elem &f) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
}

ring_elem PolyQQ::negate(const ring_elem f) const 
{
  elem *result = make_fraction(numerR_->negate(numerator(f)), denom(f));
  return FRAC_RINGELEM(result);
}

ring_elem PolyQQ::add(const ring_elem f, const ring_elem g) const 
{
  Nterm *f1 = numerator(f);
  Nterm *g1 = numerator(g);
  Nterm *c;
  elem *result;
  if (mpz_cmp(denom(f),denom(g)) == 0)
    {
      c = numerR_->add(f1,g1);
      result = make_fraction(c,denom(f));
    }
  else
    {
      Nterm *a = mult_by_coeff(denom(g),f1);
      Nterm *b = mult_by_coeff(denom(f),g1);
      c = numerR_->add(a,b);
      result = make_fraction(c);
      mpz_mul(result->denom,denom(f),denom(g));
    }
  simplify(result);
  return FRAC_RINGELEM(result);
}

ring_elem PolyQQ::subtract(const ring_elem f, const ring_elem g) const 
{
  Nterm *f1 = numerator(f);
  Nterm *g1 = numerator(g);
  Nterm *c;
  elem *result;
  if (mpz_cmp(denom(f),denom(g)) == 0)
    {
      c = numerR_->subtract(f1,g1);
      result = make_fraction(c,denom(f));
    }
  else
    {
      Nterm *a = mult_by_coeff(denom(g),f1);
      Nterm *b = mult_by_coeff(denom(f),g1);
      c = numerR_->subtract(a,b);
      result = make_fraction(c);
      mpz_mul(result->denom,denom(f),denom(g));
    }
  simplify(result);
  return FRAC_RINGELEM(result);
}

ring_elem PolyQQ::mult(const ring_elem f, const ring_elem g) const 
{
  Nterm *h = numerR_->mult(numerator(f), numerator(g));
  elem *result = make_fraction(h);
  mpz_mul(result->denom, denom(f), denom(g));
  simplify(result);
  return FRAC_RINGELEM(result);
}

ring_elem PolyQQ::power(const ring_elem f, mpz_t n) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return ZERO_RINGELEM;
}

ring_elem PolyQQ::power(const ring_elem f, int n) const 
{
  elem *result = make_fraction(numerR_->power(numerator(f),n));
  mpz_pow_ui(result->denom, denom(f), n);
  return FRAC_RINGELEM(result);
}

ring_elem PolyQQ::invert(const ring_elem f) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return ZERO_RINGELEM;
}

ring_elem PolyQQ::divide(const ring_elem f, const ring_elem g) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return ZERO_RINGELEM;
}

//ring_elem PolyQQ::gcd(const ring_elem f, const ring_elem g) const 
//{
//  const RingElement *f1 = RingElement::make_raw(this,f);
//  const RingElement *g1 = RingElement::make_raw(this,g);
//  const RingElement *h = rawGCDRingElement(f1,g1);
//  return h->get_value();
//}
//
//ring_elem PolyQQ::gcd_extended(const ring_elem f, const ring_elem g, 
//			       ring_elem &u, ring_elem &v) const 
//{
//  RingElement *f1 = RingElement::make_raw(this,f);
//  RingElement *g1 = RingElement::make_raw(this,g);
//  const RingElement *u1;
//  const RingElement *v1;
//  const RingElement *h = rawExtendedGCDRingElement(f1,g1,&u1,&v1);
//  u = u1->get_value();
//  v = v1->get_value();
//  return h->get_value();
//}

ring_elem PolyQQ::remainder(const ring_elem f, const ring_elem g) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return ZERO_RINGELEM;
}

ring_elem PolyQQ::quotient(const ring_elem f, const ring_elem g) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return ZERO_RINGELEM;
}

ring_elem PolyQQ::remainderAndQuotient(const ring_elem f, const ring_elem g, 
				       ring_elem &quot) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return ZERO_RINGELEM;
}

void PolyQQ::syzygy(const ring_elem a, const ring_elem b,
		    ring_elem &x, ring_elem &y) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
}

ring_elem PolyQQ::random() const 
{
  mpz_ptr a = MPZ_VAL(globalZZ->random());
  elem *result = make_fraction(numerR_->random(), a);
  simplify(result);
  return FRAC_RINGELEM(result);
}

void PolyQQ::elem_text_out(buffer &o, const ring_elem f) const 
{
  mpz_ptr fdenom = denom(f);
  bool use_denom = (mask_mpz_cmp_si(fdenom,1) == 0);
  if (use_denom)
    o << "(";
  numerR_->elem_text_out(o,numerator(f));
  if (use_denom)
    o << ")/";
  globalZZ->elem_text_out(o,MPZ_RINGELEM(denom(f)));
}

ring_elem PolyQQ::eval(const RingMap *map, const ring_elem f,int first_var) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return ZERO_RINGELEM;
}

/////////////////////////
// Polynomial routines //
/////////////////////////
int PolyQQ::index_of_var(const ring_elem a) const 
{
  mpz_ptr a1 = denom(a);
  if (mask_mpz_cmp_si(a1, 1) != 0)
    return -1;
  return numerR_->index_of_var(numerator(a));
}

M2_arrayint PolyQQ::support(const ring_elem a) const 
{
  return numerR_->support(numerator(a));
}

bool PolyQQ::is_homogeneous(const ring_elem f) const 
{
  return numerR_->is_homogeneous(numerator(f));
}

void PolyQQ::degree(const ring_elem f, int *d) const 
{
  return numerR_->degree(numerator(f),d);
}

bool PolyQQ::multi_degree(const ring_elem f, int *d) const 
{
  return numerR_->multi_degree(numerator(f), d);
}

void PolyQQ::degree_weights(const ring_elem f, M2_arrayint wts, 
			    int &lo, int &hi) const 
{
  return numerR_->degree_weights(numerator(f),wts,lo,hi);
}

ring_elem PolyQQ::homogenize(const ring_elem f, int v, int deg, 
			     M2_arrayint wts) const 
{
  Nterm *h = numerR_->homogenize(numerator(f),v,deg,wts);
  elem *result = make_fraction(h,denom(f));
  simplify(result);
  return FRAC_RINGELEM(result);
}

ring_elem PolyQQ::homogenize(const ring_elem f, int v, M2_arrayint wts) const 
{
  Nterm *h = numerR_->homogenize(numerator(f),v,wts);
  elem *result = make_fraction(h,denom(f));
  simplify(result);
  return FRAC_RINGELEM(result);
}

ring_elem PolyQQ::mult_by_term(const ring_elem f, 
			       const ring_elem c, const int *m) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return ZERO_RINGELEM;
}

int PolyQQ::n_flat_terms(const ring_elem f) const 
{
  return numerR_->n_flat_terms(numerator(f));
}

int PolyQQ::n_logical_terms(int nvars0,const ring_elem f) const 
{
  return numerR_->n_logical_terms(nvars0,numerator(f));
}

ArrayPairOrNull PolyQQ::list_form(const Ring *coeffR, const ring_elem f) const 
{
  Nterm *f1 = numerator(f);
  mpz_ptr fdenom = denom(f);
  // First determine which of two cases we are considering
  const PolyQQ *cP = coeffR->cast_to_PolyQQ();
  if (cP == 0)
    {
      // coeffR should be globalQQ
      if (coeffR != globalQQ)
	{
	  ERROR("expected coefficient ring to be QQ");
	  return 0;
	}

      ArrayPairOrNull result = numerR_->list_form(globalZZ,f1);
      if (result == 0) return 0;
      // Now go through each coeff and divide by denominator of f.
      ring_elem fbottom = MPZ_RINGELEM(denom(f));
      for (int i=0; i<result->coeffs->len; i++)
	  {
	    mpz_ptr a = MPZ_VAL(result->coeffs->array[i]->get_value());
	    ring_elem b = globalQQ->fraction(MPZ_RINGELEM(a), fbottom);
	    result->coeffs->array[i] = RingElement::make_raw(globalQQ,b);
	  }
      return result;
    }
  else
    {
      ArrayPairOrNull result = numerR_->list_form(cP->getNumeratorRing(),f1);
      if (result == 0) return 0;
      // Now go through each coeff and divide by denominator of f.

      for (int i=0; i<result->coeffs->len; i++)
	{
	  Nterm *t = (result->coeffs->array[i])->get_value();
	  ring_elem a = FRAC_RINGELEM(cP->make_fraction(t, fdenom));
	  result->coeffs->array[i] = RingElement::make_raw(cP,a);
	}
      return result;
    }
}

ring_elem PolyQQ::make_flat_term(const ring_elem a, const int *m) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return ZERO_RINGELEM;
}

ring_elem PolyQQ::make_logical_term(const Ring *coeffR, const ring_elem a, const int *exp) const 
{
  // 2 situations
  //  (a) coeffR is another poly ring over QQ
  //  (b) coeffR is QQ
  Nterm *f;
  mpz_ptr fbottom;
  if (coeffR == globalQQ)
    {
      // a is an element of QQ
      ring_elem atop = globalQQ->numerator(a);
      fbottom = MPZ_VAL(globalQQ->denominator(a));
      f = numerR_->make_logical_term(globalZZ,atop,exp);
    }
  else
    {
      // Otherwise coeffR is another (smaller) poly ring over QQ
      Nterm *atop = numerator(a);
      fbottom = denom(a);
      f = make_logical_term(coeffR,atop,exp);
    }

  elem *result = make_fraction(f, fbottom);
  simplify(result);
  return FRAC_RINGELEM(result);
  
}

ring_elem PolyQQ::lead_flat_coeff(const ring_elem f) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return ZERO_RINGELEM;
}

ring_elem PolyQQ::lead_logical_coeff(const Ring *coeffR, const ring_elem f) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return ZERO_RINGELEM;
}

ring_elem PolyQQ::get_coeff(const Ring *coeffR, const ring_elem f, const int *vp) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return ZERO_RINGELEM;
}

// vp is a varpower monomial, in the logical monoid.
// The result will be an element in the logical coefficient ring.

ring_elem PolyQQ::get_terms(int nvars0,const ring_elem f, int lo, int hi) const 
{
  Nterm *top = numerR_->get_terms(nvars0,numerator(f),lo,hi);
  elem *result = make_fraction(top,denom(f));
  simplify(result);
  return FRAC_RINGELEM(result);
}

// get the (logical) terms from lo to hi in f.  A negative value means count from
// the end.  get_terms(--,f,0,0) is the logical lead term of f.

const int * PolyQQ::lead_flat_monomial(const ring_elem f) const 
{
  return numerR_->lead_flat_monomial(numerator(f));
}

void PolyQQ::lead_logical_exponents(int nvars0, const ring_elem f, int * result_exp) const 
{
  numerR_->lead_logical_exponents(nvars0,numerator(f),result_exp);
}

void PolyQQ::mult_coeff_to(ring_elem a, ring_elem &f) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
}

void PolyQQ::monomial_divisor(const ring_elem a, int *exp) const 
{
  numerR_->monomial_divisor(numerator(a),exp);
}

ring_elem PolyQQ::diff(ring_elem a, ring_elem b, int use_coeff) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return ZERO_RINGELEM;
}

bool PolyQQ::in_subring(int nslots, const ring_elem a) const 
{
  return numerR_->in_subring(nslots,numerator(a));
}

void PolyQQ::degree_of_var(int n, const ring_elem a, int &lo, int &hi) const 
{
  numerR_->degree_of_var(n, numerator(a), lo, hi);
}

ring_elem PolyQQ::divide_by_var(int n, int d, const ring_elem a) const 
{
  Nterm *h = numerR_->divide_by_var(n,d,numerator(a));
  elem *result = make_fraction(h,denom(a));
  simplify(result);
  return FRAC_RINGELEM(result);
}

ring_elem PolyQQ::divide_by_expvector(const int *exp, const ring_elem a) const 
{
  Nterm *h = numerR_->divide_by_expvector(exp,numerator(a));
  elem *result = make_fraction(h,denom(a));
  simplify(result);
  return FRAC_RINGELEM(result);
}

const vecterm * PolyQQ::vec_locate_lead_term(const FreeModule *F, vec v) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return 0;
}

vec PolyQQ::vec_lead_term(int nparts, const FreeModule *F, vec v) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return 0;
}

vec PolyQQ::vec_top_coefficient(const vec v, int &x, int &e) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return 0;
}

gbvector * PolyQQ::translate_gbvector_from_ringelem(ring_elem coeff) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return 0;
}

gbvector * PolyQQ::translate_gbvector_from_vec(const FreeModule *F, 
					       const vec v, 
					       ring_elem &result_denominator) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return 0;
}

vec PolyQQ::translate_gbvector_to_vec(const FreeModule *F, const gbvector *v) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return 0;
}

vec PolyQQ::translate_gbvector_to_vec_denom(const FreeModule *F, 
					    const gbvector *v,
					    const ring_elem denom0) const 
{
#ifdef DEVELOPMENT
#warning "write this"
#endif
  return 0;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
