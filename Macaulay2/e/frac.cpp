// Copyright 1995 Michael E. Stillman

#include "frac.hpp"
#include "text_io.hpp"
#include "monoid.hpp"
#include "ringmap.hpp"
#include "gbring.hpp"

#define FRAC_VAL(f) ((frac_elem *) (f).poly_val)
#define FRAC_RINGELEM(a) ((ring_elem) (Nterm *) (a))

bool FractionField::initialize_frac(const Ring *R) 
{
  initialize_ring(R->charac(),
		  R->total_n_vars(),
		  R->total_n_vars(),
		  this,
		  Monoid::get_trivial_monoid(),
		  R->degree_monoid());

  _elem_size = sizeof(frac_elem);
  _MINUS_ONE = R->from_int(-1);
  declare_field();
  return true;
}

FractionField *FractionField::create(const Ring *R)
{
  FractionField *result = new FractionField;
  result->initialize_frac(R);
  return result;
}

void FractionField::text_out(buffer &o) const
{
  o << "Frac(";
  R_->text_out(o);
  o << ")";
}

ring_elem FractionField::numerator(ring_elem f) const
{
  frac_elem *g = FRAC_VAL(f);
  return R_->copy(g->numer);
}

ring_elem FractionField::denominator(ring_elem f) const
{
  frac_elem *g = FRAC_VAL(f);
  return R_->copy(g->denom);
}

frac_elem *FractionField::new_frac_elem() const
{
  return (frac_elem *) getmem(_elem_size);
}

ring_elem FractionField::fraction(const ring_elem top, const ring_elem bottom) const
{
  return FRAC_RINGELEM(make_elem(R_->copy(top), R_->copy(bottom)));
}

void FractionField::simplify(frac_elem *f) const
{
  ring_elem x, y;
  R_->syzygy(f->numer, f->denom, x, y);
  if (R_->is_zero(x))
    {
      //R_->zero_divisor = y;
      R_->remove(x);
      ERROR("zero divisor found");
      return;
      // NOW QUIT whatever computation is going on!! MES
    }
  R_->negate_to(y);
  R_->remove(f->numer);
  R_->remove(f->denom);
  f->numer = y;
  f->denom = x;
#if 0
  if (R_->is_zero(f->numer))
    {
      R_->remove(f->denom);
      f->denom = R_->from_int(1);
      return;
    }
  if (R_->has_gcd())
    {
      ring_elem g = R_->gcd(f->numer, f->denom);
      if (!R_->is_unit(g))
	{
	  ring_elem tmp1 = R_->divide(f->numer, g); // exact division
	  ring_elem tmp2 = R_->divide(f->denom, g); // exact division
	  R_->remove(f->numer);
	  R_->remove(f->denom);
	  f->numer = tmp1;
	  f->denom = tmp2;
	}
      R_->remove(g);
    }
  if (R_->is_unit(f->denom))
    {
      ring_elem tmp = R_->divide(f->numer, f->denom); // exact division
      R_->remove(f->numer);
      R_->remove(f->denom);
      f->numer = tmp;
      f->denom = R_->from_int(1);
    }
#endif
}

frac_elem *FractionField::make_elem(ring_elem a, ring_elem b) const
{
  frac_elem *result = new_frac_elem();
  result->numer = a;
  result->denom = b;
  simplify(result);
  return result;
}

ring_elem FractionField::random() const
{
  ring_elem a = R_->random();
  ring_elem b = R_->random();
  if (R_->is_zero(b))
    {
      R_->remove(b);
      b = R_->from_int(1);
    }
  return FRAC_RINGELEM(make_elem(a,b));
}

ring_elem FractionField::random(int homog, const int *deg) const
{
  ring_elem a = R_->random(homog,deg);
  ring_elem b = R_->random(homog,deg);
  if (R_->is_zero(b))
    {
      R_->remove(b);
      b = R_->from_int(1);
    }
  return FRAC_RINGELEM(make_elem(a,b));
}

void FractionField::elem_text_out(buffer &o, const ring_elem a) const
{
  int old_one = p_one;
  int old_plus = p_plus;
  int old_parens = p_parens;

  frac_elem *f = FRAC_VAL(a);
  ring_elem one = R_->from_int(1);
  int denom_one = R_->is_equal(f->denom, one);
  R_->remove(one);

  p_one = p_one || !denom_one;
  p_parens = old_parens || !denom_one;
  R_->elem_text_out(o, f->numer);
  if (!denom_one)
    {
      o << "/";
      p_plus = 0;
      R_->elem_text_out(o, f->denom);
    }

  p_parens = old_parens;
  p_one = old_one;
  p_plus = old_plus;
}

ring_elem FractionField::from_int(int n) const
{
  frac_elem *f = new_frac_elem();
  f->numer = R_->from_int(n);
  f->denom = R_->from_int(1);
  return FRAC_RINGELEM(f);
}

ring_elem FractionField::from_int(mpz_ptr n) const
{
  frac_elem *f = new_frac_elem();
  f->numer = R_->from_int(n);
  f->denom = R_->from_int(1);
  return FRAC_RINGELEM(f);
}

ring_elem FractionField::var(int v, int n) const
{
  frac_elem *f = new_frac_elem();
  if (n >= 0)
    {
      f->numer = R_->var(v,n);
      f->denom = R_->from_int(1);
    }
  else if (n < 0)
    {
      f->numer = R_->from_int(1);
      f->denom = R_->var(v,-n);
    }
    
  return FRAC_RINGELEM(f);
}

bool FractionField::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
{
  // Rf = R ---> frac R
  if (Rf == R_)
    {
      frac_elem *g = new_frac_elem();
      g->numer = R_->copy(f);
      g->denom = R_->from_int(1);
      result = FRAC_RINGELEM(g);
      return true;
    }

  return false;
}

bool FractionField::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
{
  // Rg = R ---> frac R
  // f is an element of frac R.

  if (Rg == R_)
    {
      frac_elem *h = FRAC_VAL(f);
      if (R_->is_unit(h->denom)) // In this case, by 'simplify', f->denom == 1.
	{
	  result = R_->copy(h->numer);
	  return true;
	}
    }
  return false;
}

bool FractionField::is_unit(const ring_elem f) const
{
  return (!R_->is_zero(FRAC_VAL(f)->numer));
}

bool FractionField::is_zero(const ring_elem f) const
{
  return (R_->is_zero(FRAC_VAL(f)->numer));
}

bool FractionField::is_equal(const ring_elem a, const ring_elem b) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = FRAC_VAL(b);
  if (R_->is_equal(f->denom, g->denom))
    {
      return R_->is_equal(f->numer, g->numer);
    }
  else
    {
      ring_elem h = subtract(a,b);
      bool result = is_zero(h);
      remove(h);
      return result;
    }
}

ring_elem FractionField::copy(const ring_elem a) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = new_frac_elem();
  g->numer = R_->copy(f->numer);
  g->denom = R_->copy(f->denom);
  return FRAC_RINGELEM(g);
}

void FractionField::remove(ring_elem &a) const
{
}

void FractionField::negate_to(ring_elem &a) const
{
  frac_elem *f = FRAC_VAL(a);
  R_->negate_to(f->numer);
}

void FractionField::add_to(ring_elem &a, ring_elem &b) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = FRAC_VAL(b);
  if (R_->is_equal(f->denom, g->denom))
    R_->add_to(f->numer, g->numer);
  else
    {
      R_->mult_to(f->numer, g->denom);
      ring_elem tmp = R_->mult(f->denom, g->numer);
      R_->add_to(f->numer, tmp);
      R_->mult_to(f->denom, g->denom);
    }
  simplify(f);
  remove(b);
  a = FRAC_RINGELEM(f);
}

void FractionField::subtract_to(ring_elem &a, ring_elem &b) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = FRAC_VAL(b);
  if (R_->is_equal(f->denom, g->denom))
    R_->subtract_to(f->numer, g->numer);
  else
    {
      R_->mult_to(f->numer, g->denom);
      ring_elem tmp = R_->mult(f->denom, g->numer);
      R_->subtract_to(f->numer, tmp);
      R_->mult_to(f->denom, g->denom);
    }
  simplify(f);
  remove(b);
  a = FRAC_RINGELEM(f);
}

ring_elem FractionField::negate(const ring_elem a) const
{
  const frac_elem *f = FRAC_VAL(a);
  frac_elem *result = new_frac_elem();
  result->numer = R_->negate(f->numer);
  result->denom = R_->copy(f->denom);
  return FRAC_RINGELEM(result);
}

ring_elem FractionField::add(const ring_elem a, const ring_elem b) const
{
  const frac_elem *f = FRAC_VAL(a);
  const frac_elem *g = FRAC_VAL(b);
  ring_elem top, bottom;

  if (R_->is_equal(f->denom, g->denom))
    {
      top = R_->add(f->numer, g->numer);
      bottom = R_->copy(f->denom);
    }
  else
    {
      top = R_->mult(f->numer, g->denom);
      ring_elem tmp = R_->mult(f->denom, g->numer);
      R_->add_to(top, tmp);
      bottom = R_->mult(f->denom, g->denom);
    }
  frac_elem *result = make_elem(top, bottom);
  return FRAC_RINGELEM(result);
}

ring_elem FractionField::subtract(const ring_elem a, const ring_elem b) const
{
  const frac_elem *f = FRAC_VAL(a);
  const frac_elem *g = FRAC_VAL(b);
  ring_elem top, bottom;

  if (R_->is_equal(f->denom, g->denom))
    {
      top = R_->subtract(f->numer, g->numer);
      bottom = R_->copy(f->denom);
    }
  else
    {
      top = R_->mult(f->numer, g->denom);
      ring_elem tmp = R_->mult(f->denom, g->numer);
      R_->subtract_to(top, tmp);
      bottom = R_->mult(f->denom, g->denom);
    }
  frac_elem *result = make_elem(top, bottom);
  return FRAC_RINGELEM(result);
}

ring_elem FractionField::mult(const ring_elem a, const ring_elem b) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = FRAC_VAL(b);
  ring_elem top = R_->mult(f->numer, g->numer);
  ring_elem bottom = R_->mult(f->denom, g->denom);
  return FRAC_RINGELEM(make_elem(top, bottom));
}

ring_elem FractionField::power(const ring_elem a, int n) const
{
  frac_elem *f = FRAC_VAL(a);
  ring_elem top, bottom;
  if (n >= 0)
    {
      top = R_->power(f->numer, n);
      bottom = R_->power(f->denom, n);
    }
  else
    {
      top = R_->power(f->denom, -n);
      bottom = R_->power(f->numer, -n);
      if (R_->is_zero(bottom))
	{
	  ERROR("attempt to divide by zero");
	  R_->remove(bottom);
	  bottom = R_->from_int(1);
	}
    }
  return FRAC_RINGELEM(make_elem(top, bottom));
}
ring_elem FractionField::power(const ring_elem a, mpz_t n) const
{
  frac_elem *f = FRAC_VAL(a);
  ring_elem top, bottom;
  if (mpz_sgn(n) >= 0)
    {
      top = R_->power(f->numer, n);
      bottom = R_->power(f->denom, n);
    }
  else
    {
      mpz_neg(n, n);
      top = R_->power(f->denom, n);
      bottom = R_->power(f->numer, n);
      mpz_neg(n, n);
      if (R_->is_zero(bottom))
	{
	  ERROR("attempt to divide by zero");
	  R_->remove(bottom);
	  bottom = R_->from_int(1);
	}
    }
  return FRAC_RINGELEM(make_elem(top, bottom));
}

ring_elem FractionField::invert(const ring_elem a) const
{
  frac_elem *f = FRAC_VAL(a);
  ring_elem top = R_->copy(f->denom);
  ring_elem bottom = R_->copy(f->numer);
  return FRAC_RINGELEM(make_elem(top, bottom));
}

ring_elem FractionField::divide(const ring_elem a, const ring_elem b) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = FRAC_VAL(b);
  ring_elem top = R_->mult(f->numer, g->denom);
  ring_elem bottom = R_->mult(f->denom, g->numer);
  return FRAC_RINGELEM(make_elem(top, bottom));
}

ring_elem FractionField::divide(const ring_elem a, const ring_elem b, ring_elem &rem) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = FRAC_VAL(b);
  ring_elem top = R_->mult(f->numer, g->denom);
  ring_elem bottom = R_->mult(f->denom, g->numer);
  rem = from_int(0);
  return FRAC_RINGELEM(make_elem(top, bottom));
}
ring_elem FractionField::gcd(const ring_elem a, const ring_elem b) const
{
  if (is_zero(a) || is_zero(b)) return from_int(0);
  return from_int(1);
}

ring_elem FractionField::gcd_extended(const ring_elem f, const ring_elem, 
				ring_elem &u, ring_elem &v) const
{
  v = from_int(0);
  u = invert(f);
  return from_int(1);
}

ring_elem FractionField::remainder(const ring_elem f, const ring_elem g) const
{
  if (FractionField::is_zero(g)) return FractionField::copy(f);
  return from_int(0);
}

ring_elem FractionField::quotient(const ring_elem f, const ring_elem g) const
{
  if (FractionField::is_zero(g)) return FractionField::from_int(0);
  if (FractionField::is_zero(f)) return FractionField::from_int(0);

  frac_elem *a = FRAC_VAL(f);
  frac_elem *b = FRAC_VAL(g);
  ring_elem top = R_->mult(a->numer, b->denom);
  ring_elem bottom = R_->mult(a->denom, b->numer);
  return FRAC_RINGELEM(make_elem(top, bottom));
}

ring_elem FractionField::remainderAndQuotient(const ring_elem f, const ring_elem g, 
				      ring_elem &quot) const
{
  if (FractionField::is_zero(g)) 
    {
      quot = FractionField::from_int(0);
      return FractionField::copy(f);
    }
  else
    {
      frac_elem *a = FRAC_VAL(f);
      frac_elem *b = FRAC_VAL(g);
      ring_elem top = R_->mult(a->numer, b->denom);
      ring_elem bottom = R_->mult(a->denom, b->numer);
      quot = FRAC_RINGELEM(make_elem(top, bottom));
      return FractionField::from_int(0);
    }
}

void FractionField::syzygy(const ring_elem a, const ring_elem b,
			   ring_elem &x, ring_elem &y) const
{
  x = FractionField::from_int(1);
  y = FractionField::divide(a,b);
  FractionField::negate_to(y);
}

ring_elem FractionField::eval(const RingMap *map, const ring_elem a) const
{
  const Ring *S = map->get_ring();
  const frac_elem *f = FRAC_VAL(a);
  ring_elem top = R_->eval(map, f->numer);
  if (S->is_zero(top)) return top;
  ring_elem bottom = R_->eval(map, f->denom);
  if (S->is_zero(bottom))
    {
      ERROR("division by zero!");
      S->remove(bottom);
      bottom = S->from_int(1);
    }
  ring_elem result = S->divide(top, bottom);
  S->remove(top);
  S->remove(bottom);
  return result;
}

bool FractionField::is_homogeneous(const ring_elem a) const
{
  if (is_zero(a)) return true;
  const frac_elem *f = FRAC_VAL(a);
  if (!R_->is_homogeneous(f->numer)
      || !R_->is_homogeneous(f->denom))
    return false;
  return true;
}

void FractionField::degree(const ring_elem a, int *d) const
{
  const frac_elem *f = FRAC_VAL(a);
  R_->degree(f->numer, d);
  int *e = degree_monoid()->make_one();
  R_->degree(f->denom, e);
  degree_monoid()->divide(d, e, d);
  degree_monoid()->remove(e);
}

bool FractionField::multi_degree(const ring_elem a, int *d) const
{
  const frac_elem *f = FRAC_VAL(a);
  bool tophom = R_->multi_degree(f->numer, d);
  int *e = degree_monoid()->make_one();
  bool bottomhom = R_->multi_degree(f->denom, e);
  degree_monoid()->divide(d, e, d);
  degree_monoid()->remove(e);
  return tophom && bottomhom;
}

void FractionField::degree_weights(const ring_elem, const M2_arrayint, int &lo, int &hi) const
{
  assert(0);
  // MES: what should this do?
  lo = hi = 0;
}

int FractionField::primary_degree(const ring_elem a) const
{
  const frac_elem *f = FRAC_VAL(a);
  return R_->primary_degree(f->numer) - R_->primary_degree(f->denom);
}

ring_elem FractionField::homogenize(const ring_elem a, int v, int deg, 
				    const M2_arrayint wts) const
{
  ring_elem top, bottom;
  frac_elem *result;
  const frac_elem *f = FRAC_VAL(a);
  int d1 = R_->primary_degree(f->numer);
  int d2 = R_->primary_degree(f->denom);
  if (deg >= d1-d2)
    {
      top = R_->homogenize(f->numer, v, deg + d2, wts);
      bottom = R_->homogenize(f->denom, v, d2, wts);
      result = make_elem(top, bottom);
    }
  else
    {
      top = R_->homogenize(f->numer, v, d1, wts);
      bottom = R_->homogenize(f->denom, v, -deg + d1, wts);
      result = make_elem(top, bottom);
    }
  return FRAC_RINGELEM(result);
}

ring_elem FractionField::homogenize(const ring_elem a, int v, const M2_arrayint wts) const
{
  const frac_elem *f = FRAC_VAL(a);
  ring_elem top = R_->homogenize(f->numer, v, wts);
  ring_elem bottom = R_->homogenize(f->denom, v, wts);
  frac_elem *result = make_elem(top, bottom);
  return FRAC_RINGELEM(result);
}

int FractionField::n_terms(const ring_elem) const
{
  return 1;
}
ring_elem FractionField::term(const ring_elem a, const int *) const
{
  return copy(a);
}
ring_elem FractionField::lead_coeff(const ring_elem f) const
{
  return f;
}
ring_elem FractionField::get_coeff(const ring_elem f, const int *) const
{
  return f;
}
ring_elem FractionField::get_terms(const ring_elem f, int, int) const
{
  return f;
}

///////////////////////////////////
// translation gbvector <--> vec //
///////////////////////////////////
ring_elem FractionField::trans_to_ringelem(ring_elem coeff, 
					   const int *exp) const
{
  ring_elem a = get_ring()->trans_to_ringelem(coeff,exp);
  return this->fraction(a, trans_one);
}

ring_elem FractionField::trans_to_ringelem_denom(ring_elem coeff, 
						 ring_elem denom, 
						 int *exp) const
{
  ring_elem a = get_ring()->trans_to_ringelem(coeff,exp);
  return this->fraction(a, denom);
}

void FractionField::trans_from_ringelem(gbvectorHeap &H, 
			     ring_elem coeff, 
			     int comp, 
			     int *exp,
			     int firstvar) const
{
  ring_elem a = this->numerator(coeff);
  get_ring()->trans_from_ringelem(H, a, comp, exp, firstvar);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:

