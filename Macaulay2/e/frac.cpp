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

  _flattened_ring = R->get_flattened_ring();
  _elem_size = sizeof(frac_elem);
  _MINUS_ONE = R->from_int(-1);
  declare_field();
  return true;
}

FractionField *FractionField::create(const Ring *R)
{
  FractionField *result = new FractionField;
  result->initialize_frac(R);
  result->_grtype = GRType::make_FRAC(result);
  return result;
}

void FractionField::text_out(buffer &o) const
{
  o << "Frac(";
  _R->text_out(o);
  o << ")";
}

ring_elem FractionField::numerator(ring_elem f) const
{
  frac_elem *g = FRAC_VAL(f);
  return _R->copy(g->numer);
}

ring_elem FractionField::denominator(ring_elem f) const
{
  frac_elem *g = FRAC_VAL(f);
  return _R->copy(g->denom);
}

frac_elem *FractionField::new_frac_elem() const
{
  return (frac_elem *) getmem(_elem_size);
}

ring_elem FractionField::fraction(const ring_elem top, const ring_elem bottom) const
{
  return FRAC_RINGELEM(make_elem(_R->copy(top), _R->copy(bottom)));
}

void FractionField::simplify(frac_elem *f) const
{
  ring_elem x, y;
  _R->syzygy(f->numer, f->denom, x, y);
  if (_R->is_zero(x))
    {
      //_R->zero_divisor = y;
      _R->remove(x);
      ERROR("zero divisor found");
      return;
      // NOW QUIT whatever computation is going on!! MES
    }
  _R->negate_to(y);
  _R->remove(f->numer);
  _R->remove(f->denom);
  f->numer = y;
  f->denom = x;
#if 0
  if (_R->is_zero(f->numer))
    {
      _R->remove(f->denom);
      f->denom = _R->from_int(1);
      return;
    }
  if (_R->has_gcd())
    {
      ring_elem g = _R->gcd(f->numer, f->denom);
      if (!_R->is_unit(g))
	{
	  ring_elem tmp1 = _R->divide(f->numer, g); // exact division
	  ring_elem tmp2 = _R->divide(f->denom, g); // exact division
	  _R->remove(f->numer);
	  _R->remove(f->denom);
	  f->numer = tmp1;
	  f->denom = tmp2;
	}
      _R->remove(g);
    }
  if (_R->is_unit(f->denom))
    {
      ring_elem tmp = _R->divide(f->numer, f->denom); // exact division
      _R->remove(f->numer);
      _R->remove(f->denom);
      f->numer = tmp;
      f->denom = _R->from_int(1);
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
  ring_elem a = _R->random();
  ring_elem b = _R->random();
  if (_R->is_zero(b))
    {
      _R->remove(b);
      b = _R->from_int(1);
    }
  return FRAC_RINGELEM(make_elem(a,b));
}

ring_elem FractionField::random(int homog, const int *deg) const
{
  ring_elem a = _R->random(homog,deg);
  ring_elem b = _R->random(homog,deg);
  if (_R->is_zero(b))
    {
      _R->remove(b);
      b = _R->from_int(1);
    }
  return FRAC_RINGELEM(make_elem(a,b));
}

void FractionField::elem_text_out(buffer &o, const ring_elem a) const
{
  int old_one = p_one;
  int old_plus = p_plus;
  int old_parens = p_parens;

  frac_elem *f = FRAC_VAL(a);
  ring_elem one = _R->from_int(1);
  int denom_one = _R->is_equal(f->denom, one);
  _R->remove(one);

  p_one = p_one || !denom_one;
  p_parens = old_parens || !denom_one;
  _R->elem_text_out(o, f->numer);
  if (!denom_one)
    {
      o << "/";
      p_plus = 0;
      _R->elem_text_out(o, f->denom);
    }

  p_parens = old_parens;
  p_one = old_one;
  p_plus = old_plus;
}

ring_elem FractionField::from_int(int n) const
{
  frac_elem *f = new_frac_elem();
  f->numer = _R->from_int(n);
  f->denom = _R->from_int(1);
  return FRAC_RINGELEM(f);
}

ring_elem FractionField::from_int(mpz_ptr n) const
{
  frac_elem *f = new_frac_elem();
  f->numer = _R->from_int(n);
  f->denom = _R->from_int(1);
  return FRAC_RINGELEM(f);
}

ring_elem FractionField::var(int v, int n) const
{
  frac_elem *f = new_frac_elem();
  if (n >= 0)
    {
      f->numer = _R->var(v,n);
      f->denom = _R->from_int(1);
    }
  else if (n < 0)
    {
      f->numer = _R->from_int(1);
      f->denom = _R->var(v,-n);
    }
    
  return FRAC_RINGELEM(f);
}

bool FractionField::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
{
  // Rf = R ---> frac R
  if (Rf == _R)
    {
      frac_elem *g = new_frac_elem();
      g->numer = _R->copy(f);
      g->denom = _R->from_int(1);
      result = FRAC_RINGELEM(g);
      return true;
    }

  return false;
}

bool FractionField::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
{
  // Rg = R ---> frac R
  // f is an element of frac R.

  if (Rg == _R)
    {
      frac_elem *h = FRAC_VAL(f);
      if (_R->is_unit(h->denom)) // In this case, by 'simplify', f->denom == 1.
	{
	  result = _R->copy(h->numer);
	  return true;
	}
    }
  return false;
}

bool FractionField::is_unit(const ring_elem f) const
{
  return (!_R->is_zero(FRAC_VAL(f)->numer));
}

bool FractionField::is_zero(const ring_elem f) const
{
  return (_R->is_zero(FRAC_VAL(f)->numer));
}

bool FractionField::is_equal(const ring_elem a, const ring_elem b) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = FRAC_VAL(b);
  if (_R->is_equal(f->denom, g->denom))
    {
      return _R->is_equal(f->numer, g->numer);
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
  g->numer = _R->copy(f->numer);
  g->denom = _R->copy(f->denom);
  return FRAC_RINGELEM(g);
}

void FractionField::remove(ring_elem &a) const
{
}

void FractionField::negate_to(ring_elem &a) const
{
  frac_elem *f = FRAC_VAL(a);
  _R->negate_to(f->numer);
}

void FractionField::add_to(ring_elem &a, ring_elem &b) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = FRAC_VAL(b);
  if (_R->is_equal(f->denom, g->denom))
    _R->add_to(f->numer, g->numer);
  else
    {
      _R->mult_to(f->numer, g->denom);
      ring_elem tmp = _R->mult(f->denom, g->numer);
      _R->add_to(f->numer, tmp);
      _R->mult_to(f->denom, g->denom);
    }
  simplify(f);
  remove(b);
  a = FRAC_RINGELEM(f);
}

void FractionField::subtract_to(ring_elem &a, ring_elem &b) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = FRAC_VAL(b);
  if (_R->is_equal(f->denom, g->denom))
    _R->subtract_to(f->numer, g->numer);
  else
    {
      _R->mult_to(f->numer, g->denom);
      ring_elem tmp = _R->mult(f->denom, g->numer);
      _R->subtract_to(f->numer, tmp);
      _R->mult_to(f->denom, g->denom);
    }
  simplify(f);
  remove(b);
  a = FRAC_RINGELEM(f);
}

ring_elem FractionField::negate(const ring_elem a) const
{
  const frac_elem *f = FRAC_VAL(a);
  frac_elem *result = new_frac_elem();
  result->numer = _R->negate(f->numer);
  result->denom = _R->copy(f->denom);
  return FRAC_RINGELEM(result);
}

ring_elem FractionField::add(const ring_elem a, const ring_elem b) const
{
  const frac_elem *f = FRAC_VAL(a);
  const frac_elem *g = FRAC_VAL(b);
  ring_elem top, bottom;

  if (_R->is_equal(f->denom, g->denom))
    {
      top = _R->add(f->numer, g->numer);
      bottom = _R->copy(f->denom);
    }
  else
    {
      top = _R->mult(f->numer, g->denom);
      ring_elem tmp = _R->mult(f->denom, g->numer);
      _R->add_to(top, tmp);
      bottom = _R->mult(f->denom, g->denom);
    }
  frac_elem *result = make_elem(top, bottom);
  return FRAC_RINGELEM(result);
}

ring_elem FractionField::subtract(const ring_elem a, const ring_elem b) const
{
  const frac_elem *f = FRAC_VAL(a);
  const frac_elem *g = FRAC_VAL(b);
  ring_elem top, bottom;

  if (_R->is_equal(f->denom, g->denom))
    {
      top = _R->subtract(f->numer, g->numer);
      bottom = _R->copy(f->denom);
    }
  else
    {
      top = _R->mult(f->numer, g->denom);
      ring_elem tmp = _R->mult(f->denom, g->numer);
      _R->subtract_to(top, tmp);
      bottom = _R->mult(f->denom, g->denom);
    }
  frac_elem *result = make_elem(top, bottom);
  return FRAC_RINGELEM(result);
}

ring_elem FractionField::mult(const ring_elem a, const ring_elem b) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = FRAC_VAL(b);
  ring_elem top = _R->mult(f->numer, g->numer);
  ring_elem bottom = _R->mult(f->denom, g->denom);
  return FRAC_RINGELEM(make_elem(top, bottom));
}

ring_elem FractionField::power(const ring_elem a, int n) const
{
  frac_elem *f = FRAC_VAL(a);
  ring_elem top, bottom;
  if (n >= 0)
    {
      top = _R->power(f->numer, n);
      bottom = _R->power(f->denom, n);
    }
  else
    {
      top = _R->power(f->denom, -n);
      bottom = _R->power(f->numer, -n);
      if (_R->is_zero(bottom))
	{
	  ERROR("attempt to divide by zero");
	  _R->remove(bottom);
	  bottom = _R->from_int(1);
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
      top = _R->power(f->numer, n);
      bottom = _R->power(f->denom, n);
    }
  else
    {
      mpz_neg(n, n);
      top = _R->power(f->denom, n);
      bottom = _R->power(f->numer, n);
      mpz_neg(n, n);
      if (_R->is_zero(bottom))
	{
	  ERROR("attempt to divide by zero");
	  _R->remove(bottom);
	  bottom = _R->from_int(1);
	}
    }
  return FRAC_RINGELEM(make_elem(top, bottom));
}

ring_elem FractionField::invert(const ring_elem a) const
{
  frac_elem *f = FRAC_VAL(a);
  ring_elem top = _R->copy(f->denom);
  ring_elem bottom = _R->copy(f->numer);
  return FRAC_RINGELEM(make_elem(top, bottom));
}

ring_elem FractionField::divide(const ring_elem a, const ring_elem b) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = FRAC_VAL(b);
  ring_elem top = _R->mult(f->numer, g->denom);
  ring_elem bottom = _R->mult(f->denom, g->numer);
  return FRAC_RINGELEM(make_elem(top, bottom));
}

ring_elem FractionField::divide(const ring_elem a, const ring_elem b, ring_elem &rem) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = FRAC_VAL(b);
  ring_elem top = _R->mult(f->numer, g->denom);
  ring_elem bottom = _R->mult(f->denom, g->numer);
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
  ring_elem top = _R->mult(a->numer, b->denom);
  ring_elem bottom = _R->mult(a->denom, b->numer);
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
      ring_elem top = _R->mult(a->numer, b->denom);
      ring_elem bottom = _R->mult(a->denom, b->numer);
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
  ring_elem top = _R->eval(map, f->numer);
  if (S->is_zero(top)) return top;
  ring_elem bottom = _R->eval(map, f->denom);
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
  if (!_R->is_homogeneous(f->numer)
      || !_R->is_homogeneous(f->denom))
    return false;
  return true;
}

void FractionField::degree(const ring_elem a, int *d) const
{
  const frac_elem *f = FRAC_VAL(a);
  _R->degree(f->numer, d);
  int *e = degree_monoid()->make_one();
  _R->degree(f->denom, e);
  degree_monoid()->divide(d, e, d);
  degree_monoid()->remove(e);
}

bool FractionField::multi_degree(const ring_elem a, int *d) const
{
  const frac_elem *f = FRAC_VAL(a);
  bool tophom = _R->multi_degree(f->numer, d);
  int *e = degree_monoid()->make_one();
  bool bottomhom = _R->multi_degree(f->denom, e);
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
  return _R->primary_degree(f->numer) - _R->primary_degree(f->denom);
}

ring_elem FractionField::homogenize(const ring_elem a, int v, int deg, 
				    const M2_arrayint wts) const
{
  ring_elem top, bottom;
  frac_elem *result;
  const frac_elem *f = FRAC_VAL(a);
  int d1 = _R->primary_degree(f->numer);
  int d2 = _R->primary_degree(f->denom);
  if (deg >= d1-d2)
    {
      top = _R->homogenize(f->numer, v, deg + d2, wts);
      bottom = _R->homogenize(f->denom, v, d2, wts);
      result = make_elem(top, bottom);
    }
  else
    {
      top = _R->homogenize(f->numer, v, d1, wts);
      bottom = _R->homogenize(f->denom, v, -deg + d1, wts);
      result = make_elem(top, bottom);
    }
  return FRAC_RINGELEM(result);
}

ring_elem FractionField::homogenize(const ring_elem a, int v, const M2_arrayint wts) const
{
  const frac_elem *f = FRAC_VAL(a);
  ring_elem top = _R->homogenize(f->numer, v, wts);
  ring_elem bottom = _R->homogenize(f->denom, v, wts);
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
