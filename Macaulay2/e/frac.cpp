// Copyright 1995 Michael E. Stillman

#include "frac.hpp"
#include "text_io.hpp"
#include "bin_io.hpp"
#include "monoid.hpp"
#include "ringmap.hpp"
#include "serial.hpp"

#define FRAC_VAL(f) ((frac_elem *) (f).poly_val)
#define FRAC_RINGELEM(a) ((ring_elem) (Nterm *) (a))

FractionField::FractionField(const Ring *RR) 
: Ring(RR->charac(),RR->total_n_vars(), RR->total_n_vars(),
	this /* Visual C WARNING */, trivial_monoid, RR->degree_monoid()),
  R(RR)
{
  MINUS_ONE = R->from_int(-1);
  bump_up((Ring *) R);
  frac_stash = new stash("fractions", sizeof(frac_elem));
}

FractionField::~FractionField()
{
  bump_down((Ring *) R);
  delete frac_stash;
}

FractionField *FractionField::create(const Ring *R)
{
  FractionField *obj = new FractionField(R);
  return (FractionField *) intern(obj);
}

void FractionField::write_object(object_writer &o) const
{
  o << class_id() << R;
  // MESXX: read_object needs to be done, as do the read/write_element.
}

void FractionField::text_out(buffer &o) const
{
  o << "Frac(";
  R->text_out(o);
  o << ")";
}

ring_elem FractionField::numerator(ring_elem f) const
{
  frac_elem *g = FRAC_VAL(f);
  return R->copy(g->numer);
}

ring_elem FractionField::denominator(ring_elem f) const
{
  frac_elem *g = FRAC_VAL(f);
  return R->copy(g->denom);
}

frac_elem *FractionField::new_frac_elem() const
{
  return (frac_elem *) frac_stash->new_elem();
}

ring_elem FractionField::fraction(const ring_elem top, const ring_elem bottom) const
{
  return FRAC_RINGELEM(make_elem(R->copy(top), R->copy(bottom)));
}

void FractionField::simplify(frac_elem *f) const
{
  if (R->is_zero(f->numer))
    {
      R->remove(f->denom);
      f->denom = R->from_int(1);
      return;
    }
  if (R->has_gcd())
    {
      ring_elem g = R->gcd(f->numer, f->denom);
      if (!R->is_unit(g))
	{
	  ring_elem tmp1 = R->divide(f->numer, g);
	  ring_elem tmp2 = R->divide(f->denom, g);
	  R->remove(f->numer);
	  R->remove(f->denom);
	  f->numer = tmp1;
	  f->denom = tmp2;
	}
      R->remove(g);
    }
  if (R->is_unit(f->denom))
    {
      ring_elem tmp = R->divide(f->numer, f->denom);
      R->remove(f->numer);
      R->remove(f->denom);
      f->numer = tmp;
      f->denom = R->from_int(1);
    }
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
  ring_elem a = R->random();
  ring_elem b = R->random();
  if (R->is_zero(b))
    {
      R->remove(b);
      b = R->from_int(1);
    }
  return FRAC_RINGELEM(make_elem(a,b));
}

ring_elem FractionField::random(int homog, const int *deg) const
{
  ring_elem a = R->random(homog,deg);
  ring_elem b = R->random(homog,deg);
  if (R->is_zero(b))
    {
      R->remove(b);
      b = R->from_int(1);
    }
  return FRAC_RINGELEM(make_elem(a,b));
}

void FractionField::elem_text_out(buffer &o, const ring_elem a) const
{
  int old_one = p_one;
  int old_plus = p_plus;
  int old_parens = p_parens;

  frac_elem *f = FRAC_VAL(a);
  ring_elem one = R->from_int(1);
  int denom_one = R->is_equal(f->denom, one);
  R->remove(one);

  p_one = p_one || !denom_one;
  p_parens = old_parens || !denom_one;
  R->elem_text_out(o, f->numer);
  if (!denom_one)
    {
      o << "/";
      p_plus = 0;
      R->elem_text_out(o, f->denom);
    }

  p_parens = old_parens;
  p_one = old_one;
  p_plus = old_plus;
}

void FractionField::elem_bin_out(buffer &o, const ring_elem a) const
{
  frac_elem *f = FRAC_VAL(a);
  R->elem_bin_out(o, f->numer);
  R->elem_bin_out(o, f->denom);
}

void FractionField::write_element(object_writer &o, const ring_elem a) const
{
  frac_elem *f = FRAC_VAL(a);
  R->write_element(o, f->numer);
  R->write_element(o, f->denom);
}
void FractionField::read_element(object_reader &i, ring_elem &result) const
{
  frac_elem *f = new_frac_elem();
  R->read_element(i, f->numer);
  R->read_element(i, f->denom);
  result = FRAC_RINGELEM(f);
}

ring_elem FractionField::from_int(int n) const
{
  frac_elem *f = new_frac_elem();
  f->numer = R->from_int(n);
  f->denom = R->from_int(1);
  return FRAC_RINGELEM(f);
}

ring_elem FractionField::from_int(mpz_ptr n) const
{
  frac_elem *f = new_frac_elem();
  f->numer = R->from_int(n);
  f->denom = R->from_int(1);
  return FRAC_RINGELEM(f);
}

ring_elem FractionField::var(int v, int n) const
{
  frac_elem *f = new_frac_elem();
  if (n >= 0)
    {
      f->numer = R->var(v,n);
      f->denom = R->from_int(1);
    }
  else if (n < 0)
    {
      f->numer = R->from_int(1);
      f->denom = R->var(v,-n);
    }
    
  return FRAC_RINGELEM(f);
}

bool FractionField::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
{
  // Rf = R ---> frac R
  if (Rf == R)
    {
      frac_elem *g = new_frac_elem();
      g->numer = R->copy(f);
      g->denom = R->from_int(1);
      result = FRAC_RINGELEM(g);
      return true;
    }

  return false;
}

bool FractionField::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
{
  // Rg = R ---> frac R
  // f is an element of frac R.

  if (Rg == R)
    {
      frac_elem *h = FRAC_VAL(f);
      if (R->is_unit(h->denom)) // In this case, by 'simplify', f->denom == 1.
	{
	  result = R->copy(h->numer);
	  return true;
	}
    }
  return false;
}

bool FractionField::is_unit(const ring_elem f) const
{
  return (!R->is_zero(FRAC_VAL(f)->numer));
}

bool FractionField::is_zero(const ring_elem f) const
{
  return (R->is_zero(FRAC_VAL(f)->numer));
}

bool FractionField::is_equal(const ring_elem a, const ring_elem b) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = FRAC_VAL(b);
  if (R->is_equal(f->denom, g->denom))
    {
      return R->is_equal(f->numer, g->numer);
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
  g->numer = R->copy(f->numer);
  g->denom = R->copy(f->denom);
  return FRAC_RINGELEM(g);
}

void FractionField::remove(ring_elem &a) const
{
  frac_elem *f = FRAC_VAL(a);
  if (f == NULL) return;
  R->remove(f->numer);
  R->remove(f->denom);
  frac_stash->delete_elem(f);
  a = FRAC_RINGELEM(NULL);
}

void FractionField::negate_to(ring_elem &a) const
{
  frac_elem *f = FRAC_VAL(a);
  R->negate_to(f->numer);
}

void FractionField::add_to(ring_elem &a, ring_elem &b) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = FRAC_VAL(b);
  if (R->is_equal(f->denom, g->denom))
    R->add_to(f->numer, g->numer);
  else
    {
      R->mult_to(f->numer, g->denom);
      ring_elem tmp = R->mult(f->denom, g->numer);
      R->add_to(f->numer, tmp);
      R->mult_to(f->denom, g->denom);
    }
  simplify(f);
  remove(b);
  a = FRAC_RINGELEM(f);
}

void FractionField::subtract_to(ring_elem &a, ring_elem &b) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = FRAC_VAL(b);
  if (R->is_equal(f->denom, g->denom))
    R->subtract_to(f->numer, g->numer);
  else
    {
      R->mult_to(f->numer, g->denom);
      ring_elem tmp = R->mult(f->denom, g->numer);
      R->subtract_to(f->numer, tmp);
      R->mult_to(f->denom, g->denom);
    }
  simplify(f);
  remove(b);
  a = FRAC_RINGELEM(f);
}

ring_elem FractionField::negate(const ring_elem a) const
{
  const frac_elem *f = FRAC_VAL(a);
  frac_elem *result = new_frac_elem();
  result->numer = R->negate(f->numer);
  result->denom = R->copy(f->denom);
  return FRAC_RINGELEM(result);
}

ring_elem FractionField::add(const ring_elem a, const ring_elem b) const
{
  const frac_elem *f = FRAC_VAL(a);
  const frac_elem *g = FRAC_VAL(b);
  ring_elem top, bottom;

  if (R->is_equal(f->denom, g->denom))
    {
      top = R->add(f->numer, g->numer);
      bottom = R->copy(f->denom);
    }
  else
    {
      top = R->mult(f->numer, g->denom);
      ring_elem tmp = R->mult(f->denom, g->numer);
      R->add_to(top, tmp);
      bottom = R->mult(f->denom, g->denom);
    }
  frac_elem *result = make_elem(top, bottom);
  return FRAC_RINGELEM(result);
}

ring_elem FractionField::subtract(const ring_elem a, const ring_elem b) const
{
  const frac_elem *f = FRAC_VAL(a);
  const frac_elem *g = FRAC_VAL(b);
  ring_elem top, bottom;

  if (R->is_equal(f->denom, g->denom))
    {
      top = R->subtract(f->numer, g->numer);
      bottom = R->copy(f->denom);
    }
  else
    {
      top = R->mult(f->numer, g->denom);
      ring_elem tmp = R->mult(f->denom, g->numer);
      R->subtract_to(top, tmp);
      bottom = R->mult(f->denom, g->denom);
    }
  frac_elem *result = make_elem(top, bottom);
  return FRAC_RINGELEM(result);
}

ring_elem FractionField::mult(const ring_elem a, const ring_elem b) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = FRAC_VAL(b);
  ring_elem top = R->mult(f->numer, g->numer);
  ring_elem bottom = R->mult(f->denom, g->denom);
  return FRAC_RINGELEM(make_elem(top, bottom));
}

ring_elem FractionField::power(const ring_elem a, int n) const
{
  frac_elem *f = FRAC_VAL(a);
  ring_elem top, bottom;
  if (n >= 0)
    {
      top = R->power(f->numer, n);
      bottom = R->power(f->denom, n);
    }
  else
    {
      top = R->power(f->denom, -n);
      bottom = R->power(f->numer, -n);
      if (R->is_zero(bottom))
	{
	  gError << "attempt to divide by zero";
	  R->remove(bottom);
	  bottom = R->from_int(1);
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
      top = R->power(f->numer, n);
      bottom = R->power(f->denom, n);
    }
  else
    {
      mpz_neg(n, n);
      top = R->power(f->denom, n);
      bottom = R->power(f->numer, n);
      mpz_neg(n, n);
      if (R->is_zero(bottom))
	{
	  gError << "attempt to divide by zero";
	  R->remove(bottom);
	  bottom = R->from_int(1);
	}
    }
  return FRAC_RINGELEM(make_elem(top, bottom));
}

ring_elem FractionField::invert(const ring_elem a) const
{
  frac_elem *f = FRAC_VAL(a);
  ring_elem top = R->copy(f->denom);
  ring_elem bottom = R->copy(f->numer);
  return FRAC_RINGELEM(make_elem(top, bottom));
}

ring_elem FractionField::divide(const ring_elem a, const ring_elem b) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = FRAC_VAL(b);
  ring_elem top = R->mult(f->numer, g->denom);
  ring_elem bottom = R->mult(f->denom, g->numer);
  return FRAC_RINGELEM(make_elem(top, bottom));
}

ring_elem FractionField::divide(const ring_elem a, const ring_elem b, ring_elem &rem) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = FRAC_VAL(b);
  ring_elem top = R->mult(f->numer, g->denom);
  ring_elem bottom = R->mult(f->denom, g->numer);
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

ring_elem FractionField::eval(const RingMap *map, const ring_elem a) const
{
  const Ring *S = map->Ring_of();
  const frac_elem *f = FRAC_VAL(a);
  ring_elem top = R->eval(map, f->numer);
  if (S->is_zero(top)) return top;
  ring_elem bottom = R->eval(map, f->denom);
  ring_elem result = S->divide(top, bottom);
  S->remove(top);
  S->remove(bottom);
  return result;
}

bool FractionField::is_homogeneous(const ring_elem a) const
{
  if (is_zero(a)) return true;
  const frac_elem *f = FRAC_VAL(a);
  if (!R->is_homogeneous(f->numer)
      || !R->is_homogeneous(f->denom))
    return false;
  return true;
}

void FractionField::degree(const ring_elem a, int *d) const
{
  const frac_elem *f = FRAC_VAL(a);
  R->degree(f->numer, d);
  int *e = degree_monoid()->make_one();
  R->degree(f->denom, e);
  degree_monoid()->divide(d, e, d);
  degree_monoid()->remove(e);
}

void FractionField::degree_weights(const ring_elem, const int *, int &lo, int &hi) const
{
  assert(0);
  // MES: what should this do?
  lo = hi = 0;
}

int FractionField::primary_degree(const ring_elem a) const
{
  const frac_elem *f = FRAC_VAL(a);
  return R->primary_degree(f->numer) - R->primary_degree(f->denom);
}

ring_elem FractionField::homogenize(const ring_elem a, int v, int deg, const int *wts) const
{
  ring_elem top, bottom;
  frac_elem *result;
  const frac_elem *f = FRAC_VAL(a);
  int d1 = R->primary_degree(f->numer);
  int d2 = R->primary_degree(f->denom);
  if (deg >= d1-d2)
    {
      top = R->homogenize(f->numer, v, deg + d2, wts);
      bottom = R->homogenize(f->denom, v, d2, wts);
      result = make_elem(top, bottom);
    }
  else
    {
      top = R->homogenize(f->numer, v, d1, wts);
      bottom = R->homogenize(f->denom, v, -deg + d1, wts);
      result = make_elem(top, bottom);
    }
  return FRAC_RINGELEM(result);
}

ring_elem FractionField::homogenize(const ring_elem a, int v, const int *wts) const
{
  const frac_elem *f = FRAC_VAL(a);
  ring_elem top = R->homogenize(f->numer, v, wts);
  ring_elem bottom = R->homogenize(f->denom, v, wts);
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
