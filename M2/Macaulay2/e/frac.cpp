// Copyright 1995 Michael E. Stillman

#include "frac.hpp"

#include "interface/factory.h"
#include "text-io.hpp"
#include "monoid.hpp"
#include "ringmap.hpp"
#include "gbring.hpp"
#include "relem.hpp"
#include "polyring.hpp"
#include "exceptions.hpp"

#define FRAC_VAL(f) (reinterpret_cast<frac_elem *>((f).poly_val))
#define FRAC_RINGELEM(a) (ring_elem(reinterpret_cast<Nterm *>(a)))

Ring::CoefficientType FractionField::coefficient_type() const
{
  const PolynomialRing *A = R_->cast_to_PolynomialRing();
  assert(A != 0);
  const Ring *K = A->getCoefficientRing();
  if (K->coefficient_type() == COEFF_ZZ) return COEFF_QQ;
  return K->coefficient_type();
}

int FractionField::n_fraction_vars() const { return R_->n_vars(); }
bool FractionField::initialize_frac(const PolyRingFlat *R)
{
  initialize_ring(
      R->characteristic(), R->get_degree_ring(), R->get_heft_vector());

  R_ = R;

  zeroV = from_long(0);
  oneV = from_long(1);
  minus_oneV = from_long(-1);

  if (R->n_quotients() > 0 ||
      R->getCoefficients()
          ->cast_to_FractionField()  // disallowed in x-relem.cpp
      ||
      R->getMonoid()->getNonTermOrderVariables()->len >
          0)  // disallowed in x-relem.cpp
    use_gcd_simplify = false;
  else
    use_gcd_simplify = true;
#ifdef DEVELOPMENT
#warning "frac simplify: doesn't handle towers of fracs"
#endif

  declare_field();
  return true;
}

FractionField *FractionField::create(const PolyRingFlat *R)
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

unsigned int FractionField::computeHashValue(const ring_elem f) const
{
  frac_elem *g = FRAC_VAL(f);
  return (16473 * R_->computeHashValue(g->numer) +
          7698908 * R_->computeHashValue(g->denom));
}

frac_elem *FractionField::new_frac_elem() const { return newitem(frac_elem); }
ring_elem FractionField::set_non_unit_frac(ring_elem top) const
{
  // Sets the non unit to be top/1 (which flags an error)
  // flags an error
  // returns 0/1

  frac_elem *f = new_frac_elem();
  f->numer = top;
  f->denom = R_->one();
  set_non_unit(FRAC_RINGELEM(f));
  return zero();
}

ring_elem FractionField::fraction(const ring_elem top,
                                  const ring_elem bottom) const
{
  return FRAC_RINGELEM(make_elem(R_->copy(top), R_->copy(bottom)));
}

void FractionField::simplify(frac_elem *f) const
{
  ring_elem x, y;
  if (use_gcd_simplify)
    {
      y = f->denom;
      if (R_->is_equal(y, R_->one())) return;
      x = f->numer;
      const RingElement *a = RingElement::make_raw(R_, x);
      const RingElement *b = RingElement::make_raw(R_, y);
      const RingElement *c = rawGCDRingElement(a, b, NULL, false);

#if 0
      // Debugging code
            buffer o;
            o << newline;
            o << "a = ";
            a->text_out(o);
            o << "  b = ";
            b->text_out(o);
            o << " gcd = ";
            c->text_out(o);
            o << newline;
            emit(o.str());
#endif
      if (!R_->is_equal(c->get_value(), R_->one()))
        {
          f->numer = R_->divide(f->numer, c->get_value());
          f->denom = R_->divide(f->denom, c->get_value());
        }
      // Now, let's take the content of the denominator, and divide the
      // numerator
      // and denominator by this value.
      ring_elem ct = R_->content(
          f->denom, f->numer);  // result is in R_->getCoefficients()

#if 0
            o.reset();
            o << "f->numer = ";
            R_->elem_text_out(o,f->numer);
            o << "  f->denom = ";
            R_->elem_text_out(o,f->denom);
            o << " ass= ";
            R_->getCoefficients()->elem_text_out(o,ct);
            o << newline;
            emit(o.str());
#endif

      if (!R_->getCoefficients()->is_equal(ct, R_->getCoefficients()->one()))
        {
          f->numer = R_->divide_by_given_content(f->numer, ct);
          f->denom = R_->divide_by_given_content(f->denom, ct);
        }
    }
  else
    {
      R_->syzygy(f->numer, f->denom, x, y);
      if (R_->is_zero(x))
        {
          R_->remove(x);
          set_non_unit_frac(f->denom);
          f->numer = R_->zero();
          f->denom = R_->one();
          return;
        }
      R_->negate_to(y);
      R_->remove(f->numer);
      R_->remove(f->denom);
      f->numer = y;
      f->denom = x;
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

void FractionField::lower_content(ring_elem &c, const ring_elem g) const
{
  if (!use_gcd_simplify) return;
  if (is_zero(c))
    {
      c = g;
      return;
    }

  frac_elem *cf = FRAC_VAL(c);
  frac_elem *gf = FRAC_VAL(g);
  const RingElement *c1 = RingElement::make_raw(R_, cf->numer);
  const RingElement *c2 = RingElement::make_raw(R_, cf->denom);
  const RingElement *g1 = RingElement::make_raw(R_, gf->numer);
  const RingElement *g2 = RingElement::make_raw(R_, gf->denom);

  c1 = rawGCDRingElement(c1, g1, NULL, false);

  const RingElement *cc2 = rawGCDRingElement(c2, g2, NULL, false);
  const RingElement *cc3 = (*c2) * (*g2);
  const RingElement *cc4 = (*cc3) / (*cc2);

  frac_elem *result = new_frac_elem();
  result->numer = c1->get_value();
  result->denom = cc4->get_value();

  c = FRAC_RINGELEM(result);
}

ring_elem FractionField::random() const
{
  ring_elem a = R_->random();
  ring_elem b = R_->random();
  if (R_->is_zero(b))
    {
      R_->remove(b);
      b = R_->from_long(1);
    }
  return FRAC_RINGELEM(make_elem(a, b));
}

void FractionField::elem_text_out(buffer &o,
                                  const ring_elem a,
                                  bool p_one,
                                  bool p_plus,
                                  bool p_parens) const
{
  frac_elem *f = FRAC_VAL(a);
  int denom_one = R_->is_equal(f->denom, R_->one());

  p_one = p_one || !denom_one;
  p_parens = p_parens || !denom_one;
  R_->elem_text_out(o, f->numer, p_one, p_plus, p_parens);
  if (!denom_one)
    {
      o << "/";
      p_plus = false;
      R_->elem_text_out(o, f->denom, p_one, p_plus, p_parens);
    }
}

ring_elem FractionField::from_long(long n) const
{
  frac_elem *f = new_frac_elem();
  f->numer = R_->from_long(n);
  f->denom = R_->from_long(1);
  return FRAC_RINGELEM(f);
}

ring_elem FractionField::from_int(mpz_srcptr n) const
{
  frac_elem *f = new_frac_elem();
  f->numer = R_->from_int(n);
  f->denom = R_->from_long(1);
  return FRAC_RINGELEM(f);
}

bool FractionField::from_rational(mpq_srcptr n, ring_elem &result) const
{
  frac_elem *f = new_frac_elem();
  f->numer = R_->from_int(mpq_numref(n));
  f->denom = R_->from_int(mpq_denref(n));
  bool ok = not R_->is_zero(f->denom);
  if (ok) result = FRAC_RINGELEM(f);
  return ok;
}

ring_elem FractionField::var(int v) const
{
  frac_elem *f = new_frac_elem();
  f->numer = R_->var(v);
  f->denom = R_->from_long(1);
  return FRAC_RINGELEM(f);
}

int FractionField::index_of_var(const ring_elem a) const
{
  const frac_elem *f = FRAC_VAL(a);
  if (!R_->is_unit(f->denom))
    // If so, a cannot be a variable, otherwise, by 'simplify', f->denom == 1.
    return -1;
  return R_->index_of_var(f->numer);
}

M2_arrayint FractionField::support(const ring_elem a) const
{
  const frac_elem *f = FRAC_VAL(a);
  M2_arrayint result1 = R_->support(f->numer);
  M2_arrayint result2 = R_->support(f->denom);
  M2_arrayint result = M2_makearrayint(result1->len + result2->len);
  for (int i = 0; i < result1->len; i++) result->array[i] = result1->array[i];
  for (int i = 0; i < result2->len; i++)
    result->array[result1->len + i] = result2->array[i];

  return result;
}

bool FractionField::promote(const Ring *Rf,
                            const ring_elem f,
                            ring_elem &result) const
{
  // Rf = R ---> frac R
  if (Rf == R_)
    {
      frac_elem *g = new_frac_elem();
      g->numer = R_->copy(f);
      g->denom = R_->from_long(1);
      result = FRAC_RINGELEM(g);
      return true;
    }

  return false;
}

bool FractionField::lift(const Ring *Rg,
                         const ring_elem f,
                         ring_elem &result) const
{
  // Rg = R ---> frac R
  // f is an element of frac R.

  ring_elem
      hdenom;  // used in the case when the denominator can be a unit, but not 1
               // e.g. when this = frac (QQ[x,y,z]).  Is an element of
  if (Rg == R_)
    {
      frac_elem *h = FRAC_VAL(f);
      if (R_->is_equal(h->denom, R_->one()))
        {
          result = R_->copy(h->numer);
          return true;
        }
      else
        {
          if (R_->is_field())
            {
              // XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
              // try to lift denominator.  If can, can lift, otherwise not.
              if (R_->lift(R_, h->denom, hdenom))
                {
                  ring_elem hinv = R_->invert(hdenom);
                  result = R_->mult(hinv, h->numer);
                  return true;
                }
            }
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
      ring_elem h = subtract(a, b);
      bool result = is_zero(h);
      remove(h);
      return result;
    }
}

int FractionField::compare_elems(const ring_elem a, const ring_elem b) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = FRAC_VAL(b);
  int cmp = R_->compare_elems(f->numer, g->numer);
  if (cmp != 0) return cmp;
  return R_->compare_elems(f->denom, g->denom);
}

ring_elem FractionField::copy(const ring_elem a) const
{
  frac_elem *f = FRAC_VAL(a);
  frac_elem *g = new_frac_elem();
  g->numer = R_->copy(f->numer);
  g->denom = R_->copy(f->denom);
  return FRAC_RINGELEM(g);
}

void FractionField::remove(ring_elem &a) const {}
void FractionField::internal_negate_to(ring_elem &a) const
{
  frac_elem *f = FRAC_VAL(a);
  R_->negate_to(f->numer);
}

void FractionField::internal_add_to(ring_elem &a, ring_elem &b) const
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
      if (R_->is_zero(f->denom))
        {
          set_non_unit_frac(g->denom);
          return;
        }
    }
  simplify(f);
  remove(b);
  a = FRAC_RINGELEM(f);
}

void FractionField::internal_subtract_to(ring_elem &a, ring_elem &b) const
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
      if (R_->is_zero(f->denom))
        {
          set_non_unit_frac(g->denom);
          return;
        }
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
      if (R_->is_zero(bottom)) return set_non_unit_frac(f->denom);
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
      if (R_->is_zero(bottom)) return set_non_unit_frac(f->denom);
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
  if (R_->is_zero(bottom)) return set_non_unit_frac(f->denom);
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

      if (R_->is_zero(bottom)) return set_non_unit_frac(f->denom);
    }
  else
    {
      if (R_->is_zero(f->numer))
        {
          throw exc::division_by_zero_error();
        }
      top = R_->power(f->denom, -n);
      bottom = R_->power(f->numer, -n);

      if (R_->is_zero(bottom)) return set_non_unit_frac(f->numer);
    }

  return FRAC_RINGELEM(make_elem(top, bottom));
}
ring_elem FractionField::power(const ring_elem a, mpz_srcptr n) const
{
  frac_elem *f = FRAC_VAL(a);
  ring_elem top, bottom;
  if (mpz_sgn(n) >= 0)
    {
      top = R_->power(f->numer, n);
      bottom = R_->power(f->denom, n);

      if (R_->is_zero(bottom)) return set_non_unit_frac(f->denom);
    }
  else
    {
      if (R_->is_zero(f->numer))
        {
          throw exc::division_by_zero_error();
        }
      mpz_t abs_n;
      mpz_init(abs_n);
      mpz_abs(abs_n, n);

      top = R_->power(f->denom, abs_n);
      bottom = R_->power(f->numer, abs_n);

      mpz_clear(abs_n);
      
      if (R_->is_zero(bottom)) return set_non_unit_frac(f->numer);
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

  if (R_->is_zero(bottom)) return set_non_unit_frac(f->denom);

  return FRAC_RINGELEM(make_elem(top, bottom));
}

void FractionField::syzygy(const ring_elem a,
                           const ring_elem b,
                           ring_elem &x,
                           ring_elem &y) const
{
  x = FractionField::from_long(1);
  y = FractionField::divide(a, b);
  y = FractionField::negate(y);
}

ring_elem FractionField::eval(const RingMap *map,
                              const ring_elem a,
                              int first_var) const
{
  const Ring *S = map->get_ring();
  const frac_elem *f = FRAC_VAL(a);
  ring_elem top = R_->eval(map, f->numer, first_var);
  if (S->is_zero(top)) return top;
  ring_elem bottom = R_->eval(map, f->denom, first_var);
  if (S->is_zero(bottom))
    {
      throw exc::division_by_zero_error();
      S->remove(bottom);
      top = S->from_long(0);
      bottom = S->from_long(1);
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
  if (!R_->is_homogeneous(f->numer) || !R_->is_homogeneous(f->denom))
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

void FractionField::degree_weights(const ring_elem,
                                   M2_arrayint,
                                   int &lo,
                                   int &hi) const
{
  assert(0);
  // MES: what should this do?
  lo = hi = 0;
}

ring_elem FractionField::homogenize(const ring_elem a,
                                    int v,
                                    int deg,
                                    M2_arrayint wts) const
{
  int d1, d2, lo1, lo2;
  ring_elem top, bottom;
  frac_elem *result;
  const frac_elem *f = FRAC_VAL(a);
  R_->degree_weights(f->numer, wts, lo1, d1);
  R_->degree_weights(f->denom, wts, lo2, d2);
  if (deg >= d1 - d2)
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

ring_elem FractionField::homogenize(const ring_elem a,
                                    int v,
                                    M2_arrayint wts) const
{
  const frac_elem *f = FRAC_VAL(a);
  ring_elem top = R_->homogenize(f->numer, v, wts);
  ring_elem bottom = R_->homogenize(f->denom, v, wts);
  frac_elem *result = make_elem(top, bottom);
  return FRAC_RINGELEM(result);
}

int FractionField::n_terms(const ring_elem) const { return 1; }
ring_elem FractionField::term(const ring_elem a, const int *) const
{
  return copy(a);
}
ring_elem FractionField::lead_coeff(const ring_elem f) const { return f; }
ring_elem FractionField::get_coeff(const ring_elem f, const int *) const
{
  return f;
}
ring_elem FractionField::get_terms(int nvars0,
                                   const ring_elem f,
                                   int,
                                   int) const
{
  return f;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
