/* Copyright 2017 Mahrud Sayrafi and Michael E. Stillman
   Mahrud Sayrafi's code in this file is in the public domain. */

#include "localring.hpp"

#include "interface/factory.h"
#include "text-io.hpp"
#include "ringmap.hpp"
#include "monoid.hpp"
#include "gbring.hpp"
#include "relem.hpp"
#include "debug.hpp"
#include "matrix.hpp"
#include "matrix-con.hpp"
#include "mutablecomplex.hpp"

LocalRing *LocalRing::create(const PolyRing *R, GBComputation *P)
{
  LocalRing *result = new LocalRing;
  result->initialize_local(R, P);
  return result;
}

bool LocalRing::initialize_local(const PolyRing *R, GBComputation *P)
{
  initialize_ring(
      R->characteristic(), R->get_degree_ring(), R->get_heft_vector());

  mRing = R;
  mPrime = P;

  oneV = from_long(1);
  zeroV = from_long(0);
  minus_oneV = from_long(-1);

  /*
  if (R->n_quotients() > 0 ||
      R->getCoefficients()
          ->cast_to_LocalRing()  // disallowed in x-relem.cpp
      ||
      R->getMonoid()->getNonTermOrderVariables()->len >
          0)  // disallowed in x-relem.cpp
    use_gcd_simplify = false;
  else
    use_gcd_simplify = true;
  */

  return true;
}

local_elem *LocalRing::make_elem(ring_elem a, ring_elem b) const
{
  local_elem *result = new_local_elem();
  result->numer = a;
  result->denom = b;
  simplify(result);
  return result;
}

local_elem *LocalRing::new_local_elem() const { return newitem(local_elem); }

bool LocalRing::is_in_prime(const ring_elem f) const
{
  MatrixConstructor mat(mRing->make_FreeModule(1), 1);
  mat.set_entry(0, 0, f);
  Matrix *M = mat.to_matrix();
  bool res = (mPrime->contains(M) == -1);
  delete M;
  return res;
}

void LocalRing::simplify(local_elem *f) const
{
  ring_elem x, y;
  if (use_gcd_simplify)
    {
      y = f->denom;
      if (mRing->is_equal(y, mRing->one())) return;
      x = f->numer;
      const RingElement *a = RingElement::make_raw(mRing, x);
      const RingElement *b = RingElement::make_raw(mRing, y);
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
      if (!mRing->is_equal(c->get_value(), mRing->one()))
        {
          f->numer = mRing->divide(f->numer, c->get_value());
          f->denom = mRing->divide(f->denom, c->get_value());
        }
      // Now, let's take the content of the denominator, and divide the
      // numerator
      // and denominator by this value.
      ring_elem ct = mRing->content(
          f->denom, f->numer);  // result is in mRing->getCoefficients()

#if 0
            o.reset();
            o << "f->numer = ";
            mRing->elem_text_out(o,f->numer);
            o << "  f->denom = ";
            mRing->elem_text_out(o,f->denom);
            o << " ass= ";
            mRing->getCoefficients()->elem_text_out(o,ct);
            o << newline;
            emit(o.str());
#endif

      if (!mRing->getCoefficients()->is_equal(ct,
                                              mRing->getCoefficients()->one()))
        {
          f->numer = mRing->divide_by_given_content(f->numer, ct);
          f->denom = mRing->divide_by_given_content(f->denom, ct);
        }
    }
  else
    {
      mRing->syzygy(f->numer, f->denom, x, y);
      if (mRing->is_zero(x))
        {
          mRing->remove(x);
          set_non_unit_frac(f->denom);
          f->numer = mRing->zero();
          f->denom = mRing->one();
          return;
        }
      mRing->negate_to(y);
      mRing->remove(f->numer);
      mRing->remove(f->denom);
      f->numer = y;
      f->denom = x;
    }
}

ring_elem LocalRing::set_non_unit_frac(ring_elem top) const
{
  // Sets the non unit to be top/1 (which flags an error)
  // flags an error
  // returns 0/1

  std::cout << "set_non_unit_frac is called!" << std::endl;

  local_elem *f = new_local_elem();
  f->numer = top;
  f->denom = mRing->one();
  set_non_unit(ring_elem(f));
  return zero();
}

Ring::CoefficientType LocalRing::coefficient_type() const
{
  const PolynomialRing *A = mRing->cast_to_PolynomialRing();
  assert(A != 0);
  const Ring *K = A->getCoefficientRing();
  if (K->coefficient_type() == COEFF_ZZ) return COEFF_QQ;
  return K->coefficient_type();
}

// TODO: extend to arbitrary multiplicative sets
bool LocalRing::is_unit(const ring_elem f) const
{
  // TODO: make sure f is a local ring element
  return (!is_in_prime(f.get_local_elem()->numer));
}

bool LocalRing::is_zero(const ring_elem f) const
{
  return (mRing->is_zero(f.get_local_elem()->numer));
}

bool LocalRing::is_equal(const ring_elem a, const ring_elem b) const
{
  const local_elem *f = a.get_local_elem();
  const local_elem *g = b.get_local_elem();
  if (mRing->is_equal(f->denom, g->denom))
    {
      return mRing->is_equal(f->numer, g->numer);
    }
  else
    {
      ring_elem h = subtract(a, b);
      bool result = is_zero(h);
      remove(h);
      return result;
    }
}

int LocalRing::compare_elems(const ring_elem a, const ring_elem b) const
{
  const local_elem *f = a.get_local_elem();
  const local_elem *g = b.get_local_elem();
  int cmp = mRing->compare_elems(f->numer, g->numer);
  if (cmp != 0) return cmp;
  return mRing->compare_elems(f->denom, g->denom);
}

ring_elem LocalRing::numerator(ring_elem f) const
{
  const local_elem *g = f.get_local_elem();
  return mRing->copy(g->numer);
}

ring_elem LocalRing::denominator(ring_elem f) const
{
  const local_elem *g = f.get_local_elem();
  return mRing->copy(g->denom);
}

ring_elem LocalRing::fraction(const ring_elem top, const ring_elem bottom) const
{
  return ring_elem(make_elem(mRing->copy(top), mRing->copy(bottom)));
}

// TODO: implement for MutableMatrix
void LocalRing::lift_up(const Ring *R, const Matrix *m, Matrix *&result) const
{
  const RingElement *a, *b, *d;
  MatrixConstructor mat(mRing->make_FreeModule(m->n_rows()), m->n_cols());
  Matrix::column_iterator i(m), end(m);
  for (int c = 0; c < m->n_cols(); c++)
    {
      // TODO: make this into a routine for vector LCM
      a = RingElement::make_raw(mRing, mRing->from_long(1));
      for (i = Matrix::column_iterator(m, c); i != end; ++i)
        {
          const local_elem * f = ((*i)->coeff).get_local_elem();
          b = RingElement::make_raw(mRing, f->denom);
          d = rawGCDRingElement(a, b, NULL, false);
#if 0 // FIXME: GCD(8,2)=1 apparently ...
          // see https://github.com/Macaulay2/M2/issues/1958
          drelem(a);
          std::cout<<" ";
          drelem(b);
          std::cout<<" ";
          drelem(d);
          std::cout<<std::endl;
#endif
          d = *b / *d;
          a = *a * *d;
        }
      for (i = Matrix::column_iterator(m, c); i != end; ++i)
        {
          const local_elem * f = ((*i)->coeff).get_local_elem();
          mat.set_entry(
              (*i)->comp,
              c,
              mRing->mult(f->numer, mRing->divide(a->get_value(), f->denom)));
        }
    }
  mat.compute_column_degrees();
  result = mat.to_matrix();
}

bool LocalRing::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
{
  // Rg = R ---> frac R
  // f is an element of frac R.

  ring_elem
      hdenom;  // used in the case when the denominator can be a unit, but not 1
               // e.g. when this = frac (QQ[x,y,z]).  Is an element of
  if (Rg == mRing)
    {
      const local_elem *h = f.get_local_elem();
      if (mRing->is_equal(h->denom, mRing->one()))
        {
          result = mRing->copy(h->numer);
          return true;
        }
      else
        {
          if (mRing->is_field())
            {
              // XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
              // try to lift denominator.  If can, can lift, otherwise not.
              if (mRing->lift(mRing, h->denom, hdenom))
                {
                  ring_elem hinv = mRing->invert(hdenom);
                  result = mRing->mult(hinv, h->numer);
                  return true;
                }
            }
        }
    }
  return false;
}

bool LocalRing::promote(const Ring *Rf,
                        const ring_elem f,
                        ring_elem &result) const
{
  // Rf = R ---> frac R
  if (Rf == mRing)
    {
      local_elem *g = new_local_elem();
      g->numer = mRing->copy(f);
      g->denom = mRing->from_long(1);
      result = ring_elem(g);
      return true;
    }

  return false;
}

bool LocalRing::from_rational(mpq_srcptr n, ring_elem &result) const
{
  local_elem *f = new_local_elem();
  f->numer = mRing->from_int(mpq_numref(n));
  f->denom = mRing->from_int(mpq_denref(n));
  bool ok = not mRing->is_zero(f->denom);
  if (ok) result = ring_elem(f);
  return ok;
}

ring_elem LocalRing::from_long(long n) const
{
  local_elem *f = new_local_elem();
  f->numer = mRing->from_long(n);
  f->denom = mRing->from_long(1);
  return ring_elem(f);
}

ring_elem LocalRing::from_int(mpz_srcptr n) const
{
  local_elem *f = new_local_elem();
  f->numer = mRing->from_int(n);
  f->denom = mRing->from_long(1);
  return ring_elem(f);
}

ring_elem LocalRing::var(int v) const
{
  local_elem *f = new_local_elem();
  f->numer = mRing->var(v);
  f->denom = mRing->from_long(1);
  return ring_elem(f);
}

int LocalRing::index_of_var(const ring_elem a) const
{
  const local_elem *f = a.get_local_elem();
  if (!mRing->is_unit(f->denom))
    // If so, a cannot be a variable, otherwise, by 'simplify', f->denom == 1.
    return -1;
  return mRing->index_of_var(f->numer);
}

M2_arrayint LocalRing::support(const ring_elem a) const
{
  const local_elem *f = a.get_local_elem();
  M2_arrayint result1 = mRing->support(f->numer);
  M2_arrayint result2 = mRing->support(f->denom);
  M2_arrayint result = M2_makearrayint(result1->len + result2->len);
  for (int i = 0; i < result1->len; i++) result->array[i] = result1->array[i];
  for (int i = 0; i < result2->len; i++)
    result->array[result1->len + i] = result2->array[i];
  return result;
}

void LocalRing::lower_content(ring_elem &c, const ring_elem g) const
{
  if (!use_gcd_simplify) return;
  if (is_zero(c))
    {
      c = g;
      return;
    }

  const local_elem *cf = c.get_local_elem();
  const local_elem *gf = g.get_local_elem();
  const RingElement *c1 = RingElement::make_raw(mRing, cf->numer);
  const RingElement *c2 = RingElement::make_raw(mRing, cf->denom);
  const RingElement *g1 = RingElement::make_raw(mRing, gf->numer);
  const RingElement *g2 = RingElement::make_raw(mRing, gf->denom);

  c1 = rawGCDRingElement(c1, g1, NULL, false);

  const RingElement *cc2 = rawGCDRingElement(c2, g2, NULL, false);
  const RingElement *cc3 = (*c2) * (*g2);
  const RingElement *cc4 = (*cc3) / (*cc2);

  local_elem *result = new_local_elem();
  result->numer = c1->get_value();
  result->denom = cc4->get_value();

  c = ring_elem(result);
}

bool LocalRing::is_homogeneous(const ring_elem a) const
{
  if (is_zero(a)) return true;
  const local_elem *f = a.get_local_elem();
  if (!mRing->is_homogeneous(f->numer) || !mRing->is_homogeneous(f->denom))
    return false;
  return true;
}

void LocalRing::degree(const ring_elem a, int *d) const
{
  const local_elem *f = a.get_local_elem();
  mRing->degree(f->numer, d);
  int *e = degree_monoid()->make_one();
  mRing->degree(f->denom, e);
  degree_monoid()->divide(d, e, d);
  degree_monoid()->remove(e);
}

bool LocalRing::multi_degree(const ring_elem a, int *d) const
{
  const local_elem *f = a.get_local_elem();
  bool tophom = mRing->multi_degree(f->numer, d);
  int *e = degree_monoid()->make_one();
  bool bottomhom = mRing->multi_degree(f->denom, e);
  degree_monoid()->divide(d, e, d);
  degree_monoid()->remove(e);
  return tophom && bottomhom;
}

void LocalRing::degree_weights(const ring_elem,
                               M2_arrayint,
                               int &lo,
                               int &hi) const
{
  assert(0);
  // MES: what should this do?
  lo = hi = 0;
}

ring_elem LocalRing::homogenize(const ring_elem a,
                                int v,
                                int deg,
                                M2_arrayint wts) const
{
  int d1, d2, lo1, lo2;
  ring_elem top, bottom;
  const local_elem *f = a.get_local_elem();
  mRing->degree_weights(f->numer, wts, lo1, d1);
  mRing->degree_weights(f->denom, wts, lo2, d2);
  if (deg >= d1 - d2)
    {
      top = mRing->homogenize(f->numer, v, deg + d2, wts);
      bottom = mRing->homogenize(f->denom, v, d2, wts);
    }
  else
    {
      top = mRing->homogenize(f->numer, v, d1, wts);
      bottom = mRing->homogenize(f->denom, v, -deg + d1, wts);
    }
  local_elem *result = make_elem(top, bottom);
  return ring_elem(result);
}

ring_elem LocalRing::homogenize(const ring_elem a, int v, M2_arrayint wts) const
{
  const local_elem *f = a.get_local_elem();
  ring_elem top = mRing->homogenize(f->numer, v, wts);
  ring_elem bottom = mRing->homogenize(f->denom, v, wts);
  local_elem *result = make_elem(top, bottom);
  return ring_elem(result);
}

ring_elem LocalRing::copy(const ring_elem a) const
{
  const local_elem *f = a.get_local_elem();
  local_elem *g = new_local_elem();
  g->numer = mRing->copy(f->numer);
  g->denom = mRing->copy(f->denom);
  return ring_elem(g);
}

void LocalRing::remove(ring_elem &a) const {}

ring_elem LocalRing::negate(const ring_elem a) const
{
  const local_elem *f = a.get_local_elem();
  local_elem *result = new_local_elem();
  result->numer = mRing->negate(f->numer);
  result->denom = mRing->copy(f->denom);
  return ring_elem(result);
}

ring_elem LocalRing::add(const ring_elem a, const ring_elem b) const
{
  const local_elem *f = a.get_local_elem();
  const local_elem *g = b.get_local_elem();
  ring_elem top, bottom;

  if (mRing->is_equal(f->denom, g->denom))
    {
      top = mRing->add(f->numer, g->numer);
      bottom = mRing->copy(f->denom);
    }
  else
    {
      top = mRing->mult(f->numer, g->denom);
      ring_elem tmp = mRing->mult(f->denom, g->numer);
      mRing->add_to(top, tmp);
      bottom = mRing->mult(f->denom, g->denom);
      if (mRing->is_zero(bottom)) return set_non_unit_frac(f->denom);
    }
  local_elem *result = make_elem(top, bottom);
  return ring_elem(result);
}

ring_elem LocalRing::subtract(const ring_elem a, const ring_elem b) const
{
  const local_elem *f = a.get_local_elem();
  const local_elem *g = b.get_local_elem();
  ring_elem top, bottom;

  if (mRing->is_equal(f->denom, g->denom))
    {
      top = mRing->subtract(f->numer, g->numer);
      bottom = mRing->copy(f->denom);
    }
  else
    {
      top = mRing->mult(f->numer, g->denom);
      ring_elem tmp = mRing->mult(f->denom, g->numer);
      mRing->subtract_to(top, tmp);
      bottom = mRing->mult(f->denom, g->denom);
      if (mRing->is_zero(bottom)) return set_non_unit_frac(f->denom);
    }
  local_elem *result = make_elem(top, bottom);
  return ring_elem(result);
}

ring_elem LocalRing::mult(const ring_elem a, const ring_elem b) const
{
  const local_elem *f = a.get_local_elem();
  const local_elem *g = b.get_local_elem();
  ring_elem top = mRing->mult(f->numer, g->numer);
  ring_elem bottom = mRing->mult(f->denom, g->denom);
  if (mRing->is_zero(bottom)) return set_non_unit_frac(f->denom);
  return ring_elem(make_elem(top, bottom));
}

ring_elem LocalRing::power(const ring_elem a, int n) const
{
  const local_elem *f = a.get_local_elem();
  ring_elem top, bottom;
  if (n >= 0)
    {
      top = mRing->power(f->numer, n);
      bottom = mRing->power(f->denom, n);

      if (mRing->is_zero(bottom)) return set_non_unit_frac(f->denom);
    }
  else
    {
      if (is_unit(a))
        {
          top = mRing->power(f->denom, -n);
          bottom = mRing->power(f->numer, -n);
        }
      else
        {
          ERROR("attempt to divide by non-unit");
          return zero();
        }

      if (mRing->is_zero(bottom)) return set_non_unit_frac(f->numer);
    }
  return ring_elem(make_elem(top, bottom));
}
ring_elem LocalRing::power(const ring_elem a, mpz_srcptr n) const
{
  const local_elem *f = a.get_local_elem();
  ring_elem top, bottom;
  if (mpz_sgn(n) >= 0)
    {
      top = mRing->power(f->numer, n);
      bottom = mRing->power(f->denom, n);

      if (mRing->is_zero(bottom)) return set_non_unit_frac(f->denom);
    }
  else
    {
      mpz_t negative_n;
      mpz_init(negative_n);
      mpz_neg(negative_n, n);
      if (is_unit(a))
        {
          top = mRing->power(f->denom, negative_n);
          bottom = mRing->power(f->numer, negative_n);
        }
      else
        {
          ERROR("attempt to divide by non-unit");
          mpz_clear(negative_n);
          return zero();
        }
      mpz_clear(negative_n);

      if (mRing->is_zero(bottom)) return set_non_unit_frac(f->numer);
    }
  return ring_elem(make_elem(top, bottom));
}

ring_elem LocalRing::invert(const ring_elem a) const
{
  const local_elem *f = a.get_local_elem();
  if (mRing->is_zero(f->numer) || !is_unit(a))
    {
      ERROR("attempt to invert a non-unit");
      return zero();
    }
  ring_elem top = mRing->copy(f->denom);
  ring_elem bottom = mRing->copy(f->numer);
  return ring_elem(make_elem(top, bottom));
}

ring_elem LocalRing::divide(const ring_elem a, const ring_elem b) const
{
  const local_elem *f = a.get_local_elem();
  const local_elem *g = b.get_local_elem();
  ring_elem top, bottom;
  if (is_unit(b))
    {
      top = mRing->mult(f->numer, g->denom);
      bottom = mRing->mult(f->denom, g->numer);
    }
  else
    {
      ERROR("attempt to divide by non-unit");
      return zero();
    }
  return ring_elem(make_elem(top, bottom));
}

void LocalRing::syzygy(const ring_elem a,
                       const ring_elem b,
                       ring_elem &x,
                       ring_elem &y) const
{
  x = LocalRing::from_long(1);
  y = LocalRing::divide(a, b);
  y = LocalRing::negate(y);
}

ring_elem LocalRing::random() const
{
  ring_elem a = mRing->random();
  ring_elem b = mRing->random();
  if (mRing->is_zero(b))
    {
      mRing->remove(b);
      b = mRing->from_long(1);
    }
  return ring_elem(make_elem(a, b));
}

ring_elem LocalRing::eval(const RingMap *map,
                          const ring_elem a,
                          int first_var) const
{
  ring_elem top, bottom, result;
  const Ring *S = map->get_ring();
  const local_elem *f = a.get_local_elem();
  top = mRing->eval(map, f->numer, first_var);
  if (S->is_zero(top)) return top;
  bottom = mRing->eval(map, f->denom, first_var);
  if (S->is_unit(bottom))
    result = S->divide(top, bottom);
  else
    {
      if (not error())  // FIXME: keep this?
        ERROR("attempt to divide by non-unit");
      result = S->from_long(0);
    }
  S->remove(top);
  S->remove(bottom);
  return result;
}

int LocalRing::n_fraction_vars() const { return mRing->n_vars(); }
int LocalRing::n_terms(const ring_elem a) const
{
  return mRing->n_terms(a.get_local_elem()->numer);
}
ring_elem LocalRing::term(const ring_elem a, const int *) const
{
  return copy(a);
}
ring_elem LocalRing::lead_coeff(const ring_elem f) const { return f; }
ring_elem LocalRing::get_coeff(const ring_elem f, const int *) const
{
  return f;
}
ring_elem LocalRing::get_terms(int nvars0, const ring_elem f, int, int) const
{
  return f;
}

void LocalRing::text_out(buffer &o) const
{
  o << "LocalRing(";
  mRing->text_out(o);
  o << ", Prime ideal => ";
  mPrime->get_mingens()->text_out(o);
  o << ")";
}

void LocalRing::elem_text_out(buffer &o,
                              const ring_elem a,
                              bool p_one,
                              bool p_plus,
                              bool p_parens) const
{
  const local_elem *f = a.get_local_elem();
  int denom_one = mRing->is_equal(f->denom, mRing->one());

  p_one = p_one || !denom_one;
  p_parens = p_parens || !denom_one;
  mRing->elem_text_out(o, f->numer, p_one, p_plus, p_parens);
  if (!denom_one)
    {
      o << "/";
      p_plus = false;
      mRing->elem_text_out(o, f->denom, p_one, p_plus, p_parens);
    }
}

unsigned int LocalRing::computeHashValue(const ring_elem f) const
{
  const local_elem *g = f.get_local_elem();
  return (16473 * mRing->computeHashValue(g->numer) +
          7698908 * mRing->computeHashValue(g->denom));
}

/********************************************************************************/
/*                               Global functions */
/********************************************************************************/

extern "C" { // TODO: remove when this function is in e/interface

Matrix *rawLiftLocalMatrix(const Ring *R, const Matrix *f)
{
  const LocalRing *L = f->get_ring()->cast_to_LocalRing();
  if (L == 0)
    {
      ERROR("expected an object over a local ring");
      return nullptr;
    }
  // TODO: Check that f is over a localization of R
  if (R != L->get_ring())
    {
      ERROR("expected an object over a localization of the first argument");
      return nullptr;
    }
  Matrix *result;
  L->lift_up(R, f, result);
  return result;
}

M2_bool rawIsLocalUnit(const RingElement *f)
{
  const LocalRing *L = f->get_ring()->cast_to_LocalRing();
  if (L == 0)
    {
      ERROR("expected an object over a local ring");
      return false;
    }
  return L->is_unit(f->get_value());
}

} // TODO: remove when this function is in e/interface

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
