// Copyright 1996-2017 Michael E. Stillman

#include "schur2.hpp"
#include <stdio.h>
#include <iostream>
#include "text-io.hpp"
#include "ZZ.hpp"
#include "relem.hpp"
#include "monomial.hpp"
#include "ringmap.hpp"
#include "monoid.hpp"

const int SCHUR_MAX_WT = 100;
const int LARGE_NUMBER = 32000;

void tableau2::initialize(int nvars, int maxwt0)
{
  maxwt = SCHUR_MAX_WT;
  wt = 0;
  lambda = nullptr;
  p = nullptr;
  xloc = newarray_atomic(int, SCHUR_MAX_WT + 1);
  yloc = newarray_atomic(int, SCHUR_MAX_WT + 1);
}

void tableau2::resize(int max_wt)
{
  if (max_wt <= SCHUR_MAX_WT) return;
  freemem(xloc);
  freemem(yloc);
  maxwt = max_wt;
  wt = max_wt;
  xloc = newarray_atomic(int, maxwt + 1);
  yloc = newarray_atomic(int, maxwt + 1);
}

int tableau2::elem(int x, int y) const
{
  // slow: only used for debugging
  for (int i = 1; i <= wt; i++)
    if (xloc[i] == x && yloc[i] == y) return i;

  // otherwise perhaps throw an error
  fprintf(stderr, "tableau2: location (%d,%d) out of range\n", x, y);
  return 0;
}

void tableau2::fill(int *lamb, int *pp)  // FLAG: should be const, schur_word..
// Fill the skew tableau2 p\lambda with 1..nboxes
// starting at top right, moving left and then down
// row by row.
{
  int i, j;
  p = pp;         // FLAG: why is this here?
  lambda = lamb;  // FLAG: why is this here?

  int next = 1;
  for (i = 1; i < p[0]; i++)
    {
      int a = lambda[i];
      for (j = p[i]; j > a; j--)
        {
          xloc[next] = i;
          yloc[next++] = j;
        }
    }
  //  display();
}

void tableau2::display() const
{
  int i, j;

  for (i = 1; i < p[0]; i++)
    {
      for (j = 1; j <= lambda[i]; j++) fprintf(stdout, "--  ");
      for (; j <= p[i]; j++) fprintf(stdout, "%2d  ", elem(i, j));
      fprintf(stdout, "\n");
    }
}

//////////////////////////////////////////

template<typename T>
static inline void hash_combine(size_t& seed, const T& val)
{
// the typical implementation
  seed ^= std::hash<T>()(val) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

unsigned int SchurRing2::computeHashValue(const ring_elem a) const
{
//  assuming a normal form: distinct monomials in the linear order introduced by compare_partitions()

  const auto& coeffs = a.get_schur_poly()->coeffs;
  const auto& monoms = a.get_schur_poly()->monoms;

  size_t seed = 95864398;  // using previous M2's constant hash value

  for(auto it=coeffs.begin(); it!=coeffs.end(); ++it)
    hash_combine(seed, coefficientRing->computeHashValue(*it));
  for(auto it=monoms.begin(); it!=monoms.end(); ++it)
    hash_combine(seed, *it);

  return static_cast<unsigned>(seed);
}

bool operator==(const schur_poly::iterator &a, const schur_poly::iterator &b)
{
  return a.ic == b.ic;
}
bool operator!=(const schur_poly::iterator &a, const schur_poly::iterator &b)
{
  return a.ic != b.ic;
}

void schur_poly::appendTerm(ring_elem coeff, const_schur_partition monom)
{
  coeffs.push_back(coeff);
  for (int i = 0; i < monom[0]; i++) monoms.push_back(monom[i]);
}
void schur_poly::append(iterator &first, iterator &last)
{
  for (; first != last; ++first)
    appendTerm(first.getCoefficient(), first.getMonomial());
}

SchurRing2 *SchurRing2::create(const Ring *A, int n)
{
  SchurRing2 *R = new SchurRing2(A, n);
  R->initialize_SchurRing2();
  return R;
}

SchurRing2 *SchurRing2::createInfinite(const Ring *A)
{
  SchurRing2 *R = new SchurRing2(A);
  R->initialize_SchurRing2();
  return R;
}

SchurRing2::SchurRing2(const Ring *A, int n) : coefficientRing(A), nvars(n) {}
bool SchurRing2::initialize_SchurRing2()
{
  initialize_ring(coefficientRing->characteristic());

  zeroV = from_long(0);
  oneV = from_long(1);
  minus_oneV = from_long(-1);

  SMinitialize(nvars, 0);
  return true;
}

bool SchurRing2::is_valid_partition(M2_arrayint part, bool set_error) const
{
  if (nvars >= 0 && part->len > nvars)
    {
      if (set_error) ERROR("expected a partition of size at most %d\n", nvars);
      return false;
    }
  for (int i = 1; i < part->len; i++)
    if (part->array[i - 1] < part->array[i])
      {
        if (set_error) ERROR("expected a non-increasing sequence of integers");
        return false;
      }
  if (part->len > 0 && part->array[part->len - 1] < 0)
    {
      if (set_error) ERROR("expected nonnegative integers only");
      return false;
    }
  return true;
}

static int last_nonzero(M2_arrayint part)
{
  for (int i = part->len - 1; i >= 0; i--)
    if (part->array[i] != 0) return i;
  return -1;
}
ring_elem SchurRing2::from_partition(M2_arrayint part) const
{
  schur_poly *f = new schur_poly;
  f->coeffs.push_back(coefficientRing->one());
  int len = last_nonzero(part) + 1;
  f->monoms.push_back(len + 1);
  for (int i = 0; i < len; i++) f->monoms.push_back(part->array[i]);
  return ring_elem(f);
}

void SchurRing2::text_out(buffer &o) const
{
  o << "SchurRing2(";
  if (nvars >= 0) o << nvars << ",";
  coefficientRing->text_out(o);
  o << ")";
}

void SchurRing2::elem_text_out(buffer &o,
                               const ring_elem f,
                               bool p_one,
                               bool p_plus,
                               bool p_parens) const
{
  const schur_poly *g = f.get_schur_poly();
  size_t n = g->size();

  bool needs_parens = p_parens && (n > 1);
  if (needs_parens)
    {
      if (p_plus) o << '+';
      o << '(';
      p_plus = false;
    }

  p_one = false;
  for (schur_poly::iterator i = g->begin(); i != g->end(); ++i)
    {
      const_schur_partition part = i.getMonomial();
      int len = *part++;
      int isone = (len == 1);  // the empty partition
      p_parens = !isone;
      coefficientRing->elem_text_out(
          o, i.getCoefficient(), p_one, p_plus, p_parens);
      o << "{";
      for (int j = 0; j < len - 1; j++)
        {
          if (j > 0) o << ",";
          o << part[j];
        }
      o << "}";
      p_plus = true;
    }
  if (needs_parens) o << ')';
}

bool SchurRing2::is_unit(const ring_elem f) const
{
  const schur_poly *g = f.get_schur_poly();
  if (g->size() != 1) return false;
  return (g->monoms.size() == 1) && (coefficientRing->is_unit(g->coeffs[0]));
}

bool SchurRing2::is_zero(const ring_elem f) const
{
  const schur_poly *g = f.get_schur_poly();
  return g->size() == 0;
}

bool SchurRing2::is_equal(const ring_elem f, const ring_elem g) const
{
  const schur_poly *f1 = f.get_schur_poly();
  const schur_poly *g1 = g.get_schur_poly();
  if (f1->size() != g1->size()) return false;
  if (f1->monoms.size() != g1->monoms.size()) return false;

  VECTOR(schur_word)::const_iterator m_f = f1->monoms.begin();
  VECTOR(schur_word)::const_iterator m_g = g1->monoms.begin();
  for (; m_f != f1->monoms.end(); ++m_f, ++m_g)
    if (*m_f != *m_g) return false;

  VECTOR(ring_elem)::const_iterator c_f = f1->coeffs.begin();
  VECTOR(ring_elem)::const_iterator c_g = g1->coeffs.begin();
  for (; c_f != f1->coeffs.end(); ++c_f, ++c_g)
    if (!coefficientRing->is_equal(*c_f, *c_g)) return false;

  return true;
}

bool SchurRing2::get_scalar(const schur_poly *g, ring_elem &result) const
{
  if (g->size() != 1) return false;
  if (g->monoms.size() != 1) return false;
  result = g->coeffs[0];
  return true;
}

ring_elem SchurRing2::from_coeff(ring_elem a) const
{
  schur_poly *f = new schur_poly;
  if (!coefficientRing->is_zero(a))
    {
      f->coeffs.push_back(a);
      f->monoms.push_back(1);
    }
  return ring_elem(f);
}
ring_elem SchurRing2::from_long(long n) const
{
  ring_elem a = coefficientRing->from_long(n);
  return from_coeff(a);
}
ring_elem SchurRing2::from_int(mpz_srcptr n) const
{
  ring_elem a = coefficientRing->from_int(n);
  return from_coeff(a);
}
bool SchurRing2::from_rational(mpq_srcptr q, ring_elem &result) const
{
  ring_elem a;
  bool ok = coefficientRing->from_rational(q, a);
  if (not ok) return false;
  result = from_coeff(a);
  return true;
}

ring_elem SchurRing2::copy(const ring_elem f) const
{
  const schur_poly *f1 = f.get_schur_poly();

  schur_poly *g = new schur_poly;
  g->coeffs.insert(g->coeffs.end(), f1->coeffs.begin(), f1->coeffs.end());
  g->monoms.insert(g->monoms.end(), f1->monoms.begin(), f1->monoms.end());

  return ring_elem(g);
}

ring_elem SchurRing2::invert(const ring_elem f) const
{
  // This function is not relevant for this ring
  return zero();
}

ring_elem SchurRing2::divide(const ring_elem f, const ring_elem g) const
{
  // This function is not relevant for this ring
  return zero();
}

void SchurRing2::syzygy(const ring_elem a,
                        const ring_elem b,
                        ring_elem &x,
                        ring_elem &y) const
{
  // This function is not relevant for this ring
  x = zero();
  y = zero();
}

int SchurRing2::compare_partitions(const_schur_partition a,
                                   const_schur_partition b) const
{
  int len = a[0];
  if (b[0] < len) len = b[0];
  for (int i = 1; i < len; i++)
    {
      int cmp = a[i] - b[i];
      if (cmp < 0) return LT;
      if (cmp > 0) return GT;
    }
  int cmp = a[0] - b[0];
  if (cmp < 0) return LT;
  if (cmp > 0) return GT;
  return EQ;
}

int SchurRing2::compare_elems(const ring_elem f, const ring_elem g) const
{
//  assuming the monomials are sorted in the linear order on the partitions
//  see SchurRing2::compare_partitions

  auto f_it = f.get_schur_poly()->begin(),
    f_end = f.get_schur_poly()->end();
  auto g_it = g.get_schur_poly()->begin(),
    g_end = g.get_schur_poly()->end();

  for(; f_it!=f_end && g_it!=g_end; ++f_it, ++g_it) {
    auto cmp = compare_partitions(f_it.getMonomial(), g_it.getMonomial());
    if(cmp) return cmp;
  }

  return (f_it!=f_end)-(g_it!=g_end);  // LT, EQ or GT
}

bool SchurRing2::promote_coeffs(const SchurRing2 *Rf,
                                const ring_elem f,
                                ring_elem &resultRE) const
{
  // Assumption in use: Rf (ring of f) is a Schur ring, with coeff ring coeffRf
  const schur_poly *f1 = f.get_schur_poly();
  schur_poly *result = new schur_poly;

  for (schur_poly::iterator i = f1->begin(); i != f1->end(); ++i)
    {
      if (i.getMonomial()[0] - 1 > nvars) continue;
      ring_elem a;
      if (!coefficientRing->promote(
              Rf->getCoefficientRing(), i.getCoefficient(), a))
        {
          delete result;
          resultRE = from_long(0);
          return false;
        }
      result->appendTerm(a, i.getMonomial());
    }
  resultRE = ring_elem(result);
  return true;
}
bool SchurRing2::lift_coeffs(const SchurRing2 *Sg,
                             const ring_elem f,
                             ring_elem &resultRE) const
{
  const schur_poly *f1 = f.get_schur_poly();
  schur_poly *result = new schur_poly;

  for (schur_poly::iterator i = f1->begin(); i != f1->end(); ++i)
    {
      if (i.getMonomial()[0] - 1 > Sg->n_vars()) continue;
      ring_elem a;
      if (!coefficientRing->lift(
              Sg->getCoefficientRing(), i.getCoefficient(), a))
        {
          delete result;
          resultRE = from_long(0);
          return false;
        }
      result->appendTerm(a, i.getMonomial());
    }
  resultRE = ring_elem(result);
  return true;
}

bool SchurRing2::promote(const Ring *Rf,
                         const ring_elem f,
                         ring_elem &result) const
{
  // Cases:
  // 1.  Rf is ZZ
  // 2. Rf is coefficientRing
  // 3. Rf is another SchurRing2

  if (Rf == globalZZ)
    {
      from_coeff(Rf->promote(globalZZ, f, result));
      return true;
    }
  else if (Rf == coefficientRing)
    {
      result = from_coeff(f);
      return true;
    }
  else
    {
      const SchurRing2 *Sf = Rf->cast_to_SchurRing2();
      if (Sf != 0)
        {
          if (coefficientRing == Sf->getCoefficientRing())
            {
              result = truncate(f);
              return true;
            }

          return promote_coeffs(Sf, f, result);
        }
    }
  return false;
}
bool SchurRing2::lift(const Ring *Rg,
                      const ring_elem f,
                      ring_elem &result) const
{
  const schur_poly *f1 = f.get_schur_poly();
  if (Rg == coefficientRing || Rg == globalZZ)
    {
      if (get_scalar(f1, result))
        {
          if (Rg == globalZZ)
            return coefficientRing->lift(globalZZ, result, result);
          return true;
        }
    }
  else
    {
      const SchurRing2 *Sg = Rg->cast_to_SchurRing2();
      if (Sg != 0)
        {
          if (coefficientRing == Sg->getCoefficientRing())
            {
              result = Sg->truncate(f);
              return true;
            }

          return lift_coeffs(Sg, f, result);
        }
    }
  return false;
}
ring_elem SchurRing2::negate(const ring_elem f) const
{
  if (is_zero(f)) return f;
  const schur_poly *f1 = f.get_schur_poly();
  schur_poly *result = new schur_poly;

  for (VECTOR(ring_elem)::const_iterator i = f1->coeffs.begin();
       i != f1->coeffs.end();
       ++i)
    result->coeffs.push_back(coefficientRing->negate(*i));

  result->monoms.insert(
      result->monoms.end(), f1->monoms.begin(), f1->monoms.end());
  return ring_elem(result);
}

ring_elem SchurRing2::truncate(const ring_elem f) const
// assumption: f is a schur poly over another schur ring, with the SAME coeff
// ring
//  each term is copied over, if the number of elements in the partition is <=
//  n_vars()
{
  if (is_zero(f)) return f;
  const schur_poly *f1 = f.get_schur_poly();
  schur_poly *result = new schur_poly;

  for (schur_poly::iterator i = f1->begin(); i != f1->end(); ++i)
    {
      if (i.getMonomial()[0] - 1 > nvars) continue;
      result->appendTerm(i.getCoefficient(), i.getMonomial());
    }
  return ring_elem(result);
}

ring_elem SchurRing2::add(const ring_elem f, const ring_elem g) const
{
  if (is_zero(f)) return g;
  if (is_zero(g)) return f;
  const schur_poly *f1 = f.get_schur_poly();
  const schur_poly *g1 = g.get_schur_poly();

  schur_poly *result = new schur_poly;

  schur_poly::iterator i = f1->begin();
  schur_poly::iterator j = g1->begin();
  schur_poly::iterator iend = f1->end();
  schur_poly::iterator jend = g1->end();

  bool done = false;
  while (!done)
    {
      int cmp = compare_partitions(i.getMonomial(), j.getMonomial());
      switch (cmp)
        {
          case LT:
            result->appendTerm(j.getCoefficient(), j.getMonomial());
            ++j;
            if (j == jend)
              {
                result->append(i, iend);
                done = true;
              }
            break;
          case GT:
            result->appendTerm(i.getCoefficient(), i.getMonomial());
            ++i;
            if (i == iend)
              {
                result->append(j, jend);
                done = true;
              }
            break;
          case EQ:
            ring_elem c =
                coefficientRing->add(i.getCoefficient(), j.getCoefficient());
            if (!coefficientRing->is_zero(c))
              result->appendTerm(c, i.getMonomial());
            ++j;
            ++i;
            if (j == jend)
              {
                result->append(i, iend);
                done = true;
              }
            else
              {
                if (i == iend)
                  {
                    result->append(j, jend);
                    done = true;
                  }
              }
            break;
        }
    }
  return ring_elem(result);
}

ring_elem SchurRing2::subtract(const ring_elem f, const ring_elem g) const
{
  ring_elem h = negate(g);
  return add(f, h);
}

schur_poly *SchurRing2::mult_by_coefficient(ring_elem a,
                                            const schur_poly *f) const
{
  schur_poly *result = new schur_poly;

  VECTOR(ring_elem)::iterator c_result;
  for (VECTOR(ring_elem)::const_iterator c_f = f->coeffs.begin();
       c_f != f->coeffs.end();
       ++c_f)
    result->coeffs.push_back(coefficientRing->mult(a, *c_f));

  result->monoms.insert(
      result->monoms.end(), f->monoms.begin(), f->monoms.end());

  return result;
}
ring_elem SchurRing2::mult(const ring_elem f, const ring_elem g) const
{
  ring_elem resultRE;
  const schur_poly *f1 = f.get_schur_poly();
  const schur_poly *g1 = g.get_schur_poly();
  ring_elem a;
  if (get_scalar(f1, a))
    {
      return ring_elem(mult_by_coefficient(a, g1));
      // In this case, do a simple multiplication
    }
  else if (get_scalar(g1, a))
    {
      return ring_elem(mult_by_coefficient(a, f1));
    }
  else
    {
      // use the poly heap
      schur_poly_heap H(this);
      for (schur_poly::iterator i = f1->begin(); i != f1->end(); ++i)
        for (schur_poly::iterator j = g1->begin(); j != g1->end(); ++j)
          {
            ring_elem c =
                coefficientRing->mult(i.getCoefficient(), j.getCoefficient());
            ring_elem r = const_cast<SchurRing2 *>(this)->mult_terms(
                i.getMonomial(), j.getMonomial());
            H.add(ring_elem(mult_by_coefficient(c, r.get_schur_poly())));
          }
      return H.value();
    }
}

void toVarpower(const_schur_partition a, intarray &result)
{
  int len = a[0];
  int *result_vp = result.alloc(2 * len);
  int *orig_result_vp = result_vp;
  result_vp++;

  if (len > 1)
    {
      int v = a[1];
      int e = 1;

      for (int i = 2; i < len; i++)
        {
          if (v == a[i])
            e++;
          else
            {
              *result_vp++ = v;
              *result_vp++ = e;
              v = a[i];
              e = 1;
            }
        }
      *result_vp++ = v;
      *result_vp++ = e;
    }

  int newlen = static_cast<int>(result_vp - orig_result_vp);
  *orig_result_vp = newlen;
  result.shrink(newlen);
}

engine_RawArrayPairOrNull SchurRing2::list_form(const Ring *coeffR,
                                                const ring_elem f) const
{
  if (coeffR != coefficientRing)
    {
      ERROR("expected coefficient ring of Schur ring");
      return 0;
    }
  const schur_poly *f1 = f.get_schur_poly();
  int n = static_cast<int>(f1->size());  // this is here because the lengths of
                                         // arrays for M3 front end use int as
                                         // length field.
  engine_RawMonomialArray monoms =
      GETMEM(engine_RawMonomialArray, sizeofarray(monoms, n));
  engine_RawRingElementArray coeffs =
      GETMEM(engine_RawRingElementArray, sizeofarray(coeffs, n));
  monoms->len = n;
  coeffs->len = n;
  engine_RawArrayPair result = newitem(struct engine_RawArrayPair_struct);
  result->monoms = monoms;
  result->coeffs = coeffs;

  // Loop through the terms
  intarray vp;
  schur_poly::iterator i = f1->begin();
  for (int next = 0; next < n; ++i, ++next)
    {
      coeffs->array[next] =
          RingElement::make_raw(coefficientRing, i.getCoefficient());
      toVarpower(i.getMonomial(), vp);
      monoms->array[next] = Monomial::make(vp.raw());
      vp.shrink(0);
    }
  return result;
}

ring_elem SchurRing2::eval(const RingMap *map,
                           const ring_elem f,
                           int first_var) const
{
  // Should we allow ring maps to other Schur rings?  No others are that well
  // defined...
  // Use promote and lift for those instead?
  return map->get_ring()->zero();
}

/////// Littlewood-Richardson algorithm /////////////////////////
// FLAG: put in a reference to the paper/algorithm being used here.
void SchurRing2::SMinitialize(int n,
                              int maxwt)  // FLAG: only called with maxwt==0
{
  SMmaxrows = n;
  SMmaxweight = maxwt;  // need this?

  SMtab.initialize(SMmaxrows, SMmaxweight);
  SMfilled.initialize(SMmaxrows, SMmaxweight);
  SMcurrent = 0;
  SMfinalwt = 0;
  SMtab.p = new int[nvars + 1];  // FLAG: is this correct? use schur_word?, what
                                 // about nvars==-1
  for (int i = 0; i <= nvars; i++) SMtab.p[i] = 0;
  SMheap = new schur_poly_heap(this);
}

void SchurRing2::SMbounds(int &lo, int &hi)
{
  int i, k;
  int x = SMfilled.xloc[SMcurrent];
  int y = SMfilled.yloc[SMcurrent];

  // First set the high bound, using info from the "one to the right"
  // in the reverse lex filled skew tableau.

  if (y == SMfilled.p[x])  // There is not one to the right
    {
      hi = SMmaxrows;
      for (k = 1; k <= SMmaxrows; k++)
        if (SMtab.p[k] == 0)
          {
            hi = k;
            break;
          }
    }
  else  // note that the case SMcurrent==1 will be handled
    {   // in the previous statement.
      hi = SMtab.xloc[SMcurrent - 1];
    }

  // Now we set the lo bound, using info from the "one above"

  if (x == 1 || y <= SMfilled.lambda[x - 1])
    lo = 1;  // There is not one above
  else
    {
      int above = SMcurrent - SMfilled.p[x] + SMfilled.lambda[x - 1];
      int xabove = SMtab.xloc[above];
      int yabove = SMtab.yloc[above];
      for (i = xabove + 1; i <= hi; i++)
        if (SMtab.p[i] < yabove) break;
      lo = i;
    }
}

void SchurRing2::SMsetPartitionLength(schur_word *p, int SMmaxrows)
{
  int i;
  for (i = 1; i <= SMmaxrows; i++)
    if (p[i] == 0) break;
  p[0] = i;
}

void SchurRing2::SM()
{
  int lo, hi;

  if (SMcurrent == SMfinalwt)
    {
      // partition is to be output
      SMsetPartitionLength(SMtab.p, SMmaxrows);
      SMappendTerm(SMtab.p);
      return;
    }

  SMcurrent++;
  SMbounds(lo, hi);
  int this_one = LARGE_NUMBER;  // larger than any entry of SMtab: SMfinalwt+1
                                // should work...
  int last_one;
  for (int i = lo; i <= hi; i++)
    {
      last_one = this_one;
      this_one = SMtab.p[i];
      if (last_one > this_one)
        {
          SMtab.p[i]++;
          SMtab.xloc[SMcurrent] = i;
          SMtab.yloc[SMcurrent] = SMtab.p[i];
          SM();
          SMtab.p[i]--;
        }
    }
  SMcurrent--;
}

void SchurRing2::SMappendTerm(const_schur_partition f)
{
  // make a poly, and insert it into the heap
  schur_poly * val = new schur_poly;
  val->appendTerm(coefficientRing->one(), f);
  SMheap->add(ring_elem(val));
}

ring_elem SchurRing2::skew_schur(const_schur_partition lambda,
                                 const_schur_partition p)
{
  SMcurrent = 0;

  SMfinalwt = 0;
  for (int i = 1; i < p[0]; i++) SMfinalwt += p[i];
  for (int i = 1; i < lambda[0]; i++) SMfinalwt -= lambda[i];
  SMmaxrows = p[0] - 1;  // this is the number of elements in the partition p
  if (nvars != -1 && SMmaxrows > nvars) SMmaxrows = nvars;

  delete[] SMtab
      .p;  // FLAG: should use gc for this?  or not use it, but not both!
  SMtab.p = new int[SMmaxrows + 1];  // FLAG: should use gc for this?  or not
                                     // use it, but not both!
  for (int i = 0; i <= SMmaxrows; i++) SMtab.p[i] = 0;

  SMtab.wt = SMfinalwt;
  SMtab.resize(SMfinalwt);
  SMfilled.resize(SMfinalwt);

  // lambda and p should not be modified in the following call
  SMfilled.fill(const_cast<schur_partition>(lambda),
                const_cast<schur_partition>(
                    p));  // FLAG:fill should take const arguments...
  lambda++;               // FLAG: why is this here?
  p++;                    // FLAG: why is this here?
  SM();
  return SMheap->value();  // resets itself back to new
}

ring_elem SchurRing2::mult_terms(const_schur_partition a,
                                 const_schur_partition b)
{
  int maxsize = (a[0] - 1 + b[0] - 1) + 1;  // this is the max number of
                                            // elements in the output partition,
                                            // plus one

  schur_partition lambda = ALLOCATE_EXPONENTS(sizeof(schur_word) * maxsize);
  schur_partition p = ALLOCATE_EXPONENTS(sizeof(schur_word) * maxsize);

  // Second: make the skew partition (note: r,s>=1)
  // this is: if a = r+1 a1 a2 ... ar
  //             b = s+1 b1 b2 ... bs
  // p is:
  //   (r+s+1) b1+a1 b1+a2 ... b1+ar b1 b2 ... bs
  // lambda is:
  //   (r+1)   b1    b1    ... b1    0  0  ... 0

  int r = a[0] - 1;
  int s = b[0] - 1;
  int c = b[1];

  assert(r + s + 1 == maxsize);

  for (int i = 1; i <= r; i++)
    {
      assert(i < maxsize);
      p[i] = c + a[i];
      lambda[i] = c;
    }
  for (int i = r + 1; i < r + s + 1; i++)
    {
      assert(i < maxsize);
      p[i] = b[i - r];
      lambda[i] = 0;
    }
  p[0] = r + s + 1;
  lambda[0] = r + 1;
  return skew_schur(lambda, p);
}

/////////////////////////////////////////////////////////////////

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
