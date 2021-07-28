#include "montableZZ.hpp"
#include <functional>
#include <algorithm>
#include <assert.h>

/********************/
/* Support routines */
/********************/

#if 0
// static bool exponents_equal(int nvars, exponents a, exponents b)
// {
//   for (int i=0; i<nvars; i++)
//     if (a[i] != b[i]) return false;
//   return true;
// }
#endif

static bool exponents_greater(int nvars, exponents a, exponents b)
{
  for (int i = 0; i < nvars; i++)
    {
      if (a[i] < b[i]) return false;
      if (a[i] > b[i]) return true;
    }
  return false;
}

static void exponents_show(FILE *fil, exponents exp, int nvars)
/* This is only for debugging */
{
  fprintf(fil, "[");
  for (int i = 0; i < nvars; i++) fprintf(fil, "%d ", exp[i]);
  fprintf(fil, "]");
}

static unsigned long monomial_mask(int nvars, exponents exp)
{
  unsigned long result = 0;
  int i, j;
  for (i = 0, j = 0; i < nvars; i++, j++)
    {
      if (j == 8 * sizeof(long)) j = 0;
      if (exp[i] > 0) result |= (1 << j);
    }
  return result;
}

/********************/
/* MonomialTableZZ ****/
/********************/

MonomialTableZZ::mon_term *MonomialTableZZ::make_list_head()
{
  mon_term *t = new mon_term;
  t->_next = t->_prev = t;
  t->_val = -1;
  t->_lead = 0;
  t->_coeff = nullptr;
  return t;
}

MonomialTableZZ *MonomialTableZZ::make(int nvars)
{
  MonomialTableZZ *result;
  result = new MonomialTableZZ;
  result->_nvars = nvars;
  result->_count = 0;
  /* The first entry is a dummy entry.  Components
     will always start at 1. */
  result->_head.push_back(0);

  return result;
}

MonomialTableZZ::~MonomialTableZZ()
{
  // Nothing needs to be freed: garbage collection will clean
  // it all out.
}

#if 0
// NEW FUNCTION, 5/21/09.  If not functional in a few days, remove it!
int MonomialTableZZ::find_divisors(int max,
                                   mpz_t coeff,
                                   exponents exp,
                                   int comp,
                                   VECTOR(int) *result_term_divisors,
                                   VECTOR(int) *result_mon_divisors) const,
  /* max: the max number of divisors to find.
     exp: the monomial whose divisors we seek.
     result: an array of mon_term's.
     return value: length of this array, i.e. the number of matches found */
{
  assert(comp >= 1);
  if (comp >= static_cast<int>(_head.size())) return 0;
  mon_term *head = _head[comp];
  mon_term *t;
  int i;

  int nmatches = 0;
  unsigned long expmask = ~(monomial_mask(_nvars, exp));

  for (t = head->_next; t != head; t = t->_next)
    if ((expmask & t->_mask) == 0)
      {
        bool is_div = 1;
        for (i=0; i<_nvars; i++)
          if (exp[i] < t->_lead[i])
            {
              is_div = 0;
              break;
            }
        if (is_div)
          {
            if (mpz_divisible_p(coeff, t->_coeff))
              {
                n_term_matches++;
                if (result_term_divisors != 0) result_term_divisors->push_back(t->_val);
              }
              else if (result_mon_divisors != 0) result_mon_divisors->push_back(t->_val);

          }
        if (is_div && mpz_divisible_p(coeff,t->_coeff))
          {
            nmatches++;
            if (result != 0) result->push_back(t);
            if (max >= 0 && nmatches >= max) break;
          }
      }
  if (M2_gbTrace == 15 && nmatches >= 2)
    {
      buffer o;
      o << "find_term_divisors called on ";
      show_mon_term(o, coeff, exp, comp);
      o << " #matches=" << nmatches << newline;
      if (result != 0)
        for (int i=0; i<result->size(); i++)
          show_mon_term(stderr, (*result)[i]);
    }
  return nmatches;
}
#endif

int MonomialTableZZ::find_term_divisors(int max,
                                        mpz_srcptr coeff,
                                        exponents exp,
                                        int comp,
                                        VECTOR(mon_term *) * result) const
/* max: the max number of divisors to find.
   exp: the monomial whose divisors we seek.
   result: an array of mon_term's.
   return value: length of this array, i.e. the number of matches found */
{
  assert(comp >= 1);
  if (comp >= static_cast<int>(_head.size())) return 0;
  mon_term *head = _head[comp];
  mon_term *t;

  int nmatches = 0;
  unsigned long expmask = ~(monomial_mask(_nvars, exp));

  for (t = head->_next; t != head; t = t->_next)
    if ((expmask & t->_mask) == 0)
      {
        bool is_div = 1;
        for (int i = 0; i < _nvars; i++)
          if (exp[i] < t->_lead[i])
            {
              is_div = 0;
              break;
            }
        if (is_div && mpz_divisible_p(coeff, t->_coeff))
          {
            nmatches++;
            if (result != 0) result->push_back(t);
            if (max >= 0 && nmatches >= max) break;
          }
      }
  if (M2_gbTrace == 15 && nmatches >= 2)
    {
      buffer o;
      o << "find_term_divisors called on ";
      show_mon_term(o, coeff, exp, comp);
      o << " #matches=" << nmatches << newline;
      if (result != 0)
        for (int i = 0; i < result->size(); i++) show_mon_term(o, (*result)[i]);
      o << newline;
    }
  return nmatches;
}

bool MonomialTableZZ::is_weak_member(mpz_srcptr c, exponents exp, int comp) const
// Is [c,exp,comp] in the submodule generated by the terms in 'this'?
// Maybe a gbvector should be returned?
{
  // Loop through the elements of component 'comp'
  // If that exponent vector is <= 'exp', then set g (eventual gcd) (if not
  // set).
  //   else mpz(g,g,...);
  //   if mpz_divisible_p(c,g): return true
  // At the end, return false

  assert(comp >= 1);
  if (comp >= static_cast<int>(_head.size())) return 0;
  mon_term *head = _head[comp];
  mon_term *t;
  int i;

  unsigned long expmask = ~(monomial_mask(_nvars, exp));
  mpz_t g;
  bool g_is_set = false;
  for (t = head->_next; t != head; t = t->_next)
    if ((expmask & t->_mask) == 0)
      {
        bool is_div = true;
        for (i = 0; i < _nvars; i++)
          if (exp[i] < t->_lead[i])
            {
              is_div = false;
              break;
            }
        if (!is_div) continue;
        if (!g_is_set)
          {
            mpz_init_set(g, t->_coeff);
            g_is_set = true;
          }
        else
          mpz_gcd(g, g, t->_coeff);
        /* g is set */
        if (mpz_divisible_p(c, t->_coeff))
          {
            mpz_clear(g);
            return true;
          }
      }
  if (g_is_set) mpz_clear(g);
  return false;
}

bool MonomialTableZZ::is_strong_member(mpz_srcptr c, exponents exp, int comp) const
{
  return (find_term_divisors(1, c, exp, comp, 0) > 0);
}

int MonomialTableZZ::find_smallest_coeff_divisor(exponents exp, int comp) const
{
  assert(comp >= 1);
  if (comp >= static_cast<int>(_head.size())) return -1;
  mon_term *head = _head[comp];
  mon_term *t;

  int smallest_val = -1;
  mpz_srcptr smallest = nullptr;

  unsigned long expmask = ~(monomial_mask(_nvars, exp));

  for (t = head->_next; t != head; t = t->_next)
    if ((expmask & t->_mask) == 0)
      {
        bool is_div = 1;
        for (int i = 0; i < _nvars; i++)
          if (exp[i] < t->_lead[i])
            {
              is_div = 0;
              break;
            }
        if (is_div)
          {
            if (smallest_val < 0 || (mpz_cmpabs(smallest, t->_coeff) > 0))
              {
                smallest_val = t->_val;
                smallest = t->_coeff;
              }
          }
      }
  return smallest_val;
}

int MonomialTableZZ::find_monomial_divisors(int max,
                                            exponents exp,
                                            int comp,
                                            VECTOR(mon_term *) * result) const
{
  assert(comp >= 1);
  if (comp >= static_cast<int>(_head.size())) return 0;
  mon_term *head = _head[comp];
  mon_term *t;

  int nmatches = 0;
  unsigned long expmask = ~(monomial_mask(_nvars, exp));

  for (t = head->_next; t != head; t = t->_next)
    if ((expmask & t->_mask) == 0)
      {
        bool is_div = 1;
        for (int i = 0; i < _nvars; i++)
          if (exp[i] < t->_lead[i])
            {
              is_div = 0;
              break;
            }
        if (is_div)
          {
            nmatches++;
            if (result != 0) result->push_back(t);
            if (max >= 0 && nmatches >= max) break;
          }
      }

  if (M2_gbTrace == 15 && nmatches >= 2)
    {
      buffer o;
      o << "find_monomial_divisors called on ";
      show_mon_term(o, nullptr, exp, comp);
      o << " #matches=" << nmatches << newline;
      if (result != 0)
        for (int i = 0; i < result->size(); i++) show_mon_term(o, (*result)[i]);
      o << newline;
    }
  return nmatches;
}

MonomialTableZZ::mon_term *MonomialTableZZ::find_exact(mpz_srcptr coeff,
                                                       exponents exp,
                                                       int comp) const
{
  if (comp >= static_cast<int>(_head.size())) return 0;
  mon_term *head = _head[comp];
  mon_term *t;
  int i;

  unsigned long expmask = monomial_mask(_nvars, exp);

  for (t = head->_next; t != head; t = t->_next)
    if (expmask == t->_mask)
      {
        bool is_eq = 1;
        for (i = 0; i < _nvars; i++)
          if (exp[i] != t->_lead[i])
            {
              is_eq = 0;
              break;
            }
        if (is_eq && !mpz_cmp(coeff, t->_coeff)) return t;
      }
  return 0;
}

MonomialTableZZ::mon_term *MonomialTableZZ::find_exact_monomial(
    exponents exp,
    int comp,
    int first_val) const
{
  if (comp >= static_cast<int>(_head.size())) return 0;
  mon_term *head = _head[comp];
  mon_term *t;
  int i;

  mon_term *result = 0;
  int neqs = 0;

  unsigned long expmask = monomial_mask(_nvars, exp);

  for (t = head->_next; t != head; t = t->_next)
    {
      if (t->_val < first_val) continue;
      if (expmask == t->_mask)
        {
          bool is_eq = 1;
          for (i = 0; i < _nvars; i++)
            if (exp[i] != t->_lead[i])
              {
                is_eq = 0;
                break;
              }
          if (is_eq)
            {
              if (result == 0) result = t;
              neqs++;
            }
        }
    }
  if (neqs > 1)
    {
      printf("number of exact matches: %d\n", neqs);
    }
  return result;
}

void MonomialTableZZ::change_coefficient(mon_term *t,
                                         mpz_srcptr new_coeff,
                                         int new_id)
{
  t->_coeff = new_coeff; // WARNING: new_coeff had better outlive the use of this element.
  t->_val = new_id;
}

void MonomialTableZZ::insert(mpz_srcptr coeff, exponents exp, int comp, int id)
{
  /* Insert 'exp' into the monomial table.  These are kept sorted in ascending
     order
     in some order (lex order?).  No element is ever removed.
  */

  if (comp >= INTSIZE(_head))
    {
      for (int i = INTSIZE(_head); i <= comp; i++)
        _head.push_back(make_list_head());
    }

  mon_term *head = _head[comp];
  mon_term *t;

  /* Make a new mon_term including exp */
  mon_term *newterm = new mon_term;
  newterm->_lead = exp;
  newterm->_mask = monomial_mask(_nvars, exp);
  newterm->_val = id;
  newterm->_coeff = coeff;
  _count++;

  /* Find where to put it */
  for (t = head; t->_next != head; t = t->_next)
    {
      if (exponents_greater(_nvars, newterm->_lead, t->_next->_lead))
        {
          /* Time to insert newterm, right between t, t->next */
          break;
        }
    }

  /* The actual insertion */
  newterm->_next = t->_next;
  newterm->_prev = t;
  t->_next->_prev = newterm;
  t->_next = newterm;
}

/****************************
 * Minimalization ***********
 ****************************/

struct montable_sorter_ZZ : public std::binary_function<int, int, bool>
{
  int nvars;
  const VECTOR(mpz_srcptr) & coeffs;
  const VECTOR(exponents) & exps;
  const VECTOR(int) & comps;
  montable_sorter_ZZ(int nvars0,
                     const VECTOR(mpz_srcptr) & coeffs0,
                     const VECTOR(exponents) & exps0,
                     const VECTOR(int) & comps0)
      : nvars(nvars0), coeffs(coeffs0), exps(exps0), comps(comps0)
  {
  }

  bool operator()(int x, int y)
  {
    exponents xx = exps[x];
    exponents yy = exps[y];
    for (int i = 0; i < nvars; i++)
      if (xx[i] < yy[i])
        return true;
      else if (xx[i] > yy[i])
        return false;
    if (comps[x] < comps[y])
      return true;
    else if (comps[x] > comps[y])
      return false;
    // Now order them in ascending order on the coeff (which should always be
    // POSITIVE).
    return (mpz_cmp(coeffs[x], coeffs[y]) < 0);
  }

#if 0
//   bool operator()(int x, int y) {
//     int result = 0; // -1 is false, 1 is true
//     exponents xx = exps[x];
//     exponents yy = exps[y];
//     for (int i=0; i<nvars; i++)
//       if (xx[i] < yy[i]) {result = 1; break;}
//       else if (xx[i] > yy[i]) {result = -1; break;}
//     if (result == 0)
//       if (comps[x] < comps[y]) result = 1;
//       else if (comps[x] > comps[y]) result = -1;
//     if (result == 0)
//     // Now order them in ascending order on the coeff (which should always be POSITIVE).
//       result = (mpz_cmp(coeffs[x],coeffs[y]) > 0);
//     fprintf(stderr, "comparing %d and %d.  Result: %d\n",x,y,result);
//     if (result > 0) return true; else return false;
//   }
#endif
};

void MonomialTableZZ::show_weak(FILE *fil,
                                mpz_srcptr coeff,
                                exponents exp,
                                int comp,
                                int val) const
{
  fprintf(fil, " elem coeff=");
  mpz_out_str(fil, 10, coeff);
  fprintf(fil, " exp=");
  exponents_show(fil, exp, _nvars);
  fprintf(fil, " comp=");
  fprintf(fil, "%d", comp);
  fprintf(fil, " val=");
  fprintf(fil, "%d\n", val);
}

void MonomialTableZZ::find_weak_generators(int nvars,
                                           const VECTOR(mpz_srcptr) & coeffs,
                                           const VECTOR(exponents) & exps,
                                           const VECTOR(int) & comps,
                                           VECTOR(int) & result_positions,
                                           bool use_stable_sort)
{
  // Find a set of elements which generate all of them, as a submodule.
  // The indices for these are placed into result_positions.

  // The plan for this is simple, although it could be easily optimized.
  // First, sort the elements into increasing order, with coeffs for each
  // specific
  // exponent vector also in increasing order (ASSUMPTION: all coeffs are > 0).

  // Second, loop through each one, checking whether it is in the submodule gen
  // by the previous.
  MonomialTableZZ *T = MonomialTableZZ::make(nvars);

#if 0
  // debugging
  if (coeffs.size() != exps.size())
    fprintf(stderr, "size mismatch\n");
  if (coeffs.size() != exps.size())
    fprintf(stderr, "size mismatch2\n");
  if (coeffs.size() != comps.size())
    fprintf(stderr, "size mismatch3\n");
#endif
#if 0
  // debugging
  fprintf(stderr, "-------------\n");
  fprintf(stderr, "find_weak_generators %ld\n", coeffs.size());
  for (size_t i = 0; i < coeffs.size(); i++)
    T->show_weak(stderr, coeffs[i], exps[i], comps[i], i);
#endif

  VECTOR(int) positions;
  positions.reserve(exps.size());
  for (unsigned int i = 0; i < exps.size(); i++) positions.push_back(i);

  /* The following sorts in ascending lex order, considering the component, exp
     vector
     and finally the coefficient */
  if (use_stable_sort)
    std::stable_sort(positions.begin(),
                     positions.end(),
                     montable_sorter_ZZ(nvars, coeffs, exps, comps));
  else
    std::sort(positions.begin(),
              positions.end(),
              montable_sorter_ZZ(nvars, coeffs, exps, comps));

#if 0
  // debugging
  fprintf(stderr, "sorted find_weak_generators\n");
  for (size_t i = 0; i < coeffs.size(); i++)
    T->show_weak(stderr, coeffs[i], exps[i], comps[i], positions[i]);
#endif

#if 0
//   fprintf(stderr, "sorted terms: ");
//   for (int i=0; i<positions.size(); i++)
//     fprintf(stderr, "%d ", positions[i]);
//   fprintf(stderr, "\n");
#endif

  for (VECTOR(int)::iterator j = positions.begin(); j != positions.end(); j++)
    if (!T->is_weak_member(coeffs[*j], exps[*j], comps[*j]))
      {
        result_positions.push_back(*j);
        T->insert(coeffs[*j], exps[*j], comps[*j], *j);
      }

#if 0
  // debugging
  fprintf(stderr, "ones we take: find_weak_generators %ld\n", coeffs.size());
  for (size_t i = 0; i < result_positions.size(); i++)
    T->show_weak(stderr, 
                 coeffs[result_positions[i]], 
                 exps[result_positions[i]], 
                 comps[result_positions[i]],
                 result_positions[i]);
  fprintf(stderr, "\n\n");
#endif
  /* We could return T if that is desired */
  //  freemem(T);
}

void MonomialTableZZ::find_strong_generators(int nvars,
                                             const VECTOR(mpz_srcptr) & coeffs,
                                             const VECTOR(exponents) & exps,
                                             const VECTOR(int) & comps,
                                             VECTOR(int) & result_positions)
{
  // Find the set of terms c*exp*comp such that every other one is divisible
  // by at least one of these.

  VECTOR(int) positions;
  positions.reserve(exps.size());
  for (unsigned int i = 0; i < exps.size(); i++) positions.push_back(i);

  /* The following sorts in ascending lex order, considering the component, exp
     vector
     and finally the coefficient */
  std::stable_sort(positions.begin(),
                   positions.end(),
                   montable_sorter_ZZ(nvars, coeffs, exps, comps));

#if 0
//   fprintf(stderr, "sorted terms: ");
//   for (int i=0; i<positions.size(); i++)
//     fprintf(stderr, "%d ", positions[i]);
//   fprintf(stderr, "\n");
#endif

  MonomialTableZZ *T = MonomialTableZZ::make(nvars);
  for (VECTOR(int)::iterator j = positions.begin(); j != positions.end(); j++)
    if (!T->is_strong_member(coeffs[*j], exps[*j], comps[*j]))
      {
        result_positions.push_back(*j);
        T->insert(coeffs[*j], exps[*j], comps[*j], *j);
      }
  /* We could return T if that is desired */
  //  freemem(T);
}

void MonomialTableZZ::show_mon_term(FILE *fil, mon_term *t) const
{
  buffer o;
  show_mon_term(o, t);
  fprintf(fil, "%s", o.str());
}

void MonomialTableZZ::show_mon_term(buffer &o, mon_term *t) const
{
  show_mon_term(o, t->_coeff, t->_lead, t->_val);
}

void MonomialTableZZ::show_mon_term(buffer &o,
                                    mpz_srcptr coeff,
                                    exponents lead,
                                    int comp) const
{
  if (coeff != nullptr)
    {
      char s[100000];
      mpz_get_str(s, 10, coeff);
      o << s;
    }
  if (_nvars == 0)
    o << "[";
  else
    {
      o << "[" << lead[0];
      for (int i = 1; i < _nvars; i++) o << "," << lead[i];
    }
  o << "] (" << comp << ")" << newline;
}

void MonomialTableZZ::show(FILE *fil) const
{
  mon_term *t, *head;
  /* Loop through each component, display monomials(val) 10 per line */
  fprintf(fil,
          "monomial table: %d vars, %d components, %d elements\n",
          this->_nvars,
          static_cast<int>(_head.size()),
          this->_count);
  for (unsigned i = 1; i < _head.size(); i++)
    {
      head = this->_head[i];
      if (head->_next == head) continue;
      fprintf(fil, "  -- component %d --\n", i);
      for (t = head->_next; t != head; t = t->_next)
        {
          show_mon_term(fil, t);
        }
    }
  fprintf(fil, "\n");
}

void MonomialTableZZ::showmontable() { show(stdout); }
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
