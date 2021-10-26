#define MOVE_UP_JUST_ONE
#define INSERT_AT_END

#include "montable.hpp"
#include "ntuple.hpp"
#include <functional>
#include <algorithm>
#include <assert.h>

/********************/
/* Support routines */
/********************/

static bool exponents_equal(int nvars, exponents a, exponents b)
{
  for (int i = 0; i < nvars; i++)
    if (a[i] != b[i]) return false;
  return true;
}

#ifndef INSERT_AT_END
static bool exponents_greater(int nvars, exponents a, exponents b)
{
  for (int i = 0; i < nvars; i++)
    {
      if (a[i] < b[i]) return false;
      if (a[i] > b[i]) return true;
    }
  return false;
}
#endif

static void exponents_show(FILE *fil, exponents exp, int nvars)
/* This is only for debugging */
{
  fprintf(fil, "[");
  for (int i = 0; i < nvars; i++) fprintf(fil, "%d ", exp[i]);
  fprintf(fil, "]");
}

/********************/
/* MonomialTable ****/
/********************/

MonomialTable::mon_term *MonomialTable::make_list_head()
{
  mon_term *t = reinterpret_cast<mon_term *>(mon_term_stash->new_elem());
  t->_next = t->_prev = t;
  t->_val = -1;
  t->_lead = 0;
  return t;
}

MonomialTable::MonomialTable()
{
  _last_match = NULL;
  _last_match_comp = -1;
}

MonomialTable *MonomialTable::make(int nvars)
{
  MonomialTable *result;
  result = new MonomialTable;
  result->mon_term_stash = new stash("montable terms", sizeof(mon_term));
  result->_nvars = nvars;
  result->_count = 0;
  /* The first entry is a dummy entry.  Components
     will always start at 1. */
  result->_head.push_back(0);

  return result;
}

MonomialTable::~MonomialTable()
{
  /* Loop through each component, and remove all mon_terms */
  for (unsigned int i = 1; i < _head.size(); i++)
    {
      mon_term *t = _head[i];
      while (t->_next != t)
        {
          mon_term *tmp = t->_next;
          tmp->_prev->_next = tmp->_next;
          tmp->_next->_prev = t;
          mon_term_stash->delete_elem(tmp);
        }
      _head[i] = 0;
    }
  delete mon_term_stash;
  _count = 0;
}

void MonomialTable::move_up(mon_term *const y, mon_term *const head)
{
  if (head->_next == y) return;
  mon_term *const x = y->_prev;
  mon_term *const w = x->_prev;
  mon_term *const z = y->_next;
  w->_next = y;
  z->_prev = x;
  x->_next = z;
  x->_prev = y;
  y->_next = x;
  y->_prev = w;
}

void MonomialTable::remove(mon_term *const y)
{
  mon_term *const x = y->_prev;
  mon_term *const z = y->_next;
  x->_next = z;
  z->_prev = x;
}

void MonomialTable::insert_before(mon_term *const y, mon_term *const z)
{
  mon_term *const x = z->_prev;
  x->_next = y;
  y->_next = z;
  y->_prev = x;
  z->_prev = y;
}

#ifdef MOVE_UP_JUST_ONE
#define MOVE_UP(t, head) move_up(t, head)
#else
#define MOVE_UP(t, head) (remove(t), insert_before(t, head->_next))
#endif

int MonomialTable::find_divisor(exponents exp, int comp)
{
  assert(comp >= 1);
  if (comp >= static_cast<int>(_head.size())) return -1;
  if (comp == _last_match_comp && _last_match != NULL &&
      ntuple::divides(_nvars, _last_match->_lead, exp))
    return _last_match->_val;
  unsigned long expmask = ~ntuple::mask(_nvars, exp);
  mon_term *head = _head[comp];
  for (mon_term *t = head->_next; t != head; t = t->_next)
    if ((expmask & t->_mask) == 0)
      if (ntuple::divides(_nvars, t->_lead, exp))
        {
          _last_match = t;
          _last_match_comp = comp;
          // move_up(t,head);
          return t->_val;
        }
  return -1;
}

int MonomialTable::find_divisors(int max,
                                 exponents exp,
                                 int comp,
                                 VECTOR(mon_term *) * result)
{
  assert(comp >= 1);
  assert(max != 0);
  if (comp >= static_cast<int>(_head.size())) return 0;
  if (max == 1 && comp == _last_match_comp && _last_match != NULL &&
      ntuple::divides(_nvars, _last_match->_lead, exp))
    {
      if (result != NULL) result->push_back(_last_match);
      return 1;
    }
  mon_term *head = _head[comp];
  int nmatches = 0;
  unsigned long expmask = ~ntuple::mask(_nvars, exp);
  //*DEBUG*/ long nviewed = 0;
  //*DEBUG*/ long nmasked = 0;
  for (mon_term *t = head->_next, *tnext = t->_next; t != head;
       t = tnext, tnext = t->_next)
    if ((expmask & t->_mask) == 0)
      {
        //*DEBUG*/   nviewed++;
        if (ntuple::divides(_nvars, t->_lead, exp))
          {
            nmatches++;  // this doesn't happen very often
            _last_match = t;
            _last_match_comp = comp;
            // move_up(t,head);
            if (result != NULL) result->push_back(t);
            if (max >= 0 && nmatches >= max) break;
          }
      }
  //*DEBUG*/    else
  //*DEBUG*/      nmasked++;
  //*DEBUG*/  fprintf(stderr, "nviewed %d nmasked %ld max %d nfound %ld\n",
  // nviewed, nmasked, max, nmatches);
  return nmatches;
}

MonomialTable::mon_term *MonomialTable::find_exact(exponents exp,
                                                   int comp) const
{
  if (comp >= static_cast<int>(_head.size())) return 0;
  mon_term *head = _head[comp];
  mon_term *t;
  int i;

  unsigned long expmask = ntuple::mask(_nvars, exp);

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
        if (is_eq) return t;
      }
  return 0;
}

void MonomialTable::insert(exponents exp, int comp, int id)
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
  mon_term *newterm = reinterpret_cast<mon_term *>(mon_term_stash->new_elem());
  newterm->_lead = exp;
  newterm->_mask = ntuple::mask(_nvars, exp);
  newterm->_val = id;

  _count++;

/* Find where to put it */
#ifdef INSERT_AT_END
  // put it at the end
  t = head->_prev;
#else
  // insert it in sequence (stupid ordering, though)
  for (t = head; t->_next != head; t = t->_next)
    {
      if (exponents_greater(_nvars, newterm->_lead, t->_next->_lead))
        {
          /* Time to insert newterm, right between t, t->next */
          break;
        }
    }
#endif

  /* The actual insertion */
  newterm->_next = t->_next;
  newterm->_prev = t;
  t->_next->_prev = newterm;
  t->_next = newterm;
}

/****************************
 * Minimalization ***********
 ****************************/

struct sorter : public std::binary_function<exponents, exponents, bool>
{
  int nvars;
  const VECTOR(exponents) & exps;
  const VECTOR(int) & comps;
  sorter(int nvars0,
         const VECTOR(exponents) & exps0,
         const VECTOR(int) & comps0)
      : nvars(nvars0), exps(exps0), comps(comps0)
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
    return false;
  }
};

void MonomialTable::minimalize(int nvars,
                               const VECTOR(exponents) & exps,
                               const VECTOR(int) & comps,
                               bool keep_duplicates,
                               VECTOR(int) & result_positions)
{
  /* Step 1: Sort an intarray into ascending order.
     In this order, if e divides f, then e should appear
     before f. Don't actually change 'exp'.  Need a special compare routine.  */

  /* Step 2: Make a monomial table. */

  /* Step 3: Loop through each element in the intarray.  If the exponent is not
     in the
     monomial ideal, put that index into the result, and insert into the
     monomial ideal.
     If it is in the monomial ideal, go on. */

  /* Step alternate3: If ALL minimal elements are to be taken. (e.g. if [1,1,0]
     is
     minimal, but occurs more than once, then keep all occurrences of [1,1,0].
     */

  /* Step 4: Remove the monomial table.  Note that the exp vectors should not
     be recreated. */

  MonomialTable *T;

  VECTOR(int) positions;
  positions.reserve(exps.size());
  for (unsigned int i = 0; i < exps.size(); i++) positions.push_back(i);

  /* The following sorts in ascending lex order, considering the component and
     the
     inhomogeneous part of the exponent vector */
  std::stable_sort(
      positions.begin(), positions.end(), sorter(nvars, exps, comps));

  T = MonomialTable::make(nvars);

  VECTOR(int)::iterator first, end;
  first = positions.begin();
  end = positions.end();
  while (first != end)
    {
      VECTOR(int)::iterator next = first + 1;
      exponents this_exp = exps[*first];
      int comp = comps[*first];
      while (next != end)
        {
          if (!exponents_equal(nvars, this_exp, exps[*next])) break;
          if (comp != comps[*next]) break;
          next++;
        }
      if (T->find_divisor(this_exp, comp) == -1)
        {
          /* We have a minimal element */

          T->insert(this_exp, comp, *first);
          result_positions.push_back(*first);
          if (keep_duplicates)
            while (++first != next) result_positions.push_back(*first);
        }

      first = next;
      /* At this point: [first,next) is the range of equal monomials */
    }
  freemem(T);
}

MonomialTable *MonomialTable::make_minimal(int nvars,
                                           const VECTOR(exponents) & exps,
                                           const VECTOR(int) & comps,
                                           const VECTOR(int) & vals,
                                           VECTOR(int) & rejects)
{
  MonomialTable *T;

  VECTOR(int) positions;
  positions.reserve(exps.size());
  for (unsigned int i = 0; i < exps.size(); i++) positions.push_back(i);

  /* The following sorts in ascending lex order, considering the component and
     the
     inhomogeneous part of the exponent vector */
  std::stable_sort(
      positions.begin(), positions.end(), sorter(nvars, exps, comps));

  T = MonomialTable::make(nvars);

  VECTOR(int)::iterator first, end, last_minimal;
  first = positions.begin();
  end = positions.end();
  last_minimal = first;
  while (first != end)
    {
      VECTOR(int)::iterator next = first + 1;
      exponents this_exp = exps[*first];
      int comp = comps[*first];
      while (next != end)
        {
          if (!exponents_equal(nvars, this_exp, exps[*next])) break;
          if (comp != comps[*next]) break;
          rejects.push_back(*next);
          next++;
        }
      if (T->find_divisor(this_exp, comp) == -1)
        {
          /* We have a minimal element */

          T->insert(this_exp, comp, vals[*first]);
          *last_minimal++ = *first;
        }
      else
        rejects.push_back(*first);

      first = next;
      /* At this point: [first,next) is the range of equal monomials */
    }
  return T;
}

void MonomialTable::show(FILE *fil)
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
          exponents_show(fil, t->_lead, _nvars);
          fprintf(fil, " (%d)\n", t->_val);
        }
    }
  fprintf(fil, "\n");
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
