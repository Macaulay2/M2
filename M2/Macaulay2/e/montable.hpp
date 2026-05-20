#ifndef __montable_h
#define __montable_h

/**
 * @file montable.hpp
 * @brief `MonomialTable` --- leading-monomial divisor index used by the GB reducer.
 *
 * Declares `MonomialTable`, the data structure GB code queries
 * thousands of times per reduction: given a monomial `m`, find
 * a basis element whose leading monomial divides `m`. Each
 * `mon_term` carries a doubly-linked-list pair (`_next` /
 * `_prev`), the borrowed exponent pointer `_lead` (the owning
 * polynomial still owns it, with trailing sugar entries
 * ignored), a precomputed bitmask `_mask` that fast-rejects
 * non-divisors before a full exponent compare, and the basis
 * index `_val`. Per the in-source comment, terms are kept in
 * lex order so the divisibility scan can prune early; the
 * implementation maintains a per-component list head in `_head`
 * and a `_last_match` cache to speed repeated queries.
 *
 * Public operations: `make(nvars)` (the actual constructor),
 * `insert(exp, comp, id)`, `find_divisor`,
 * `find_divisors(max, exp, comp, *result)`, and
 * `find_exact(exp, comp)`. The static `make_minimal` and
 * `minimalize` helpers (defined at `montable.cpp:367` and `:297`)
 * read as if they compute a minimal subset by lead monomial,
 * but they are declared `private` with no `friend` access and
 * no internal self-calls --- nothing in the engine can reach
 * them, so they are effectively dead scaffolding kept around in
 * the header. The coefficient-aware `ZZ`-coefficient analogue
 * lives in `montableZZ.hpp`, where the divisibility test
 * additionally checks that a candidate's leading coefficient
 * divides the reducee's.
 *
 * @see montableZZ.hpp
 * @see gb-default.hpp
 * @see ExponentVector.hpp
 */

#include "mem.hpp"
#include <vector>
#include <memory>
#include <algorithm>
#include <stdio.h>
#include <stddef.h>

#include "ExponentVector.hpp"
#include "newdelete.hpp"
#include "style.hpp"

/* "Tricks" used in this implementation */
/*
 1. exponent vectors: these look like: [e1, ..., en],
    where n is the number of variables.  HOWEVER, these
    exponents are never created or freed by these routines,
    so if they have more entries (e.g. a "sugar" homogenization)
    then that (those) value(s) are ignored.
 2. comparison routine: elements are kept in (increasing?) lex order.
    Is this really an OK idea?
 */

/**
 * @brief Indexed table of monomials with fast "find a divisor" lookup,
 * keyed by a free integer `val` per entry.
 *
 * @details Stores entries as a per-component doubly-linked list of
 * `mon_term`s sorted in increasing lex order. Each entry caches a
 * `_mask` (popcount-style divisibility filter) so divisor searches
 * can skip most monomials with a single bitwise test. Pure
 * `(exponents, component) -> val` index --- the exponent vectors
 * themselves are owned by the caller (the table does not free
 * them). `MonomialTableZZ` is the companion specialisation when
 * entries also carry a `ZZ` coefficient and the GB needs to track
 * leading-term divisibility modulo content.
 */
class MonomialTable : public our_new_delete
{
  static MonomialTable *make_minimal(int nvars,
                                     const VECTOR(exponents_t) & exps,
                                     const VECTOR(int) & comps,
                                     const VECTOR(int) & vals,
                                     VECTOR(int) & rejects);

  static void minimalize(int nvars,
                         const VECTOR(exponents_t) & exps,
                         const VECTOR(int) & comps,
                         bool keep_duplicates,
                         VECTOR(int) & result_positions);

  MonomialTable();  // the public must use "make" below
 public:
  /**
   * @brief Doubly-linked-list node of a `MonomialTable`'s per-component
   * monomial list.
   *
   * @details `_lead` points to the entry's exponent vector (owned by the
   * caller, not by the table), `_mask` is the precomputed
   * divisibility bitmask used to skip non-divisors during search,
   * and `_val` is the caller-supplied opaque index returned on a
   * hit.
   */
  struct mon_term
  {
    mon_term *_next;
    mon_term *_prev;
    exponents_t _lead; /* Who owns this? */
    unsigned long _mask;
    int _val;
  };

  static MonomialTable *make(
      int nvars);  // this function serves as the constructor
  /* Create a zero element table */

  ~MonomialTable();

  void insert(exponents_t exp, int comp, int id);
  /* Insert [exp,comp,id] into the table.  If there is already
     an element which is <= [exp,comp], this triple is still
     inserted.  If that is not desired, use find_divisors.
  */

  int find_divisor(exponents_t exp, int comp);
  /* returns the integer 'val' of the first divisor of exp*comp found,
     or, returns -1 if none is found. */

  int find_divisors(int max,
                    exponents_t exp,
                    int comp,
                    VECTOR(mon_term *) *result = nullptr);
  /* max: the max number of divisors to find.
     exp: the monomial whose divisors we seek.
     result: an array of mon_term's.
     return value: length of this array, i.e. the number of matches found */

  mon_term *find_exact(exponents_t exp, int comp) const;
  /* If this returns non-NULL, it is valid to grab the 'val' field, and/or to
     assign to it.
     All other fields should be considered read only */

  /* Need a way of looping through the elements? */

  void show(FILE *fil); /* Only for debugging */

 private:
  stash *mon_term_stash;
  int _nvars;
  int _count;
  VECTOR(mon_term *) _head; /* One per component */
  mon_term *_last_match;    // optimization cache for find_divisors
  int _last_match_comp;     // optimization cache for find_divisors
  mon_term *make_list_head();
  static void move_up(mon_term *const y, mon_term *const head);
  static void insert_before(mon_term *const y, mon_term *const z);
  static void remove(mon_term *const y);
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
