#ifndef __exptable_h_
#define __exptable_h_

/**
 * @file exptable.h
 * @brief Hash table specialisation for `(exponent vector, unsigned long)` pairs.
 *
 * Specialises David R. Hanson's generic C `Table_T`
 * (declared in `table.h`) for the case the engine cares about
 * most: keys are fixed-length integer exponent vectors and values
 * are `unsigned long`. The `exponent_table_*` API wraps the
 * generic operations --- `_new(hint, nvars)`, `_put`, `_get`,
 * `_length`, `_free` --- with the right key-comparison and
 * hashing for monomial exponents, so callers don't have to wire
 * up `hash` / `cmp` callbacks themselves. The convention is to
 * avoid storing zero values: `_put` returns the previously
 * stored value (or `0` if absent), so a zero return uniformly
 * means "key was not present."
 *
 * Used wherever the engine accumulates a sparse exponent-keyed
 * histogram --- Hilbert-function buckets, monomial-coefficient
 * tallies in res-monomials, F4 monomial-index pools. The base
 * `table.h` predates the engine's C++ refactor and is one of the
 * remaining C-only files in the engine.
 *
 * @see table.h
 */

/* Implementation of a hashtable [exponent vectors, of fixed length] --> unsigned long int. */
/* The implementation uses table.{h,c}, which was written by David R. Hanson */

typedef int * exponent;
typedef struct exponent_table exponent_table;

#if defined(__cplusplus)
extern "C" {
#endif

exponent_table * exponent_table_new(int hint,
                                    int nvars);

int exponent_table_length(exponent_table *E);

void exponent_table_free(exponent_table **E);

long exponent_table_put(exponent_table *E, const exponent expon, long value);
/* Puts the element 'expon => value' into the table, and if an element is
   already there, its value is returned, otherwise 0 is returned.
   Thus, it is a good idea to not insert zero values into the table */

long exponent_table_get(exponent_table *E, const exponent expon);
/* Returns the value associated to 'expon', returning zero, if 'expon'
   is not in the table */

const void ** exponent_table_to_array(exponent_table *E);

#if defined(__cplusplus)
}
#endif
#endif

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/
