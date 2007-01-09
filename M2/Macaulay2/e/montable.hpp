#ifndef __montable_h
#define __montable_h

#include <vector>
#include <memory>
#include <algorithm>
#include <stdio.h>
#include <gmp.h>

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

typedef int * exponents;

class MonomialTable : public our_new_delete {
  MonomialTable();		// the public must use "make" below
public:
  struct mon_term {
    mon_term  *_next;
    mon_term  *_prev;
    exponents _lead;		/* Who owns this? */
    unsigned long _mask;
    int       _val;
  };

  static MonomialTable *make(int nvars); // this function serves as the constructor
  /* Create a zero element table */

  static MonomialTable *make_minimal(int nvars, 
				     const VECTOR(exponents) &exps,
				     const VECTOR(int) &comps,
				     const VECTOR(int) &vals,
				     VECTOR(int) &rejects);

  ~MonomialTable();

  void insert(exponents exp, int comp, int id);
  /* Insert [exp,comp,id] into the table.  If there is already
     an element which is <= [exp,comp], this triple is still
     inserted.  If that is not desired, use find_divisors.
  */

  int find_divisor(exponents exp, int comp);
  /* returns the integer 'val' of the first divisor of exp*comp found,
     or, returns -1 if none is found. */

  int find_divisors(int max,
		    exponents exp, 
		    int comp,
		    VECTOR(mon_term *) *result = 0);
  /* max: the max number of divisors to find. 
     exp: the monomial whose divisors we seek.
     result: an array of mon_term's.
     return value: length of this array, i.e. the number of matches found */

  mon_term *find_exact(exponents exp, int comp) const;
  /* If this returns non-NULL, it is valid to grab the 'val' field, and/or to assign to it.
     All other fields should be considered read only */

  static void minimalize(int nvars,
			 const VECTOR(exponents) &exps, 
			 const VECTOR(int) &comps,
			 bool keep_duplicates, 
			 VECTOR(int) &result_positions
			 );

  /* Need a way of looping through the elements? */

  void show(FILE *fil); /* Only for debugging */

private:
  stash *mon_term_stash;
  int _nvars;
  int _count;
  VECTOR(mon_term *) _head; /* One per component */
  mon_term *_last_match; 	// optimization cache for find_divisors
  mon_term *make_list_head();
};
 

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
