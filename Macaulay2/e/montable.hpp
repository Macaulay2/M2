#ifndef __montable_h
#define __montable_h

using namespace std;

#include <vector>
#include <algorithm>
#include <stdio.h>

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

class MonomialTable {
public:
  struct mon_term {
    mon_term  *_next;
    mon_term  *_prev;
    exponents _lead;		/* Who owns this? */
    unsigned long _mask;
    int       _val;
  };

  static MonomialTable *make(int nvars);
  /* Create a zero element table */

  static MonomialTable *make_minimal(int nvars, 
				     const vector<exponents> &exps,
				     const vector<int> &comps,
				     const vector<int> &vals,
				     vector<int> &rejects);

  ~MonomialTable();

  void insert(exponents exp, int comp, int id);
  /* Insert [exp,comp,id] into the table.  If there is already
     an element which is <= [exp,comp], this triple is still
     inserted.  If that is not desired, use find_divisors.
  */

  int find_divisors(int max,
		    exponents exp, 
		    int comp,
		    vector< mon_term *> *result = 0) const;
  /* max: the max number of divisors to find. 
     exp: the monomial whose divisors we seek.
     result: an array of mon_term's.
     return value: length of this array, i.e. the number of matches found */

  mon_term *find_exact(exponents exp, int comp) const;
  /* If this returns non-NULL, it is valid to grab the 'val' field, and/or to assign to it.
     All other fields should be considered read only */

  static void minimalize(int nvars,
			 const vector < exponents> &exps, 
			 const vector<int> &comps,
			 bool keep_duplicates, 
			 vector<int> &result_positions
			 );

  /* Need a way of looping through the elements? */

  void show(FILE *fil); /* Only for debugging */

private:
  int _nvars;
  int _count;
  vector<mon_term *> _head; /* One per component */

  static mon_term *make_list_head();
};
 

#endif
