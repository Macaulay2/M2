// Copyright 2005  Michael E. Stillman

#ifndef _monhashtable_h_
#define _monhashtable_h_

#include "moninfo.hpp"

class MonomialHashTable
{
  typedef long *monomial;
  
private:
  MonomialInfo *M;
  monomial *hashtab;

  long size;
  long logsize;
  unsigned long hashmask;

  long threshold;
  long count;
  long nclashes;
  long max_run_length;

  void insert(monomial m);
  void grow(); // Increase logsize by 1, remake hashtab.
  void initialize(int logsize0);
public:

  MonomialHashTable(MonomialInfo *M0, int logsize = 16);
  // The hash table size will be a power of 2, and this
  // is the initial power.
  
  ~MonomialHashTable();
  
  bool find_or_insert(monomial m, monomial &result);
  // return true if the monomial already exists in the table.
  // otherwise, result is set to the new monomial.

  void dump() const;
  // displays on stderr some info about the hash table
  // and number of monomials

  void show() const;
  // displays the hash table, first by dots and x'x: ...x...xxx....
  // and then each monomial is displayed, with hash value.
  // in form [loc, hash, monomial]
  // Each blank line has a dot, with multiple dots per line.
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
// End:
