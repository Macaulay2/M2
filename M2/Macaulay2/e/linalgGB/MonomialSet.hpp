// Copyright 2004 Michael E. Stillman
#ifndef __MonomialSet_h_
#define __MonomialSet_h_

#include "memblock.hpp"

typedef int * monomial;
typedef int * uninterned_monomial;
class MonomialSet;

class MonomialSet : public our_new_delete {
  struct hash_node : public our_new_delete {
    struct hash_node *next;
    unsigned long key;
    monomial monom;
  };
  
 public:
  MonomialSet::MonomialSet();
  
  static MonomialSet *make();

  uninterned_monomial reserve(int len);

  bool find_or_insert(uninterned_monomial m, monomial &result);
  // return true if the monomial already exists in the table.
  // otherwise, result is set to the new monomial.
  // The monomial is copied into the 'slab' space locally.
  
  void dump();
  // displays on stderr some info about the hash table
  // and number of monomials
  
 private:
  int hashtab_len;
  hash_node **hashtab;

  int *last_alloc; // The last call to 'reserve'.  If this matches the
                   // monomial to be interned, then a copy is not done.
  MemoryBlock B;
 private:
  unsigned long hash(uninterned_monomial m);
  hash_node *get_next_hash_node();

  monomial intern_monomial(uninterned_monomial m);
};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
//  End:
