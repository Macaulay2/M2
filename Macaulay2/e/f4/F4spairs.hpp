// Copyright 2004 Michael E. Stillman

#ifndef __f4spairs_h_
#define __f4spairs_h_

#include "memblock.hpp"
#include "moninfo.hpp"
#include "F4types.hpp"


class F4SPairSet : public our_new_delete
{
private:
  int determine_next_degree(int &result_number);

  spair *make_spair(int deg, 
		     packed_monomial lcm, 
		     int i,
		     int j);

  spair *make_spair_gen(int deg, packed_monomial lcm, int column);

  bool pair_not_needed(spair *p, gbelem *m);

  int remove_unneeded_pairs();
  // returns the number of pairs removed, based on the element gb[gb.size()-1]

public:
  F4SPairSet(MonomialInfo *MI0, const gb_array &gb0);

  ~F4SPairSet();

  void insert_generator(int deg, packed_monomial lcm, int column);
  // The lcm is a pointer to the space in the polynomial itself

  void insert_spair(pre_spair *p, int me);

  void delete_spair(spair *p);

  int find_new_pairs(bool remove_disjoints);
  // returns the number of new pairs found, using the last element on this list

  int prepare_next_degree(int max, int &result_number);
  // Returns the (sugar) degree being done next, and collects all (or at
  // most 'max', if max>0) spairs in this lowest degree.
  // Returns the degree, sets result_number.
  // These spairs are not sorted in any way.  Should they be?

  spair *get_next_pair();
  // get the next pair in this degree (the one 'prepare_next_degree' set up')
  // returns 0 if at the end

  void display_spair(spair *p);
  // A debugging routine which displays an spair

  void display();
  // A debugging routine which displays the spairs in the set
 private:
  MemoryBlock<monomial_word> B; // for all of the packed monomials in the spairs

  MonomialInfo *M;
  const gb_array &gb;
  spair *heap; // list of pairs
  spair *this_set;
  stash *spair_stash;
  stash *gen_stash;
};

class F4SPairConstructor : public our_new_delete
{
 private:
  pre_spair *create_pre_spair(int i);

  F4SPairConstructor(MonomialInfo *MI,
		     F4SPairSet *S0,
		     const gb_array &gb,
		     bool remove_disjoints);

  void insert_pre_spair(VECTOR(VECTOR(pre_spair *) *) &bins, 
			pre_spair *p);

  int construct_pairs();

 private:
  MonomialInfo *M;
  F4SPairSet *S;
  const gb_array &gb;
  bool remove_disjoints;

  MemoryBlock<pre_spair> P;
  MemoryBlock<varpower_word> B;
  int max_varpower_size;
  
  gbelem *me;
  int me_component;
 public:
  static int make(MonomialInfo *MI,
		  F4SPairSet *S0,
		  const gb_array &gb,
		  bool remove_disjoints);
};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:

