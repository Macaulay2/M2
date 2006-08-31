// Copyright 2004 Michael E. Stillman

#ifndef __f4spairs_h_
#define __f4spairs_h_

#include "memblock.hpp"
#include "moninfo.hpp"
#include "F4types.hpp"

#if 0
template <typename MonInfo>
class F4SPairs : public our_new_delete
{
  packed_monomial get_lcm(spair *s) const;
  packed_monomial get_first_monomial(spair *s) const;
  packed_monomial get_second_monomial(spair *s) const;
  int get_first(spair *s) const;
  int get_second(spair *s) const;
  enum spair_type get_type(spair *s) const;
  int get_degree(spair *s) const;
};
#endif

class F4SPairSet : public our_new_delete
{
private:
  int determine_next_degree(int &result_number);
  
  spair *make_spair(int deg, 
		    packed_monomial lcm, 
		    packed_monomial first_monom,
		    int first_gb_num,
		    packed_monomial second_monom,
		    int second_gb_num);

  spair *make_spair_gen(int deg, packed_monomial lcm, int column);

  int remove_unneeded_pairs();
  // returns the number of pairs removed, based on the element gb[gb.size()-1]

public:
  F4SPairSet(MonomialInfo *MI0);

  ~F4SPairSet();

  void insert_generator(int deg, packed_monomial lcm, int column);
  // The lcm is a pointer to the space in the polynomial itself

  int find_new_pairs(const gb_array &gb,
		     bool remove_disjoints);
  // returns the number of new pairs found, using the last element on this list

  int prepare_next_degree(int max, int &result_number);
  // Returns the (sugar) degree being done next, and collects all (or at
  // most 'max', if max>0) spairs in this lowest degree.
  // Returns the degree, sets result_number.
  // These spairs are not sorted in any way.  Should they be?

  spair *get_next_pair();
  // get the next pair in this degree (the one 'prepare_next_degree' set up')
  // returns 0 if at the end

  void insert(spair *p);  

  void display_spair(spair *p);
  // A debugging routine which displays an spair

  void display();
  // A debugging routine which displays the spairs in the set
 private:
  MonomialInfo *MI;
  spair *heap; // list of pairs
  spair *this_set;
};

class SPairConstructor : public our_new_delete
{
 public:
  struct pre_spair : public our_new_delete {
    pre_spair * next;
    int deg1; // simple degree of quot
    packed_monomial quot;
    int deg2; // simple degree of lead term of 'first_gb_num'
    int first_gb_num;
  };

 private:
  void send_spair(pre_spair *p);

  pre_spair *create_pre_spair(int i);

  SPairConstructor(MemoryBlock<monomial_word> *B0,
		   F4SPairSet *S0,
		   const gb_array &gb,
		   bool remove_disjoints);

  int construct_pairs();

 private:
  MemoryBlock<monomial_word> *B;
  F4SPairSet *S;

  const gb_array &gb;
  bool remove_disjoints;

 public:
  static int make(MemoryBlock<monomial_word> *H0,
		  F4SPairSet *S0,
		  const gb_array &gb,
		  bool remove_disjoints);
};
#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:

