// Copyright 2004 Michael E. Stillman

#ifndef __SPairSet_h_
#define __SPairSet_h_

#include "MonomialSet.h"
#include "interface.h"

class LinearAlgebraGB;
class gbelem;

class SPairSet : public our_new_delete
{

  int determine_next_degree(int &result_number);
  
 public:
  enum spair_type {
    SPAIR_SPAIR,
    SPAIR_GCD_ZZ,
    SPAIR_RING,
    SPAIR_SKEW,
    SPAIR_GEN,
    SPAIR_ELEM
  };

  struct spair : public our_new_delete {
    spair * next;
    spair_type type;
    int deg; /* sugar degree of this spair */
    monomial lcm;
    union {
      struct {
	monomial first_monom;
	int first_gb_num;
	monomial second_monom;
	int second_gb_num;
      } spair;
      struct {
	poly * gen;
	int column; // original column
      } poly;
    } s;
  };

  typedef spair * spair_list;

  SPairSet(MonomialSet *H0);

  ~SPairSet();

  static spair *make_spair(int deg, 
		    monomial lcm, 
		    monomial first_monom,
		    int first_gb_num,
		    monomial second_monom,
		    int second_gb_num);

  static spair *make_spair_gen(int deg, poly *f, int column);

  int find_new_pairs(const std::vector<gbelem, gc_allocator<gbelem> > &gb,
		    bool remove_disjoints);
  // returns the number of new pairs found, using the last element on this list

  int remove_unneeded_pairs();
  // returns the number of pairs removed, based on the element gb[gb.size()-1]

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
  MonomialSet *H; // all monomials in an spair are interned here
  spair *heap; // list of pairs
  spair *this_set;

};

class SPairConstructor : public our_new_delete
{
 public:
  struct pre_spair : public our_new_delete {
    pre_spair * next;
    int deg1; // simple degree of quot
    uninterned_monomial quot;
    int deg2; // simple degree of lead term of 'first_gb_num'
    int first_gb_num;
  };

 private:
  void send_spair(pre_spair *p);

  pre_spair *create_pre_spair(int i);

  SPairConstructor(MonomialSet *H0,
		   SPairSet *S0,
		   const std::vector<gbelem, 
                                     gc_allocator<gbelem> > &gb,
		   bool remove_disjoints);

  int construct_pairs();

 private:
  MonomialSet *H; // all monomials in an spair are interned here
  MemoryBlock B;
  SPairSet *S;

  const std::vector<gbelem, gc_allocator<gbelem> > &gb;
  bool remove_disjoints;

 public:
  static int make(MonomialSet *H0,
		  SPairSet *S0,
	          const std::vector<gbelem, 
                                     gc_allocator<gbelem> > &gb,
		  bool remove_disjoints);
};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
//  End:

