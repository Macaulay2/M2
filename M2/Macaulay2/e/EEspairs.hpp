// Copyright 2000  Michael E. Stillman
#ifndef _EEspairs_hh_
#define _EEspairs_hh_

#include "EEbasics.hpp"

class SPairSet
{
  // TODO: need higher level information...

  EGBMemory *MEM;

  es_pair *heap;
  es_pair *this_set;

  int _nelems;			// Number of elements in 'heap'
  int _ndegree;			// Number of elements in 'this_set'.

  // statistics information
  intarray _stats;		// array of (degree,npairs in degree).
				// information is set at each degree.
  
  int _n_computed;		// Total number of pairs reduced to completion
  int _n_saved_gcd;		// Total number of pairs not needed due to the gcd=1 criterion
  int _n_saved_gcd_choice;	// Total number of pairs not needed, because exists a gcd=1 pair.
  int _n_saved_lcm;		// Total number of pairs not needed, because of second level criterion.

  // higher level decision information TODO: set this info.
  bool is_ideal;
  int nvars;

  int next_degree(int &nextdeg) const;	//OK
    // Returns number to be done in the next degree, sets nextdeg.
  bool pair_not_needed(es_pair *p, egb_elem *m) const; //OK

public:
  SPairSet(); //TODO -- isideal,nvars,MEM

  ~SPairSet(); //TODO

  void insert(es_pair *p);  //OK
    // Insert a list of pairs.

  int get_next_degree(int &nextdeg);  //OK
    // Returns number to be done in the next degree, sets nextdeg.
    // Pairs in this degree are sorted, and should be removed by
    // subsequent calls to 'remove_smallest'.

  es_pair *remove_smallest(); //OK
    // Gets the smallest element in the current degree.
    // Once NULL is returned, call get_next_degree to get
    // new elements set. Callee now owns this es_pair.

  int n_pairs_in_degree() const { return _ndegree; }
  int n_pairs_total() const { return _nelems; }

  void stats(buffer &o); //TODO	// Displays some statistics about this set.

  int n_computed() const; // ??

private:
  // The following operations are all subroutines for
  // 'insert_only_minimals'.
  bool is_gcd_one_pair(es_pair *p) const; //OK
  void remove_pairs(es_pair *&pair_list) const; //OK
  int compare_pairs(es_pair *f, es_pair *g) const; // TODO: Don't use lex order!!
  es_pair *merge_pairs(es_pair *f, es_pair *g) const; //OK
  void sort_pairs(es_pair *&p) const; //OK
  void choose_nice_pair(es_pair *&p); //OK
  void choose_unique_pairs(es_pair *&p); //OK
  void minimalize_pairs(es_pair *&p); //OK
  
public:
  //// Higher level operations ////
  void remove_unneeded(egb_elem *m); //OK
  void insert_only_minimals(es_pair *&new_list); //OK

};


#endif
