// Copyright 2000  Michael E. Stillman
#ifndef _EEGB_hh_
#define _EEGB_hh_

#include "EEdefs.hpp"
#include "EEbasics.hpp"
#include "EElookup.hpp"

class EGB
{
  class iterator;
  friend class iterator;

  EGBMemory *MEM;

  
  ERingTable * ring_table;
  array< EGBLookupTable * > divisor_table;
  EGBLookupTable *  minimals;  // These are in all components, and are sorted.

  int _n_non_minimal;

  void reduce(EVector &f, EVector &fsyz, int exclude);  //TODO
public:
  EGB(EGBMemory *MEM); //TODO
  ~EGB(); //TODO

  void insert(egb_elem *g);
  // Insert the given element into the GB.  If this element is minimal, or trimmed
  // then that should be already set in the egb_elem.

  void insert_non_minimal(egb_elem *g);
  // Similar to insert, except that 'g' is never inserted into the minimal
  // GB.

  int find_good_divisor(int component, 
			ring_elem coeff,
			exponent_vector *exponents,
			ering_elem *&r,
			egb_elem *&g,
			int & alpha); //TODO

  int n_gb_large() const; //TODO
  int n_gb_minimal() const;  //TODO
  int n_inserted_non_minimal() const; //TODO
  int n_components() const; //TODO

  void sort_minimal_GB();  // Is it kept sorted?? //TODO

  void inter_reduce_minimal_GB(); //TODO

  void reduce(EVector &f, EVector &fsyz);  //TODO


  //////////////////////////////////////////////////////////
  // Iterator for going through all minimal GB elements ////
  //////////////////////////////////////////////////////////
  // TODO.  This iterator should use 'minimals'?  Also: need a second iterator
  class iterator {
    const EGB *basis;
    int i;
    EGBLookupTable::iterator p;
    void next();
#if 0
      {
      for (--i; i >= 0; --i)
	{
	  p = basis->minimals[i]->first();
	  if (p.valid())
	    return;
	}
    }
#endif
  public:
    iterator(const EGB *basis) : basis(basis), i(basis->n_components()) { next(); }

    bool valid() { return (i >= 0 && p.valid()); }
    void operator++() { 
      ++p;
      if (p.valid()) return;
      else next();
    }    
    egb_elem *operator*() { return *p; }
  };

  iterator first() const { return iterator(this); }

};

#endif
