// Copyright 2000  Michael E. Stillman

#include "EEGB.hpp"

EGB::EGB(EGBMemory *MEM)
  : _n_non_minimal(0)
{
}

EGB::~EGB()
{
}

void EGB::insert(egb_elem *g)
{
  // insert 'g' into both sets.
  int x = g->lead_component();
  divisor_table[x]->insert(g);
  minimals->insert_w_deletions(g);
}

void EGB::insert_non_minimal(egb_elem *g)
{
  _n_non_minimal++;
  int x = g->lead_component();
  divisor_table[x]->insert(g);
}

int EGB::find_good_divisor(int component, 
			   ring_elem coeff,
			   exponent_vector *exponents,
			   ering_elem *&r,
			   egb_elem *&g,
			   int & alpha)
{
  array < ering_elem * > rdivs;
  array < egb_elem * > divs;

  // First we find all divisors with minimal alpha.
  ring_table->find_all_divisors(exponents,rdivs);
  divisor_table[component]->find_all_divisors(exponents,divs);

  // Find smallest alpha.
  
  // Remove all others
  
  // 
  // Now we pick one.
  return 1;
}


