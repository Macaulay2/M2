// Copyright 2000  Michael E. Stillman
#ifndef _EEbasics_hh_
#define _EEbasics_hh_

#include "EEdefs.hpp"

class EGB1;
class EGBMemory;
class ERingTable;
class EGBLookupTable;

struct ering_elem
{
  friend class EGBMemory;  // for creation, removal.

  int _degree;
  int _tdegree;			// The degree of the homog variable in the lead term.
  const exponent_vector *_lcm;	// Thus, degree = tdegree + weight(lcm).
  ring_elem _f;

  const exponent_vector *lead_exponents() { return _lcm; }
  int degree() { return _degree; }
};

class egb_elem
{
  friend class EGBMemory;  // for creation, removal.

  int _degree;
  int _tdegree;
  const exponent_vector *_lcm;  // Lead monomial
  bool _is_minimal;  // Maybe also the largest 'n' for which 
		// this element is minimal for the subring.
  EVector _f;
  EVector _fsyz;
  ring_elem _denom;		// denominator of fsyz, if the coefficient ring is QQ.
				// or a fraction field.
public:
  int lead_component() const { return _f->comp; }
  const exponent_vector *lead_exponents() const { return _lcm; }
  int degree() const { return _degree; }
  EVector & f() { return _f; }
  EVector & fsyz() { return _fsyz; }
  ring_elem denominator() { return _denom; }
};

class es_pair
{
public:
  es_pair *next;
private:
  int _type; // SP_SYZ, SP_RING, SP_SKEW, SP_GEN, SP_DEFERRED
  int _degree; 
  const exponent_vector *_lcm; // owned in the case SP_SYZ, SP_RING, SP_SKEW.
  egb_elem *_first; // owned in the case SP_GEN, SP_DEFERRED
  union {
    egb_elem *_syz;
    ering_elem *_ringsyz;
    int _skewvar;
  } _s;

public:
  int type() { return _type; }
  int degree() { return _degree; }
  const exponent_vector *lead_exponents() { return _lcm; }
  int lead_component() { return _first->f()->comp; }
  const exponent_vector *first_exponents() { return _first->lead_exponents(); }
  const exponent_vector *second_exponents()  // Only used in SP_SYZ, SP_RING
    {
      if (_type == SP_RING) return _s._ringsyz->_lcm;
      else return _s._syz->lead_exponents();
    }
  egb_elem *element() { return _first; }	// Only used in SP_GEN, SP_DEFERRED

  friend class EGBMemory;  // for creation, removal.
  //  friend es_pair *EGB1::compute_s_pair(es_pair *&p);
};

class EGBMemory
{

  // Handles allocation, deallocation, and initialization
  // of: egb_elem, ering_elem, es_pair, and exponent vectors.

  // egb_elem, ering_elem, es_pair: all have their own stashes
  stash *exponent_stash;
  int nvars;
  const FreeModule *F;
  const FreeModule *Fsyz;
  const Ring *R;
  const EInterface &I;
  int *heuristicWeightVector;

  ERingTable * ring_table;  // Eventually will be placed in the ring itself.

  int F_degree(int x) const;

  exponent_vector *make_skew_lcm(const exponent_vector *exp,
				 int v,
				 int deg_of_exp,
				 int & result_degree) const;
  exponent_vector *make_lcm(const exponent_vector *exp1,
			    const exponent_vector *exp2,
			    int deg_of_exp1,
			    int deg_of_exp2,
			    int & result_degree) const;

  ERingTable *create_ring_table() const;

public:
  // TODO: these two routines
  // general intialization
  EGBMemory(const EInterface &I, const FreeModule *F, const FreeModule *Fsyz);
  ~EGBMemory();

  ////////////////////////
  // RingTable ///////////
  ////////////////////////
  ERingTable *get_ring_table() const;

  exponent_vector *new_exponent_vector() const;
  void remove_exponent_vector(const exponent_vector *&a) const;

  ////////////////////////
  // ering_elem //////////
  ////////////////////////
  ering_elem *make_ering_elem(ring_elem g) const;
  void remove_ering_elem(ering_elem *&g);
  // This removes all fields of the ering_elem.


  ////////////////////////
  // egb_elem ////////////
  ////////////////////////
  egb_elem *make_egb_elem(int degree,
			  EVector &f, //grabs
			  EVector &fsyz, // grabs
			  ring_elem denom, // grabs
			  bool minimal) const;

  void remove_egb_elem(egb_elem *&g);

  ////////////////////////
  // es_pair /////////////
  ////////////////////////
  es_pair *make_ring_s_pair(egb_elem *p, ering_elem *r) const;
  es_pair *make_skew_s_pair(egb_elem *p, int v) const;
  es_pair *make_s_pair(egb_elem *a, egb_elem *b) const;
  es_pair *make_gen_pair(EVector &f, EVector &fsyz, ring_elem &denom) const;
  es_pair *make_gen_pair(EVector &f, EVector &fsyz) const;

  void remove_pair(es_pair *& p);

  // Debug routine
  void ering_elem_out(buffer &o, ering_elem *r) const;
  void egb_elem_out(buffer &o, egb_elem *g) const;
  void spair_out(buffer &o, es_pair *p) const;

};

#endif
