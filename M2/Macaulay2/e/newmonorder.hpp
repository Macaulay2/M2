// Copyright 1997 Michael E. Stillman.

#ifndef _newmon_order_hh_
#define _newmon_order_hh_

#include "object.hpp"
#include "intarray.hpp"

const int NEWMO_LEX8 = 0;
const int NEWMO_LEX16 = 1;
const int NEWMO_LEX32 = 2;
const int NEWMO_LEX64 = 3;
const int NEWMO_REVLEX8 = 4;
const int NEWMO_REVLEX16 = 5;
const int NEWMO_REVLEX32 = 6;
const int NEWMO_REVLEX64 = 7;
const int NEWMO_WTFCN = 8;
const int NEWMO_COMPONENT = 9;
const int NEWMO_GROUP = 10;	// A block of variables with lex order,
				// with elements in (ZZ,ZZ,...,ZZ,ZZ/d1,...,ZZ/dr)
const int NEWMO_SKEW = 11;

class new_mon_order
{
  struct mon_order_list {
    int typ;		// NEWMO_LEX, NEWMO_REVLEX, NEWMO_WEIGHT, NEWMO_COMPONENT
    int n;			// number of variables in this block, for NEWMO_LEX, NEWMO_REVLEX
    int nslots;
    int ntorsion;
    int *weights;		// weight vector, in the case NEWMO_WEIGHT
				// if NEWMO_GROUP: values to mod out by in each component
  };
  int nvars;
  int nblocks;
  int nwords;
  mon_order_list *order;	// An array 0..nblocks-1
public:
  new_mon_order(const int *moncodes);
  virtual ~new_mon_order();

  // Creation and combination routines
  static mon_order *product(mon_order *mo1, mon_order *mo2);
  
  // Query routines (about the order)
  int n_vars() const { return nvars; }
  bool is_revlex() const;
  bool is_elimination(int i) const;
  bool is_product(int i) const;
  void get_monomial_codes(intarray &result) const;

  // Comparison

  // Multiplication

  // To/from exponent vectors
  void encode(const int *exp, int *m) const;
  void decode(const int *m, int *exp) const;
  void text_out(buffer &o) const;
};

class object_new_mon_order : public type
{
public:
  const new_mon_order *val;

  object_new_mon_order() {}
  object_new_mon_order(const new_mon_order *mo) : val(mo) {}
  ~object_new_mon_order() {}

  const new_mon_order * mon_order_of() const { return val; }

  class_identifier class_id() const { return CLASS_MonomialOrder; }
  type_identifier  type_id () const { return TY_NEW_MON_ORDER; }
  const char * type_name   () const { return "MonomialOrder"; }

  object_new_mon_order *       cast_to_new_mon_order()       { return this; }
  const object_new_mon_order * cast_to_new_mon_order() const { return this; }
  
  int length_of() const { return val->n_vars(); }

  void text_out(buffer &o) const 
    {
      if (val != NULL) val->text_out(o);
    }
};

#endif
