// Copyright 1998 by Michael Stillman
#ifndef __monoid_hpp_
#define __monoid_hpp_

#include "Edefs.hpp"
#include "Emonorder.hpp"

class polynomial;
class EMonomialTable;
class ECommMonoid;
class ENCMonoid;

class monomial
{
public:
  monomial *next;		// Next hash bucket...
  uint32 hashval;
  int * partial_sums;
  int * exponents;
};

class EMonoid : public type
{
protected:
  int nvars;
  int componentloc;
  int nslots;
  EMonomialOrder *monorder;

  int *print_order; // print_order[i] is the variable that should appear
		    // in the i th place, during text output.  Only
		    // used for commutative case.
  char **var_names;	// Names of the variables. 0..nvars-1
  monomial *_one;

  bool *isnonnegativevar;  // array 0..nvars-1: if true, then negative exponents
                           // are not allowed.
                           // These values are determined from the monomial order.
                           
  // EMonoid constructor: use only by calling the routine 'make'.
  EMonoid(EMonomialOrder *mo,              // grabs
		    const int *print,      // copies
		    const char **names);   // copies

public:
  // setNames: create an array 0..nvars-1 of the names of the variables 
  // (each a null terminated string).  's' should be a sequence of 'slength'
  // characters, giving the variable names, separated by spaces, as in:
  // "a b x1 x_2  hithere".  Variables for which a name is not provided are
  // given the name "".  The user of this routine is required to free the space
  // associated with the result.
  static char **setNames(int nvars, const char *s, int slength);
  
  virtual ~EMonoid();

  const EMonomialOrder *getMonomialOrder() const
    {return monorder;}

  const int *getPrintOrder() const
    {return print_order;}

  const char *getVariableName(int i) const
    {return var_names[i];}

  // Query routines regarding the monoid:
  bool is_commutative() const 
    {return monorder->is_commutative();}

  virtual bool isCommutativeVariable(int v) const = 0;

  int n_vars() const 
    {return nvars;}

  int n_slots() const
    {return nslots;}

  const monomial *one() const 
    {return _one;}

  void remove(monomial *) const
    {}

  monomial *clone(const monomial *a) const 
    { return (monomial *) a; }
    
  bool is_equal(const monomial *a, const monomial *b) const
    { return a == b; }

  bool is_one(const monomial *a) const
    { return a == _one; }

  const int *get_partial_sums(const monomial *m) const
    { return m->partial_sums; }

  virtual void get_variable_exponent_pairs(const monomial *m, intarray &result) const = 0;

  virtual uint32 hash_exponents(const int *exponents) const = 0;
  virtual uint32 hash_encoded(const int *exponents) const = 0;

  virtual int compare(const monomial *a, int acomp,
		      const monomial *b, int bcomp) const = 0;
  virtual int compare(const monomial *a,
		      const monomial *b, 
		      int n) const = 0;
  virtual monomial *monomial_from_exponents(const int *exp) const = 0;
  virtual monomial *monomial_from_encoded(const int *encoded) const = 0;
  virtual monomial *monomial_from_variable_exponent_pairs(const intarray &term) const = 0;
  virtual const int * to_exponents(const monomial *m) const = 0;
  virtual int encoded_length(const int *encoded) const = 0;

  virtual monomial *mult(const monomial *a, const monomial *b) const = 0;
  virtual monomial *divide(const monomial *a, const monomial *b) const = 0;

  int degree(const monomial *a, const int *wts) const;
  
  virtual void stats() const = 0;
  virtual void text_out(buffer &o) const = 0;
  virtual void bin_out(buffer &o) const = 0;
  virtual void elem_text_out(buffer &o, const monomial *a) const = 0;
  virtual void elem_bin_out(buffer &o, const monomial *a) const = 0;
  virtual monomial *elem_binary_in(istream &i) const = 0;

  virtual const ENCMonoid *toNCMonoid() const { return 0; }
  virtual const ECommMonoid *toCommMonoid() const { return 0; }

  virtual EMonoid * cast_to_EMonoid() { return this; }
  virtual const EMonoid * cast_to_EMonoid() const { return this; }

  type_identifier  type_id () const { return TY_EMonoid; }
  const char * type_name   () const { return "EMonoid"; }
};

class ENCMonoid : public EMonoid
{
  EMonomialTable *T;  // Or, how should this be set?
  int *MULT_exp;

  // Noncommutative fields:
  int n_nc_blocks;
  int *nclength;     // array 0..n_nc_blocks-1 : slot where the length field is located
  bool *is_nc_block; // array 0..nslots-1
  bool *is_comm;     // array 0..nvars-1

  ENCMonoid(EMonomialOrder *mo,  // grabs
	   const int *print,     // copies
	   const char **names);  // copies

public:
  virtual ~ENCMonoid();

  static ENCMonoid *make(EMonomialOrder *mo,            // grabs
				 const int *print,      // copies
				 const char **names);   // copies

  bool isCommutativeVariable(int v) const
    { return is_comm[v]; }

  int encoded_length(const int *encoded) const;
  void get_variable_exponent_pairs(const monomial *m, intarray &result) const;
  monomial *monomial_from_encoded(const int *encoded) const;
  monomial *monomial_from_exponents(const int *exp) const;
  monomial *monomial_from_variable_exponent_pairs(const intarray &term) const;
  const int * to_exponents(const monomial *m) const;
  
  int compare(const monomial *a, int acomponent,
	      const monomial *b, int bcomponent) const;
  int compare(const monomial *a,
	      const monomial *b,
	      int n) const;

  virtual uint32 hash_exponents(const int *exponents) const { return 0; }
  virtual uint32 hash_encoded(const int *encoded) const;

  monomial *mult(const monomial *a, const monomial *b) const;
  monomial *divide(const monomial *a, const monomial *b) const; // return c, if b = a*c
    // divide should only be used when it is known that there exists such a c in the monoid.

  virtual void stats() const;

  virtual void text_out(buffer &o) const;
  virtual void bin_out(buffer &o) const;

  virtual void elem_text_out(buffer &o, const monomial *a) const;
  virtual void elem_bin_out(buffer &o, const monomial *a) const;
  virtual monomial *elem_binary_in(istream &i) const;

  virtual const ENCMonoid *toNCMonoid() const { return this; }

  class_identifier class_id() const { return CLASS_ENCMonoid; }
};

class ECommMonoid : public EMonoid
{
  static ECommMonoid *_trivial;
  EMonomialTable *T;  // Or, how should this be set?
  uint32 *hash_multiplier;
  int *MULT_exp;

  ECommMonoid(EMonomialOrder *mo,          // grabs
		    const int *print,      // copies
		    const char **names);   // copies
public:
  static const ECommMonoid *getTrivialMonoid();
  
  static ECommMonoid *make(EMonomialOrder *mo,        // grabs
				 const int *print,    // copies
				 const char **names); // copies
  virtual ~ECommMonoid();

  virtual void text_out(buffer &o) const;
  virtual void bin_out(buffer &o) const;
  static ECommMonoid *binary_in(istream &i);
  
  int encoded_length(const int *) const
    { return nslots; }

  void get_variable_exponent_pairs(const monomial *m, intarray &result) const;
  monomial *unchecked_monomial_from_exponents(const int *exp) const;
  monomial *monomial_from_exponents(const int *exp) const;
  monomial *monomial_from_encoded(const int *encoded) const;
  monomial *monomial_from_variable_exponent_pairs(const intarray &term) const;

  bool isCommutativeVariable(int) const 
    { return true; }

  const int * to_exponents(const monomial *m) const;
  int * to_product(const monomial *m) const;
  // What about removing these elements?

  // key operations
  virtual uint32 hash_exponents(const int *exponents) const;
  virtual uint32 hash_encoded(const int *exponents) const { return 0; }

  monomial *mult(const monomial *a, const monomial *b) const; 
  monomial *divide(const monomial *a, const monomial *b) const; // return c, if b = a*c

  int compare(const monomial *a, int acomponent,
	      const monomial *b, int bcomponent) const;
  int compare(const monomial *a,
	      const monomial *b,
	      int n) const;
  int compare(const monomial *a, const monomial *sa, int ai, 
	      const monomial *b, const monomial *sb, int bi) const;  // Schreyer order compare

  bool divides(const monomial *a, const monomial *b) const;
  // Commutative case: is b-a >= 0 in every component.

  // Other useful routines
  monomial *lcm(const monomial *a, const monomial *b) const;
  monomial *gcd(const monomial *a, const monomial *b) const;

  virtual void stats() const;

  // I/O
  virtual void elem_text_out(buffer &o, const monomial *a) const;
  virtual void elem_bin_out(buffer &o, const monomial *a) const;
  virtual monomial *elem_binary_in(istream &i) const;

  virtual const ECommMonoid *toCommMonoid() const { return this; }

  class_identifier class_id() const { return CLASS_ECommMonoid; }
};

// These are currently in Ecommands.cpp
void appendMonomialToIntarray(const EMonoid *M, const monomial *m, intarray &result);
intarray monomialToIntarray(const EMonoid *M, const monomial *m);
monomial *monomialFromIntarray(const EMonoid *D, const intarray &mapdeg);

#endif
