// Copyright 2000.  Michael E. Stillman

#ifndef _Emonoid_hh_
#define _Emonoid_hh_

#include "Emonorder.hpp"

typedef const int *monomial;
typedef const int *exponents;

class Monoid : public type
{
private:
  static Monoid *_trivial_monoid;
  int _nvars;
  int _nwords;			// The number of ints in an (encoded) monomial
  int _component_loc;
  array<char *> _var_names;
  int *_print_order;		// Array 0.._nvars-1: a permutation of 0.._nvars-1.

  bool _is_group;		// If so, then nwords == nvars, no packing is done
				// and display is altered as well.

  stash *_monom_stash;

  monomial _one;

  /////////////////////////
  // Degree information ///
  /////////////////////////
  array<int *> _degree_of_var;	// [0]..[nvars-1] are the multi-degrees of the 
				// variables, and [nvars] = zero element in the 
				// degree monoid.
  int *_primary_degree_of_var;

  const Monoid *_D;		// The degree monoid

  /////////////////////////
  // Monomial order ///////
  /////////////////////////

  EMonomialOrder *_mo;

  ////////////////////////////////////
  // Skew commutative information ////
  ////////////////////////////////////
  // This should be in the polynomial ring?
  int _n_skew;			// If multiplication is skew commutative
				// (this is just used by rings, NOT by
				// monoid multiplication, although this may affect
				// implementation).
  
  int *_skew_vars;		// 0..nvars-1: skew_vars[v] = 1 iff v has odd (skew)degree
  int *_skew_list;		// 0..n_skew-1: skew_list[i] = (skew var in range 0..nvars-1)



  // Used so we don't have to keep allocating all the time
  int *_skew_mvars;
  int *_skew_nvars;		// To save ALOT of allocations...
  int *_EXP1, *_EXP2, *_EXP3;	// allocated ntuples.
				// A local routine may use these ONLY if
				// they call no other monoid routine, except
				// to/from expvector.

  Monoid();  // Only used by the create_trivial_monoid routine
  Monoid(EMonomialOrder *mo,
	 const int *print, 
	 const char **names,
	 const Monoid *deg_monoid,
	 const intarray &degs,
	 const intarray &skewvars,
	 bool is_group);

  void set_skew_info(const intarray &skewvariables);
  void set_degrees(const intarray &degvals);
	 
public:
  static const Monoid *trivial_monoid();
  static Monoid *create(EMonomialOrder *mo,
			const int *print, 
			const char **names,
			const Monoid *D,
			const intarray &degs,
			const intarray &skewvars,   // This should really go with the ring.
			bool is_group); // Possibly ignored?
  ~Monoid();

  static char **make_name_array(int nvars, const char *s, int slength);

  bool is_group() const { return false; } // FIX: MES
  int n_vars()        const { return _nvars; }
  int monomial_size() const { return _nwords; }
  int max_degree()    const { return 1 << (8*sizeof(long)-1); }

  void from_varpower(const int *vp, int *result) const;
  void to_varpower(const int *m, intarray &result_vp) const;

  void from_expvector(const int *exp, int *result) const;
  void to_expvector(const int *m, int *result_exp) const;

  bool in_subring(int n, const int *m) const;
  int compare(int nslots, const int *m, const int *n) const;

  int *make_new(const int *d) const;
  int *make_one() const;
  void remove(int *d) const;

  bool is_one(const int *m) const;
  void one(int *result) const;
  void copy(const int *m, int *result) const;

  void mult(const int *m, const int *n, int *result) const;
  void power(const int *m, int n, int *result) const;
  int compare(const int *m, const int *n) const;
  int compare(const int *m, int mcomp, const int *n, int ncomp) const;
  bool divides(const int *m, const int *n) const;
  void divide(const int *m, const int *n, int *result) const;
  void lcm(const int *m, const int *n, int *result) const;
  void gcd(const int *m, const int *n, int *result) const;
  void monsyz(const int *m, const int *n, int *sm, int *sn) const;

  ///////////////////////////////
  // Skew commutative routines //
  ///////////////////////////////
  bool is_skew() const;
  int is_skew_var(int v) const;
  int skew_mult_sign(const int *m, const int *n) const;
  int exp_skew_mult_sign(const int *exp1, const int *exp2) const;
  int skew_mult(const int *m, const int *n, int *result) const;
  int skew_divide(const int *m, const int *n, int *result) const;
      // If the result is s (1,or -1), then m = s * n * result
  int skew_diff(const int *m, const int *n, int *result) const;
      // m acting as a differential operator on n is s * result, s = 0, 1, or -1.
  int exp_skew_vars(const int *exp, int *result) const;
      // The number s of skew variables in 'exp' is returned, and their
      // indices are placed in result[0], ..., result[s-1].
  int skew_vars(const int *m, int *result) const;
      // The number s of skew variables in 'm' is returned, and their
      // indices are placed in result[0], ..., result[s-1].
  bool skew_is_zero(const int *exp) const;
      // Return whether any skew variable in the exponent vector has exponent >= 2

  void elem_text_out(buffer &o, const int *m) const;
  void elem_bin_out(buffer &o, const int *m) const;

  int primary_value(const int *m) const;
  void multi_degree(const int *m, int *result) const;
  int primary_degree(const int *m) const;
  int degree_weights(const int *m, const int *wts) const;
  void degree_of_varpower(const int *vp, int *result) const;

  const Monoid *degree_monoid() const { return _D; }
  monomial degree_of_var(int v) const { return _degree_of_var[v]; }
  int primary_degree_of_var(int v) const { return _primary_degree_of_var[v]; }

  // Infrastructure here
  void text_out(buffer &o) const;

  int          length_of() const      { return n_vars(); }
  const Monoid * cast_to_Monoid() const { return this; }
  Monoid *       cast_to_Monoid()       { return this; }

  class_identifier class_id() const { return CLASS_Monoid; }
  type_identifier  type_id () const { return TY_MONOID; }
  const char * type_name   () const { return "Monoid"; }

};

extern const Monoid *trivial_monoid; // Set in x_monoid.cpp

inline int Monoid::compare(const int *m, const int *n) const
{
  for (int i=0; i<_nwords; i++, m++, n++)
    {
      if (*m > *n) return GT;
      if (*m < *n) return LT;
    }
  return EQ;
}
inline int Monoid::compare(const int *m, int mcomp, const int *n, int ncomp) const
{
  const int *mend = m + _component_loc;
  const int *mend2 = m + _nwords;
  for ( ; m != mend; m++, n++)
    {
      if (*m > *n) return GT;
      if (*m < *n) return LT;
    }
  int cmp = mcomp - ncomp;
  if (cmp < 0) return LT;
  if (cmp > 0) return GT;
  for ( ; m != mend2; m++, n++)
    {
      if (*m > *n) return GT;
      if (*m < *n) return LT;
    }
  return EQ;
}

#endif
