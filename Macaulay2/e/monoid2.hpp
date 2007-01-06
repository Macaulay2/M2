// Copyright 2004.  Michael E. Stillman

#ifndef _monoid2_hpp_
#define _monoid2_hpp_

#include "monordering.h"
#include "array.hpp"
#include "newdelete.hpp"
#include "hash.hpp"
#include "intarray.hpp"
#include <vector>
#include "imonorder.h"

class PolynomialRing;

typedef int * exponents;
typedef int * graded_exponents;
typedef int * partial_sums;
typedef int * monomial;

typedef const int * const_exponents;
typedef const int * const_graded_exponents;
typedef const int * const_partial_sums;
typedef const int * const_monomial;
typedef const int * const_varpower;

class Monoid : public mutable_object
{
  int nvars_;
  M2_stringarray varnames_;
  M2_arrayint degvals_;
  array<const_monomial> degree_of_var_;	// [0]..[nvars-1] are the multi-degrees of the 
				// variables, and [nvars] = zero element in the 
				// degree monoid.
  M2_arrayint primary_degree_of_var_;

  const PolynomialRing *degree_ring_;
  const Monoid *degree_monoid_;

  MonomialOrdering *mo_;
  MonomialOrder *monorder_; // Internal version, with encoding information
  enum overflow_type { OVER, OVER2, OVER4 } *overflow_;

  int monomial_size_;  // in ints
  int monomial_bound_;

  int n_invertible_vars_;
  int n_before_component_;
  int n_after_component_;
  bool component_up_;

  M2_arrayint local_vars; // These are the variables which are < 1 in the monomial order.

  VECTOR(int) nslots_;
  
  
  void set_degrees();
  void set_overflow_flags();

  exponents EXP1_, EXP2_, EXP3_;// allocated ntuples.
				// A local routine may use these ONLY if
				// they call no other monoid routine, except
				// to/from expvector.
  monomial MONlocal_;		// To be used ONLY by to/from expvector.


  static Monoid *trivial_monoid;

  Monoid();

  Monoid(MonomialOrdering *mo,
	 M2_stringarray names,
	 const PolynomialRing *DR, /* degree ring */
	 M2_arrayint degs);
  
  void decode(const_monomial m, exponents result) const;
  void encode(const_exponents exp, monomial esult) const;

public:
  static Monoid *create(MonomialOrdering *mo,
			M2_stringarray names,
			const PolynomialRing *DR, /* degree ring */
			M2_arrayint degs);

  static Monoid *tensor_product(const Monoid *M1, const Monoid *M2);

  ~Monoid();

  static void set_trivial_monoid_degree_ring(const PolynomialRing *DR);
  // ONLY to be called by PolyRing::get_trivial_poly_ring()

  static Monoid *get_trivial_monoid();

  const MonomialOrdering * getMonomialOrdering() const { return mo_; }
  M2_arrayint getNonTermOrderVariables() const { return local_vars; }
  const PolynomialRing *get_degree_ring() const { return degree_ring_; }
  const Monoid *degree_monoid() const { return degree_monoid_; }
  const_monomial degree_of_var(int v) const { return degree_of_var_[v]; }
  int primary_degree_of_var(int v) const { return primary_degree_of_var_->array[v]; }
  M2_arrayint primary_degree_of_vars() const { return primary_degree_of_var_; }

  bool is_group() const { return n_invertible_vars_ == nvars_; }

  void text_out(buffer &o) const;

  int n_vars()        const { return nvars_; }
  int max_degree()    const { return monomial_bound_; }
  int monomial_size() const { return monomial_size_; }

  int n_slots(int nparts) const;
  int num_parts() const;

  /////////////////////////
  // Monomial arithmetic //
  /////////////////////////
  void from_varpower(const_varpower vp, monomial result) const;
  void to_varpower(const_monomial m, intarray &result_vp) const;

  void from_expvector(const_exponents exp, monomial result) const;
  void to_expvector(const_monomial m, exponents result_exp) const;

  M2_arrayint to_arrayint(const_monomial monom) const; /* Returns an exponent vector representation 
						      of the monomial */
  bool in_subring(int nslots, const_monomial m) const;
  int compare(int nslots, const_monomial m, const_monomial n) const;

  monomial make_new(const_monomial d) const;
  monomial make_one() const;
  void remove(monomial d) const;

  bool is_one(const_monomial m) const;
  bool is_invertible(const_monomial m) const; // is every variable that occurs 
  // in 'm' allowed to be negative?

  void one(monomial result) const;
  void copy(const_monomial m, monomial result) const;

  void mult(const_monomial m, const_monomial n, monomial result) const;
  void power(const_monomial m, int n, monomial result) const;
  int compare(const_monomial m, const_monomial n) const;
  int compare(const_monomial m, int mcomp, const_monomial n, int ncomp) const;
  bool divides(const_monomial m, const_monomial n) const;
  void divide(const_monomial m, const_monomial n, monomial result) const;
  void lcm(const_monomial m, const_monomial n, monomial result) const;
  void gcd(const_monomial m, const_monomial n, monomial result) const;
  void monsyz(const_monomial m, 
	      const_monomial n, 
	      monomial result_sm, 
	      monomial result_sn) const;

  void elem_text_out(buffer &o, const_monomial m) const;

  int primary_value(const_monomial m) const;

  void multi_degree(const_monomial m, monomial result) const;
  int primary_degree(const_monomial m) const;
  int degree_weights(const_monomial m, M2_arrayint wts) const;
  void degree_of_varpower(const_varpower vp, monomial result) const;

};

#if 0
// // These will become the unsafe versions, I guess, if they are still
// // needed...
// inline void Monoid::mult(const_monomial m, const_monomial n, monomial result) const
//     { for (int i=0; i<monomial_size_; i++) *result++ = *m++ + *n++; }
// 
// inline void Monoid::power(const_monomial m, int n, monomial result) const
//     { for (int i=0; i<monomial_size_; i++) *result++ = *m++ * n; }
#endif

// WARNING!! 'divide' assumes that division is possible
inline void Monoid::divide(const_monomial m, const_monomial n, monomial result) const
    { 
      for (int i=monomial_size_; i>0; i--) 
	*result++ = *m++ - *n++; 
    }


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
