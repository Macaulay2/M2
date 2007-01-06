// Copyright 1996.  Michael E. Stillman

#ifndef _monoid_hh_
#define _monoid_hh_

#include "monordering.h"
#include "monorder.hpp"
#include "array.hpp"
#include "newdelete.hpp"

class PolynomialRing;

typedef int * exponents;
typedef int * graded_exponents;
typedef int * partial_sums;
typedef int * monomial;
typedef int * varpower_monomial;

typedef const int * const_exponents;
typedef const int * const_graded_exponents;
typedef const int * const_partial_sums;
typedef const int * const_monomial;
typedef const int * const_varpower;

class monoid_info : our_new_delete
{
  friend class Monoid;

  int nvars;
  M2_stringarray varnames;
  M2_arrayint degvals;
  array<const_monomial> degree_of_var;	// [0]..[nvars-1] are the multi-degrees of the 
				// variables, and [nvars] = zero element in the 
				// degree monoid.
  M2_arrayint primary_degree_of_var;

  const PolynomialRing *degree_ring;
  const Monoid *degree_monoid;

  MonomialOrdering *_mo;
  const mon_order *mo;


  bool isgroup;		
  bool use_packing;	// If so, then monomial_size_ == nvars, no packing is done

  void set_degrees();

public:
  monoid_info();
  ~monoid_info();

  monoid_info(MonomialOrdering *mo,
	      mon_order *mmo,
	      M2_stringarray s,
	      const PolynomialRing *deg_ring,
	      M2_arrayint degs);
};

class Monoid : public mutable_object
{
protected:
  monoid_info *moninfo;

  int nvars;
  int nbits;
  int n_per_word;
  int npacked_words;
  int nweights;
  int monomial_size_;
  int top_bits;			// Only needed for 'divides'.
  int bit_mask;
  int mon_bound;		// Entries this size or larger may not be 
				// used.  

  exponents EXP1, EXP2, EXP3;	// allocated ntuples.
				// A local routine may use these ONLY if
				// they call no other monoid routine, except
				// to/from expvector.
  monomial MONlocal;		// To be used ONLY by to/from expvector.

  static Monoid *trivial_monoid;

  Monoid(monoid_info *mo, int nbits);

  void unpack(const int *m, int *result) const;
  void pack(const int *exp, int *result) const;


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

  const PolynomialRing *get_degree_ring() const { return moninfo->degree_ring; }
  const Monoid *degree_monoid() const { return moninfo->degree_monoid; }
  const_monomial degree_of_var(int v) const { return moninfo->degree_of_var[v]; }
  int primary_degree_of_var(int v) const { return moninfo->primary_degree_of_var->array[v]; }
  M2_arrayint primary_degree_of_vars() const { return moninfo->primary_degree_of_var; }

  bool is_group() const { return moninfo->isgroup; }

  void text_out(buffer &o) const;

  int n_vars()        const { return nvars; }
  int max_degree()    const { return mon_bound; }
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

  int n_slots(int nparts) const;
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

inline int Monoid::compare(const_monomial m, const_monomial n) const
{
  int i = monomial_size_;
  if (i == 0) return EQ;
  while (1)
    {
      if (*m > *n) return GT;
      if (*m < *n) return LT;
      if (--i == 0) return EQ;
      m++, n++;
    }
}

inline int Monoid::compare(const_monomial m, int mcomp, const_monomial n, int ncomp) const
{
  int cmp = compare(m,n);
  if (cmp != EQ) return cmp;
  if (mcomp < ncomp) return LT;
  if (mcomp > ncomp) return GT;
  return EQ;
}
#endif


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
