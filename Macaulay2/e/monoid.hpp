// Copyright 1996.  Michael E. Stillman

#ifndef _monoid_hh_
#define _monoid_hh_

#include "monordering.h"
#include "monorder.hpp"
#include "array.hpp"

class monoid_info
{
  friend class Monoid;

  int nvars;
  M2_stringarray varnames;
  M2_arrayint degvals;
  array<int *> degree_of_var;	// [0]..[nvars-1] are the multi-degrees of the 
				// variables, and [nvars] = zero element in the 
				// degree monoid.
  M2_arrayint primary_degree_of_var;

  const Monoid *degree_monoid;

  MonomialOrdering *_mo;
  const mon_order *mo;

  bool isgroup;		
  bool use_packing;	// If so, then nwords == nvars, no packing is done

  void set_degrees();

public:
  monoid_info();
  ~monoid_info();

  monoid_info(MonomialOrdering *mo,
	      mon_order *mmo,
	      M2_stringarray s,
	      const Monoid *deg_monoid,
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
  int nwords;
  int top_bits;			// Only needed for 'divides'.
  int bit_mask;
  int mon_bound;		// Entries this size or larger may not be 
				// used.  

  stash *monom_stash;
  int *EXP1, *EXP2, *EXP3;	// allocated ntuples.
				// A local routine may use these ONLY if
				// they call no other monoid routine, except
				// to/from expvector.
  int *MONlocal;		// To be used ONLY by to/from expvector.

  static Monoid *trivial_monoid;

  Monoid(monoid_info *mo, int nbits);
public:
  static Monoid *create(MonomialOrdering *mo,
			M2_stringarray names,
			const Monoid *D,
			M2_arrayint degs);

  static Monoid *tensor_product(const Monoid *M1, const Monoid *M2);

  ~Monoid();

  monoid_info *get_private_monoid_info() const { return moninfo; }

  static Monoid *get_trivial_monoid();

  bool is_group() const { return moninfo->isgroup; }

  void from_varpower(const int *vp, int *result) const;
  void to_varpower(const int *m, intarray &result_vp) const;

  void from_expvector(const int *exp, int *result) const;
  void to_expvector(const int *m, int *result_exp) const;

  M2_arrayint to_arrayint(const int *monom) const; /* Returns an exponent vector representation 
						      of the monomial */

  void unpack(const int *m, int *result) const;
  void pack(const int *exp, int *result) const;

  int in_subring(int n, const int *m) const;
  int compare(int nslots, const int *m, const int *n) const;

  int *make_new(const int *d) const;
  int *make_one() const;
  void remove(int *d) const;

  int is_one(const int *m) const;
  void one(int *result) const;
  void copy(const int *m, int *result) const;

  int n_vars()        const { return nvars; }
  int max_degree()    const { return mon_bound; }
  int monomial_size() const { return nwords; }

  void mult(const int *m, const int *n, int *result) const;
  void power(const int *m, int n, int *result) const;
  int compare(const int *m, const int *n) const;
  int compare(const int *m, int mcomp, const int *n, int ncomp) const;
  bool divides(const int *m, const int *n) const;
  void divide(const int *m, const int *n, int *result) const;
  void lcm(const int *m, const int *n, int *result) const;
  void gcd(const int *m, const int *n, int *result) const;
  void monsyz(const int *m, const int *n, int *sm, int *sn) const;

  void elem_text_out(buffer &o, const int *m) const;

  int primary_value(const int *m) const;
  void multi_degree(const int *m, int *result) const;
  int primary_degree(const int *m) const;
  int degree_weights(const int *m, const M2_arrayint wts) const;
  void degree_of_varpower(const int *vp, int *result) const;

  const Monoid *degree_monoid() const { return moninfo->degree_monoid; }
  const int *degree_of_var(int v) const { return moninfo->degree_of_var[v]; }
  int primary_degree_of_var(int v) const { return moninfo->primary_degree_of_var->array[v]; }

  // Infrastructure here
  void text_out(buffer &o) const;

  int          length_of() const      { return n_vars(); }
};

#if 0
// These will become the unsafe versions, I guess, if they are still
// needed...
inline void Monoid::mult(const int *m, const int *n, int *result) const
    { for (int i=0; i<nwords; i++) *result++ = *m++ + *n++; }

inline void Monoid::power(const int *m, int n, int *result) const
    { for (int i=0; i<nwords; i++) *result++ = *m++ * n; }
#endif

// WARNING!! 'divide' assumes that division is possible
inline void Monoid::divide(const int *m, const int *n, int *result) const
    { 
      for (int i=nwords; i>0; i--) 
	*result++ = *m++ - *n++; 
    }

inline int Monoid::compare(const int *m, const int *n) const
{
  int i = nwords;
  if (i == 0) return EQ;
  while (1)
    {
      if (*m > *n) return GT;
      if (*m < *n) return LT;
      if (--i == 0) return EQ;
      m++, n++;
    }
}

inline int Monoid::compare(const int *m, int mcomp, const int *n, int ncomp) const
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
