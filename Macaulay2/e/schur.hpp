// Copyright 1996 Michael E. Stillman

#ifndef _Schurring_hh_
#define _Schurring_hh_

#include "polyring.hpp"

const int SCHUR_MAX_WT = 100;
const int LARGE_NUMBER = 32000;

class tableau
{
  friend class SchurRing;

  int dim;
  int maxwt;
  int wt;
  int *lambda;			// A partition vector 0..nvars-1
  int *p;			// A partition vector 0..nvars-1
  int *xloc;			// array 1..|p|-|lambda| of the horizontal location
				// of this number in the skew table
  int *yloc;			// array 1..|p|-|lambda| of the vertical location
				// of this number in the skew table

  tableau(int nvars);
  ~tableau();
  void resize(int max_wt);

  int elem(int x, int y) const;
  void fill(int *lamb, int *pp);
  void display() const;
};

class SchurRing : public PolynomialRing
{
private:
  // These are variables that are used in the recursive routine 
  // SM(), which is called from skew_schur().
  tableau SMtab;
  tableau SMfilled;
  int SMcurrent;
  int SMfinalwt;
  Nterm *SMresult;
  intarray part_exp_a;		// Used in to_partition, from_partition
  int *part_exp;		// Used in to_partition, from_partition

  void to_partition(const int *m, int *exp) const;
    // exp[1]..exp[nvars] are set
  void from_partition(const int *exp, int *m) const;

  void bounds(int &lo, int &hi);
  void SM();
  Nterm *skew_schur(int *lambda, int *p);
  ring_elem mult_monomials(const int *m, const int *n);
public:
  SchurRing(const Ring *KK, const Monoid *MMF);
  ~SchurRing();

  class_identifier class_id() const { return CLASS_SchurRing; }

  virtual const SchurRing * cast_to_SchurRing() const { return this; }
  virtual       SchurRing * cast_to_SchurRing()       { return this; }

  virtual void text_out(buffer &o) const;
  virtual void elem_text_out(buffer &o, const ring_elem f) const;

  void dimension(const int *exp, mpz_t result) const;
  ring_elem dimension(const ring_elem f) const;

  virtual ring_elem mult_by_term(const ring_elem f, 
				  const ring_elem c, const int *m) const;

  ring_elem power(const ring_elem f, mpz_t n) const;
  ring_elem power(const ring_elem f, int n) const;
};

#endif
