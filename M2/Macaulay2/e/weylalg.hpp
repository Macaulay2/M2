// Copyright 1997 Michael E. Stillman

#ifndef _weylalg_hh_
#define _weylalg_hh_

#include "polyring.hpp"

class WeylFreeModule;

///// Ring Hierarchy ///////////////////////////////////

class WeylAlgebra : public PolynomialRing
{
  friend class WeylFreeModule;

protected:
  int nderivatives;
  bool homogeneous_weyl_algebra;
  int homog_var;		// Only used if 'homogeneous_weyl_algebra' is true.
  int *derivative;		// a value _derivative[i] = r >= 0 means that i is diff(r).
				// If < 0 : the variable i does not have a diff op.
  int *commutative;		// Same as above, but in opposite direction.
  
  static int binomtop;
  static int diffcoeffstop;
  static int **binomtable;
  static int **diffcoeffstable;

  WeylAlgebra(const Ring *KK, 
	      const Monoid *MF, 
	      int npairs, 
	      const int *deriv, const int *comm,
	      int homog_var);
protected:
  virtual ~WeylAlgebra();

  void initialize1();
  
  void extractDerivativePart(const int *exponents, int *result) const;
  void extractCommutativePart(const int *exponents, int *result) const;
  ring_elem binomial(int top, int bottom) const;
  ring_elem multinomial(const ring_elem a, const int *exptop, const int *expbottom) const;
  bool increment(int *current_derivative, const int *top_derivative) const;

  bool divides(const int *expbottom, const int *exptop) const;
  ring_elem diff_coefficients(const ring_elem c, const int *derivatives, const int *exponents) const;

  Nterm * weyl_diff(
	  const ring_elem c,
	  const int *expf,  // The exponent vector of f
	  const int *derivatives, 
	  const Nterm *g) const;  // An entire polynomial
  vec weyl_diff(
	  const ring_elem c,
	  const int *expf,  // The exponent vector of f
	  const int *derivatives, 
	  const vec g) const;  // An entire polynomial
  vec weyl_diff(
	  const FreeModule *resultF,
	  const ring_elem c,
	  const int *expf,  // The exponent vector of f
	  int component,
	  const int *derivatives, 
	  const Nterm *g) const;  // An entire polynomial
public:
  static WeylAlgebra *create(const Ring *K, const Monoid *MF, 
			     int npairs,
			     const int *derivative,
			     const int *commutative,
			     int homog_var);

  class_identifier class_id() const { return CLASS_WeylAlgebra; }

  virtual FreeModule *make_FreeModule() const;
  virtual FreeModule *make_FreeModule(int n) const;

  virtual bool is_commutative_ring() const { return false; }
  virtual bool is_pid() const       { return false; }
  virtual bool has_gcd() const      { return false; }

  virtual bool is_Z() const         { return false; }
  virtual bool is_poly_ring() const { return true; }
  virtual bool is_weyl_algebra() const { return true; }
  virtual bool is_graded() const    { return isgraded; }
  virtual bool is_expensive() const { return true; }

  virtual const WeylAlgebra *cast_to_WeylAlgebra() const { return this; }

  virtual void text_out(buffer &o) const;

  virtual ring_elem power(const ring_elem f, mpz_t n) const;
  virtual ring_elem power(const ring_elem f, int n) const;
#if 0
  // These are not well defined here, so an error is given.
  virtual ring_elem gcd(const ring_elem f, const ring_elem g) const;
  virtual ring_elem gcd_extended(const ring_elem f, const ring_elem g, 
				  ring_elem &u, ring_elem &v) const;
#endif

  ring_elem multinomial(const int *exptop, const int *exp) const;
  
public:
  virtual ring_elem imp_mult_by_term(const ring_elem f, 
				  const ring_elem c, const int *m) const;

#if 0
public:
  Nterm *resize(const PolynomialRing *R, Nterm *f) const;
#endif
};

#endif
