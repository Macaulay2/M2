// Copyright 2010 Michael E. Stillman.
#ifndef _tower_hpp_
#define _tower_hpp_

#include "relem.hpp"

class RingMap;

class DRing;

class Tower : public Ring
{
  friend class TowerEvaluator;
  int level;
  int nvars;

  M2_ArrayString names;

  DRing *D;

protected:
  Tower() {}

  bool initialize(long charac0, M2_ArrayString names0, const VECTOR(ring_elem) &extensions);
public:
  virtual ~Tower();

  Tower * cast_to_Tower() { return this; }
  const Tower * cast_to_Tower() const { return this; }

  int n_vars() const { return nvars; }

  static Tower * create(int charac, M2_ArrayString names);
  static Tower * create(const Tower *R, M2_ArrayString new_names);
  static Tower * create(const Tower *R, VECTOR(ring_elem) &extensions);

// The following are all the routines required by 'ring'
  virtual unsigned int computeHashValue(const ring_elem a) const;

  virtual void text_out(buffer &o) const;

  virtual ring_elem from_long(long n) const;
  virtual ring_elem from_int(mpz_ptr n) const;
  virtual ring_elem var(int v) const;
  virtual ring_elem from_rational(mpq_ptr q) const;

  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;
  // promote and lift work between what rings?

  virtual ring_elem eval(const RingMap *map, const ring_elem f, int first_var) const;

  virtual bool is_unit(const ring_elem f) const;
  virtual bool is_zero(const ring_elem f) const;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const;
  virtual int compare_elems(const ring_elem f, const ring_elem g) const;

  virtual ring_elem copy(const ring_elem f) const;
  virtual void remove(ring_elem &f) const;

  virtual ring_elem negate(const ring_elem f) const;
  virtual ring_elem add(const ring_elem f, const ring_elem g) const;
  virtual ring_elem subtract(const ring_elem f, const ring_elem g) const;
  virtual ring_elem mult(const ring_elem f, const ring_elem g) const;
  virtual ring_elem invert(const ring_elem f) const;
  virtual ring_elem divide(const ring_elem f, const ring_elem g) const;

  virtual ring_elem remainder(const ring_elem f, const ring_elem g) const;

  virtual ring_elem random() const;

  virtual void elem_text_out(buffer &o,
                             const ring_elem f,
                             bool p_one=true,
                             bool p_plus=false,
                             bool p_parens=false) const;

  virtual void syzygy(const ring_elem a, const ring_elem b,
                      ring_elem &x, ring_elem &y) const;

  virtual int index_of_var(const ring_elem a) const;
  virtual M2_arrayint support(const ring_elem a) const;

  ring_elem gcd(const ring_elem f, const ring_elem g) const;
  ring_elem gcd_extended(const ring_elem f, const ring_elem g,
                         ring_elem &u, ring_elem &v) const;

  // These routines are here so we can write higher level operations in M2 to test the (eventual) engine routines
  int degreeInVariable(int var, const ring_elem f) const;
  ring_elem differentiate(int var, const ring_elem f) const;
  int extension_degree(int nvars) const; // returns -1 if infinite
  ring_elem power_mod(const ring_elem f, mpz_t n, const ring_elem g) const;  // f^n mod g
  ring_elem lowerP(const ring_elem f) const;

  ring_elem translate(const PolynomialRing *R, ring_elem fR) const;
  // translate the element fR into a dpoly for this ring

};


class RingElement;

extern const RingElement *towerGCD(const RingElement *F,
                                   const RingElement *G);
extern const RingElement *towerExtendedGCD(const RingElement *F,
                                           const RingElement *G,
                                           const RingElement **A,
                                           const RingElement **B);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
