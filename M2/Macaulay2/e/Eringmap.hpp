// Copyright 1998 by Michael Stillman
#ifndef __Eringmap_hpp_
#define __Eringmap_hpp_

#include "Evector.hpp"

class ERingMap : public type
{
  struct var {
    bool is_zero;		// Does this variable map to 0?

    bool coeff_is_one;
    bool monom_is_one;
    bool bigelem_is_one;

    field coeff;		// this variable maps to coeff*monom*bigelem
				// where coeff is 1 if isone is true.
				// and   monom is 1 if mon_isone is true,
				// and   bigelem is 1 if bigone_isone is true.
				// coeff is an element of type K.
    monomial *monom;		// This is a monomial in R.
    EVector *bigelem;
  };

  const EPolynomialRing *R;	// This is the target ring.

  int nvars;			// Number of variables in the source ring.
  var *_elem;			// elem[i] is the structure representing the image of
				// the i th variable.
public:
  ERingMap(const EMatrix &m);
  ~ERingMap();

  void text_out(ostream &o) const;
  virtual void text_out(buffer &o) const;
  void binary_out(ostream &o) const;
  static EFreeModule *binary_in(istream &i);

  virtual ERingMap * cast_to_ERingMap() { return this; }
  virtual const ERingMap * cast_to_ERingmap() const { return this; }

  class_identifier class_id() const { return CLASS_ERingMap; }
  type_identifier  type_id () const { return TY_ERingMap; }
  const char * type_name   () const { return "ERingMap"; }

public:
  const EPolynomialRing *getTarget() const { return R; }

  EVector *evaluateTerm(const EFreeModule *Ftarget,
			const ECoefficientRing *K, const EMonoid *M, 
			field a, monomial *m, int x) const;

  // Each ring type must implement:
  // Evector *evaluate(const ERingMap *map, const EFreeModule *Ftarget,
  //                   const EVector *v)
  // This routine should call evaluateTerm

  EVector *evaluate(const EFreeModule *Ftarget, EVector *v) const;
  EMatrix *evaluate(const EFreeModule *Ftarget, EMatrix *m) const;
};

#endif

