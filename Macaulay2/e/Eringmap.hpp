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

    ERingElement coeff;		// this variable maps to coeff*monom*bigelem
				// where coeff is 1 if isone is true.
				// and   monom is 1 if mon_isone is true,
				// and   bigelem is 1 if bigone_isone is true.
				// coeff is an element of type K.
    const monomial *monom;	// This is a monomial in R.
    ERingElement bigelem;
  };

  bool ispolyring;
  const ERing *R;		// This is the target ring.
  const EMonoid *M;		
  const ERing *K;		// This is the coefficient ring of the target, 
				// if R is a polynomial ring
  int nvars;			// Number of variables in the source ring.
  var *_elem;			// elem[i] is the structure representing the image of
				// the i th variable.
public:
  ERingMap(const EMatrix *m);
  ~ERingMap();

  virtual void text_out(buffer &o) const;
  virtual void bin_out(buffer &o) const;
  // static EFreeModule *binary_in(istream &i);

  virtual ERingMap * cast_to_ERingMap() { return this; }
  virtual const ERingMap * cast_to_ERingmap() const { return this; }

  class_identifier class_id() const { return CLASS_ERingMap; }
  type_identifier  type_id () const { return TY_ERingMap; }
  const char * type_name   () const { return "ERingMap"; }

public:
  const ERing *getTarget() const { return R; }

  ERingElement evaluateTerm(
			const ERing *K,
			ERingElement a, const intarray &varexp) const;

  EVector evaluateVectorTerm(const EFreeModule *Ftarget,
			const ERing *K,
			ERingElement a, const intarray &varexp, int x) const;

  // Each ring type must implement:
  // Evector *evaluate(const ERingMap *map, const EFreeModule *Ftarget,
  //                   const EVector *v)
  // This routine should call evaluateTerm

  ERingElement evaluate(const ERing *source, ERingElement f) const;
  EVector evaluate(const EFreeModule *Ftarget, const EVector &v) const;
  EMatrix *evaluate(const EFreeModule *Ftarget, const EMatrix *m) const;
};

#endif

