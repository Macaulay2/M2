// Copyright 1995 Michael E. Stillman

#ifndef _polywrap_hh_
#define _polywrap_hh_

#include "Epoly.hpp"
#include "Ering.hpp"

class PolynomialRing : public object_element
{
  const Polynomials *R;

public:
  PolynomialRing(const Polynomials *RR)
    : R(RR) {}

  ~PolynomialRing () {}

  // Infrastructure
  class_identifier class_id() const { return CLASS_PolynomialRing; }
  type_identifier  type_id () const { return TY_PolynomialRing; }
  const char * type_name   () const { return "PolynomialRing"; }

  PolynomialRing *cast_to_PolynomialRing() { return this; }
  const PolynomialRing *cast_to_PolynomialRing() const { return this; }

  int          int_of() const { return 0; }
  int          length_of() const;

  void         text_out (buffer &o) const;
  void         bin_out  (buffer &o) const;
public:
  const Polynomials *getRing() const { return R; }
};

class Polynomial : public object_element
{
  const PolynomialRing *R;
  poly *val;
public:
  Polynomial(const PolynomialRing *RR)
    : R(RR), val(R->from_int(0)) { bump_up((PolynomialRing *)R); }

  Polynomial  (const PolynomialRing *RR, poly *f) 
    : R(RR), val(f) { bump_up((PolynomialRing *)R); }

  ~Polynomial () { R->remove(val); bump_down((PolynomialRing *)R); }

  // Infrastructure
  class_identifier class_id() const { return CLASS_Polynomial; }
  type_identifier  type_id () const { return TY_Polynomial; }
  const char * type_name   () const { return "Polynomial"; }

  Polynomial cast_to_Polynomial() { return this; }

  int          int_of() const { return 0; }
  int          length_of() const;

  void         text_out (buffer &o) const;
  void         bin_out  (buffer &o) const;
public:
  const PolynomialRing *getPolynomialRing() const { return R; }
  const Polynomials *getRing() const { return R->getRing(); }
  const poly *getPolynomial() const { return val; }
};

#endif
