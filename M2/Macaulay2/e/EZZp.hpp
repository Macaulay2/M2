// Copyright 1998 by Michael Stillman
#ifndef __EZZp_hpp_
#define __EZZp_hpp_

#include "Edefs.hpp"

typedef int field;

class ECoefficientRing : public type
{
public:
  virtual ~ECoefficientRing() {}

  virtual void text_out(ostream &o) const = 0;
  virtual void text_out(buffer &o) const = 0;

  virtual int characteristic() const = 0;
  virtual field from_int(int n) const = 0;
  virtual void remove(field a) const = 0;
  virtual field clone(field a) const = 0;

  virtual field zero() const = 0;
  virtual field one() const = 0;
  virtual field negate(field a) const = 0;
  virtual field add(field a, field b) const = 0;
  virtual field mult(field a, field b) const = 0;
  virtual field divide(field a, field b) const = 0;
  virtual bool is_zero(field a) const = 0;
  virtual bool is_equal(field a, field b) const = 0;

  virtual void elem_text_out(ostream &o, field a) const = 0;
  virtual void elem_text_out(buffer &o, field a) const = 0;

  virtual ECoefficientRing * cast_to_ECoefficientRing() { return this; }
  virtual const ECoefficientRing * cast_to_ECoefficientRing() const { return this; }
  type_identifier  type_id () const { return TY_ECoefficientRing; }
  const char * type_name   () const { return "ECoefficientRing"; }
};

class EZZ : public ECoefficientRing
{
  static EZZ *_ZZ;
public:
  EZZ() : ECoefficientRing() {}
  virtual ~EZZ() {}

  static const EZZ *ZZ() { if (_ZZ == 0) _ZZ = new EZZ; return _ZZ; }
  
  static const EZZ *make() { return ZZ(); }

  virtual void text_out(ostream &o) const;
  virtual void text_out(buffer &o) const;
  void binary_out(ostream &o) const;
  static EZZ *binary_in(istream &i);

  virtual int characteristic() const
    { return 0; }
    
  virtual field from_int(int n) const
    { return n; }
    
  virtual void remove(field a) const
    { }
     
  virtual field clone(field a) const
    { return a; }

  virtual field zero() const
    { return 0; }
    
  virtual field one() const
    { return 1; }
    
  virtual field negate(field a) const
    { return -a; }
    
  virtual field add(field a, field b) const
    { return a+b; }
    
  virtual field mult(field a, field b) const
    { return a*b; }

  virtual field divide(field a, field b) const
    { return a/b; }
    
  virtual bool is_zero(field a) const
    { return a == 0; }
    
  virtual bool is_equal(field a, field b) const
    { return a == b; }

  virtual void elem_text_out(ostream &o, field a) const;
  virtual void elem_text_out(buffer &o, field a) const;
  void elem_binary_out(ostream &o, int a) const;
  int elem_binary_in(istream &i) const;
  
  class_identifier class_id() const { return CLASS_EZZ; }

};
class EZZp : public ECoefficientRing
{
  int P;
public:
  EZZp(int p) : ECoefficientRing(), P(p) {}
  virtual ~EZZp() {}

  static EZZp *make(int p) { return new EZZp(p); }

  virtual void text_out(ostream &o) const;
  virtual void text_out(buffer &o) const;

  virtual int characteristic() const { return P; }

  void binary_out(ostream &o) const;
  static EZZp *binary_in(istream &i);

  void gcd_extended(int a, int b, int &u, int &v, int &g) const;

  void remove(int) const {}
  int clone(int a) const 
    { return a; }

  int zero() const
    { return 0; }
  int one() const
    { return 1; }
  int from_int(int n) const
    { int c = (n % P); if (c < 0) c += P; return c; }

  bool is_zero(int a) const
    { return a == 0; }
  bool is_equal(int a, int b) const
    { return a == b; }
  int mult(int a, int b) const 
    { return from_int(a*b); }
  int add(int a, int b) const 
    { int c = a+b; if (c >= P) c -= P; return c; }
  int invert(int a) const
    { int u,v,g; gcd_extended(a,P,u,v,g); return u; }
  int negate(int a) const
    { return P - a; }
  int divide(int a, int b) const
    { int c = invert(b); return mult(a,c); }

  // I/O
  void elem_text_out(ostream &o, int a) const;
  void elem_text_out(buffer &o, int a) const;
  void elem_binary_out(ostream &o, int a) const;
  int elem_binary_in(istream &i) const;

  class_identifier class_id() const { return CLASS_EZZp; }
};

#endif
