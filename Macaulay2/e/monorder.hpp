// Copyright 1995 Michael E. Stillman.

#ifndef _mon_order_hh_
#define _mon_order_hh_

#include "object.hpp"
#include "intarray.hpp"

enum mon_order_types
{
  MO_GRLEX = 0,
  MO_RLEX = 1,			// not usable with monoid
  MO_GLEX = 2,
  MO_LEX = 3,
  MO_ELIM = 4,
  MO_WTFCN = 5,
  MO_PRODUCT = 6,
  MO_GENERAL = 7,
  MO_TRIVIAL = 8
};

class mon_order
{
protected:
  mon_order_types ty;
  int n;
  int *degs;
  int **order;
  int **inv_order;
  int *inv_degs;

  mon_order(mon_order_types ty, const intarray &degs);
public:
  virtual ~mon_order();

  static mon_order *trivial();
  static mon_order *grlex(const intarray &degs);
  static mon_order *rlex(const intarray &degs);
  static mon_order *glex(const intarray &degs);
  static mon_order *lex(const intarray &degs);
  static mon_order *elim(const intarray &degs, int i);
  static mon_order *product(const intarray &degs, const intarray &blocks);

  static mon_order *product(const mon_order *m1, const mon_order *m2);
  static mon_order *elim_product(const mon_order *m1, const mon_order *m2);
  static mon_order *graded_product(const mon_order *m1, const mon_order *m2);
  static mon_order *general_order(const intarray &degs, 
				  const intarray &order, 
				  const intarray &invorder,
				  const intarray &invdegs);

  virtual void encode(const int *exp, int *m) const;
  virtual void decode(const int *m, int *exp) const;

  int degree(int v) const { return degs[v]; }
  mon_order_types type() const { return ty; }
  int n_vars() const { return n; }
  virtual void text_out(buffer &o) const;
};

class grlex_mon_order : public mon_order
{
public:
  grlex_mon_order(const intarray &degs);
  ~grlex_mon_order();

  virtual void encode(const int *exp, int *m) const;
  virtual void decode(const int *m, int *exp) const;
  virtual void text_out(buffer &o) const;
};

class grlex1_mon_order : public mon_order
{
public:
  grlex1_mon_order(const intarray &degs);
  ~grlex1_mon_order();

  virtual void encode(const int *exp, int *m) const;
  virtual void decode(const int *m, int *exp) const;
  virtual void text_out(buffer &o) const;
};
class product_mon_order : public mon_order
{
  int nblocks;
  int *blocks;
public:
  product_mon_order(const intarray &degs, const intarray &blocks);
  ~product_mon_order();

  virtual void encode(const int *exp, int *m) const;
  virtual void decode(const int *m, int *exp) const;
  virtual void text_out(buffer &o) const;
};
class elim_mon_order : public mon_order
{
  int nelim;			// Number of variables to eliminate
public:
  elim_mon_order(const intarray &degs, int n);
  ~elim_mon_order();

  virtual void encode(const int *exp, int *m) const;
  virtual void decode(const int *m, int *exp) const;
  virtual void text_out(buffer &o) const;
};

class object_mon_order : public type
{
public:
  const mon_order *val;

  object_mon_order() {}
  object_mon_order(const mon_order *mo) : val(mo) {}
  ~object_mon_order() {}

  const mon_order * mon_order_of() const { return val; }

  object_types type_of         () const { return TY_MON_ORDER; }
  const char * type_name       () const { return "monorder"; }
  object_mon_order *       cast_to_mon_order()       { return this; }
  const object_mon_order * cast_to_mon_order() const { return this; }
  
  int length_of() const { return val->n_vars(); }

  void text_out(buffer &o) const 
    {
      if (val != NULL) val->text_out(o);
    }
};

#endif
