// Copyright 1995 Michael E. Stillman.

#ifndef _mon_order_hh_
#define _mon_order_hh_

#include "hash.hpp"
#include "intarray.hpp"

enum mon_order_types
{
  MO1_GRLEX = 0,
  MO1_RLEX = 1,			// not usable with monoid
  MO1_GLEX = 2,
  MO1_LEX = 3,
  MO1_ELIM = 4,
  MO1_WTFCN = 5,
  MO1_PRODUCT = 6,
  MO1_GENERAL = 7,
  MO1_TRIVIAL = 8
};

class mon_order
{
protected:
  mon_order_types ty;
  int n;

  int nweights;
  int *weights;  // array 0..n*nweights-1.

  int *degs;
  int **order;
  int **inv_order;
  int *inv_degs;

  mon_order(mon_order_types ty, const M2_arrayint degs, const M2_arrayint wts);
  void set_weights(const int *exp, int *m) const;
public:
  virtual ~mon_order();

  static mon_order *trivial();
  static mon_order *grlex(const M2_arrayint degs,const M2_arrayint wts);
  static mon_order *rlex(const M2_arrayint degs,const M2_arrayint wts);
  static mon_order *glex(const M2_arrayint degs,const M2_arrayint wts);
  static mon_order *lex(const M2_arrayint degs,const M2_arrayint wts);
  static mon_order *elim(const M2_arrayint degs, unsigned int i,const M2_arrayint wts);
  static mon_order *product(const M2_arrayint degs, const M2_arrayint blocks,
			    const M2_arrayint wts);

  static mon_order *product(const mon_order *m1, const mon_order *m2);
  static mon_order *elim_product(const mon_order *m1, const mon_order *m2);
  static mon_order *graded_product(const mon_order *m1, const mon_order *m2);
  static mon_order *general_order(const M2_arrayint degs, 
				  const M2_arrayint order, 
				  const M2_arrayint invorder,
				  const M2_arrayint invdegs);

  virtual void encode(const int *exp, int *m) const;
  virtual void decode(const int *m, int *exp) const;

  int n_weights() const { return nweights; }

  int degree(int v) const { return degs[v]; }
  mon_order_types type() const { return ty; }
  int n_vars() const { return n; }
  virtual void text_out(buffer &o) const;
};

class grlex_mon_order : public mon_order
{
public:
  grlex_mon_order(const M2_arrayint degs, const M2_arrayint weights);
  ~grlex_mon_order();

  virtual void encode(const int *exp, int *m) const;
  virtual void decode(const int *m, int *exp) const;
  virtual void text_out(buffer &o) const;
};

class grlex1_mon_order : public mon_order
{
public:
  grlex1_mon_order(const M2_arrayint degs, const M2_arrayint weights);
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
  product_mon_order(const M2_arrayint degs, const M2_arrayint blocks,
		    const M2_arrayint weights);
  ~product_mon_order();

  virtual void encode(const int *exp, int *m) const;
  virtual void decode(const int *m, int *exp) const;
  virtual void text_out(buffer &o) const;
};
class elim_mon_order : public mon_order
{
  int nelim;			// Number of variables to eliminate
public:
  elim_mon_order(const M2_arrayint degs, unsigned int n, const M2_arrayint weights);
  ~elim_mon_order();

  virtual void encode(const int *exp, int *m) const;
  virtual void decode(const int *m, int *exp) const;
  virtual void text_out(buffer &o) const;
};

class IM2_mon_order : public mutable_object
{
public:
  mon_order *val;

  IM2_mon_order() {}
  IM2_mon_order(mon_order *mo) : val(mo) {}
  ~IM2_mon_order() {}

  mon_order * grab_mon_order(); // Note: can only use monomial order once!

  int length_of() const { return val->n_vars(); }

  void text_out(buffer &o) const 
    {
      if (val != NULL) val->text_out(o);
    }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
