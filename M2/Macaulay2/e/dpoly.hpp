#ifndef __dpoly_h_
#define __dpoly_h_
// Code for univariate polynomials over algebraic extensions of QQ
// and over finite fields

// The basic operations:
//   "monic gcd mod p" over extension fields
//   modular gcd algorithm
// Later, we will extend this to multivariate polynomials and function fields

#include <cstdio>
#include <sstream>
#include "ringelem.hpp"
#include "buffer.hpp"
#include <vector>

class Tower;
class DPolyTraverser;
typedef int *exponents;  // used only in add_term

typedef struct poly_struct *poly;
typedef const struct poly_struct *const_poly;

/**
 * \ingroup polynomialrings
 */

struct poly_struct : public our_new_delete
{
  int deg;
  int len;
  union
  {
    long *ints;   // array of integers.  at level == 0
    poly *polys;  // array of more ptrs to poly structs, at level > 0
  } arr;
};

/**
 * \ingroup polynomialrings
 */
class DPoly
{
  friend class DPolyTraverser;
  int nvars;
  int nlevels;  // #vars is nlevels+1
  poly *extensions;
  long charac;

 private:
  void initialize(long p, int nvars0, const_poly *ext0);

  void reset_degree_0(poly &f);             // possibly sets f to 0
  void reset_degree_n(int level, poly &f);  // ditto

  void mult_by_coeff_0(poly &f, long b);
  void mult_by_coeff_n(int level, poly &f, poly b);
  // f *= b.  b should have level 'level-1'.

  void make_monic_0(poly &f, long &result_multiplier);
  void make_monic_n(int level, poly &f, poly &result_multiplier);

  bool make_monic3(int level, poly &u1, poly &u2, poly &u3);

  static poly read_poly_0(char *&str);
  static poly read_poly_n(char *&str, int level);

  void add_in_place_0(poly &f, const poly g);
  void add_in_place_n(int level, poly &f, const poly g);

  void subtract_in_place_0(poly &f, const poly g);
  void subtract_in_place_n(int level, poly &f, const poly g);

  poly mult_0(const poly f, const poly g, bool reduce_by_extension);
  poly mult_n(int level, const poly f, const poly g, bool reduce_by_extension);

  poly random_0(int deg);
  poly random_n(int level, int deg);

  poly diff_0(const poly f);
  poly diff_n(int level, int whichvar, const poly f);

  poly mult_by_int_0(long c, const poly f);
  poly mult_by_int_n(int level, long c, const poly f);

 public:
  int degree_of_extension(int level);  // if negative, then that variable is
                                       // transcendental over lower vars
  bool down_level(int newlevel, int oldlevel, poly &f);

  static void increase_size_0(int newdeg, poly &f);
  static void increase_size_n(int newdeg, poly &f);
  static poly alloc_poly_n(int deg, poly *elems = 0);
  static poly alloc_poly_0(int deg, long *elems = 0);
  static void dealloc_poly(poly &f);

  static void display_poly(FILE *fil, int level, const poly f);
  static poly read_poly(char *&str, int level);
  static std::ostream &append_to_stream(std::ostream &o,
                                        int level,
                                        const poly f);
  static char *to_string(int level, const poly f);

  static bool is_equal(int level, const poly f, const poly g);
  static poly copy(int level, const_poly f);

  static poly from_long(int level, long c);  // c should be reduced mod p

  static bool is_zero(poly f) { return f == 0; }
  void remove(int level, poly &f);

  int compare(int level, const poly f, const poly g);  // this is a total order

  poly random(int level, int deg);
  poly random(int level);  // obtains a random element, using only variables
                           // which are algebraic over the base

  poly var(int level, int v);  // make the variable v (but at level 'level')

  void add_term(int level,
                poly &result,
                long coeff,
                exponents exp) const;  // modifies result.
  // exp is an array [0..level-1] of exponent values for each variable
  // 0..level-1
  // the outer variable is at index 0.
  // coeff is an already normalized coefficient.

  void negate_in_place(int level, poly &f);
  void add_in_place(int level, poly &f, const poly g);
  void subtract_in_place(int level, poly &f, const poly g);
  poly mult_by_int(int level, long c, const poly f);
  poly mult(int level, const poly f, const poly g, bool reduce_by_extension);
  void remainder(int level, poly &f, const poly g);
  poly division_in_place_monic(int level, poly &f, const poly g);
  bool division_in_place(int level, poly &f, const poly g, poly &result_quot);

  void pseudo_remainder(int level, poly &f, const poly g);
  poly pseudo_division(int level, poly &f, const poly g);

  poly gcd(int level, const poly f, const poly g);
  poly gcd_coefficients(int level,
                        const poly f,
                        const poly g,
                        poly &result_u,
                        poly &result_v);
  poly resultant(int level, poly f, poly g);

  void make_monic(int level, poly &f);
  poly invert(int level, const poly a);

  void normal_form(int level,
                   poly &f);  // hmmm, I need to think this one through...

  void subtract_multiple_to(int level, poly &f, long a, int i, poly g);

  void elem_text_out(buffer &o,
                     int level,
                     const poly f,
                     bool p_one,
                     bool p_plus,
                     bool p_parens,
                     M2_ArrayString names) const;

  void extensions_text_out(buffer &o, M2_ArrayString names) const;

  int degree(int level, int var, const poly f) const;
  poly diff(int level, int var, const poly f);
  poly power_mod(int level, const poly f, mpz_srcptr n, const poly g);  // f^n mod g
  poly lowerP(int level, const poly f);

  static bool is_one(int level, const poly f);
  int index_of_var(int level, const poly f) const;
  void degrees_of_vars(int level,
                       const poly f,
                       std::vector<int> &result_max_degs) const;

  // DPoly management
  ~DPoly() {}
  DPoly(long p, int nvars0, const_poly *extensions = 0);
};

/**
 * \ingroup polynomialrings
 */
class DRing : public our_new_delete
{
  int level;
  mutable DPoly D;
  long P;

  DRing(long charac, int nvars, const_poly *exts);

 public:
  typedef Tower ring_type;
  typedef poly elem;

  DPoly *getDPoly() const { return &D; }
  static DRing *create(long p, int nvars0, const_poly *ext0);
  // ext0 should be an array of poly's of level 'nvars0'? 0..nvars0-1

  void init_set(elem &result, elem a) const { result = a; }
  void set_zero(elem &result) const { result = 0; }
  void set(elem &result, elem a) const
  {
    D.remove(level, result);
    result = a;
  }

  bool is_zero(elem result) const { return result == 0; }
  bool invert(elem &result, elem a) const
  // returns true if invertible.  Returns true if not, and then result is set to
  // 0.
  {
    result = D.invert(level, a);
    return result != 0;
  }

  void add_term(elem &result, long coeff, exponents exp) const;

  void add(elem &result, elem a, elem b) const
  {
    if (a == 0)
      result = b;
    else if (b == 0)
      result = a;
    else
      {
        poly a1 = D.copy(level, a);
        D.add_in_place(level, a1, b);
        result = a1;
      }
  }

  void subtract(elem &result, elem a, elem b) const
  {
    poly a1 = D.copy(level, a);
    D.subtract_in_place(level, a1, b);
    result = a1;
  }

  void subtract_multiple(elem &result, elem a, elem b) const
  {
    if (a == 0 || b == 0) return;
    elem ab = D.mult(level, a, b, true);
    D.subtract_in_place(level, result, ab);
  }

  void mult(elem &result, elem a, elem b) const
  {
    if (a == 0 || b == 0)
      result = 0;
    else
      result = D.mult(level, a, b, true);
  }

  void divide(elem &result, elem a, elem b) const
  {
    if (a == 0 || b == 0)
      result = 0;
    else
      {
        elem a1 = D.copy(level, a);
        if (!D.division_in_place(level, a1, b, result)) result = 0;
        D.dealloc_poly(a1);
      }
  }

  void remainder(elem &result, elem a, elem b) const
  {
    if (a == 0 || b == 0)
      result = 0;
    else
      {
        result = D.copy(level, a);
        D.remainder(level, result, b);
      }
  }

  void to_ring_elem(ring_elem &result, const elem a) const
  {
    poly h = D.copy(level, a);
    result = TOWER_RINGELEM(h);
  }

  void from_ring_elem(elem &result, const ring_elem &a) const
  {
    poly a1 = TOWER_VAL(a);
    result = D.copy(level, a1);
  }

  void swap(elem &a, elem &b) const
  {
    elem tmp = a;
    a = b;
    b = tmp;
  }

  bool is_one(const poly f) { return D.is_one(level, f); }
  bool is_equal(const poly f, const poly g) { return D.is_equal(level, f, g); }
  bool compare(const poly f, const poly g) { return D.compare(level, f, g); }
  bool is_unit(const poly g);  // what does this really do?

  void set_var(poly &result, int n)
  {
    // n from 0..nvars-1, sets result to 0 f n is out of range
    result = D.var(level, n);
  }

  void set_from_long(poly &result, long r)
  {
    r = r % P;
    if (r < 0) r += P;
    result = D.from_long(level, r);
  }

  void set_from_int(poly &result, mpz_srcptr r);  // written

  bool set_from_mpq(poly &result, mpq_srcptr r);  // written

  void set_random(poly &result) { result = D.random(level); }
  void elem_text_out(buffer &o,
                     const poly f,
                     bool p_one,
                     bool p_plus,
                     bool p_parens,
                     M2_ArrayString names) const;

  void gcd(poly &result, const poly f, const poly g)
  {
    result = D.gcd(level, f, g);
  }

  void gcd_coefficients(poly &result_gcd,
                        poly &result_u,
                        poly &result_v,
                        const poly f,
                        const poly g)
  {
    result_gcd = D.gcd_coefficients(level, f, g, result_u, result_v);
  }

  void extensions_text_out(buffer &o, M2_ArrayString names) const
  {
    D.extensions_text_out(o, names);
  }

  int degree(int var, const poly f) const { return D.degree(level, var, f); }
  void diff(int var, poly &result, const poly f) const
  {
    result = D.diff(level, var, f);
  }
  int extension_degree(int firstvar);  // returns -1 if infinite
  void power_mod(poly &result, const poly f, mpz_srcptr n, const poly g) const
  {
    result = D.power_mod(level, f, n, g);
  }  // f^n mod g
  void lowerP(poly &result, const poly f) { result = D.lowerP(level, f); }
  int index_of_var(const poly f) const { return D.index_of_var(level, f); }
  void degrees_of_vars(const poly f, std::vector<int> &result) const
  {
    result.resize(level + 1);
    for (size_t i = 0; i <= level; i++) result[i] = 0;
    D.degrees_of_vars(level, f, result);
  }
};

/**
 * \ingroup polynomialrings
 */
class DPolyTraverser : public our_new_delete
{
  const DPoly *D;

  bool traverse1(int level, const_poly g, exponents exp);

 protected:
  virtual bool viewTerm(long coeff, const exponents exp) = 0;

 public:
  DPolyTraverser(const DRing *D0) : D(D0->getDPoly()) {}
  virtual ~DPolyTraverser() {}
  void traverse(const_poly f);
};

// Format for polynomials in a file:
//  [[,,[,,1,2]],,[1,3,4,,8]]

// write following functions:
//  read_poly, read_polys
//  write_poly, write_polys
//  add, subtract

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
