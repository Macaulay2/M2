// Copyright 2010-2012 Michael E. Stillman.
#ifndef _aring_tower_hpp_
#define _aring_tower_hpp_

#include <vector>
#include <string>
#include "aring-zzp-ffpack.hpp"

#include "style.hpp"
#include "aring.hpp"
#include "ringelem.hpp"

class RingMap;

namespace M2 {
/**
 * \ingroup polynomialrings
 */
typedef struct ARingPolynomialStruct *ARingPolynomial;

struct ARingPolynomialStruct
{
  int deg;
  int len;
  union
  {
    ARingZZpFFPACK::ElementType *coeffs;
    //      long* ints;  // array of integers.  at level == 0
    ARingPolynomial *polys;  // array of more ptrs to poly structs, at level > 0
  };
};

typedef int *exponents;

class DRing;

// TODO: make this a template type, with the base e.g. ZZ/p, ZZ, QQ, etc.
/**
\ingroup rings
*/
class ARingTower : public RingInterface
{
  friend class ARingTowerEvaluator;

 public:
  typedef ARingZZpFFPACK BaseRingType;
  typedef BaseRingType::ElementType BaseCoefficientType;

  static const RingID ringID = ring_tower_ZZp;
  typedef ARingPolynomial ElementType;
  typedef ElementType elem;

  //////////////////////////////
  // Creation and destruction //
  //////////////////////////////

  ARingTower(const BaseRingType &baseRing,
             const std::vector<std::string> &names,
             const std::vector<ElementType> &extensions);

  virtual ~ARingTower();

  // TODO: the interface for these three need to change
  static const ARingTower *create(const BaseRingType &baseRing,
                                  const std::vector<std::string> &names);
  static const ARingTower *create(const ARingTower &R,
                                  const std::vector<std::string> &new_names);
  static const ARingTower *create(const ARingTower &R,
                                  const std::vector<ElementType> &extensions);

  size_t n_vars() const { return mNumVars; }
  const ARingZZpFFPACK &baseRing() const { return mBaseRing; }
  const std::vector<std::string> &varNames() const { return mVarNames; }
  unsigned long characteristic() const { return mBaseRing.characteristic(); }
  ///////////////////////////////////
  // The following are he routines required to be of type "RingInterface"
  ///////////////////////////////////

  unsigned int computeHashValue(const elem &a) const { return a->deg; }
  void text_out(buffer &o) const;

  /////////////////////////////////////////////////////////
  // Routines to help in switch from coeffrings to aring //
  // these will be renamed or go away (hopefully) /////////
  /////////////////////////////////////////////////////////
  void init_set(elem &result, elem a) const {}  // TODO: write this
  void set(elem &result, elem a) const {}       // TODO: write this
  /////////////////////////////////
  // ElementType informational ////
  /////////////////////////////////

  bool is_unit(ElementType f) const { return false; }  // TODO: write this
  bool is_zero(ElementType f) const { return f == NULL; }
  bool is_equal(ElementType f, ElementType g) const
  {
    return is_equal(mStartLevel, f, g);
  }

  int compare_elems(ElementType f, ElementType g) const
  {
    // TODO: write this
    return 0;
  }

  ////////////////////////////
  // to/from ringelem ////////
  ////////////////////////////
  // These simply repackage the element as either a ringelem or an
  // 'ElementType'.
  // No reinitialization is done.
  // Do not take the same element and store it as two different ring_elem's!!
  void to_ring_elem(ring_elem &result, const ElementType &a) const
  {
    ElementType b = const_cast<ElementType>(a);
    result.poly_val = reinterpret_cast<Nterm *>(b);
  }

  void from_ring_elem(ElementType &result, const ring_elem &a) const
  {
    Nterm *b = const_cast<Nterm *>(a.poly_val);
    result = reinterpret_cast<ElementType>(b);
  }

  // 'init', 'init_set' functions

  void init(elem &result) const { result = NULL; }
  void clear(elem &f) const { clear(mStartLevel, f); }
  void set_zero(elem &result) const { result = NULL; }
  void copy(elem &result, elem a) const { result = copy(mStartLevel, a); }
  void set_from_long(elem &result, long a) const
  {  // TODO: write this
  }

  // v from 0..n_vars()-1, sets result to 0 if v is out of range
  void set_var(elem &result, int v) const { result = var(mStartLevel, v); }
  void set_from_mpz(elem &result, mpz_srcptr a) const
  {
    assert(false);
  }  // TODO: write this

  bool set_from_mpq(elem &result, mpq_srcptr a) const
  {
    assert(false);
    return false;
  }  // TODO: write this

  bool set_from_BigReal(elem &result, gmp_RR a) const { return false; }
  // arithmetic
  void negate(elem &result, elem a) const
  {
    result = copy(mStartLevel, a);
    negate_in_place(mStartLevel, result);
  }

  // we silently assume that a != 0.  If it is, result is set to a^0, i.e. 1
  void invert(elem &result, elem a) const {}  // TODO: write this
  void add(elem &result, elem a, elem b) const
  {
    if (a == 0)
      result = b;
    else if (b == 0)
      result = a;
    else
      {
        ARingPolynomial a1 = copy(mStartLevel, a);
        add_in_place(mStartLevel, a1, b);
        result = a1;
      }
  }  // TODO: should b be consumed?

  void subtract(elem &result, elem a, elem b) const
  {
    result = copy(mStartLevel, a);
    subtract_in_place(mStartLevel, result, b);
  }  // TODO: write this

  void subtract_multiple(elem &result, elem a, elem b) const {}  // TODO: write
                                                                 // this
  void mult(elem &result, elem a, elem b) const {}          // TODO: write this
  void divide(elem &result, elem a, elem b) const {}        // TODO: write this
  void power(elem &result, elem a, int n) const {}          // TODO: write this
  void power_mpz(elem &result, elem a, mpz_srcptr n) const {}  // TODO: write this
  void swap(ElementType &a, ElementType &b) const {}        // TODO: write this
  void elem_text_out(buffer &o,
                     ElementType a,
                     bool p_one = true,
                     bool p_plus = false,
                     bool p_parens = false) const
  {
    elem_text_out(o, mStartLevel, a, p_one, p_plus, p_parens);
  }

  // returns x,y s.y. x*a + y*b == 0.
  // if possible, x is set to 1.
  // no need to consider the case a==0 or b==0.
  void syzygy(ElementType a,
              ElementType b,
              ElementType &x,
              ElementType &y) const
  {
  }  // TODO: write this

  void random(ElementType &result) const {}  // TODO: write this
  void eval(const RingMap *map,
            const elem f,
            int first_var,
            ring_elem &result) const
  {
  }  // TODO: write this

  // f *= b, where b is an element in mBaseRing
  void mult_by_coeff(ARingPolynomial &f, const BaseCoefficientType &b) const;

 private:
  void extensions_text_out(buffer &o) const;  // TODO: write this

  void elem_text_out(buffer &o,
                     int level,
                     const ElementType f,
                     bool p_one,
                     bool p_plus,
                     bool p_parens) const;

 private:
  bool is_one(int level, const ARingPolynomial f) const;  // TODO: write this
  bool is_equal(int level, const ARingPolynomial f, const ARingPolynomial g) const;

  ARingPolynomial alloc_poly_n(int deg) const;
  ARingPolynomial alloc_poly_0(int deg) const;
  void dealloc_poly(ARingPolynomial &f) const;

  ARingPolynomial copy(int level, const ARingPolynomial f) const;

  // possibly increase the capacity of 'f', to accommodate polynomials of degree
  // 'newdeg'
  void increase_capacity(int newdeg, ARingPolynomial &f) const;

  // sets the (top level) degree of f to be correct.  If f is the 0 poly, then f
  // is deallocated
  void reset_degree(ARingPolynomial &f) const;

  // Create a polynomial at level 'level', representing the variable 'v'
  // v should be in the range 0..mNumVars-1.  If not, then the 0 elem is
  // returned.
  // ASSUMPTION: level >= v.  If not, 0 is returned.
  ARingPolynomial var(int level, int v) const;

  // f += g.  f and g should both be of level 'level'
  void add_in_place(int level, ARingPolynomial &f, const ARingPolynomial g) const;

  void subtract_in_place(int level, ARingPolynomial &f, const ARingPolynomial g) const;

  void negate_in_place(int level, ARingPolynomial &f) const;

  void mult_by_coeff(int level, ARingPolynomial &f, const BaseCoefficientType &b) const;

  // free all space associated to f, set f to 0.
  void clear(int level, ARingPolynomial &f) const;

 private:
  const ARingZZpFFPACK &mBaseRing;
  int mStartLevel;
  int mNumVars;

  const std::vector<std::string> mVarNames;
  std::vector<ElementType> mExtensions;
};

};  // M2 namespace

#endif

#if 0
  public:
    bool is_unit(ElementType f) const;
    bool is_zero(ElementType f) const { return f == 0; }
    bool is_equal(ElementType f, ElementType g) const { return mRing.is_equal(mStartLevel, f, g); }
    int compare_elems(ElementType f, ElementType g) const { return mRing.compare(mStartLevel, f, g); }

    //TODO: NO! this should be a copy!!
    void init_set(ElementType &result, ElementType a) const { result = a; } 

    //TODO: NO! this should be a copy!!
    void set(ElementType &result, ElementType a) const { mRing.remove(mStartLevel, result); result = a; }

    //TODO: should this remove previous value??
    void set_zero(ElementType &result) const { result = 0; }
    
    
    void set_from_long(ElementType &result, long r) {
      r = r % mCharacteristic;
      if (r < 0) r += P;
      result = mRing.from_long(mStartLevel, r);
    }
    
    void set_from_int(ElementType &result, mpz_ptr r);
    
    bool set_from_mpq(ElementType &result, mpq_srcptr r);
    
    void set_random(ElementType &result) { result = mRing.random(mStartLevel); }
    
    bool invert(ElementType &result, ElementType a) const
    // returns true if invertible.  Returns false if not, and then result is set to 0.
    {
      result = mRing.invert(mStartLevel, a);
      return result != 0;
    }
    
    void add(ElementType &result, ElementType a, ElementType b) const
    {
      if (a == 0) result = b;
      else if (b == 0) result = a;
      else
        {
          ElementType a1 = mRing.copy(mStartLevel, a);
          mRing.add_in_place(mStartLevel, a1, b);
          result = a1;
        }
    }
    
    void subtract(ElementType &result, ElementType a, ElementType b) const
    {
      ElementType a1 = mRing.copy(mStartLevel, a);
      mRing.subtract_in_place(mStartLevel, a1, b);
      result = a1;
    }
    
    void subtract_multiple(ElementType &result, ElementType a, ElementType b) const
    {
      if (a == 0 || b == 0) return;
      ElementType ab = mRing.mult(mStartLevel,a,b,true);
      mRing.subtract_in_place(mStartLevel, result, ab);
    }
    
    void mult(ElementType &result, ElementType a, ElementType b) const
    {
      if (a == 0 || b == 0)
        result = 0;
      else
        result = mRing.mult(mStartLevel, a, b, true);
    }
    
    void divide(ElementType &result, ElementType a, ElementType b) const
    {
      if (a == 0 || b == 0)
        result = 0;
      else
        {
          ElementType a1 = mRing.copy(mStartLevel, a);
          if (!mRing.division_in_place(mStartLevel, a1, b, result))
            result = 0;
          mRing.dealloc_poly(a1);
        }
    }
    
    void remainder(ElementType &result, ElementType a, ElementType b) const
    {
      if (a == 0 || b == 0)
        result = 0;
      else
        {
          result = mRing.copy(mStartLevel, a);
          mRing.remainder(mStartLevel, result, b);
        }
    }
    
    void to_ring_elem(ring_elem &result, const ElementType a) const
    {
      ElementType h = mRing.copy(mStartLevel, a);
      result = TOWER_RINGELEM(h);
    }
    
    void from_ring_elem(ElementType &result, const ring_elem &a) const
    {
      ElementType a1 = reinterpret_cast<TowerPolynomial>(a.poly_val);
      result = mRing.copy(mStartLevel, a1);
    }
    
    void swap(ElementType &a, ElementType &b) const
    {
      ElementType tmp = a;
      a = b;
      b = tmp;
    }
    
    void elem_text_out(buffer &o,
                       const ElementType f,
                       bool p_one,
                       bool p_plus,
                       bool p_parens) const;
    
    void gcd(ElementType &result, const ElementType f, const ElementType g) { result = mRing.gcd(mStartLevel,f,g); }
    
    void gcd_coefficients(ElementType &result_gcd,
                          ElementType &result_u, ElementType &result_v,
                          const ElementType f, const ElementType g)
    {
      result_gcd = mRing.gcd_coefficients(mStartLevel, f, g, result_u, result_v);
    }
    
    
    int degree(int var, const ElementType f) const { return mRing.degree(mStartLevel,var,f); }
    void diff(int var, ElementType &result, const ElementType f) const { result = mRing.diff(mStartLevel, var, f); }
    int extension_degree(int firstvar); // returns -1 if infinite
    void power_mod(ElementType &result, const ElementType f, mpz_srcptr n, const ElementType g) const { result = mRing.power_mod(mStartLevel, f, n, g); } // f^n mod g
    void lowerP(ElementType &result, const ElementType f) { result = mRing.lowerP(mStartLevel, f); }
    
  private:
    void extensions_text_out(buffer &o) const;

    /////////////////////////////////////
    // Predicates ///////////////////////
    /////////////////////////////////////
    bool is_equal(int level, const ARingPolynomial f, const ARingPolynomial g);
    bool is_zero(ARingPolynomial f) { return f == 0; }

    /////////////////////////////////////
    // Construction of new elements /////
    /////////////////////////////////////

    ARingPolynomial copy(int level, const_poly f);
    ARingPolynomial var(int level, int v); // make the variable v (but at level 'level')
    ARingPolynomial from_long(int level, long c);  // c should be reduced mod p
    
    /////////////////////////////////////
    // Private routines for arithmetic //
    /////////////////////////////////////
    void reset_degree_0(ARingPolynomial &f); // possibly sets f to 0
    void reset_degree_n(int level, ARingPolynomial &f); // ditto

    void mult_by_coeff_0(ARingPolynomial &f, long b);
    void mult_by_coeff_n(int level, ARingPolynomial &f, ARingPolynomial b);
    // f *= b.  b should have level 'level-1'.

    void make_monic_0(ARingPolynomial & f, long &result_multiplier);
    void make_monic_n(int level, ARingPolynomial & f, ARingPolynomial &result_multiplier);

    bool make_monic3(int level, ARingPolynomial & u1, ARingPolynomial &u2, ARingPolynomial &u3);


    void add_in_place_0(ARingPolynomial &f, const ARingPolynomial g);
    void add_in_place_n(int level, ARingPolynomial &f, const ARingPolynomial g);
    void add_in_place(int level, ARingPolynomial &f, const ARingPolynomial g);

    void subtract_in_place_0(ARingPolynomial &f, const ARingPolynomial g);
    void subtract_in_place_n(int level, ARingPolynomial &f, const ARingPolynomial g);
    void subtract_in_place(int level, ARingPolynomial &f, const ARingPolynomial g);

    ARingPolynomial mult_0(const ARingPolynomial f, const ARingPolynomial g, bool reduce_by_extension);
    ARingPolynomial mult_n(int level, const ARingPolynomial f, const ARingPolynomial g, bool reduce_by_extension);
    ARingPolynomial mult(int level, const ARingPolynomial f, const ARingPolynomial g, bool reduce_by_extension);

    ARingPolynomial random_0(int deg);
    ARingPolynomial random_n(int level, int deg);
    ARingPolynomial random(int level, int deg);
    ARingPolynomial random(int level); // obtains a random element, using only variables which are algebraic over the base

    ARingPolynomial diff_0(const ARingPolynomial f);
    ARingPolynomial diff_n(int level, int whichvar, const ARingPolynomial f);

    ARingPolynomial mult_by_int_0(long c, const ARingPolynomial f);
    ARingPolynomial mult_by_int_n(int level, long c, const ARingPolynomial f);
    ARingPolynomial mult_by_int(int level, long c, const ARingPolynomial f);

    /////////////////////////////////////
    // Translation to/from other rings //
    /////////////////////////////////////
    void add_term(int level, ARingPolynomial &result, long coeff, exponents exp) const; // modifies result.
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
