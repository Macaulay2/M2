// Copyright 2012 Michael E. Stillman

#ifndef _aring_gf_m2_hpp_
#define _aring_gf_m2_hpp_

#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"

#include "aring-glue.hpp"

class GF;
class PolynomialRing;
class RingElement;

namespace M2 {

  /// ingroup rings
  ///
  /// @brief 
  ///
  class GaloisFieldTable
  {
    friend class GF;
    friend class ARingGFM2;
  public:
    
    /// R should be the ring of the element prim.
    /// preferably, prim is the generator of R,
    /// but it is allowed to be something else as well.
    GaloisFieldTable(const PolynomialRing &R,
                     const ring_elem prim);

    // debug display of the tables
    void display(std::ostream &o) const;

    size_t characteristic() const { return mCharac; }
    size_t dimension() const { return mDimension; }
    size_t order() const { return mOrder; }
    size_t one() const { return mOne; }
    size_t minusOne() const { return mMinusOne; }
    size_t orderMinusOne() const { return mOrderMinusOne; }
    size_t oneTable(size_t a) const { return mOneTable[a]; }
    size_t fromZZTable(size_t a) const { return mFromIntTable[a]; }
    
    const PolynomialRing& ring() const { return mOriginalRing; }
    const ring_elem primitiveElement() const { return mPrimitiveElement; }
    size_t generatorExponent() const { return mGeneratorExponent; }
  private:
    // CONSTANT usable fields. 
    size_t mCharac;
    size_t mDimension;
    size_t mOrder;
    size_t mOne;
    size_t mMinusOne;
    size_t mOrderMinusOne;
    
    size_t* mOneTable;
    size_t* mFromIntTable;
    
    const PolynomialRing& mOriginalRing;
    const ring_elem mPrimitiveElement; // is an element of mOriginalRing
    size_t mGeneratorExponent;  
    // the given generator of mOriginalRing is 
    // mPrimitiveElement^mGeneratorExponent (in this ring).
    
  };

/**
\ingroup rings
*/


  class ARingGFM2 : public RingInterface
  {

    

  public:
    static const RingID ringID = ring_GFM2;
    typedef ConcreteRing<ARingGFM2> ring_type;
    typedef int ElementType;
    typedef int elem;
    
    /// a is a polynomial in a ring R = ZZ/p[x]/(f(x))
    /// where 
    ///  (a) f(x) is irreducible of degree n
    ///  (b) a is a primitive element of mOriginalRing, i.e.
    ///     a non-zero element such that
    ///     a^(p^n-1) == 1, and no smaller power has this property.
    ///
    /// We also assume that these elements are chosen (for different GF rings)
    /// such that if (GF(p^m) sits inside GF(p^n) (i.e. m|n), then the inclusion
    /// is given by 0 --> 0, and a --> a^N, where N = (p^n-1)/(p^m-1).
    ARingGFM2(const PolynomialRing &R,
              const ring_elem a);

    size_t characteristic() const { return mGF.characteristic(); }

    void text_out(buffer &o) const;

  private:
    GaloisFieldTable mGF;

    ////////////////////////////////
    /// Arithmetic functions ///////
    ////////////////////////////////

    static inline int modulus_add(int a, int b, int p)
    {
      int t = a+b;
      return (t <= p ? t : t-p);
    }
    
    static inline int modulus_sub(int a, int b, int p)
    {
      int t = a-b;
      return (t <= 0 ? t+p : t);
    }

  public:
    int get_repr(elem f) const { /*TODO: WRITE WRITE ;*/ assert(false); return 0; }

    void to_ring_elem(ring_elem &result, const ElementType &a) const
    {
      result.int_val = a;
    }

    void from_ring_elem(ElementType &result, const ring_elem &a) const
    {
      result = a.int_val;
    }

    bool is_unit(ElementType f) const { return f != 0; }
    bool is_zero(ElementType f) const { return f == 0; }
    bool is_equal(ElementType f, ElementType g) const { return f == g; }

    int compare_elems(ElementType f, ElementType g) const {
      if (f < g) return -1;
      if (f > g) return 1;
      return 0;
    }

    void copy(elem &result, elem a) const { result = a; }
    void init(elem &result) const {result = 0;}
    void init_set(elem &result, elem a) const { result = a; }
    void set(elem &result, elem a) const { result = a; }
    void set_zero(elem &result) const { result = 0; }
    void clear(elem &result) const { /* nothing */ }
    
    void set_from_int(elem &result, int a) const {
      a = a % characteristic();
      if (a < 0) a += characteristic();
      result = mGF.fromZZTable(a);
    }

    void set_var(elem &result, int v) const { result = 1; }

    void set_from_mpz(elem &result, mpz_ptr a) const {
      int b = static_cast<int>(mpz_fdiv_ui(a, characteristic()));
      result = mGF.fromZZTable(b);
    }

    void set_from_mpq(elem &result, mpq_ptr a) const {
      elem n, d;
      set_from_mpz(n, mpq_numref(a));
      set_from_mpz(d, mpq_denref(a));
      ASSERT(d != 0);  //TODO actually: we need to check for this...
      divide(result,n,d);
    }
    
    void negate(elem &result, elem a) const
    {
      if (a != 0)
        result = modulus_add(a, mGF.minusOne(), mGF.orderMinusOne());
      else
        result = 0;
    }

    void invert(elem &result, elem a) const
      // we silently assume that a != 0.  If it is, result is set to a^0, i.e. 1
    {
      ASSERT(a != 0);
      result = (a == mGF.one() ? mGF.one() : mGF.orderMinusOne() - a);
    }

    void add(elem &result, elem a, elem b) const
    {
      if (a == 0) result = b;
      else if (b == 0) result = a;
      else 
        {
          int n=a-b;
          if (n > 0)
            {
              if (n == mGF.minusOne())
                result = 0;
              else
                result = modulus_add(b, mGF.oneTable(n), mGF.orderMinusOne());
            }
          else if (n < 0)
            {
              if (-n == mGF.minusOne())
                result = 0;
              else
                result = modulus_add(a, mGF.oneTable(-n), mGF.orderMinusOne());
            }
          else
            {
              if (mGF.characteristic() == 2)
                result = 0;
              else
                result = modulus_add(a, mGF.oneTable(mGF.one()), mGF.orderMinusOne());
            }
        }
    }

    void subtract(elem &result, elem a, elem b) const
    {
      result = a;
      if (b == 0) return;
      elem c = modulus_add(b, mGF.minusOne(), mGF.orderMinusOne()); // c = -b
      add(result,a,c);
    }

    void subtract_multiple(elem &result, elem a, elem b) const
    {
      // result -= a*b
      ASSERT(a != 0);
      ASSERT(b != 0);

      int ab = a+b;
      if (ab > mGF.minusOne()) ab -= mGF.orderMinusOne();
      subtract(result, ab, result);
    }

    void mult(elem &result, elem a, elem b) const
    {
      if (a != 0 && b != 0)
        {
          int c = a+b;
          if (c > mGF.orderMinusOne()) c -= mGF.orderMinusOne();
          result = c;
        }
      else
        result = 0;
    }

    void divide(elem &result, elem a, elem b) const
    {
      ASSERT(b != 0);
      if (a != 0)
        {
          int c = a-b;
          if (c <= 0) c += mGF.orderMinusOne();
          result = c;
        }
      else
        result = 0;
    }

    void power(elem &result, elem a, int n) const
    {
      if (a != 0)
        {
          result = (a*n) % mGF.orderMinusOne();
          if (result <= 0) result += mGF.orderMinusOne();
        }
      else
        result = 0;
    }

    void power_mpz(elem &result, elem a, mpz_ptr n) const
    {
      int n1 = static_cast<int>(mpz_fdiv_ui(n, mGF.orderMinusOne()));
      power(result,a,n1);
    }

    void swap(ElementType &a, ElementType &b) const
    {
      ElementType tmp = a;
      a = b;
      b = tmp;
    }

    void elem_text_out(buffer &o,
                       ElementType a,
                       bool p_one,
                       bool p_plus,
                       bool p_parens) const;

    void syzygy(ElementType a, ElementType b,
                ElementType &x, ElementType &y) const
    // returns x,y s.y. x*a + y*b == 0.
    // if possible, x is set to 1.
    // no need to consider the case a==0 or b==0.
    {
      ASSERT(a != 0);
      ASSERT(b != 0);
      x = mGF.one();
      divide(y,a,b);
      negate(y,y);
    }

    void random(ElementType &result) const
    {
      result = rawRandomInt((int32_t)characteristic());
    }


    bool promote(const Ring *Rf, const ring_elem f, elem &result) const;

    bool lift(const Ring *Rg, const elem f, ring_elem &result) const;

    // map : this --> target(map)
    //       primelem --> map->elem(first_var)
    // evaluate map(f)
    void eval(const RingMap *map, const elem f, int first_var, ring_elem &result) const;
  };

};

#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
