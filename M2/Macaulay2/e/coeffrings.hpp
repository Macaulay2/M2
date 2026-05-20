// Copyright 2005  Michael E. Stillman

#ifndef _coeffrings_hpp_
#define _coeffrings_hpp_

/**
 * @file coeffrings.hpp
 * @brief Two `SimpleARing`-style coefficient adapters: `CoefficientRingZZp` and `CoefficientRingR`.
 *
 * Declares `CoefficientRingZZp`, the engine's discrete-log
 * `Z/p` implementation: a `SimpleARing<CoefficientRingZZp>`
 * CRTP class that represents each non-zero residue as its
 * exponent index relative to a generator and uses parallel
 * `log_table` / `exp_table` arrays of size `p` to move between
 * the index world and the residue world. Multiplication and
 * division of non-zero elements become `modulus_add` /
 * `modulus_sub` on the indices modulo `p - 1` with no table
 * lookup; addition and subtraction first hop to residues via
 * `exp_table`, do a `modulus_add` / `modulus_sub` mod `p`, and
 * hop back via `log_table`. Inversion of a non-zero index `a`
 * is `p - 1 - a` and `negate` shifts by `(p - 1) / 2` (the
 * index of `-1`). The class is used for the small primes that
 * the matching M2 raw entry point accepts (`2 <= p <= 32749`,
 * per `interface/aring.h`).
 *
 * Also declares `CoefficientRingR`, the generic adapter that
 * wraps an arbitrary `const Ring*` in the same operation
 * surface --- forwarding `add` / `mult` / `subtract` /
 * `invert` / ... to the wrapped ring's methods, and exposing
 * `Element` (a `M2::ElementImpl` subclass) and `ElementArray`
 * helpers for managed temporaries. This is the catch-all
 * implementation used wherever code that expects a
 * `CoefficientRing` interface needs to talk to a ring that
 * doesn't have a faster specialisation.
 *
 * @see aring.hpp
 * @see aring-glue.hpp
 * @see ZZp.hpp
 */

class Z_mod;
#include "aring.hpp"
#include "ringelem.hpp"
#include "ZZ.hpp"

/**
 * @brief Discrete-log `Z/p` adapter that represents non-zero residues by
 * their exponent index relative to a generator.
 *
 * @details Holds two size-`p` tables: `exp_table` maps an index `a` to the
 * residue `g^a mod p` and `log_table` is its inverse. Multiplication
 * and division on indices become `modulus_add` / `modulus_sub` mod
 * `p - 1` with no table lookup, and inversion of a non-zero index
 * `a` is `p - 1 - a`. Addition / subtraction first hop to residues
 * via `exp_table`, do a modular add/sub, and hop back via
 * `log_table`. `negate` shifts by `(p - 1) / 2` (the index of -1).
 * Used for small primes (`2 <= p <= 32749` per `interface/aring.h`)
 * via the `SimpleARing` CRTP layer.
 *
 * @ingroup coeffrings
 */
class CoefficientRingZZp : public M2::SimpleARing<CoefficientRingZZp>
{
  int p;
  int p1;  // p-1
  int minus_one;
  int zero;
  int *log_table;  // 0..p-1
  int *exp_table;  // 0..p-1

  static inline int modulus_add(int a, int b, int p)
  {
    int t = a + b;
    return (t < p ? t : t - p);
  }

  static inline int modulus_sub(int a, int b, int p)
  {
    int t = a - b;
    return (t < 0 ? t + p : t);
  }

 public:
  typedef Z_mod ring_type;
  typedef int elem;
  typedef elem ElementType;

  typedef std::vector<elem> ElementContainerType;

  CoefficientRingZZp(int p0, int *log, int *exps)
      : p(p0), p1(p - 1), zero(p - 1), log_table(log), exp_table(exps)
  {
    if (p == 2)
      minus_one = 0;
    else
      minus_one = (p - 1) / 2;


#if 0
    fprintf(stderr, "char %d\n", p);
    fprintf(stderr, "exp: ");
    for (int i=0; i<p; i++)
      fprintf(stderr, "%d ", exp_table[i]);
    fprintf(stderr, "\nlog: ");
    for (int i=0; i<p; i++)
      fprintf(stderr, "%d ", log_table[i]);
    fprintf(stderr, "\n");
#endif
  }

  void set_from_long(elem &result, long a) const
  {
    a = a % p;
    if (a < 0) a += p;
    result = log_table[a];
  }
  
  void set_from_mpz(elem &result, mpz_t a) const
  {
    mpz_t tmp;
    mpz_init_set_si(tmp, p);        // Convert int p to mpz_t
    mpz_mod(a, a, tmp);             // a = a mod p (always non-negative)
    mpz_clear(tmp);
    result = log_table[mpz_get_si(a)];
  }

  long coerceToLongInteger(const elem &f) const
  {
    int n = exp_table[f];
    if (n > p / 2) n -= p;
    return n;
  }
  
  int to_int(int f) const { return exp_table[f]; }
  void init(elem &result) const { (void) result; }
  static void clear(elem &result) { (void) result; }
  void init_set(elem &result, elem a) const { result = a; }
  void set_zero(elem &result) const { result = zero; }
  void set(elem &result, elem a) const { result = a; }
  bool is_zero(elem result) const { return result == zero; }
  bool is_equal(elem a, elem b) const { return a == b; }
  void invert(elem &result, elem a) const
  {
    if (a == 0)
      result = 0;  // this is the case a == ONE
    else
      result = p - 1 - a;
  }

  void add(elem &result, elem a, elem b) const
  {
    if (a == zero)
      result = b;
    else if (b == zero)
      result = a;
    else
      {
        int n = modulus_add(exp_table[a], exp_table[b], p);
        result = log_table[n];
      }
  }

  void negate(elem &result, elem a) const
  {
    result = modulus_add(a, minus_one, p1);
  }

  void subtract(elem &result, elem a, elem b) const
  {
    if (b == zero)
      result = a;
    else if (a == zero)
      result = modulus_add(b, minus_one, p1);
    else
      {
        int n = modulus_sub(exp_table[a], exp_table[b], p);
        result = log_table[n];
      }
  }

  void subtract_multiple(elem &result, elem a, elem b) const
  {
    // we assume: a, b are NONZERO!!
    // result -= a*b
    elem ab = modulus_add(a, b, p1);
    subtract(result, result, ab);
    return;
    // if (result==zero)
    //   result = ab;
    // else
    //   {
    //  int n = modulus_sub(exp_table[result], exp_table[ab], p);
    //  result = log_table[n];
    //   }
  }

  void mult(elem &result, elem a, elem b) const
  {
    if (a == zero || b == zero)
      result = zero;
    else
      result = modulus_add(a, b, p1);
  }

  void divide(elem &result, elem a, elem b) const
  {
    if (a == zero || b == zero)
      result = zero;
    else
      result = modulus_sub(a, b, p1);
  }

  void to_ring_elem(ring_elem &result, const elem a) const
  {
    result = ring_elem(a);
  }

  void from_ring_elem(elem &result, const ring_elem &a) const
  {
    result = a.get_int();
  }

  void swap(elem &a, elem &b) const
  {
    elem tmp = a;
    a = b;
    b = tmp;
  }

  void elem_text_out(buffer &o,
                     ElementType a,
                     bool p_one = true,
                     bool p_plus = false,
                     bool p_parens = false) const;
};

/**
 * @brief Generic `CoefficientRing` adapter that wraps an arbitrary
 * `const Ring*` and forwards every operation to it.
 *
 * @details The catch-all implementation used wherever code expects the
 * `CoefficientRing` operation surface but talks to a ring without
 * a faster specialisation. All `add` / `mult` / `subtract` /
 * `invert` calls delegate to the corresponding `Ring` virtual
 * methods on `R`, and `Element` / `ElementArray` (defined nested
 * here) supply the value-semantics wrappers the templated linear
 * algebra code expects.
 *
 * @ingroup coeffrings
 */
class CoefficientRingR
{
  const Ring *R;

 public:
  typedef Ring ring_type;
  typedef ring_elem elem;
  typedef elem ElementType;
  typedef VECTOR(elem) ElementContainerType;

  /**
   * @brief Managed scalar value: an `M2::ElementImpl<ring_elem>` that
   * initialises itself through the parent `CoefficientRingR`.
   *
   * @details Holds the wrapped `ring_elem` so callers do not have to call
   * `init` / `clear` by hand. Constructors initialise to zero or
   * copy from an existing element via the ring.
   */
  class Element : public M2::ElementImpl<ElementType>, public our_new_delete
  {
   public:
    explicit Element(const CoefficientRingR &ring) { ring.init(mValue); }
    Element(const CoefficientRingR &ring, const ElementType &value)
    {
      ring.init_set(mValue, value);
    }
  };

  /**
   * @brief Fixed-size, owned array of `ElementType`s for the linear-algebra
   * templates that want a flat buffer they can `operator[]` into.
   *
   * @details Allocates via `newarray`, initialises every slot through
   * `ring.init`, and frees via `freemem` in the destructor. Used as
   * the per-row storage backing dense matrix code that runs over
   * `CoefficientRingR`.
   */
  class ElementArray : public our_new_delete
  {
    ElementType *mData;
   public:
    ElementArray(const CoefficientRingR &ring, size_t size)
        : mData(newarray(ElementType, size))
    {
      for (size_t i = 0; i < size; i++) ring.init(mData[i]);
    }
    ~ElementArray() { freemem(mData); }
    ElementType &operator[](size_t idx) { return mData[idx]; }
    const ElementType &operator[](size_t idx) const { return mData[idx]; }
    ElementType *data() { return mData; }
    const ElementType *data() const { return mData; }
  };

  CoefficientRingR(const Ring *R0) : R(R0) {}
  void init_set(elem &result, elem a) const { result = a; }
  void init(elem &result) const { result = R->zero(); }
  void clear(elem &result) const { (void) result; }

  void set_zero(elem &result) const { result = R->zero(); }
  void set(elem &result, elem a) const { result = a; }
  void set_from_long(elem &result, long a) const { result = R->from_long(a); }
  void set_from_mpz(elem &result, mpz_t a) const { result = R->from_int(a); }
  bool is_zero(elem result) const { return R->is_zero(result); }
  bool is_equal(elem a, elem b) const { return R->is_equal(a, b); }
  bool is_unit(elem f) const { return R->is_unit(f); }
  void invert(elem &result, elem a) const { result = R->invert(a); }
  void subtract_multiple(elem &result, elem a, elem b) const 
  {
    // result -= a*b
    elem tmp = R->mult(a,b);
    result = R->subtract(result,tmp);
  }

  void add(elem &result, elem a, elem b) const { result = R->add(a, b); }
  void negate(elem &result, elem a) const { result = R->negate(a); }
  void subtract(elem &result, elem a, elem b) const
  {
    result = R->subtract(a, b);
  }

  void mult(elem &result, elem a, elem b) const { result = R->mult(a, b); }
  void divide(elem &result, elem a, elem b) const { result = R->divide(a, b); }
  void to_ring_elem(ring_elem &result, const elem &a) const { result = a; }
  void from_ring_elem(elem &result, const ring_elem &a) const { result = a; }
  // do not make the return type here a reference, otherwise
  // dangling references become very easy to make
  elem from_ring_elem_const(const ring_elem &a) const { return a; }
  void swap(elem &a, elem &b) const
  {
    elem tmp = a;
    a = b;
    b = tmp;
  }

  void elem_text_out(buffer &o,
                     ElementType a,
                     bool p_one = true,
                     bool p_plus = false,
                     bool p_parens = false) const;

  void text_out(buffer &o) const { o << "CoefficientRingR"; }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
