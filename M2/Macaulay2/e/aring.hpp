// Copyright 2011 Michael E. Stillman

#ifndef _aring_hpp_
#define _aring_hpp_

#include <cassert>
#include <memory>
#include "ringelem.hpp"
#include "buffer.hpp"

class PolynomialRing;
class RingMap;

namespace M2 {

////////////////////////////////////////////////////////
// Programming level interfaces ////////////////////////
////////////////////////////////////////////////////////

/**
\ingroup rings
*/
enum RingID {
  ring_example = 0,
  ring_ZZ,
  ring_ZZFlint,
  ring_QQ,
  ring_QQFlint,
  ring_ZZp,
  ring_ZZpFfpack,
  ring_ZZpFlint,
  ring_GFM2,
  ring_GFFlintBig,
  ring_GFFlintZech,
  ring_RRi,
  ring_RR,
  ring_CC,
  ring_RRR,
  ring_CCC,
  ring_tower_ZZp,
  ring_old      ///< refers to all rings which are not ConcreteRing's.
};

/**
\ingroup rings
*/
class RingInterface : public our_new_delete
{
};  ///< inherit from this if the class is to be used as a template parameter
    /// for ConcreteRing

/**
 * \brief A base class for Element
 *
 * \tparam ElementType the raw type to be wrapped
 *
 * This class template serves as a base class for the Element classes in the various ARing types,
 * It only implements the functions to convert to an ElementType.
 * This class has a protected destructor so that users cannot
 * accidentally try to destroy an Element using an ElementImpl pointer.
 */
template <class ElementType>
class ElementImpl
{
 protected:
  ElementType mValue;
  ElementImpl() = default;
  ElementImpl(const ElementType &value) : mValue(value) {}
  ElementImpl(ElementType &&value) : mValue(value) {}
  ElementImpl(const ElementImpl &other) noexcept = default;
  ElementImpl(ElementImpl &&other) noexcept = default;
  ElementImpl &operator=(const ElementImpl &other) noexcept = default;
  ElementImpl &operator=(ElementImpl &&other) noexcept = default;
  ~ElementImpl() noexcept = default;

 public:
  operator const ElementType &() const { return mValue; }
  operator ElementType &() { return mValue; }
  const ElementType &value() const { return mValue; }
  ElementType &value() { return mValue; }
};

/**
 * \ingroup rings
 *
 * \brief A base class for simple ARings
 *
 * An ARing class inheriting from this should provide the clear method
 * as a static member function. This class will then provide a simple
 * implementation of an Element class
 */
template <class ARing>
class SimpleARing : public RingInterface
{
 public:
  /**
   * \brief A wrapper class for ElementType
   */
  class Element : public ElementImpl<typename ARing::ElementType>
  {
    typedef typename ARing::ElementType ElementType;
    typedef ElementImpl<ElementType> Impl;

   public:
    explicit Element(const ARing &ring)
    {
      // without the Impl::, the compiler can't figure out where mValue comes
      // from
      ring.init(Impl::mValue);
    }
    Element(const ARing &ring, const ElementType& other)
    {
      ring.init_set(Impl::mValue,other);
    }
    ~Element() { ARing::clear(Impl::mValue); }
  };
  /**
   * \brief A wrapper for an array of ElementType
   *
   * This class is intended to replace dynamically allocated arrays of ElementType.
   * In particular, this will correctly clear the data from the ring upon destruction
   */
  class ElementArray
  {
    typedef typename ARing::ElementType ElementType;
    const size_t mSize;
    std::unique_ptr<ElementType[]> mData;

   public:
    ElementArray(const ARing &ring, size_t size)
        : mSize(size), mData(new ElementType[size])
    {
      for (size_t i = 0; i < size; i++) ring.init(mData[i]);
    }
    ~ElementArray()
    {
      for (size_t i = 0; i < mSize; i++) ARing::clear(mData[i]);
    }
    ElementType &operator[](size_t idx) { return mData[idx]; }
    const ElementType &operator[](size_t idx) const { return mData[idx]; }
    ElementType *data() { return mData.get(); }
    const ElementType *data() const { return mData.get(); }
  };
};

class DummyRing : public SimpleARing<DummyRing>
{
 public:
  const PolynomialRing *mOriginalRing;
  typedef long FieldType;
  typedef long ElementType;

  typedef ElementType elem;
  typedef std::vector<elem> ElementContainerType;

  int characteristic() const { return 0; }
  unsigned int computeHashValue(const elem &a) const
  {
    return static_cast<unsigned int>(a);
  }

  M2_arrayint getModPolynomialCoeffs() const { return 0; }
  M2_arrayint getGeneratorCoeffs() const { return 0; }
  void getGenerator(elem &result) const { result = 0; }
  const PolynomialRing &originalRing() const { return *mOriginalRing; }
  long coerceToLongInteger(ElementType a) const { return a; }
  void lift_to_original_ring(ring_elem &result, const ElementType &f) const {}
  M2_arrayint fieldElementToM2Array(ElementType el) const { return 0; }
  void to_ring_elem(ring_elem &result, const ElementType &a) const {}
  void from_ring_elem(ElementType &result, const ring_elem &a) const {}
  bool promote(const Ring *Rf, const ring_elem f, ElementType &result) const
  {
    return false;
  }

  bool lift(const Ring *Rg, const ElementType f, ring_elem &result) const
  {
    return false;
  }

  void eval(const RingMap *map,
            const elem f,
            int first_var,
            ring_elem &result) const
  {
  }

  void text_out(buffer &o) const { o << "GF(dummy)"; }
  void elem_text_out(buffer &o,
                     const ElementType a,
                     bool p_one,
                     bool p_plus,
                     bool p_parens) const {};

  void init_set(elem &result, elem a) const { result = a; }
  void set(elem &result, elem a) const { result = a; }
  void set_from_long(elem &result, long a) const { result = a; }
  void init(elem &result) const { result = 0; }
  void set_from_mpz(elem &result, mpz_srcptr a) const { result = 0; }
  bool set_from_mpq(elem &result, mpq_srcptr a) const { return false; }
  bool set_from_BigReal(elem &result, gmp_RR a) const { return false; }
  void set_var(elem &result, int v) const { result = 1; }
  bool is_unit(const ElementType f) const { return false; }
  bool is_zero(const ElementType f) const { return true; }
  bool is_equal(const ElementType f, const ElementType g) const
  {
    return false;
  }
  int compare_elems(const ElementType f, const ElementType g) const
  {
    return 1;
  }

  static void clear(elem &result) { result = 0; }
  void set_zero(elem &result) const { result = 0; }
  void copy(elem &result, const elem a) const { result = a; }
  void negate(elem &result, const elem a) const {};
  ;

  void invert(elem &result, const elem a) const {};
  ;

  void add(elem &result, const elem a, const elem b) const {};
  ;

  void subtract(ElementType &result,
                const ElementType a,
                const ElementType b) const {};
  ;

  void subtract_multiple(elem &result, const elem a, const elem b) const {};
  ;

  void mult(elem &result, const elem a, const elem b) const {};
  ;

  ///@brief test doc
  void divide(elem &result, const elem a, const elem b) const {};
  ;

  void power(elem &result, const elem a, const int n) const {};
  ;

  void power_mpz(elem &result, const elem a, mpz_srcptr n) const {};
  ;

  void syzygy(const ElementType a,
              const ElementType b,
              ElementType &x,
              ElementType &y) const {};
  ;

  void random(ElementType &result) const { result = 0; }
  void swap(ElementType &a, ElementType &b) const { assert(false); };
};

};  // namespace M2

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
