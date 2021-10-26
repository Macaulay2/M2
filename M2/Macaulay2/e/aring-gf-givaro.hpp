// Copyright 2011 Michael E. Stillman

#ifndef _aring_gf_hpp_
#define _aring_gf_hpp_

#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include <iostream>

#include "polyring.hpp"
class RingMap;

#if 0

#include "aring-m2-gf.hpp"

namespace M2 {

   
   class ARingGFGivaro : public DummyRing
   //class ARingGF : public ARingGFM2
   {
    public:
        static const RingID ringID = ring_GFGivaro;

        typedef M2::ARingGFGivaro             ring_type ;
     
        ARingGFGivaro( long charac_,   int dimension_)  {};
        ARingGFGivaro( long charac_,  
           const M2_arrayint & modPolynomial, 
           const PolynomialRing &originalR
           )  {}
        ARingGFGivaro( long charac_,  
           const M2_arrayint & modPolynomial, 
           const M2_arrayint & primitiveElement, 
           const PolynomialRing &originalR
           )  {}
   };
};

#else
#define bool_constant givaro_bool_constant
#include <givaro/gfq.h>
#include <givaro/givpower.h>
#include <givaro/givtimer.h>
#include <givaro/gfq.h>
#include <math.h>
#include <givaro/givinteger.h>
#include <givaro/givintnumtheo.h>
#include <givaro/givpower.h>
#include <givaro/givpoly1padic.h>
#undef bool_constant
#include <type_traits>

namespace M2 {

/**
    @ingroup rings

    @brief wrapper for the  Givaro::GFqDom<>  galois field implementation
*/
/// @todo think about deriving from RingInterface AND from Ring
class ARingGFGivaro : public RingInterface
{
 public:
  static const RingID ringID = ring_GFGivaro;

  typedef Givaro::GFqDom<int64_t> FieldType;
  typedef FieldType::Element ElementType;
  typedef M2::ARingGFGivaro ring_type;
  using GivaroRandIter = FieldType::RandIter;
  typedef ElementType elem;
  typedef std::vector<elem> ElementContainerType;

  typedef FieldType::Residu_t UTT;  ///< types depends on FieldType definition!
  // typedef Signed_Trait<FieldType::Residu_t>::signed_type  STT;///< types
  // depends on FieldType definition!

  typedef std::make_signed<FieldType::Residu_t>::type STT;

  ARingGFGivaro(UTT charac_, UTT dimension_);

  // returns a polynomial that Givaro would choose for this GF(mCharac^dim).
  // We hope that if the polynomial is F(t), that t is a generator of the
  // multiplicative group.  We need to check this.
  // TODO: check whether Givaro can handle F(t) with t not primitive.
  static M2_arrayint findMinimalPolynomial(UTT charac, UTT dim);

  ARingGFGivaro(UTT charac_,
                const M2_arrayint &modPolynomial,
                const PolynomialRing &originalR
                // TODO: other information too?
                );

  ARingGFGivaro(UTT charac_,
                const M2_arrayint &modPolynomial,
                const M2_arrayint &generatorPoly,
                const PolynomialRing &originalR
                // TODO: other information too?
                );

  const FieldType field() const { return givaroField; }
 private:
  UTT mCharac;
  UTT mDimension;    ///< same as extensionDegree
  UTT mCardinality;  ///< number of elements in the field, if less than some
                     /// bound, otherwise 0.

  const PolynomialRing *mOriginalRing;
  const ring_elem mPrimitiveElement;  // is an element of mOriginalRing

  const FieldType givaroField;

  mutable GivaroRandIter givaroRandomIterator;

  M2_arrayint representationToM2Array(UTT representation, long coeffNum) const;

  static M2_arrayint representationToM2Array(UTT representation,
                                             long coeffNum,
                                             UTT charac);

  M2_arrayint modPolynomialRepresentationToM2Array(UTT representation) const;
  M2_arrayint elementRepresentationToM2Array(UTT representation) const;

 public:
  M2_arrayint fieldElementToM2Array(ElementType el) const;

 private:
  static UTT M2arrayToGFRepresentation(UTT pCharac, const M2_arrayint &m2array);
  static std::vector<UTT> M2arrayToStdVec(UTT pCharac,
                                          const M2_arrayint &m2array);

  static UTT M2arrayGetDegree(const M2_arrayint &m2array);

 public:
  // ring informational
  UTT characteristic() const { return mCharac; }
  UTT cardinality() const { return mCardinality; }
  unsigned int computeHashValue(const elem &a) const
  {
    return static_cast<unsigned int>(a);
  }

  /** @name IO
  @{ */
  void text_out(buffer &o) const
  {
    o << "GF(" << mCharac << "," << mDimension << ",Givaro)";
  }

  void elem_text_out(buffer &o,
                     const ElementType a,
                     bool p_one = true,
                     bool p_plus = false,
                     bool p_parens = false) const;

  /** @} */

  /** @name properties
  @{ */
  bool is_unit(const ElementType f) const;
  bool is_zero(const ElementType f) const;
  /** @} */

  /** @name translation functions
  @{ */
  void to_ring_elem(ring_elem &result, const ElementType &a) const
  {
    result = ring_elem(static_cast<int>(a));
  }

  void from_ring_elem(ElementType &result, const ring_elem &a) const
  {
    result = a.get_int();
  }
  /** @} */

  /** @name operators
    @{ */
  bool is_equal(const ElementType f, const ElementType g) const;
  int compare_elems(const ElementType f, const ElementType g) const;
  /** @} */

  /** @name get functions
      @{ */
  M2_arrayint getModPolynomialCoeffs() const;
  M2_arrayint getGeneratorCoeffs() const;

  void getGenerator(
      ElementType &result_gen) const;  // returns the generator in this ring.
  const PolynomialRing &originalRing() const { return *mOriginalRing; }
  /** @} */

  /** @name init_set
  @{ */

  void init_set(elem &result, elem a) const { result = a; }
  void set(elem &result, elem a) const { result = a; }
  void init(elem &result) const;

  void clear(elem &result) const;

  void set_zero(elem &result) const;

  void copy(elem &result, const elem a) const;

  void set_from_long(elem &result, int64_t a) const;

  void set_from_mpz(elem &result, mpz_srcptr a) const;

  bool set_from_mpq(elem &result, mpq_srcptr a) const;

  bool set_from_BigReal(elem &result, gmp_RR a) const { return false; }
  void set_var(elem &result, int v) const { result = 1; }
  /** @} */

  /** @name arithmetic
  @{ */
  void negate(elem &result, const elem a) const;

  void invert(elem &result, const elem a) const;

  void add(elem &result, const elem a, const elem b) const;

  void subtract(ElementType &result,
                const ElementType a,
                const ElementType b) const;

  void subtract_multiple(elem &result, const elem a, const elem b) const;

  void mult(elem &result, const elem a, const elem b) const;

  ///@brief test doc
  void divide(elem &result, const elem a, const elem b) const;

  void power(elem &result, const elem a, const STT n) const;

  void power_mpz(elem &result, const elem a, mpz_srcptr n) const;

  void syzygy(const ElementType a,
              const ElementType b,
              ElementType &x,
              ElementType &y) const;
  /** @} */

  /** @name misc
  @{ */
  void swap(ElementType &a, ElementType &b) const;

  void random(GivaroRandIter &it, ElementType &result) const;
  void random(ElementType &result) const;

  /** @} */

  bool promote(const Ring *Rf, const ring_elem f, ElementType &result) const;

  void lift_to_original_ring(ring_elem &result, const ElementType &f) const;
  // GF specific routine, used in getRepresentation

  bool lift(const Ring *Rg, const ElementType f, ring_elem &result) const;

  // map : this --> target(map)
  //       primelem --> map->elem(first_var)
  // evaluate map(f)
  void eval(const RingMap *map,
            const elem f,
            int first_var,
            ring_elem &result) const;
};
};

#endif
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
