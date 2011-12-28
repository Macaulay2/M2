// Copyright 2011 Michael E. Stillman

#include "aring-glue.hpp"
#include "aring-zzp.hpp"
#include "aring-gf.hpp"
namespace M2 {



  template<class RingType>
  RingWrap<RingType> * RingWrap<RingType>::create(const RingType *R)
  {
    RingWrap<RingType> *result = new RingWrap<RingType>(R);
    result->initialize_ring(R->characteristic());
    result->declare_field();

    //    zeroV = from_int(0);
    //    oneV = from_int(1);
    //    minus_oneV = from_int(-1);

    return result;
  }

  template<>
  void RingWrap<ARingZZp>::to_ring_elem(ring_elem &result, const ElementType &a) const
  {
    result.int_val = a;
  }

  template<>
  void RingWrap<ARingZZp>::from_ring_elem(ElementType &result, const ring_elem &a) const
  {
    result = a.int_val;
  }


  //explicit instantiation
 template class RingWrap< ARingZZp >;

#if defined(HAVE_FFLAS_FFPACK) && defined(HAVE_GIVARO)
 template<>
  void RingWrap<ARingGF>::to_ring_elem(ring_elem &result, const ElementType &a) const
  {
    result.int_val = a;
  }

  template<>
  void RingWrap<ARingGF>::from_ring_elem(ElementType &result, const ring_elem &a) const
  {
    result = a.int_val;
  }

  //explicit instantiation
 template class RingWrap< ARingGF >;
#endif



};


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
