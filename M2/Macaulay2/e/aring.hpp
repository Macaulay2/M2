// Copyright 2011 Michael E. Stillman

#ifndef _aring_hpp_
#define _aring_hpp_

#include <vector>

#define RING(T,A) static_cast<const RingWrap<T> *>(A)->R_
#define RELEM(T,a) static_cast<RElementWrap<T> &>(a).val_
#define constRELEM(T,a) static_cast<const RElementWrap<T> &>(a).val_

namespace M2 {

  ////////////////////////////////////////////////////////
  // Programming level interfaces ////////////////////////
  ////////////////////////////////////////////////////////

/**
\ingroup rings
*/
  class RingInterface {}; ///< inherit from this if the class is to be used as a template parameter for RingWrap

/**
\ingroup rings
*/
  class PolynomialRingInterface {}; ///< inherit from this if the class is to be used as a template param for PolynomialRingWrap
  class UserObject {};

/**
\ingroup rings
*/
  enum RingID {
    ring_example = 0,
    ring_ZZZ,
    ring_ZZp,
    ring_logZZp,
    ring_GF,
    ring_FFPACK,
    ring_RRR,
    ring_CCC,
    ring_top = 8 ///< used to determine the number of ring types
  };

  template <class RingType> class ARingWrap;


  class RElement
  {
  public:
    RElement() {}
    virtual ~RElement() {}
    //    virtual RElement *clone() = 0;
    //    virtual RElement &operator=(const RElement &a) = 0;
  };

/**
\ingroup rings
*/
  class ARing : public UserObject
  {
  public:
    template<class RingType>
    const ARingWrap<RingType> * cast_to_ARingWrap() const { return dynamic_cast< const ARingWrap<RingType> * >(this) ; }
    // result will be either 0, or this.

    virtual RingID getRingID() const = 0;

    virtual void init_set(RElement &a, long b) const = 0;

    virtual void add_to(RElement &a, const RElement &b) const = 0;

    static bool converter(const ARing *sourceR, const ARing *targetR, const RElement &a, RElement &b);
  };

};


#endif

// Some basic functions:
// (a) Create a ring
//  create a ZZp ring:
//    top level function (in x-relem.cpp)
//    function: create an ARing from a basic ring.
//      issue: the basic ring needs to know its ARing?  Preferably not?
//      if so: the createRing needs to set the ARing in the basic ring object afterwards.
//      if not: to make a toplevel object, need to know ARing from somewhere else.
//       problem: make the lead coefficient (i.e. need to know the coefficient ring.  So
//       APolynomialRing will need to know this, and it will mirror what the RingInterface object knows already)
//  CLEANER: keep the two layers separate...  SO:
//    An APolynomialRing must have functions: getCoefficientRing.
//    And what about creating a matrix over a ring?
// (b) m1 = m + n (matrices).
// (c) Create a new matrix

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
