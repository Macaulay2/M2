#ifndef __ring_test_h_
#define  __ring_test_h_

#include "ring.hpp"
#include "polyring.hpp"

#define RING(T,A) static_cast<const RingWrap<T> *>(A)->R_
#define RELEM(T,a) static_cast<RElementWrap<T> &>(a).val_
#define constRELEM(T,a) static_cast<const RElementWrap<T> &>(a).val_

namespace M2 {

  ////////////////////////////////////////////////////////
  // Programming level interfaces ////////////////////////
  ////////////////////////////////////////////////////////
  class RingInterface {}; // inherit from this if the class is to be used as a template parameter for RingWrap
  class PolynomialRingInterface {}; // inherit from this if the class is to be used as a template param for PolynomialRingWrap

  enum RingID {
    ring_example = 0,
    ring_ZZZ,
    ring_ZZp,
    ring_logZZp,
    ring_GF,
    ring_RRR,
    ring_CCC,
    ring_top = 8 // used to determine the number of ring types
  };

  class RingInterfaceExample : RingInterface
  {
  public:
    static const RingID ringId = ring_example;
    typedef unsigned long ElementType;

    void init_set(ElementType &a, long val) const { a = val; }
    void add_to(ElementType &a, const ElementType &b) const { a += b; }
  };

  // ring-zz.hpp
  class RingZZZ : RingInterface
  {
  };
  
  // ring-zzp.hpp
  class RingZZp : RingInterface
  {
  public:
    static const RingID ringID = ring_ZZp;
    typedef unsigned long ElementType;

    void initialize(unsigned long charac);

    void init_set(ElementType &a, long val) const { a = val; }
    void add_to(ElementType &a, const ElementType &b) const { a += b; }

  private:
    unsigned long charac_;
  };

  // ring-log-zzp.hpp
  class RingLogZZp : RingInterface
  {
  };
  
  // ring-gf.hpp
  class RingGF : RingInterface
  {
  };
  
  // ring-rr.hpp
  class RingRRR : RingInterface
  {
  };
  
  // ring-cc.hpp
  class RingCCC : RingInterface
  {
  };

  ////////////////////////////////////////////////////////
  // User level types ////////////////////////////////////
  ////////////////////////////////////////////////////////
  template <class RingType> class RingWrap;
  
  class UserObject {};

  class RElement
  {
  public:
    RElement() {}
    virtual ~RElement() {}
    //    virtual RElement *clone() = 0;
    //    virtual RElement &operator=(const RElement &a) = 0;
  };

  class ARing : public UserObject
  {
  public:
    template<class RingType> 
    const RingWrap<RingType> * cast_to_RingWrap() const { return 0; }

    virtual RingID getRingID() const = 0;
    virtual void init_set(RElement &a, long b) const = 0;
    virtual void add_to(RElement &a, const RElement &b) const = 0;

    static bool converter(const ARing *sourceR, const ARing *targetR, const RElement &a, RElement &b);
  };
  
  ////////////////////////////////////////////////////////
  // Matrices ////////////////////////////////////////////
  ////////////////////////////////////////////////////////
  template<typename RingType>
  class DenseMatrix
  {
  public:
    static const bool isDense = true;
  };

  template<typename RingType>
  class SparseVector
  {
  };

  template<typename RingType>
  class SparseMatrix
  {
  public:
    static const bool isDense = false;
  };


  class AMatrix : public UserObject
  {
    // this is like MutableMatrix
  };

  class AGradedMatrix : public AMatrix
  {
    // this is like Matrix
  };

  template<typename MatrixType> 
  class MatrixWrap : public AMatrix
  {
    typedef typename MatrixType::RingType RingType;

  };

  ////////////////////////////////////////////////////////
  // Wrapper routines ////////////////////////////////////
  ////////////////////////////////////////////////////////

  template <class RingType>
  class RElementWrap : public RElement
  {
    friend bool ARing::converter(const ARing *sourceR, const ARing *targetR, const RElement &a, RElement &b);
  public:
    typedef typename RingType::ElementType element_type;
    ~RElementWrap() {}

    RElementWrap() {}
    RElementWrap(const element_type &a) : val_(a) {}
    RElementWrap(const RElement &a) : val_( static_cast<const RElementWrap&>(a).val_ ) {}

  private:
    friend class RingWrap<RingType>;
    element_type val_;
  };

  template <class RingType>     // RingType should inherit from RingInterface
  class RingWrap : public ARing
  {
    friend bool ARing::converter(const ARing *sourceR, const ARing *targetR, const RElement &a, RElement &b);
  public:
    typedef typename RingType::ElementType element_type;
    typedef RElementWrap<RingType> ringelem_type;

    RingWrap() {}
    RingWrap(RingType R) : R_(R) {}

    virtual RingID getRingID() const { return RingType::ringID; }
    RingType & getInternalRing() { return R_; }
    const RingType & getInternalRing() const { return R_; }

    virtual void init_set(RElement &a, long val) const { 
      R_.init_set( RELEM(RingType, a),
		   val ); 
    }

    virtual void add_to(RElement &a, const RElement &b) const { 
      R_.add_to( RELEM(RingType, a),
		 constRELEM(RingType, b) );
    }

  private:
    RingType R_;
  };
  
  ///////////////////

  template <class R>
  class PolynomialRingWrap : public PolynomialRing
  {
  };

  ////////////////////////////////////////////////////////
  // RingElement's ///////////////////////////////////////
  ////////////////////////////////////////////////////////

  // approach #1.  For this one, it is important for ring identity that ther eis only one ARing * 
  // per ring.
  class ARingElement : public UserObject {
  public:
    const ARing *getRing() const { return R_; }
    
    ARingElement * add(ARingElement *b); // requires ring of this and of b to be the same

  private:
    const ARing *R_;
    RElement val_;
  };

  class BRingElement : public UserObject {
    // Here, we do not keep the data here.  That will be in the
    // (templated) subclasses.  The problem with this approach is:
    // how do we tell if we can add elements?  We do not have equality
    // checks for rings easily here.  Unless, we also keep the ARing * pointer.
    // But this adds more space per element.
  public:
    virtual const ARing *getRing() const = 0;
    
    ARingElement * add(BRingElement *b); // requires ring of this and of b to be the same

  private:
    const ARing *R_;
    RElement val_;
  };


  ////////////////////////////////////////////////////////
  // Converters //////////////////////////////////////////
  ////////////////////////////////////////////////////////

  template<typename SourceRingType, typename TargetRingType>
  bool convert(const SourceRingType *A,
	       const TargetRingType *B,
	       const typename SourceRingType::ElementType & a,
	       typename SourceRingType::ElementType & b);

}; // namespace M2
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// End:
