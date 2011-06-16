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

  class ElementExample
  {
    
  };

  enum RingID {
    ring_example = 0,
    ring_ZZZ,
    ring_ZZp,
    ring_GF,
    ring_RRR,
    ring_CCC,
    ring_top = 8 // used to determine the number of ring types
  };

  class RingInterfaceExample : RingInterface
  {
  public:
    class DenseMatrixExample;
    class SparseMatrixExample;

    static const RingID ringId = ring_example;
    typedef unsigned long ElementType;
    typedef ElementType elem;
    typedef DenseMatrixExample dense_matrix_type;
    typedef SparseMatrixExample sparse_matrix_type;

    void init_set(elem &a, long val) const { a = val; }
    void add_to(elem &a, const elem &b) const { a += b; }
  };
  
  class RingZZZ : RingInterface
  {
  };
  
  class RingZZp : RingInterface
  {
  };
  
  class RingGF : RingInterface
  {
  };
  
  class RingRRR : RingInterface
  {
  };
  
  class RingCCC : RingInterface
  {
  };

  template<typename RingType>
  class DenseMatrix
  {
  };

  template<typename RingType>
  class SparseMatrix
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

    virtual bool conv(const Ring *target, const RElement &a, RElement &b) const = 0;
  };
  
  class ARingElement : public UserObject
  {
    // This should be like our RingElement class.
  };

  ////////////////////////////////////////////////////////
  // Matrices ////////////////////////////////////////////
  ////////////////////////////////////////////////////////
  class AMatrix : public UserObject
  {
    // this is like MutableMatrix
  };

  class AGradedMatrix : public AMatrix
  {
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
    friend bool converter(const ARing *A, const ARing *B, const RElement &a, RElement &b);
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
    friend bool converter(const ARing *A, const ARing *B, const RElement &a, RElement &b);
  public:
    typedef typename RingType::ElementType element_type;
    typedef RElementWrap<RingType> ringelem_type;

    virtual RingID getRingID() const { return RingType::ringID; }
#if 0
    virtual bool conv(const ARing *target, const RElement &a, RElement &b) const {
      return target->convB(this->R_, constRELEM(RingType,a), b);
    }

    bool convB(const RingWrap<RingInterfaceExample> *sourceR, 
	       const RingInterfaceExample::ElementType &a,
	       RElement &b);
#endif
    virtual void init_set(RElement &a, long val) const { 
      R_.init_set( static_cast<ringelem_type &>(a).val_, 
		   val ); 
    }

    virtual void add_to(RElement &a, const RElement &b) const { 
      R_.add_to( static_cast<ringelem_type &>(a).val_, 
		 static_cast<const ringelem_type &>(b).val_ );
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
  // Converters //////////////////////////////////////////
  ////////////////////////////////////////////////////////
  bool converter(const ARing *sourceR, const ARing *targetR, const RElement &a, RElement &b);

  template<typename Source, typename Target>
  bool convert(const Source *A, 
	       const Target *B, 
	       const typename Source::ElementType &a,
	       typename Target::ElementType &b);


#if 0
  // Here is one way to do the converter:
  // We have another class tree:
  //   ConverterBase (with a virtual function
  //     Converter<RingType>
  // Each class RingWrap<RingType> has a method returning 
  class ConverterBase {
  };

  template <typename RingType>
  class Converter : public ConverterBase {
  };

  // R->convert(targetring, a, b) is virtual and calls
  // Converter<SourceRingType>::convert_to()
#endif

}; // namespace M2
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// End:
