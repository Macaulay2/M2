#ifndef __ring_test_h_
#define  __ring_test_h_

#include <vector>

//#include "ring.hpp"
//#include "polyring.hpp"

#define RING(T,A) static_cast<const ConcreteRing<T> *>(A)->R_
#define RELEM(T,a) static_cast<RElementWrap<T> &>(a).val_
#define constRELEM(T,a) static_cast<const RElementWrap<T> &>(a).val_

namespace M2 {

  ////////////////////////////////////////////////////////
  // Programming level interfaces ////////////////////////
  ////////////////////////////////////////////////////////
  class RingInterface {}; // inherit from this if the class is to be used as a template parameter for ConcreteRing
  class PolynomialRingInterface {}; // inherit from this if the class is to be used as a template param for PolynomialConcreteRing

  enum RingID {
    ring_example = 0,
    ring_ZZZ,
    ring_ZZp,
    ring_logZZp,
    ring_GF,
     ring_FFPACK,
    ring_RRR,
    ring_CCC,
    ring_top = 8 // used to determine the number of ring types
  };

  class ARing;

  class RingInterfaceExample : public RingInterface
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
  public:
    static const RingID ringID = ring_GF;
    typedef unsigned int ElementType;

    RingGF(int p, int n) {}

    void initialize(unsigned long charac);

    void init_set(ElementType &a, long val) const { a = val; }
    void add_to(ElementType &a, const ElementType &b) const { a += b; }
  };

   class RingFFPACK : RingInterface
  {
  public:
    static const RingID ringID = ring_FFPACK;
    typedef unsigned int ElementType;

    RingFFPACK(int p, int n) {}

    void initialize(unsigned long charac);

    void init_set(ElementType &a, long val) const { a = val; }
    void add_to(ElementType &a, const ElementType &b) const { a += b; }
  };
  
  // ring-rr.hpp
  class RingRRR : RingInterface
  {
  public:
    static const RingID ringID = ring_GF;
    typedef double ElementType;
  };

  // ring-cc.hpp
  class RingCCC : RingInterface
  {
  };

  ////////////////////////////////////////////////////////
  // User level types ////////////////////////////////////
  ////////////////////////////////////////////////////////
  template <class RingType> class ConcreteRing;

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
    const ConcreteRing<RingType> * cast_to_ConcreteRing() const { return dynamic_cast< const ConcreteRing<RingType> * >(this) ; }
    // result will be either 0, or this.

    virtual RingID getRingID() const = 0;
    virtual void init_set(RElement &a, long b) const = 0;
    virtual void add_to(RElement &a, const RElement &b) const = 0;

    static bool converter(const ARing *sourceR, const ARing *targetR, const RElement &a, RElement &b);
  };

  class APolynomialRing : public ARing
  {
  };

  ////////////////////////////////////////////////////////
  // Matrices ////////////////////////////////////////////
  ////////////////////////////////////////////////////////
  // This will incorporate/replace: mat, dmat, smat, ring-vecs

  class Dense {
  public:
    static const bool isDense = true;
  };

  class Sparse {
  public:
    static const bool isDense = false;
  };

  ////////////
  template<typename RT>
  class DenseMatrix : public Dense
  {
  public:
    typedef RT RingType;
    typedef typename RT::ElementType ElementType;

    DenseMatrix(const RingType &R0,  size_t nrows0, size_t ncols0) : R_(R0), nrows(nrows0), ncols(ncols0) {}

  private:
    const RT & R_;
    size_t nrows;
    size_t ncols;
    ElementType *data;
  };
  ////////////
  template<> class DenseMatrix<RingRRR>
  {
  public:
    typedef RingRRR RingType;
    typedef RingRRR::ElementType ElementType;

    DenseMatrix(size_t nrows0, size_t ncols0) : nrows(nrows0), ncols(ncols0) {}

  private:
    size_t nrows;
    size_t ncols;
    double *data;
  };
  ////////////

  template<typename RingType>
  class SparseVector
  {
  };

  template<typename RT>
  class SparseMatrix : public Sparse
  {
  public:
    typedef RT RingType;
    typedef typename RT::ElementType ElementType;

    SparseMatrix(const RingType &R0,  size_t nrows0, size_t ncols0) : R_(R0), nrows(nrows0), ncols(ncols0) {}
  private:
    const RT & R_;
    size_t nrows;
    size_t ncols;
    std::vector<SparseVector<RT> *> data;
  };

  ////////////
  template<class MatrixType> class MatrixWrap;

  class AMatrix : public UserObject
  // this is like MutableMatrix
  {
  public:
    virtual const ARing * getRing() const { return 0; }

    template<class MatrixType>
    MatrixWrap<MatrixType> * cast_to_MatrixWrap() { return dynamic_cast< MatrixWrap<MatrixType> * >(this) ; }
    // result will be either 0, or this.

    template<class MatrixType>
    const MatrixWrap<MatrixType> * cast_to_MatrixWrap() const { return dynamic_cast< const MatrixWrap<MatrixType> * >(this) ; }
#if 0
    // Informational
    virtual size_t nRows() const;
    virtual size_t nColumns() const;
    virtual bool isDense() const;
    virtual bool isZero() const;

    virtual bool isEqual(const AMatrix *B) const;

    // Row and column operations
    virtual bool getEntry(size_t r, size_t c, RElement &result) const;
    virtual bool setEntry(size_t r, size_t c, const RElement &result);

    virtual bool interchangeRows(size_t r1, size_t r2);
    virtual bool scaleRow(size_t r, const RElement &a);
    virtual bool divideRow(size_t r, const RElement &a);
    virtual bool rowOp(size_t r, const RElement &a, size_t r1);
    virtual bool row2by2(size_t r1, size_t r2,
                         const RElement &a1, const RElement &a2,
                         const RElement &b1, const RElement &b2);
    //    virtual bool rowPermute(size_t start_row, M2_arrayint perm);
    virtual bool insertRows(size_t r, size_t n_to_add);
    virtual bool deleteRows(size_t r, size_t n_to_delete);

    // Arithmetic
    // Model these off of ffpack, linbox?
    //    virtual AMatrix * submatrix(M2_arrayint rows, M2_arrayint cols) const;
    //    virtual AMatrix * submatrix(M2_arrayint cols) const;
    virtual AMatrix * transpose() const;
    virtual void AXPY(AMatrix *B, AMatrix *C);  // this += B*C
    virtual AMatrix * add(const AMatrix *B);
    virtual AMatrix * subtract(const AMatrix *B);

    // Linear algebra routines
    virtual size_t rank() const;
    virtual RElement determinant() const;
    virtual AMatrix *nullSpace(bool right_side=true) const;
    virtual AMatrix *solve(const AMatrix *B, bool right_side=true) const;
    virtual AMatrix *invert() const;
    //    virtual M2_arrayint rankProfile(bool row_profile) const;

    // To and from strings and files

    // What about special routines for matrices over polynomial rings?
    // I guess that these should be a subclass
#endif
  };

  class AGradedMatrix : public AMatrix
  {
    // this is like Matrix
  };

  ////////////
  template<typename MatrixType>
  class MatrixWrap : public AMatrix
  {
  public:
    typedef typename MatrixType::RingType RingType;

    const ConcreteRing<RingType> * getRing() const { return R_; }

    MatrixWrap(const ConcreteRing<RingType> *R, size_t nrows, size_t ncols)
      : R_(R), mat(R->getInternalRing(),nrows,ncols)
    {
    }

    bool isDense() const { return MatrixType::isDense; }

  private:
    const ConcreteRing<RingType> *R_;
    MatrixType mat;
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
    friend class ConcreteRing<RingType>;
    element_type val_;
  };

  template <class RingType>     // RingType should inherit from RingInterface
  class ConcreteRing : public ARing
  {
    friend bool ARing::converter(const ARing *sourceR, const ARing *targetR, const RElement &a, RElement &b);
  public:
    typedef typename RingType::ElementType element_type;
    typedef RElementWrap<RingType> ringelem_type;

    ConcreteRing() {}
    ConcreteRing(RingType R) : R_(R) {}

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
  class PolynomialConcreteRing : public APolynomialRing
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

  /////////////////////////////////////////////////////////
  // Straightline programs ////////////////////////////////
  /////////////////////////////////////////////////////////

  template<typename RingType>
  class SLP {
  public:
    typedef typename RingType::ElementType ElementType;

    void evaluate(const DenseMatrix<RingType> &var_values,
                  DenseMatrix<RingType> &results);

    void evaluate(const ElementType *var_values,
                  ElementType *results);

    static SLP<RingType> * createSLP();
  private:
    const RingType *R_;
  };

  class ASLP : UserObject {
  public:
    virtual void evaluate(const AMatrix &var_values, AMatrix &result) = 0;
  };

  template <typename SLPType>
  class SLPWrap : public ASLP
  {
    virtual void evaluate(AMatrix &var_values, AMatrix &result);
  private:
    SLPType S_;
  };

  void testit();

}; // namespace M2
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
