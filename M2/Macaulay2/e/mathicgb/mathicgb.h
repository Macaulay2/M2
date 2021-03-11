#ifndef MATHICGB_MATHICGB_GUARD
#define MATHICGB_MATHICGB_GUARD

#include <ostream>
#include <vector>
#include <utility>

// The main function in this file is computeGroebnerBasis. See the comment
// preceding that function for an example of how to use this library
// interface.

extern "C" {
  // Put a C function in the library so that it can be detected by the autoconf
  // macro AC_CHECK_LIB. That macro can only check for libraries that contain
  // at least one C function.
  void libmathicgbIsPresent(void); // This function does nothing.
  extern char MATHICGB_VERSION_STRING[];
}
#define MATHICGB_VERSION VERSION

/// The classes and functions in this namespace make up the public interface
/// of MathicGB. You should not have to update your code when upgrading to a
/// newer minor revision of MathicGB if you only use the public interface.
namespace mgb { // Part of the public interface of MathicGB
  /// Code in this namespace is not part of the public interface of MathicGB.
  /// If you take a dependency on code in this namespace, except indirectly
  /// through using code in the mgb namespace, then you are not using the
  /// library interface as intended.
  namespace mgbi { // Not part of the public interface of MathicGB
    class PimplOf;
  }

  /// Sets time to the number of seconds accumulated on the internal
  /// MathicGB log named logName. Returns true if the logName log was
  /// found and false otherwise.
  ///
  /// The available logs and what they measure are not part of
  /// the public interface of MathicGB and indeed the availability of a log
  /// depends on both compile-time and run-time events. Therefore you must
  /// ensure that your program works even if this function always returns
  /// false.
  bool logTime(const char* logName, double& time);

  /// As logTime, but retrieves a number associated to logName that is
  /// not necessarily a time.
  bool logNumber(const char* logName, double& number);

  /// Use this class to describe a configuration of a Groebner basis algorithm
  /// that you want to run.
  ///
  /// If you came here to compute the Groebner basis of an ideal in a
  /// polynomial ring and you don't want to deal with modules: Don't worry
  /// about it. A module with one component over a polynomial ring is the
  /// same thing as that ring. Simply set the componentCount to 1. Then all
  /// (module) monomials will have component 0 and you can just ignore that.
  /// The internal implementation has a special case for this case you will
  /// not pay an overhead for this other than passing a few 0 components
  /// through the interface.
  ///
  /// @todo: expose more of the available functionality.
  class GroebnerConfiguration {
  public:
    // Type for the coefficient of a term. 2*x is a term with coefficient 2.
    typedef unsigned int Coefficient;

    /// Type to specify a variable in the polynomial ring. If the variables
    /// are x, y, z in that order, then x has index 0, y has index 1 and z
    /// has index 2.
    typedef size_t VarIndex;

    /// Type for the exponent of a variable. If x is a variable, then
    /// x*x = x^2 has exponent 2.
    typedef int Exponent;

    /// Type for the component of a module monomial. The module monomial
    /// x*y*z * e_i has component i.
    typedef unsigned int Component;

    /// A configuration in a module over a polynomial ring with varCount
    /// variables and the coefficients are from the finite field with
    /// modulus elements. modulus must be a prime. The module has the basis
    /// e_0, ..., e_k where k is componentCount - 1.
    GroebnerConfiguration(
      Coefficient modulus,
      VarIndex varCount,
      Component comCount
    );

    GroebnerConfiguration(const GroebnerConfiguration& conf);
    ~GroebnerConfiguration();

    Coefficient modulus() const;
    VarIndex varCount() const;
    Component comCount() const;

    enum BaseOrder {
      /// Reverse lexicographic order with x_1 > x_2 > ... > x_n.
      /// Can be described as rev-lex-descending or rev-lex-from-right.
      /// This is what people usually mean when they say rev-lex.
      RevLexDescendingBaseOrder = 0,

      /// Lexicographic order with x_1 > x_2 > ... > x_n.
      /// Can be described as lex-descending or lex-from-left.
      /// This is what people usually mean when they say lex.
      LexDescendingBaseOrder = 1,

      /// Reverse lexicographic order with x_1 < x_2 < ... < x_n.
      /// Can be described as rev-lex-ascending or rev-lex-from-left.
      RevLexAscendingBaseOrder = 2,

      /// Lexicographic order with x_1 < x_2 < ... < x_n.
      /// Can be described as lex-ascending or lex-from-right.
      LexAscendingBaseOrder = 3
    };

    /// Returns a name for the passed in base order.
    /// Ownership of the string is not passed on. You should not depend on
    /// the string having any particular value, except that it is something
    /// that will identify the order to a human.
    static const char* baseOrderName(BaseOrder order);

    /// Specifies the monomial order to compute a Groebner basis with
    /// respect to. You must ensure that the order that you are specifying
    /// is in fact a monomial order.
    ///
    /// The specified monomial order has two parts - a set of gradings and
    /// a base order. The base order is used to break ties for monomials with
    /// identical grades.
    ///
    /// The gradings parameter represents a matrix where each row of the matrix
    /// defines a grading. The matrix is represented in row-major order.
    /// The matrix has varCount() columns so gradings.size() must be a multiple
    /// of varCount().
    ///
    /// Suppose gradings has one row U and that x^a and x^b are two monomials
    /// with exponent vectors a and b respectively. Then a < b if a*U < b*U
    /// where * is dot product. If there are several rows in gradings, then
    /// the first row U is considered first. If a*U=b*U then the second row
    /// is considered and so on. If a and b have the same degree with respect
    /// to all the rows of the matrix, then the base order is used to break the
    /// tie.
    ///
    /// An order is global if 1 is the smallest of all monomials. Equivalently,
    /// if 1 < x for each variable x in the ambient ring. In MathicGB only
    /// global orders are considered monomial orders and non-global orders
    /// are not supported. setMonomialOrder will return true if the specified
    /// order is global. Otherwise, setMonomialOrder returns false and the
    /// requested order is NOT set. So you can ignore the return value if
    /// and only if you are certain that the order you are requesting is in
    /// fact global. For example, ungraded reverse lex is not a (global) monomial
    /// order.
    ///
    /// Each row of the matrix adds overhead to the Groebner basis
    /// computation both in terms of time and space.
    ///
    /// The default grading is (1, ..., 1)-graded reverse lex.
    bool setMonomialOrder(
      BaseOrder order,
      const std::vector<Exponent>& gradings
    );
    std::pair<BaseOrder, std::vector<Exponent> > monomialOrder() const;

    static const size_t ComponentAfterBaseOrder = static_cast<size_t>(-1);

    /// Sets the module monomial order. This order extends the
    /// monomial order in the sense that a<b <=> aM<bM for a,b
    /// monomials and M a module monomial. So if you want to change
    /// the matrix of the comparison, change the monomial order instead.
    ///
    /// componentBefore specifies at what point to compare the
    /// components of two module monomials. If componentBefore == 0
    /// then the component is considered before anything else. Let R
    /// be the number of rows in the grading matrix. If
    /// componentBefore < R, then the component is considered before the
    /// grading with index componentBefore and after the grading with index
    /// componentBefore - 1. If componentBefore == R, then the
    /// component is considered after all gradings and before the base
    /// order. If componentBefore == ComponentAfterBaseOrder, then the
    /// component is considered after everything else.
    ///
    /// If componentBefore > R and componentBefore !=
    /// ComponentAfterBaseOrder, then that is an invalid value and you
    /// will experience undefined behavior. This is only checked at a
    /// later stage, so there is no problem in setting a value that
    /// was invalid at the time but that becomes valid later, either
    /// because you reset it to something valid or because you change the
    /// matrix.
    ///
    /// Setting this value to anything other than
    /// ComponentAfterBaseOrder incurs overhead similar to adding a
    /// row to the comparison matrix.
    ///
    /// The default value is ComponentAfterBaseOrder.
    void setComponentBefore(size_t value);
    VarIndex componentBefore() const;

    /// This setting modifies the module monomial order.
    /// If componentsAscending is true then a comparison of components
    /// is done such that a module monomial with greater component is
    /// considered greater once we get the point where components are
    /// compared. So when not using the Schreyer setting, the impact is:
    ///
    ///   true value: e_0 < e_1 < ... < e_n
    ///  false value: e_n < ... < e_1 < e_0.
    ///
    /// The default value is true. Either setting is implemented with a
    /// pre-processing step, so there is little-to-no overhead for it.
    void setComponentsAscending(bool value);
    bool componentsAscending() const;

    /// This setting modifies the module monomial order.
    /// We are Schreyering if we are using an ordering <' that is
    /// derived from the usual module mononial order < in the
    /// following way. Let c_i be the leading monomial of the input
    /// basis element with index i. Then
    ///
    ///   ae_i <' be_i   if and only if   ac_ie_i < bc_je_j.
    ///
    /// The default value is true. Either setting is implemented with a
    /// pre-processing step, so there is little-to-no overhead.
    ///
    /// A possible future extension would be to allow setting the c_i
    /// to be something other than leading monomials. This would be useful
    /// for the higher levels in computation of resolutions. This
    /// functionality is not currently available.
    void setSchreyering(bool value);
    bool schreyering() const;

    enum Reducer {
      DefaultReducer = 0, /// Let the library decide for itself.
      ClassicReducer = 1, /// The classic polynomial division algorithm.
      MatrixReducer = 2 /// use linear algebra as in F4.
    };

    /// Specify the way that polynoials are reduced.
    void setReducer(Reducer reducer);
    Reducer reducer() const;

    /// Sets the maximum number of S-pairs to reduce at one time. This is
    /// mainly useful as a (weak) control on memory usage for F4 reducers.
    /// A value of 0 indicates to let the library decide this value for
    /// itself, which is also the default and highly recommended value.
    ///
    /// For matrix-based reducers, use a high value. For serial classic
    /// reduction, use a low value, preferably 1. Setting the value to
    /// 0 already takes care of this.
    void setMaxSPairGroupSize(unsigned int size);
    unsigned int maxSPairGroupSize() const;

    /// Sets the maximum number of threads to use. May use fewer threads.
    /// A value of 0 indicates to let the library decide this value for
    /// itself, which is also the default value.
    void setMaxThreadCount(unsigned int maxThreadCount);
    unsigned int maxThreadCount() const;

    /// Sets logging to occur according to the string. The format of the
    /// string is the same as for the -logs command line parameter.
    /// Ownership of the string is not taken over.
    /// @todo: describe the format in more detail.
    void setLogging(const char* logging);
    const char* logging() const;

    /// Class used for setCallback().
    class Callback {
    public:
      enum Action {
        ContinueAction = 0,
        StopWithNoOutputAction = 1,
        StopWithPartialOutputAction = 2
      };
      virtual Action call() = 0;
    };

    /// Set callback to be called at various unspecified times during
    /// the computation. Callback has the ability to stop or continue
    /// the computation based on its return value. If callback is null
    /// then no function will be called.
    void setCallback(Callback* callback);
    Callback* callback();
    const Callback* callback() const;

  private:
    friend class mgbi::PimplOf;

    void operator=(const GroebnerConfiguration&); // not available
    bool operator==(const GroebnerConfiguration&); // not available

    struct MonomialOrderData {
      BaseOrder baseOrder;
      const Exponent* gradings;
      size_t gradingsSize;
    };
    bool setMonomialOrderInternal(MonomialOrderData order);
    MonomialOrderData monomialOrderInternal() const;

    static Callback::Action callbackCaller(void* obj);
    void setCallbackInternal(void* data, Callback::Action (*func) (void*));
    void* callbackDataInternal() const;

    struct Pimpl;
    Pimpl* const mPimpl;
  };

  /// After making a configuration, use this class to communicate a basis
  /// of the input ideal that you want to run a Groebner basis algorithm on.
  class GroebnerInputIdealStream {
  public:
    GroebnerInputIdealStream(const GroebnerConfiguration& conf);
    ~GroebnerInputIdealStream();

    typedef GroebnerConfiguration::Coefficient Coefficient;
    typedef GroebnerConfiguration::VarIndex VarIndex;
    typedef GroebnerConfiguration::Exponent Exponent;
    typedef GroebnerConfiguration::Component Component;

    const GroebnerConfiguration& configuration() const;
    Coefficient modulus() const;
    VarIndex varCount() const;
    Component comCount() const;

    void idealBegin();
    void idealBegin(size_t polyCount);
    void appendPolynomialBegin();
    void appendPolynomialBegin(size_t termCount);

    /// Signals the beginning of communication of a module term of the
    /// current module element.
    void appendTermBegin(Component com);

    /// The sequence of indices appended to a term must be in strictly
    /// ascending order.
    void appendExponent(VarIndex index, Exponent exponent);
    void appendTermDone(Coefficient coefficient);
    void appendPolynomialDone();
    void idealDone();

  private:
    bool debugAssertValid() const;
    Exponent* const mExponents;

    struct Pimpl;
    friend class mgbi::PimplOf;
    Pimpl* const mPimpl;
  };

  /// After making a configuration and an ideal, use this function to compute
  /// a Groebner basis. The output basis is constructed on output, which must
  /// resemble GroebnerInputIdealStream by having the following functions.
  ///
  ///   - modulus() const;
  ///   - varCount() const;
  ///   - idealBegin(size_t polyCount);
  ///   - void appendPolynomialBegin(size_t termCount);
  ///   - void appendTermBegin(Component com);
  ///   - void appendExponent(VarIndex index, Exponent exponent);
  ///   - void appendTermDone(Coefficient coefficient);
  ///   - void appendPolynomialDone();
  ///   - void idealDone();
  ///
  /// ** Example
  ///
  /// This example uses a default configuration, constructs an ideal and then
  /// outputs the Groebner basis to a NullIdealStream which does not do
  /// anything with the output. However, we wrap the NullIdealStream in a
  /// IdealStreamLog, which prints out the method calls done on the stream
  /// to std::cerr. We also wrap the GroebnerInputIdealStream in a
  /// IdealStreamChecker which checks that we are correctly following the
  /// protocol of GroebnerInputIdealStream - that's only recommended when
  /// debugging as it is slow.
  ///
  /// GroebnerConfiguration configuration(101, 4); // mod 101, 4 variables
  /// GroebnerInputIdealStream input(configuration);
  /// IdealStreamChecker<GroebnerInputIdealStream> checked(input);
  /// checked.idealBegin(2); // describe ideal with 2 basis elements
  ///   checked.appendPolynomial(2); // describe generator with 2 terms
  ///     checked.appendTermBegin();
  ///       checked.appendExponent(0, 40); // x0^40
  ///     checked.appendTermDone(3); // 3 * x0^40
  ///     checked.appendTermBegin();
  ///       checked.appendExponent(1, 5); // x1^5
  ///       checked.appendExponent(2, 7); // x2^7
  ///     checked.appendTermDone(11); // 11 * x1^5 * x2^7
  ///   checked.appendPolynomialDone(); // 3 * x0^40 + 11 * x1^5 * x2^7
  ///   checked.appendPolynomialBegin(1);
  ///     checked.appendTermBegin();
  ///     checked.appendTermDone(13); // 13
  ///   checked.appendPolynomialDone(); // 13
  /// checked.idealDone(); // the generators are 3*x0^40 + 11*x1^5*x2^7 and 13
  /// NullIdealStream nullStream;
  /// IdealStreamLog<NullIdealStream> logStream(nullStream, std::cerr);
  /// computeGroebnerBasis(input, logStream);
  ///
  /// The ideal constructed on the passed-in GroebnerInputIdealStream will
  /// be cleared. If you need it again for something else, you will have
  /// to re-construct it.
  template<class OutputStream>
  void computeGroebnerBasis(
    GroebnerInputIdealStream& inputWhichWillBeCleared,
    OutputStream& output
  );

  class NullIdealStream;

  /// Passes on all method calls to an inner ideal stream while printing out
  /// what methods get called to an std::ostream.
  template<class Stream = NullIdealStream>
  class IdealStreamLog {
  public:
    typedef GroebnerConfiguration::Coefficient Coefficient;
    typedef GroebnerConfiguration::VarIndex VarIndex;
    typedef GroebnerConfiguration::Exponent Exponent;
    typedef GroebnerConfiguration::Component Component;

    /// All calls are written to log and then passed on to stream.
    IdealStreamLog(std::ostream& log, Stream& stream);

    /// All calls are written to log.
    IdealStreamLog(
      std::ostream& log,
      Coefficient modulus,
      VarIndex varCount,
      Component comCount
    );

    ~IdealStreamLog();

    Coefficient modulus() const;
    VarIndex varCount() const;
    Component comCount() const;

    void idealBegin();
    void idealBegin(size_t polyCount);
    void appendPolynomialBegin();
    void appendPolynomialBegin(size_t termCount);
    void appendTermBegin(Component com);
    void appendExponent(VarIndex index, Exponent exponent);
    void appendTermDone(Coefficient coefficient);
    void appendPolynomialDone();
    void idealDone();

  private:
    const Coefficient mModulus;
    const VarIndex mVarCount;
    const Component mComCount;
    Stream* const mStream;
    std::ostream& mLog;
  };

  /// An ideal stream that simply ignores all the method calls it receives.
  /// Can be handy in combination with IdealStreamLog or as a temporary
  /// stand-in if you have not yet written your own ideal stream.
  class NullIdealStream {
  public:
    typedef GroebnerConfiguration::Coefficient Coefficient;
    typedef GroebnerConfiguration::VarIndex VarIndex;
    typedef GroebnerConfiguration::Exponent Exponent;
    typedef GroebnerConfiguration::Component Component;

    NullIdealStream(
      Coefficient modulus,
      VarIndex varCount,
      Component comCount
    );

    Coefficient modulus() const {return mModulus;}
    VarIndex varCount() const {return mVarCount;}
    Component comCount() const {return mComCount;}

    void idealBegin() {}
    void idealBegin(size_t polyCount) {}
    void appendPolynomialBegin() {}
    void appendPolynomialBegin(size_t termCount) {}
    void appendTermBegin(Component com) {}
    void appendExponent(VarIndex index, Exponent exponent) {}
    void appendTermDone(Coefficient coefficient) {}
    void appendPolynomialDone() {}
    void idealDone() {}

  private:
    const Coefficient mModulus;
    const VarIndex mVarCount;
    const Component mComCount;
  };

  template<class OutputStream>
  void streamSimpleIdeal(OutputStream& output);

  namespace mgbi { // Not part of the public interface of MathicGB
    /// Class that checks to see if the protocol for an ideal stream is followed.
    /// A method will return false if that is not the case. This class is not
    /// itself an ideal stream - it is intended to be used with ideal streams for
    /// debugging.
    class StreamStateChecker {
    public:
      typedef GroebnerConfiguration::Coefficient Coefficient;
      typedef GroebnerConfiguration::VarIndex VarIndex;
      typedef GroebnerConfiguration::Exponent Exponent;
      typedef GroebnerConfiguration::Component Component;

      StreamStateChecker(
        const Coefficient modulus,
        const VarIndex varCount,
        const Component comCount
      );
      ~StreamStateChecker();

      void idealBegin();
      void idealBegin(size_t polyCount);
      void appendPolynomialBegin();
      void appendPolynomialBegin(size_t termCount);
      void appendTermBegin(Component com);
      void appendExponent(VarIndex index, Exponent exponent);
      void appendTermDone(Coefficient coefficient);
      void appendPolynomialDone();
      void idealDone();

      bool hasIdeal() const;

    private:
      struct Pimpl;
      Pimpl* const mPimpl;
    };
  }

  /// Use this class to check that you are following the correct protocol
  /// for calling methods on an ideal stream. This has significant overhead
  /// so it is not recommended for production use. If you have built the
  /// MathicGB library in debug mode then this is already automatically
  /// used for GroebnerInputIdealStream.
  template<class Stream>
  class IdealStreamChecker {
  public:
    typedef GroebnerConfiguration::Coefficient Coefficient;
    typedef GroebnerConfiguration::VarIndex VarIndex;
    typedef GroebnerConfiguration::Exponent Exponent;
    typedef GroebnerConfiguration::Component Component;

    IdealStreamChecker(Stream& stream);

    Coefficient modulus() const;
    VarIndex varCount() const;
    Component comCount() const;

    void idealBegin();
    void idealBegin(size_t polyCount);
    void appendPolynomialBegin();
    void appendPolynomialBegin(size_t termCount);
    void appendTermBegin(Component com);
    void appendExponent(VarIndex index, Exponent exponent);
    void appendTermDone(Coefficient coefficient);
    void appendPolynomialDone();
    void idealDone();

  private:
    Stream& mStream;
    mgbi::StreamStateChecker mChecker;
  };
}

// ********************************************************
// Nothing below this line is part of the public interface of MathicGB.

#ifdef MATHICGB_DEBUG
#include <cassert>
#endif

namespace mgb {
  // ** Functions

  // This method is made inline to avoid the overhead from calling a function
  // for every exponent. This is also why mExponents is not inside the pimpl -
  // otherwise we couldn't access it from here. That then explains why
  // mExponents is a raw pointer instead of a std::vector - the compiler for the
  // caller and the library must agree on the memory layout of the object and
  // that is less likely to introduce problems for a raw pointer than for a
  // std::vector. In particular, doing it this way allows the library and
  // the caller to use different implementations of the STL.
  inline void GroebnerInputIdealStream::appendExponent(
    const VarIndex index,
    const Exponent exponent
  ) {
#ifdef MATHICGB_DEBUG
    assert(index < varCount());
#endif
    mExponents[index] = exponent;
  }

  // ** Implementation of class GroebnerConfiguration
  // This code is inline so that things will still work even if
  // the caller uses a different implementation of std::vector than
  // the library does internally. So we have to decay objects of
  // type std::vector to pointers.

  inline bool GroebnerConfiguration::setMonomialOrder(
    const BaseOrder baseOrder,
    const std::vector<Exponent>& gradings
  ) {
    // We cannot do gradings.data() since we may be compiling without C++11
    // support. We also cannot do &*gradings.begin() if gradings is empty
    // since then we are dereferencing an invalid iterator - the debug build
    // of MSVC's STL will correctly flag this as an error.
    const MonomialOrderData data = {
      baseOrder,
      gradings.empty() ? static_cast<Exponent*>(0) : &*gradings.begin(),
      gradings.size()
    };
    return setMonomialOrderInternal(data);
  }

  inline std::pair<
    GroebnerConfiguration::BaseOrder,
    std::vector<GroebnerConfiguration::Exponent>
  > GroebnerConfiguration::monomialOrder() const {
    const MonomialOrderData data = monomialOrderInternal();
    return std::make_pair(
      data.baseOrder,
      std::vector<Exponent>(data.gradings, data.gradings + data.gradingsSize)
    );
  }

  inline GroebnerConfiguration::Callback::Action
  GroebnerConfiguration::callbackCaller(void* obj) {
    return static_cast<Callback*>(obj)->call();
  };

  inline void GroebnerConfiguration::setCallback(Callback* callback) {
    setCallbackInternal(static_cast<void*>(callback), callbackCaller);
  }

  inline const GroebnerConfiguration::Callback*
  GroebnerConfiguration::callback() const {
    return const_cast<GroebnerConfiguration&>(*this).callback();
  }

  inline GroebnerConfiguration::Callback*
  GroebnerConfiguration::callback() {
    return static_cast<Callback*>(callbackDataInternal());
  }

  // ** Implementation of the class IdealStreamLog
  // This class has to be inline as it is a template.

  template<class Stream> 
  IdealStreamLog<Stream>::IdealStreamLog(std::ostream& log, Stream& stream):
    mModulus(stream.modulus()),
    mVarCount(stream.varCount()),
    mComCount(stream.comCount()),
    mStream(&stream),
    mLog(log)
  {
    mLog << "IdealStreamLog s(stream, log); // modulus=" << mModulus
      << ", varCount=" << mVarCount
      << ", comCount=" << mComCount << '\n';
  }

  template<class Stream> 
  IdealStreamLog<Stream>::IdealStreamLog(
    std::ostream& log,
    Coefficient modulus,
    VarIndex varCount,
    Component comCount
  ):
    mModulus(modulus),
    mVarCount(varCount),
    mComCount(comCount),
    mStream(0),
    mLog(log)
  {
    mLog
      << "IdealStreamLog s(stream, "
      << mModulus << ", "
      << mVarCount << ", "
      << mComCount << ");\n";
  }

  template<class Stream> 
  IdealStreamLog<Stream>::~IdealStreamLog() {
    mLog << "// s.~IdealStreamLog();\n";
  }

  template<class Stream> 
  typename IdealStreamLog<Stream>::Coefficient
  IdealStreamLog<Stream>::modulus() const {
    return mModulus;
  }

  template<class Stream> 
  typename IdealStreamLog<Stream>::VarIndex
  IdealStreamLog<Stream>::varCount() const {
    return mVarCount;
  }

  template<class Stream> 
  typename IdealStreamLog<Stream>::Component
  IdealStreamLog<Stream>::comCount() const {
    return mComCount;
  }

  template<class Stream>
  void IdealStreamLog<Stream>::idealBegin() {
    mLog << "s.idealBegin();\n";
    if (mStream != 0)
      mStream->idealBegin();
  }

  template<class Stream> 
  void IdealStreamLog<Stream>::idealBegin(size_t polyCount) {
    mLog << "s.idealBegin(" << polyCount << "); // polyCount\n";
    if (mStream != 0)
      mStream->idealBegin(polyCount);
  }

  template<class Stream> 
  void IdealStreamLog<Stream>::appendPolynomialBegin() {
    mLog << "s.appendPolynomialBegin();\n";
    if (mStream != 0)
      mStream->appendPolynomialBegin();
  }

  template<class Stream> 
  void IdealStreamLog<Stream>::appendPolynomialBegin(size_t termCount) {
    mLog << "s.appendPolynomialBegin(" << termCount << ");\n";
    if (mStream != 0)
      mStream->appendPolynomialBegin(termCount);
  }

  template<class Stream> 
  void IdealStreamLog<Stream>::appendTermBegin(const Component com) {
    mLog << "s.appendTermBegin(" << com << ");\n";
    if (mStream != 0)
      mStream->appendTermBegin(com);
  }

  template<class Stream> 
  void IdealStreamLog<Stream>::appendExponent(VarIndex index, Exponent exponent) {
    mLog << "s.appendExponent(" << index << ", " << exponent <<
      "); // index, exponent\n";
    if (mStream != 0)
      mStream->appendExponent(index, exponent);
  }

  template<class Stream> 
  void IdealStreamLog<Stream>::appendTermDone(Coefficient coefficient) {
    mLog << "s.appendTermDone(" << coefficient << "); // coefficient\n";
    if (mStream != 0)
      mStream->appendTermDone(coefficient);
  }

  template<class Stream> 
  void IdealStreamLog<Stream>::appendPolynomialDone() {
    mLog << "s.appendPolynomialDone();\n";
    if (mStream != 0)
      mStream->appendPolynomialDone();
  }

  template<class Stream> 
  void IdealStreamLog<Stream>::idealDone() {
    mLog << "s.idealDone();\n";
    if (mStream != 0)
      mStream->idealDone();
  }


  // ** Implementation of the class IdealStreamChecker
  // This class has to be inline as it is a template.

  template<class Stream>
  IdealStreamChecker<Stream>::IdealStreamChecker(Stream& stream):
    mStream(stream),
    mChecker(stream.modulus(), stream.varCount(), stream.comCount())
  {}

  template<class Stream>
  typename IdealStreamChecker<Stream>::Coefficient
  IdealStreamChecker<Stream>::modulus() const {
    return mStream.modulus();
  }

  template<class Stream>
  typename IdealStreamChecker<Stream>::VarIndex
  IdealStreamChecker<Stream>::varCount() const {
    return mStream.varCount();
  }

  template<class Stream>
  typename IdealStreamChecker<Stream>::Component
  IdealStreamChecker<Stream>::comCount() const {
    return mStream.comCount();
  }

  template<class Stream>
  void IdealStreamChecker<Stream>::idealBegin() {
    mChecker.idealBegin();
    mStream.idealBegin();
  }

  template<class Stream>
  void IdealStreamChecker<Stream>::idealBegin(size_t polyCount) {
    mChecker.idealBegin(polyCount);
    mStream.idealBegin(polyCount);
  }

  template<class Stream>
  void IdealStreamChecker<Stream>::appendPolynomialBegin() {
    mChecker.appendPolynomialBegin();
    mStream.appendPolynomialBegin();
  }

  template<class Stream>
  void IdealStreamChecker<Stream>::appendPolynomialBegin(size_t termCount) {
    mChecker.appendPolynomialBegin(termCount);
    mStream.appendPolynomialBegin(termCount);
  }

  template<class Stream>
  void IdealStreamChecker<Stream>::appendTermBegin(const Component com) {
    mChecker.appendTermBegin(com);
    mStream.appendTermBegin(com);
  }

  template<class Stream>
  void IdealStreamChecker<Stream>::appendExponent(VarIndex index, Exponent exponent) {
    mChecker.appendExponent(index, exponent);
    mStream.appendExponent(index, exponent);
  }

  template<class Stream>
  void IdealStreamChecker<Stream>::appendTermDone(Coefficient coefficient) {
    mChecker.appendTermDone(coefficient);
    mStream.appendTermDone(coefficient);
  }

  template<class Stream>
  void IdealStreamChecker<Stream>::appendPolynomialDone() {
    mChecker.appendPolynomialDone();
    mStream.appendPolynomialDone();
  }

  template<class Stream>
  void IdealStreamChecker<Stream>::idealDone() {
    mChecker.idealDone();
    mStream.idealDone();
  }

  // ** Implementation of the class NullIdealStream
  // This class isn't a template, but it might become one and it is so
  // trivial that making it inline is not a big deal - it's not much more
  // than just its interface. The idea is also for this class to have minimal
  // overhead and making it inline helps with that.

  inline NullIdealStream::NullIdealStream(
    Coefficient modulus,
    VarIndex varCount,
    Component comCount
  ):
    mModulus(modulus),
    mVarCount(varCount),
    mComCount(comCount)
  {}

  namespace mgbi {
    /// Used to read an internal MathicGB ideal without exposing the type of
    /// the ideal.
    class IdealAdapter {
    public:
      typedef GroebnerConfiguration::Coefficient Coefficient;
      typedef GroebnerConfiguration::VarIndex VarIndex;
      typedef GroebnerConfiguration::Exponent Exponent;
      typedef GroebnerConfiguration::Component Component;
      typedef size_t PolyIndex;
      typedef size_t TermIndex;

      struct ConstTerm {
        Coefficient coef;
        const Exponent* exponents;
        Component com;
      };

      IdealAdapter();
      ~IdealAdapter();

      VarIndex varCount() const;
      size_t polyCount() const;
      size_t termCount(PolyIndex poly) const;
      Component componentCount() const;

      /// Sets the internal position to the first term of the first polynomial.
      void toFirstTerm();

      /// Returns the next term. First all terms of polynomial 0 are returned,
      /// then all terms of the next polynomial and so on.
      ConstTerm nextTerm() const;

    private:
      friend class mgbi::PimplOf;
      struct Pimpl;
      Pimpl* mPimpl;
    };

    bool internalComputeGroebnerBasis(
      GroebnerInputIdealStream& inputWhichWillBeCleared,
      IdealAdapter& output
    );
  }

  template<class OutputStream>
  void computeGroebnerBasis(
    GroebnerInputIdealStream& inputWhichWillBeCleared,
    OutputStream& output
  ) {
    typedef mgbi::IdealAdapter::ConstTerm ConstTerm;
    mgbi::IdealAdapter ideal;
    const bool doOutput =
      mgbi::internalComputeGroebnerBasis(inputWhichWillBeCleared, ideal);
    if (!doOutput)
      return;

    ideal.toFirstTerm();
    const size_t varCount = ideal.varCount();
    const size_t polyCount = ideal.polyCount();
    output.idealBegin(polyCount);
    for (size_t polyIndex = 0; polyIndex < polyCount; ++polyIndex) {
      const size_t termCount = ideal.termCount(polyIndex);
      output.appendPolynomialBegin(termCount);
      for (size_t termIndex = 0; termIndex < termCount; ++termIndex) {
        const ConstTerm term = ideal.nextTerm();
        output.appendTermBegin(term.com);
        for (size_t var = 0; var < varCount; ++var)
          output.appendExponent(var, term.exponents[var]);
        output.appendTermDone(term.coef);
      }
      output.appendPolynomialDone();
    }
    output.idealDone();
  }
}

#endif
