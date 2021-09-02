// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_MONO_MONOID_GUARD
#define MATHICGB_MONO_MONOID_GUARD

#include "MonoOrder.hpp"
#include "NonCopyable.hpp"
#include "memtailor/memtailor.h"
#include "mathic/mathic.h"
#include <cstddef>
#include <vector>
#include <algorithm>
#include <type_traits>
#include <istream>
#include <utility>
#include <ostream>
#include <cstdlib>
#include <cstring>
#include <cctype>

MATHICGB_NAMESPACE_BEGIN

/// Implements the monoid of (monic) monomials with integer
/// non-negative exponents. Exponent must be an unsigned integer type that is
/// used to store each exponent of a monomial.
template<
  class Exponent,
  bool HasComponent = true,
  bool StoreHash = true,
  bool StoreOrder = true
>
class MonoMonoid;

namespace MonoMonoidInternal {
  template<class E, bool HC, bool SH, bool SO>
  class Base {
  public:
    static const bool HasComponent = HC;
    static const bool StoreHash = SH;
    static const bool StoreOrder = SO;

    typedef size_t VarIndex;
    typedef E Exponent;
    typedef typename std::make_unsigned<E>::type Component;
    typedef typename std::make_unsigned<E>::type HashValue;
    typedef const Exponent* const_iterator;
    typedef MonoOrder<Exponent> Order;

    Base(const Order& order):
      mVarCount(order.varCount()),
      mGradingCount(
        order.gradingCount() +
          (order.componentBefore() != Order::ComponentAfterBaseOrder)
      ),
      mOrderIndexBegin(HasComponent + order.varCount()),
      mOrderIndexEnd(mOrderIndexBegin + StoreOrder * mGradingCount),
      mEntryCount(std::max<VarIndex>(mOrderIndexEnd + StoreHash, 1)),
      mComponentGradingIndex(
        reverseComponentGradingIndex(mGradingCount, order.componentBefore())
      ),
      mHashCoefficients(makeHashCoefficients(order.varCount())),
      mOrderIsTotalDegreeRevLex(
        !order.hasLexBaseOrder() &&
        order.isTotalDegree() &&
        order.componentBefore() == Order::ComponentAfterBaseOrder
      ),
      mLexBaseOrder(order.hasLexBaseOrder()),
      mGradings(makeGradings(order)),
      mVarsReversed(order.hasFromLeftBaseOrder())
    {
      MATHICGB_ASSERT(order.isMonomialOrder());
      MATHICGB_ASSERT(mGradings.size() == gradingCount() * varCount());
    }

    VarIndex varCount() const {return mVarCount;}
    VarIndex gradingCount() const {return mGradingCount;}

    VarIndex entryCount() const {return mEntryCount;}
    VarIndex orderIndexEnd() const {return mOrderIndexEnd;}
    VarIndex orderIndexBegin() const {return mOrderIndexBegin;}
    VarIndex hashIndex() const {return mOrderIndexEnd;}
    VarIndex componentGradingIndex() const {return mComponentGradingIndex;}
    bool varsReversed() const {return mVarsReversed;}

  protected:
    typedef std::vector<Exponent> HashCoefficients;
    typedef std::vector<Exponent> Gradings;

    static Gradings makeGradings(const Order& order) {
      auto gradings = order.gradings();
      if (order.componentBefore() != Order::ComponentAfterBaseOrder)
        insertZeroRow(order.varCount(), order.componentBefore(), gradings);
      reverseGradings(order.varCount(), gradings);
      if (order.hasFromLeftBaseOrder())
        reverseVarsInGradings(order.varCount(), gradings);
      if (!order.hasLexBaseOrder())
        negateGradings(gradings);
      return gradings;
    }

    /// Reverse the relative order among the gradings - the first one
    /// becomes the last one, the second first becomes the second last
    /// and so on.
    static void reverseGradings(const VarIndex varCount, Gradings& gradings) {
      if (varCount == 0)
        return;
      MATHICGB_ASSERT(gradings.size() % varCount == 0);
      const auto gradingCount = gradings.size() / varCount;

      for (VarIndex grading = 0; grading < gradingCount / 2; ++grading) {
        for (VarIndex var = 0; var < varCount; ++var) {
          const auto index = gradingsIndex(grading, var, varCount);
          const auto oppositeIndex = gradingsOppositeRowIndex
            (grading, gradingCount, var, varCount);
          std::swap(gradings[index], gradings[oppositeIndex]);
        }
      }
    }

    /// Replace each entry in the grading matrix with its negative.
    static void negateGradings(Gradings& gradings) {
      const auto size = gradings.size();
      for (size_t i = 0; i < size; ++i)
        gradings[i] = -gradings[i];
    }

    static void insertZeroRow(
      const VarIndex varCount,
      const size_t insertBeforeRow,
      Gradings& gradings
    ) {
      if (varCount == 0)
        return;
      MATHICGB_ASSERT(gradings.size() % varCount == 0);
      MATHICGB_ASSERT(insertBeforeRow <= gradings.size() / varCount);
      gradings.resize(gradings.size() + varCount);
      const auto rowBegin = gradings.begin() + insertBeforeRow * varCount;
      std::copy_backward(rowBegin, gradings.end() - varCount, gradings.end());
      std::fill_n(rowBegin, varCount, 0);
    }

    static void removeZeroRow(
      const VarIndex varCount,
      const size_t row,
      Gradings& gradings
    ) {
      if (varCount == 0)
        return;
      MATHICGB_ASSERT(gradings.size() % varCount == 0);
      MATHICGB_ASSERT(row < gradings.size() / varCount);
      const auto rowBegin = gradings.begin() + row * varCount;
      std::copy(rowBegin + varCount, gradings.end(), rowBegin);
      gradings.resize(gradings.size() - varCount);
    }

    /// Replace each row (e_0, e_1, ..., e_n) with (e_n, ..., e_1, e_0).
    static void reverseVarsInGradings(
      const size_t varCount,
      Gradings& gradings
    ) {
      if (varCount == 0)
        return;
      MATHICGB_ASSERT(gradings.size() % varCount == 0);
      auto rowBegin = gradings.begin();
      while (rowBegin != gradings.end()) {
        const auto rowEnd = rowBegin + varCount;
        std::reverse(rowBegin, rowEnd);
        rowBegin = rowEnd;
      }
    }

    /// Since comparisons go opposite direction, we need to reverse
    /// the component grading index, unless it's the special value
    /// indicating that it goes last.
    static VarIndex reverseComponentGradingIndex(
      const VarIndex gradingCount,
      const VarIndex componentGradingIndex
    ) {
      if (componentGradingIndex == Order::ComponentAfterBaseOrder)
        return Order::ComponentAfterBaseOrder;
      else
        return gradingCount - 1 - componentGradingIndex;
    }

    const HashCoefficients& hashCoefficients() const {return mHashCoefficients;}
    bool orderIsTotalDegreeRevLex() const {return mOrderIsTotalDegreeRevLex;}
    Gradings& gradings() {return mGradings;} // todo: remove this overload
    const Gradings& gradings() const {return mGradings;}
    bool isLexBaseOrder() const {return mLexBaseOrder;}

    static size_t gradingsIndex(
      const VarIndex grading,
      const VarIndex var,
      const VarIndex varCount
    ) {
      MATHICGB_ASSERT(var < varCount);
      return grading * static_cast<size_t>(varCount) + var;
    }

    size_t gradingsIndex(const VarIndex grading, const VarIndex var) const {
      MATHICGB_ASSERT(grading < gradingCount());
      MATHICGB_ASSERT(var < varCount());
      const auto index = gradingsIndex(grading, var, varCount());
      MATHICGB_ASSERT(index < gradings().size());
      return index;
    }

    static size_t gradingsOppositeRowIndex(
      const VarIndex grading,
      const VarIndex gradingCount,
      const VarIndex var,
      const VarIndex varCount
    ) {
      MATHICGB_ASSERT(grading < gradingCount);
      MATHICGB_ASSERT(var < varCount);
      return gradingsIndex(gradingCount - 1 - grading, var, varCount);
    }

    size_t gradingsOppositeRowIndex(
      const VarIndex grading,
      const VarIndex var
    ) const {
      MATHICGB_ASSERT(grading < gradingCount());
      MATHICGB_ASSERT(var < varCount());
      const auto index =
        gradingsOppositeRowIndex(grading, gradingCount(), var, varCount());
      MATHICGB_ASSERT(index < gradings().size());
      return index;
    }

  private:
    HashCoefficients static makeHashCoefficients(const VarIndex varCount) {
      std::srand(0); // To use the same hash coefficients every time.
      HashCoefficients coeffs(varCount);
      for (VarIndex var = 0; var < varCount; ++var)
        coeffs[var] = static_cast<HashValue>(std::rand());
      return coeffs;
    }

    const VarIndex mVarCount;
    const VarIndex mGradingCount;
    const VarIndex mOrderIndexBegin;
    const VarIndex mOrderIndexEnd;
    const VarIndex mEntryCount;
    const VarIndex mComponentGradingIndex;

    /// Take dot product of exponents with this vector to get hash value.
    const HashCoefficients mHashCoefficients;

    /// This is initialized before mGradings, so it has to be ordered
    /// above mGradings. 
    const bool mOrderIsTotalDegreeRevLex;

    /// If true then lex is used to break ties. Otherwise, revlex is
    /// used. This applies as well to degrees, which implies that
    /// degrees have to be stored negated if doing revlex.
    const bool mLexBaseOrder;

    /// Defines a matrix where each row is a grading. The degree of a
    /// monomial with respect to grading g is the dot product of the
    /// exponent vector of that monomial with row g of the matrix
    /// (starting at g=0). The matrix is stored in row-major order. If
    /// mOrderIsTotalDegreeRevLex is true then mGradings is empty but
    /// implicitly it is a single grading consisting of all 1s and the
    /// base order is revlex.
    std::vector<Exponent> mGradings;

    /// All base comparison considers exponents starting from the right,
    /// yet we need to support base orders starting from the left. This
    /// is achieved by reversing the order of the variables. The value
    /// of this variable indicates whether this has happened, in which
    /// case it needs to be done again before showing a monomial to the
    /// outside world.
    const bool mVarsReversed;
  };
}

template<class E, bool HC, bool SH, bool SO>
class MonoMonoid : private MonoMonoidInternal::Base<E, HC, SH, SO> {
private:
  typedef MonoMonoidInternal::Base<E, HC, SH, SO> Base;

public:
  static_assert(std::numeric_limits<E>::is_signed, "");

  // *** Types

  // Integer index representing a variable. Indices start at 0 and go
  // up to varCount() - 1 where varCount() is the number of variables.
  typedef typename Base::VarIndex VarIndex;

  /// The type of each exponent of a monomial.
  typedef typename Base::Exponent Exponent;

  /// Is true if the monomials come from a module.
  using Base::HasComponent;

  /// Is true if the hash value is stored rather than computed at each 
  /// hash request. This imposes extra computation when updating a monomial,
  /// but for most operations that overhead is much less than the time for
  /// computing a hash value from scratch.
  using Base::StoreHash;

  /// Is true if data to compare monomials is stored rather than computed
  /// at each comparison. As storeHash, there is overhead for this, but it
  /// is not much for most operations.
  using Base::StoreOrder;

  /// Type used to indicate the component of a module monomial. For example,
  /// the component of xe_3 is 3.
  typedef typename Base::Component Component;

  /// Type used to store hash values of monomials.
  typedef typename Base::HashValue HashValue;

  /// Iterator for the exponents in a monomial.
  typedef typename Base::const_iterator const_iterator;

  /// Represents a monomial and manages the memory underlying it. To
  /// refer to a non-owned monomial or to refer to a Mono, use MonoRef
  /// or ConstMonoRef. Do not use Mono& or Mono* if you do not have
  /// to, since that implies a double indirection when accessing the
  /// monomial.
  class Mono;

  /// A reference to a non-const monomial. Cannot be null, cannot be
  /// reassigned to refer to a different monomial and does not connote
  /// ownership - the same semantics as C++ references.
  class MonoRef;

  /// A reference to a monomial. As MonoRef, but you cannot change the
  /// monomial through this reference. Prefer this class over the
  /// other reference/pointer classes unless there is a reason not to.
  class ConstMonoRef;

  /// A pointer to a non-const monomial. Can be null and can be
  /// reassigned to refer to a different monomial - the same semantics
  /// as C++ pointers. Does not connote ownership.
  class MonoPtr;

  /// A pointer to a monomial. As MonoPtr, but you cannot change the
  /// monomial through this pointer.
  class ConstMonoPtr;

  /// A pool of memory for monomials.
  ///
  /// @todo: This approach is a poor fit for variable-sized
  /// monomials. So prefer other solutions where reasonable.
  class MonoPool;

  /// A vector of monomials. The interface is a subset of
  /// std::vector. Monomials can be appended (push_back). Only the
  /// last monomial can be mutated and monomials cannot be reordered
  /// or removed. These restrictions should make it easier to support
  /// variable-sized monomials in future. Change it if you need to
  /// break these restrictions, but first try to find an alternative.
  class MonoVector;

  /// For indicating the result of comparing one monomial to another.
  enum CompareResult {
    LessThan = -1,
    EqualTo = 0,
    GreaterThan = 1
  };

  /// Used to describe a monomial order when constructing a monoid.
  typedef typename Base::Order Order;

  // *** Temporary compatibility code for migrating off PolyRing
  friend class PolyRing;
  friend class Poly;
  static MonoRef toRef(Exponent* e) {return MonoRef(e);}
  static ConstMonoRef toRef(const Exponent* e) {return ConstMonoRef(e);}
  static Exponent* toOld(MonoRef e) {return rawPtr(e);}
  static const Exponent* toOld(ConstMonoRef e) {return rawPtr(e);}
  static Exponent* toOld(Mono& e) {return rawPtr(e);}
  static const Exponent* toOld(const Mono& e) {return rawPtr(e);}


  // *** Constructors and accessors

  MonoMonoid(MonoMonoid&& monoid): Base(std::move(monoid)), mPool(*this) {
    MATHICGB_ASSERT(debugAssertValid());
  }

  MonoMonoid(const MonoMonoid& monoid): Base(monoid), mPool(*this) {
    MATHICGB_ASSERT(debugAssertValid());
  }

  MonoMonoid(const Order& order): Base(order), mPool(*this) {
    MATHICGB_ASSERT(debugAssertValid());
  }

  /// Creates a compatible copy of monoid.
  template<class E2, bool HC2, bool SH2, bool SO2>
  static MonoMonoid create(const MonoMonoid<E2, HC2, SH2, SO2>& monoid) {
    return MonoMonoid(monoid.makeOrder(false, false));
  }

  /// Returns an Order object that is equivalent to the order that
  /// this monoid was constructed with. The settings not handled by
  /// the monoid, and therefore not known by the monoid, are passed in
  /// as parameters. The purpose of that is to make it clear that this
  /// information must be supplied separately.
  Order makeOrder(
    const bool componentsAscendingDesired,
    const bool schreyering
  ) const {
    std::vector<Exponent> orderGradings(gradings());
    reverseGradings(varCount(), orderGradings);
    if (!isLexBaseOrder())
      negateGradings(orderGradings);
    const auto componentIndex = Base::reverseComponentGradingIndex
      (gradingCount(), componentGradingIndex());
    if (componentIndex != Order::ComponentAfterBaseOrder)
      Base::removeZeroRow(varCount(), componentIndex, orderGradings);
    return Order(
      varCount(),
      std::move(orderGradings),
      isLexBaseOrder() ?
        Order::LexBaseOrderFromRight : Order::RevLexBaseOrderFromRight,
      componentIndex,
      componentsAscendingDesired,
      schreyering
    );
  }

  /// Returns true if higher component is considered greater when
  /// comparing module monomials. Only relevant once actually
  /// considering the component. This is only relevant for module
  /// monomials.
  bool componentsAscending() const {return isLexBaseOrder();}

  /// Returns the number of variables. This is also the number of
  /// exponents in the exponent vector of a monomial.
  using Base::varCount;

  /// Returns true if the variables in this ring have been reversed
  /// so that the first one becomes the last one and so on. Use
  /// MonoProcessor to reverse monomials for input and output.
  using Base::varsReversed;


  // *** Monomial accessors and queries

  /// Returns iterator to the first exponent.
  const_iterator begin(ConstMonoRef mono) const {
    return ptr(mono, exponentsIndexBegin());
  }

  /// Returns iterator to one-past-the-end of the range of exponents.
  const_iterator end(ConstMonoRef mono) const {
    return ptr(mono, exponentsIndexEnd());
  }

  /// The indices of variables might be permuted in this monoid in order to
  /// implement certain monomial orders in a fast way. This method returns
  /// the unpermuted index that gets permuted to var. Knowing this mapping
  /// is necessary when importing monomials from and when exporting monomials
  /// to a party that does not use this scheme. For example when writing a
  /// monomial to a file in a format that is intended to be readable by
  /// other programs or by a human.
  VarIndex externalVar(const VarIndex var) const {
    // At the moment, this method happens to be its own inverse. Do not depend
    // on that.
    MATHICGB_ASSERT(var < varCount());
    const auto eVar = varsReversed() ? varCount() - 1 - var : var;
    MATHICGB_ASSERT(eVar < varCount());
    return eVar;
  }

  /// The inverse mapping of externalVar().
  VarIndex internalVar(const VarIndex var) const {
    // Do not depend on knowing that this is the implementation.
    return externalVar(var);
  }

  /// Returns the exponent of var in mono.
  Exponent exponent(ConstMonoRef mono, const VarIndex var) const {
    MATHICGB_ASSERT(var < varCount());
    return access(mono, exponentsIndexBegin() + var);
  }

  // The exponent of var as it would be if variables were not permuted.
  Exponent externalExponent(ConstMonoRef mono, const VarIndex var) const {
    MATHICGB_ASSERT(var < varCount());
    return exponent(mono, externalVar(var));
  }

  /// Returns the component of the monomial. Monomials not from a
  /// module have component zero. In a module mono*e_i has component
  /// i. @todo: Have different monoids for module monomials and
  /// monomials and only offer this method for the module monomials.
  Component component(ConstMonoRef mono) const {
    MATHICGB_ASSERT(HasComponent);
    return access(mono, componentIndex());
  }

  /// Returns a hash value for the monomial. These are not guaranteed
  /// to be unique.
  HashValue hash(ConstMonoRef mono) const {
    MATHICGB_ASSERT(debugHashValid(mono));
    if (StoreHash)
      return static_cast<HashValue>(access(mono, hashIndex()));
    else
      return computeHash(mono);
  }

  /// Returns true if a and b are equal. Includes check for component.
  bool equal(ConstMonoRef a, ConstMonoRef b) const {
    for (auto i = entriesIndexBegin(); i != exponentsIndexEnd(); ++i)
      if (access(a, i) != access(b, i))
        return false;
    return true;
  }

  template<class MonoidA>
  bool equal(
    const MonoidA& monoidA,
    typename MonoidA::ConstMonoRef a,
    ConstMonoRef b
  ) const {
    // todo: assert compatible
    for (VarIndex var = 0; var < varCount(); ++var)
      if (monoidA.exponent(a, var) != exponent(b, var))
        return false;
    return true;
  }

  /// As equal(), but optimized for the case where true is returned.
  bool equalHintTrue(ConstMonoRef a, ConstMonoRef b) const {
    // if a[i] != b[i] then a[i] ^ b[i] != 0, so the or of all xors is zero
    // if and only if a equals b. This way we avoid having a branch to check
    // equality for every iteration of the loop, which is a win in the case
    // that none of the early-exit branches are taken - that is, when a equals
    // b.
    Exponent orOfXor = 0;
    for (VarIndex i = lastExponentIndex(); i != beforeEntriesIndexBegin(); --i)
      orOfXor |= access(a, i) ^ access(b, i);
    MATHICGB_ASSERT((orOfXor == 0) == equal(a, b));
    return orOfXor == 0;
  }

  bool isProductOf(
    ConstMonoRef a,
    ConstMonoRef b,
    ConstMonoRef ab
  ) const {
    for (VarIndex i = entriesIndexBegin(); i != exponentsIndexEnd(); ++i)
      if (access(ab, i) != access(a, i) + access(b, i))
        return false;
    return true;
  }

  bool isProductOfHintTrue(
    ConstMonoRef a, 
    ConstMonoRef b, 
    ConstMonoRef ab
  ) const {
    // We compare more than one exponent at a time using 64 bit integers. This 
    // might go one 32 bit value at the end too far, but since that space is
    // either a degree or a hash value that is fine --- those values will also
    // match if the monomials are equal. This does not work for negative
    // exponents since the overflowing bit will go into the next word.
    // It is OK that the degree field can be negative (a field we might go
    // into without caring about it because it shares a 64 bit field with
    // the last exponent), because it is at the end so the overflowing
    // bit will not interfere. For this reason we need to have a degree
    // or a hash value stored there - otherwise two equal monomials could
    // have different things stored next to them which would confuse this code.
    
    // todo: ensure 8 byte alignment. Though there seem to be no ill effects
    // for unaligned access. Performance seems to be no worse than for using
    // 32 bit integers directly.

    if (sizeof(Exponent) != 4 || (!StoreHash && !StoreOrder))
      return isProductOf(a, b, ab);

    uint64 orOfXor = 0;
    for (VarIndex i = varCount() / 2; i != beforeEntriesIndexBegin(); --i) {
      MATHICGB_ASSERT(access(a, i*2) >= 0);
      MATHICGB_ASSERT(i == varCount() / 2 || access(a, i*2+1) >= 0);
      
      uint64 A, B, AB;
      // We have to use std::memcpy here because just casting to a int64 breaks
      // the strict aliasing rule which implies undefined behavior. Both MSVC and
      // gcc don't actually call memcpy here. MSVC is a tiny bit slower for this
      // code than for casting while GCC seems to be exactly the same speed.
      std::memcpy(&A, ptr(a, i*2), 8);
      std::memcpy(&B, ptr(b, i*2), 8);
      std::memcpy(&AB, ptr(ab, i*2), 8);
      orOfXor |= AB ^ (A + B);
    }
    MATHICGB_ASSERT((orOfXor == 0) == isProductOf(a, b, ab));
    return orOfXor == 0; 
  }

  MATHICGB_INLINE bool isTwoProductsOfHintTrue(
    ConstMonoRef a1,
    ConstMonoRef a2,
    ConstMonoRef b,
    ConstMonoRef a1b,
    ConstMonoRef a2b
  ) const {
    if (sizeof(Exponent) != 4 || (!StoreHash && !StoreOrder))
      return (isProductOf(a1, b, a1b) && isProductOf(a2, b, a2b));

    uint64 orOfXor = 0;
    for (VarIndex i = varCount() / 2; i != beforeEntriesIndexBegin(); --i) {
      uint64 A1, A2, B, A1B, A2B;
      std::memcpy(&A1, ptr(a1, i*2), 8);
      std::memcpy(&A2, ptr(a2, i*2), 8);
      std::memcpy(&B, ptr(b, i*2), 8);
      std::memcpy(&A1B, ptr(a1b, i*2), 8);
      std::memcpy(&A2B, ptr(a2b, i*2), 8);
      orOfXor |= (A1B ^ (A1 + B)) | (A2B ^ (A2 + B));
    }
    MATHICGB_ASSERT
      ((orOfXor == 0) == (isProductOf(a1, b, a1b) && isProductOf(a2, b, a2b)));
    return orOfXor == 0;
  }

  /// Returns the hash of the product of a and b.
  HashValue hashOfProduct(ConstMonoRef a, ConstMonoRef b) const {
    // See computeHash() for an explanation of all the casts.
    const auto hashA = static_cast<HashValue>(hash(a));
    const auto hashB = static_cast<HashValue>(hash(b));
    return static_cast<HashValue>(static_cast<Exponent>(hashA + hashB));
  }

  /// Returns true if all the exponents of mono are zero. In other
  /// words, returns true if mono is the identity for multiplication
  /// of monomials.
  bool isIdentity(ConstMonoRef mono) const {
    return std::all_of(begin(mono), end(mono), [](Exponent e) {return e == 0;});
  }

  /// Returns true if a divides b. Equal monomials divide each other.
  /// Doesn't take component into account - see dividesWithComponent.
  bool divides(ConstMonoRef div, ConstMonoRef into) const {
    // todo: enable this when the code works with it - see 
    // dividesWithComponent.
    //if (HasComponent && component(div) != component(into))
    //  return false;
    for (auto i = exponentsIndexBegin(); i < exponentsIndexEnd(); ++i)
      if (access(div, i) > access(into, i))
        return false;
    return true;
  }

  /// Returns true if a divides b. Equal monomials divide each other.
  /// This also takes the component into account. Once the code base is
  /// fixed to properly observe the distinction between monomials and
  /// module monomials, there will only need to be the one divides()
  /// which takes component into account if and only if HasComponent is
  /// true. For now, we're left with this imperfect solution of two
  /// overloads.
  ///
  /// @todo: get rid of this method.
  bool dividesWithComponent(ConstMonoRef div, ConstMonoRef into) const {
    if (HasComponent && component(div) != component(into))
      return false;
    for (auto i = exponentsIndexBegin(); i < exponentsIndexEnd(); ++i)
      if (access(div, i) > access(into, i))
        return false;
    return true;
  }

  template<class MonoidA>
  bool divides(
    const MonoidA& monoidA,
    typename MonoidA::ConstMonoRef a,
    ConstMonoRef b
  ) const {
    // todo: fix other divisibility functions to work properly for component too.
    MATHICGB_ASSERT(monoidA.varCount() == varCount());
    MATHICGB_ASSERT(!MonoidA::HasComponent || HasComponent);
    MATHICGB_ASSERT(monoidA.debugValid(a));
    MATHICGB_ASSERT(debugValid(b));
    // todo: enable this when the code works with it
    //if (HasComponent && component(div) != component(into))
    //  return false;
    //if (
    //  MonoidA::HasComponent &&
    //  HasComponent &&
    //  monoidA.component(a) != component(b)
    //)
    //  return false;
    for (VarIndex var = 0; var < varCount(); ++var)
      if (monoidA.exponent(a, var) > exponent(b, var))
        return false;
    return true;
  }

  /// @todo: get rid of this -- see other overload.
  template<class MonoidA>
  bool dividesWithComponent(
    const MonoidA& monoidA,
    typename MonoidA::ConstMonoRef a,
    ConstMonoRef b
  ) const {
    // todo: fix other divisibility functions to work properly for component too.
    MATHICGB_ASSERT(monoidA.varCount() == varCount());
    MATHICGB_ASSERT(!MonoidA::HasComponent || HasComponent);
    MATHICGB_ASSERT(monoidA.debugValid(a));
    MATHICGB_ASSERT(debugValid(b));
    if (
      MonoidA::HasComponent &&
      HasComponent &&
      monoidA.component(a) != component(b)
    )
      return false;
    for (VarIndex var = 0; var < varCount(); ++var)
      if (monoidA.exponent(a, var) > exponent(b, var))
        return false;
    return true;
  }

  /// Returns true if div divides lcm(a, b).
  bool dividesLcm(ConstMonoRef div, ConstMonoRef a, ConstMonoRef b) const {
    MATHICGB_ASSERT(debugLcmCheck(*this, a, *this, b));
    MATHICGB_ASSERT(debugValid(div));

    for (auto i = exponentsIndexBegin(); i != exponentsIndexEnd(); ++i) {
      //const auto dive = access(div, i);
      if (access(div, i) > access(a, i) && access(div, i) > access(b, i))
        return false;
    }
    return true;
  }

  template<class MonoidDiv, class MonoidA>
  bool dividesLcm(
    const MonoidDiv& monoidDiv,
    typename MonoidDiv::ConstMonoRef div,
    const MonoidA& monoidA,
    typename MonoidA::ConstMonoRef a,
    ConstMonoRef b
  ) const {
    MATHICGB_ASSERT(monoidDiv.debugLcmCheck(monoidA, a, *this, b));
    MATHICGB_ASSERT(monoidDiv.debugValid(div));

    for (VarIndex var = 0; var < varCount(); ++var) {
      const auto e = monoidDiv.exponent(div, var);
      if (e > monoidA.exponent(a, var) && e > exponent(b, var))
        return false;
    }
    return true;
  }

  /// Returns true if lcm(a,b) == lcmAB.
  bool isLcm(ConstMonoRef a, ConstMonoRef b, ConstMonoRef lcmAB) const {
    MATHICGB_ASSERT(debugLcmCheck(*this, a, *this, b));
    MATHICGB_ASSERT(debugValid(lcmAB));

    for (auto i = exponentsIndexBegin(); i != exponentsIndexEnd(); ++i)
      if (access(lcmAB, i) != std::max(access(a, i), access(b, i)))
        return false;
    return true;
  }

  template<class MonoidA, class MonoidB>
  bool isLcm(
    const MonoidA& monoidA,
    typename MonoidA::ConstMonoRef a,
    const MonoidB& monoidB,
    typename MonoidB::ConstMonoRef b,
    ConstMonoRef lcmAB
  ) const {
    MATHICGB_ASSERT(debugLcmCheck(monoidA, a, monoidB, b));
    MATHICGB_ASSERT(debugValid(lcmAB));

    if (HasComponent) {
      if (MonoidA::HasComponent) {
        if (monoidA.component(a) != component(lcmAB))
          return false;
      } else {
        MATHICGB_ASSERT(MonoidB::HasComponent);
        if (monoidB.component(b) != component(lcmAB))
          return false;
      }
    }

    for (VarIndex var = 0; var < varCount(); ++var) {
      if (
        ptr(lcmAB, exponentsIndexBegin())[var] !=
        std::max(monoidA.exponent(a, var), monoidB.exponent(b, var))
      )
        return false;
    }
    return true;
  }

  CompareResult compare(ConstMonoRef a, ConstMonoRef b) const {
    MATHICGB_ASSERT(debugOrderValid(a));
    MATHICGB_ASSERT(debugOrderValid(b));

    VarIndex index;
    
    if (StoreOrder)
      index = orderIndexEnd();
    else {
      // Check the degrees seperately since they are not stored.
      auto grading = gradingCount();
      while (grading != 0) {
        --grading;
        const auto cmp = degree(a, grading) - degree(b, grading);
        if (cmp < 0) return isLexBaseOrder() ? LessThan : GreaterThan;
        if (cmp > 0) return isLexBaseOrder() ? GreaterThan : LessThan;
      }
      index = exponentsIndexEnd();
    }

    // If StoreOrder is true then this first checks the degrees.
    // Then the exponents are checked.
    // Finally, if HasComponent is true, the component is checked.
    while (index != entriesIndexBegin()) {
      --index;
      const auto cmp = access(a, index) - access(b, index);
      if (cmp < 0) return isLexBaseOrder() ? LessThan : GreaterThan;
      if (cmp > 0) return isLexBaseOrder() ? GreaterThan : LessThan;
    }
    return EqualTo;
  }

  /// Compares a to b1*b2.
  /// @todo: Test this method. Also, is this method actually useful, or could
  /// it just as well be replaced by a multiplication and a comparison?
  CompareResult compare(ConstMonoRef a, ConstMonoRef b1, ConstMonoRef b2) const {
    MATHICGB_ASSERT(debugOrderValid(a));
    MATHICGB_ASSERT(debugOrderValid(b1));
    MATHICGB_ASSERT(debugOrderValid(b2));

    VarIndex index;

    if (StoreOrder)
      index = orderIndexEnd();
    else {
      // Check the degrees seperately since they are not stored.
      auto grading = gradingCount();
      while (grading != 0) {
        --grading;
        const auto cmp =
          degree(a, grading) - (degree(b1, grading) + degree(b2, grading));
        if (cmp < 0) return isLexBaseOrder() ? LessThan : GreaterThan;
        if (cmp > 0) return isLexBaseOrder() ? GreaterThan : LessThan;
      }
      index = exponentsIndexEnd();
    }

    // If StoreOrder is true then this first checks the degrees.
    // Then the exponents are checked.
    // Finally, if HasComponent is true, the component is checked.
    while (index != entriesIndexBegin()) {
      --index;
      const auto cmp =
        access(a, index) - (access(b1, index) + access(b2, index));
      if (cmp < 0) return isLexBaseOrder() ? LessThan : GreaterThan;
      if (cmp > 0) return isLexBaseOrder() ? GreaterThan : LessThan;
    }
    return EqualTo;
  }

  bool lessThan(ConstMonoRef a, ConstMonoRef b) const {
    return compare(a, b) == LessThan;
  }

  /// Returns true if gcd(a, b) == 1.
  bool relativelyPrime(ConstMonoRef a, ConstMonoRef b) const {
    for (auto i = exponentsIndexBegin(); i != exponentsIndexEnd(); ++i)
      if (access(a, i) > 0 && access(b, i) > 0)
        return false;
    return true;
  }

  // If this method returns true for monomials a and b then it is
  // guaranteed that multiplying a and b together will not overflow
  // the integers in the representation.
  bool hasAmpleCapacity(ConstMonoRef mono) const {
    const auto halfMin = std::numeric_limits<Exponent>::min() / 2;
    const auto halfMax = std::numeric_limits<Exponent>::max() / 2;
    MATHICGB_ASSERT(halfMin <= 0);
    const auto limit = std::min(-halfMin, halfMax);
    const auto inRange = [&](Exponent value)
      {return -limit <= value && value <= limit;};

    for (VarIndex i = exponentsIndexBegin(); i != exponentsIndexEnd(); ++i)
      if (!inRange(access(mono, i)))
        return false;
    for (VarIndex grading = 0; grading < gradingCount(); ++grading)
      if (!inRange(degree(mono, grading)))
        return false;
    return true;
  }

  /// Returns the degree of mono using the most significant grading on
  /// the monoid. This is the grading with index gradingCount() -
  /// 1. This object must have at least one grading associated to it
  /// before calling this method.
  Exponent degree(ConstMonoRef mono) const {
    MATHICGB_ASSERT(gradingCount() > 0);
    return degree(mono, gradingCount() - 1);
  }

  /// Returns the degree of mono according to the grading with the
  /// given index.
  Exponent degree(ConstMonoRef mono, VarIndex grading) const {
    MATHICGB_ASSERT(grading < gradingCount());
    MATHICGB_ASSERT(debugOrderValid(mono));
    if (StoreOrder)
      return access(mono, orderIndexBegin() + grading);
    else
      return computeDegree(mono, grading);
  }

  /// Returns the number of gradings.
  using Base::gradingCount;

  /// Returns the number of entries per monomial. This includes entries
  /// used fo r internal book-keeping, so it can be greater than varCount().
  using Base::entryCount;

  // *** Monomial mutating computations

  /// Copes the parameter from to the parameter to.
  void copy(ConstMonoRef from, MonoRef to) const {
    MATHICGB_ASSERT(debugValid(from));

    std::copy_n(rawPtr(from), entryCount(), rawPtr(to));

    MATHICGB_ASSERT(debugValid(to));
  }

  template<class MonoidFrom>
  void copy(
    const MonoidFrom& monoidFrom,
    typename MonoidFrom::ConstMonoRef from,
    MonoRef to
  ) const {
    // todo: extract this in checker method
    MATHICGB_ASSERT(HasComponent == MonoidFrom::HasComponent);
    MATHICGB_ASSERT(monoidFrom.debugValid(from));
    MATHICGB_ASSERT(monoidFrom.varCount() == varCount());
    MATHICGB_ASSERT(monoidFrom.varsReversed() == varsReversed());
    MATHICGB_ASSERT
      ((std::is_same<Exponent, typename MonoidFrom::Exponent>::value));

    if (HasComponent)
      access(to, componentIndex()) = monoidFrom.component(from);
    const auto expsTo = ptr(to, exponentsIndexBegin());
    for (VarIndex var = 0; var < varCount(); ++var)
      expsTo[var] = monoidFrom.exponent(from, var);
    if (StoreOrder) {
      const auto degrees = ptr(to, orderIndexBegin());
      for (VarIndex grading = 0; grading < gradingCount(); ++grading)
        degrees[grading] = monoidFrom.degree(from, grading);
    }
    if (StoreHash)
      access(to, hashIndex()) = monoidFrom.hash(from);

    MATHICGB_ASSERT(debugValid(to));
    // todo: check equal
  }

  /// Set the exponent of var to newExponent in mono.
  void setExponent(
    const VarIndex var,
    const Exponent newExponent,
    MonoRef mono
  ) const {
    MATHICGB_ASSERT(var < varCount());

    auto& exponent = access(mono, exponentsIndexBegin() + var);
    const auto oldExponent = exponent;
    exponent = newExponent;

    updateOrderData(var, oldExponent, newExponent, mono);
    updateHashExponent(var, oldExponent, newExponent, mono);

    MATHICGB_ASSERT(debugValid(mono));
  }

  /// Sets an exponent based on external/unpermtuted var.
  /// After this, exponent(mono, internvalVar(exVar)) is newExponent.
  void setExternalExponent(
    const VarIndex exVar,
    const Exponent newExponent,
    MonoRef mono
  ) const {
    setExponent(internalVar(exVar), newExponent, mono);
  }

  /// Sets all the exponents of mono from an external/unpermuted array.
  /// exponents must point to an array of size varCount().
  /// After this, exponent(mono, var) is exponents[externalVar(var)].
  /// The value of exponents[var] becomes the exponent of internalVar(var).
  /// Does not set the component.
  void setExternalExponents(const Exponent* exponents, MonoRef mono) const {
    MATHICGB_ASSERT(exponents != 0);

    for (VarIndex iVar = 0; iVar < varCount(); ++iVar) {
      const auto eVar = externalVar(iVar);
      access(mono, exponentsIndexBegin() + iVar) = exponents[eVar];
    }
    setOrderData(mono);
    setHash(mono);

    MATHICGB_ASSERT(debugValid(mono));
  }

  /// Sets mono to 1, which is the identity for multiplication.
  void setIdentity(MonoRef mono) const {
    std::fill_n(rawPtr(mono), entryCount(), 0);

    MATHICGB_ASSERT(debugValid(mono));
    MATHICGB_ASSERT(isIdentity(mono));
  }

  /// Sets the component of mono to newComponent.
  void setComponent(Component newComponent, MonoRef mono) const {
    MATHICGB_ASSERT(HasComponent);

    auto& component = access(mono, componentIndex());
    const auto oldComponent = component;
    component = newComponent;
    updateHashComponent(oldComponent, newComponent, mono);
    updateOrderComponent(newComponent, mono);

    MATHICGB_ASSERT(debugValid(mono));
  }

  /// Sets prod to a*b.
  void multiply(ConstMonoRef a, ConstMonoRef b, MonoRef prod) const {
    MATHICGB_ASSERT(debugValid(a));
    MATHICGB_ASSERT(debugValid(b));

    for (auto i = lastEntryIndex(); i != beforeEntriesIndexBegin(); --i)
      access(prod, i) = access(a, i) + access(b, i);

    MATHICGB_ASSERT(debugValid(prod));
  }

  /// Sets prod to a*prod.
  void multiplyInPlace(ConstMonoRef a, MonoRef prod) const {
    MATHICGB_ASSERT(debugValid(a));
    MATHICGB_ASSERT(debugValid(prod));

    for (auto i = entriesIndexBegin(); i < entriesIndexEnd(); ++i)
      access(prod, i) += access(a, i);

    MATHICGB_ASSERT(debugValid(prod));      
  }

  /// Sets quo to num/by. by must divide num.
  void divide(ConstMonoRef by, ConstMonoRef num, MonoRef quo) const {
    MATHICGB_ASSERT(divides(by, num));
    MATHICGB_ASSERT(debugValid(num));
    MATHICGB_ASSERT(debugValid(by));

    for (auto i = entriesIndexBegin(); i < entriesIndexEnd(); ++i)
      access(quo, i) = access(num, i) - access(by, i);

    MATHICGB_ASSERT(debugValid(quo));
  }

  /// Sets num to num/by. by must divide num.
  void divideInPlace(ConstMonoRef by, MonoRef num) const {
    MATHICGB_ASSERT(divides(by, num));
    MATHICGB_ASSERT(debugValid(by));
    MATHICGB_ASSERT(debugValid(num));

    for (auto i = entriesIndexBegin(); i < entriesIndexEnd(); ++i)
      access(num, i) -= access(by, i);

    MATHICGB_ASSERT(debugValid(num));
  }

  /// Sets quo to num/by. If by does not divide num then quo will have
  /// negative exponents.
  void divideToNegative(ConstMonoRef by, ConstMonoRef num, MonoRef quo) const {
    MATHICGB_ASSERT(debugValid(num));
    MATHICGB_ASSERT(debugValid(by));
    MATHICGB_ASSERT(
      !HasComponent ||
      component(by) == 0 ||
      component(by) == component(num)
    );

    for (auto i = entriesIndexBegin(); i < entriesIndexEnd(); ++i)
      access(quo, i) = access(num, i) - access(by, i);

    MATHICGB_ASSERT(debugValid(quo));
  }

  /// Set out to (colonBy : colonNum) * mult.
  /// @todo: test
  void colonMultiply(
    ConstMonoRef colonBy,
    ConstMonoRef colonNum,
    ConstMonoRef mult,
    MonoRef out
  ) const {
    // todo: consider what happens with exponent overflow here
    if (HasComponent) {
      MATHICGB_ASSERT(component(colonBy) == component(colonNum));
      access(out, componentIndex()) = component(mult);
    }
    for (auto i = exponentsIndexBegin(); i != exponentsIndexEnd(); ++i) {
      const auto colon = access(colonNum, i) - access(colonBy, i);
      auto result = access(mult, i);
      if (colon > 0)
        result += colon;
      access(out, i) = result;
    }
    setOrderData(out);
    setHash(out);
    MATHICGB_ASSERT(debugValid(out));
  }

  /// Returns the number of variables that divide mono.
  /// @todo: test
  VarIndex sizeOfSupport(ConstMonoRef mono) const {
    VarIndex count = 0;
    for (VarIndex var = 0; var < varCount(); ++var)
      if (exponent(mono, var) != 0)
        ++count;
    return count;
  }

  /// Sets aColonB to a:b and bColonA to b:a.
  void colons(
    ConstMonoRef a,
    ConstMonoRef b,
    MonoRef aColonB,
    MonoRef bColonA
  ) const {
    MATHICGB_ASSERT(debugValid(a));
    MATHICGB_ASSERT(debugValid(b));

    if (HasComponent) {
      MATHICGB_ASSERT(component(a) == component(b));
      access(aColonB, componentIndex()) = 0;
      access(bColonA, componentIndex()) = 0;
    }

    for (auto i = exponentsIndexBegin(); i != exponentsIndexEnd(); ++i) {
      const auto ae = access(a, i);
      const auto be = access(b, i);
      const auto max = std::max(ae, be);
      access(aColonB, i) = max - be;
      access(bColonA, i) = max - ae;
    }
    setOrderData(aColonB);
    setHash(aColonB);
    setOrderData(bColonA);
    setHash(bColonA);

    MATHICGB_ASSERT(debugValid(aColonB));
    MATHICGB_ASSERT(debugValid(bColonA));
  }

  /// Sets lcmAB to the lcm of a and b.
  void lcm(ConstMonoRef a, ConstMonoRef b, MonoRef lcmAB) const {
    if (HasComponent) {
      MATHICGB_ASSERT(component(a) == component(b));
      access(lcmAB, componentIndex()) = access(a, componentIndex());
    }
    for (auto i = exponentsIndexBegin(); i != exponentsIndexEnd(); ++i)
      access(lcmAB, i) = std::max(access(a, i), access(b, i));
    setOrderData(lcmAB);
    setHash(lcmAB);

    MATHICGB_ASSERT(debugValid(lcmAB));
    MATHICGB_ASSERT(isLcm(a, b, lcmAB));
  }

  template<class MonoidA, class MonoidB>
  void lcm(
    const MonoidA& monoidA,
    typename MonoidA::ConstMonoRef a,
    const MonoidB& monoidB,
    typename MonoidB::ConstMonoRef b,
    MonoRef lcmAB
  ) const {
    MATHICGB_ASSERT(debugLcmCheck(monoidA, a, monoidB, b));

    if (HasComponent) {
      access(lcmAB, componentIndex()) =
        MonoidA::HasComponent ? monoidA.component(a) : monoidB.component(b);
    }

    for (VarIndex var = 0; var < varCount(); ++var) {
      ptr(lcmAB, exponentsIndexBegin())[var] =
        std::max(monoidA.exponent(a, var), monoidB.exponent(b, var));
    }

    setOrderData(lcmAB);
    setHash(lcmAB);

    MATHICGB_ASSERT(debugValid(lcmAB));
    MATHICGB_ASSERT(isLcm(monoidA, a, monoidB, b, lcmAB));
  }

  Mono alloc() const {return mPool.alloc();}

  void free(Mono&& mono) const {mPool.free(std::move(mono));}

  void freeRaw(MonoRef mono) const {mPool.freeRaw(mono);}

  void freeRaw(MonoPtr mono) const {
    if (mono != nullptr)
      freeRaw(*mono);
  }

  bool fromPool(ConstMonoRef mono) const {return mPool.fromPool(mono);}

  // *** Classes for holding and referring to monomials

  class ConstMonoPtr {
  public:
    ConstMonoPtr(): mMono(nullptr) {}
    ConstMonoPtr(std::nullptr_t): mMono(nullptr) {}
    ConstMonoPtr(const ConstMonoPtr& mono): mMono(rawPtr(mono)) {}

    ConstMonoPtr operator=(const ConstMonoPtr& mono) {
      mMono = mono.mMono;
      return *this;
    }

    ConstMonoRef operator*() const {return ConstMonoRef(*this);}

    /// @todo: Get rid of this as soon as all code has been migrated
    /// to observe the ptr/ref distinction. Kill it with fire!
    operator ConstMonoRef() const {return ConstMonoRef(*this);}

    bool isNull() const {return mMono == nullptr;}
    void toNull() {mMono = nullptr;}

    bool operator==(std::nullptr_t) const {return isNull();}
    bool operator!=(std::nullptr_t) const {return !isNull();}

    MonoPtr castAwayConst() const {
      return MonoPtr(const_cast<Exponent*>(mMono));
    }

  private:
    friend class MonoMonoid;

    const Exponent* internalRawPtr() const {return mMono;}
    ConstMonoPtr(const Exponent* mono): mMono(mono) {}

    const Exponent* mMono;
  };

  class MonoPtr {
  public:
    MonoPtr(): mMono(nullptr) {}
    MonoPtr(std::nullptr_t): mMono(nullptr) {}
    MonoPtr(const MonoPtr& mono): mMono(mono.mMono) {}

    MonoPtr operator=(const MonoPtr& mono) {
      mMono = mono.mMono;
      return *this;
    }

    MonoRef operator*() const {return MonoRef(*this);}

    /// @todo: Get rid of this as soon as all code has been migrated
    /// to observe the ptr/ref distinction. Kill it with fire!
    operator MonoRef() const {return MonoRef(*this);}

    bool isNull() const {return mMono == nullptr;}
    void toNull() {mMono = nullptr;}

    bool operator==(std::nullptr_t) const {return isNull();}
    bool operator!=(std::nullptr_t) const {return !isNull();}

    operator ConstMonoPtr() const {return ConstMonoPtr(mMono);}

  private:
    friend class MonoMonoid;
    friend class ConstMonoPtr;
    friend class PolyRing; // todo: remove
    friend class Poly; // todo: remove

    Exponent* internalRawPtr() const {return mMono;}
    MonoPtr(Exponent* mono): mMono(mono) {}

    Exponent* mMono;
  };

  class Mono : public NonCopyable<Mono> {
  public:
    Mono(): mMono(), mPool(nullptr) {}
    Mono(std::nullptr_t): mMono(), mPool(nullptr) {}

    /// Passes ownership of the resources of mono to this object. Mono must
    /// have been allocated from pool and it must have no other owner.
    /// In particular, it must have been release()'ed from its original
    /// owner.
    Mono(MonoRef mono, MonoPool& pool):
      mMono(mono.ptr()), mPool(&pool)
    {
      MATHICGB_ASSERT(pool.fromPool(mono));
    }

    Mono(Mono&& mono): mMono(mono.mMono), mPool(mono.mPool) {
      mono.mMono.toNull();
      mono.mPool = nullptr;
    }

    ~Mono() {toNull();}

    void operator=(Mono&& mono) {
      toNull();

      mMono = mono.mMono;
      mono.mMono.toNull();

      mPool = mono.mPool;
      mono.mPool = nullptr;
    }

    /// Sets this object to null but does NOT free the resources previously
    /// held by this object. The returned MonoPtr points to the resources
    /// that this object had prior to calling release(). If this object was
    /// already null then the returned MonoPtr is also null.
    MonoPtr release() {
      const auto oldPtr = ptr();
      mMono.toNull();
      mPool = nullptr;
      return oldPtr;
    }

    bool isNull() const {return mMono.isNull();}
    void toNull() {mPool->free(std::move(*this));}

    MonoPtr ptr() {return mMono;}
    ConstMonoPtr ptr() const {return mMono;}

    MonoRef operator*() {
      MATHICGB_ASSERT(!isNull());
      return *mMono;
    }

    ConstMonoRef operator*() const {
      MATHICGB_ASSERT(!isNull());
      return *mMono;
    }

    operator MonoPtr() const {return ptr();}
    operator ConstMonoPtr() const {return ptr();}

    /// @todo: Get rid of this as soon as all code has been migrated
    /// to observe the ptr/ref distinction. Kill it with fire!
    operator MonoRef() const {
      MATHICGB_ASSERT(!isNull());
      return *mMono;
    }

    /// @todo: Get rid of this as soon as all code has been migrated
    /// to observe the ptr/ref distinction. Kill it with fire!
    operator ConstMonoRef() const {
      MATHICGB_ASSERT(!isNull());
      return *mMono;
    }

  private:
    friend class MonoMonoid;

    Exponent* internalRawPtr() const {return rawPtr(mMono);}

    MonoPtr mMono;
    MonoPool* mPool;
  };

  class MonoRef {
  public:
    MonoRef(const MonoRef& mono): mMono(mono.ptr()) {}

    MonoPtr ptr() const {return mMono;}
    MonoPtr operator&() const {return ptr();}

    /// @todo: Get rid of this as soon as all code has been migrated
    /// to observe the ptr/ref distinction. Kill it with fire!
    operator MonoPtr() const {return ptr();}

    operator ConstMonoRef() const {return *static_cast<ConstMonoPtr>(mMono);}

  private:
    void operator=(const MonoRef&); // not available
    friend class MonoMonoid;
    friend class ConstMonoRef;

    MonoRef(MonoPtr mono): mMono(mono) {}
    Exponent* internalRawPtr() const {return rawPtr(mMono);}

    const MonoPtr mMono;
  };

  class ConstMonoRef {
  public:
    ConstMonoRef(const ConstMonoRef& mono): mMono(mono.ptr()) {}
    ConstMonoRef(const Mono& mono): mMono(mono.ptr()) {
      MATHICGB_ASSERT(!mono.isNull());
    }

    ConstMonoPtr ptr() const {return mMono;}
    ConstMonoPtr operator&() const {return ptr();}

    /// @todo: Get rid of this as soon as all code has been migrated
    /// to observe the ptr/ref distinction. Kill it with fire!
    operator ConstMonoPtr() const {return ptr();}

    MonoRef castAwayConst() const {return MonoRef(mMono.castAwayConst());}

  private:
    void operator=(const MonoRef&); // not available
    friend class MonoMonoid;

    ConstMonoRef(ConstMonoPtr mono): mMono(mono) {}
    const Exponent* internalRawPtr() const {return rawPtr(mMono);}

    const ConstMonoPtr mMono;
  };


  // *** Classes that provide memory resources for monomials

  class MonoPool : public NonCopyable<MonoPool> {
  public:
    MonoPool(const MonoMonoid& monoid):
      mMonoid(monoid),
      mPool(sizeof(Exponent) * mMonoid.entryCount())
    {}

    MonoPool(MonoPool&& pool):
      mMonoid(pool.mMonoid),
      mPool(std::move(pool.mPool))
    {}

    Mono alloc() {
      const auto ptr = static_cast<Exponent*>(mPool.alloc());
      Mono mono(*MonoPtr(ptr), *this);
      monoid().setIdentity(mono);
      return mono;
    }

    void free(Mono&& mono) {
      if (mono.isNull())
        return;
      freeRaw(mono);
      mono.mMono.toNull();
      mono.mPool = nullptr;
    }
    void freeRaw(MonoRef mono) {mPool.free(rawPtr(mono));}

    const MonoMonoid& monoid() const {return mMonoid;}

    bool fromPool(ConstMonoRef mono) const {
      return mPool.fromPool(rawPtr(mono));
    }

  private:
    const MonoMonoid& mMonoid;
    memt::BufferPool mPool;
  };

  class MonoVector {
  private:
    typedef std::vector<Exponent> RawVector;

  public:
    /// Class for iterating through the monomials in a MonoVector.
    ///
    /// There is no operator->() since MonoRef does not have any
    /// relevant methods to call. Implement it if you need it.
    ///
    /// There are no postfix increment operator as prefix is
    /// better. Add it if you need it (you probably do not).
    ///
    /// We could make this a random access iterator, but that would
    /// make it tricky to support variable-sized exponent vectors
    /// (e.g. sparse) in future and so far we have not needed random
    /// access.
    class const_iterator {
    public:
      typedef std::forward_iterator_tag iterator_category;
      typedef ConstMonoRef value_type;
      typedef ptrdiff_t difference_type;
      typedef ConstMonoPtr pointer;
      typedef ConstMonoRef reference;
    
      const_iterator(): mIt(), mEntriesPerMono(0) {}
      const_iterator(const const_iterator& it):
        mIt(it.mIt), mEntriesPerMono(it.mEntriesPerMono) {}
    
      bool operator==(const const_iterator& it) const {return mIt == it.mIt;}
      bool operator!=(const const_iterator& it) const {return mIt != it.mIt;}

      ConstMonoRef operator*() const {
        MATHICGB_ASSERT(debugValid());
        return *ConstMonoPtr(&*mIt);
      }

      const_iterator operator++() {
        MATHICGB_ASSERT(debugValid());
        mIt += mEntriesPerMono;
        return *this;
      }

    private:
      friend class MonoVector;
      bool debugValid() const {return mEntriesPerMono > 0;}

      const_iterator(
        typename RawVector::const_iterator it,
        size_t entryCount
      ): mIt(it), mEntriesPerMono(entryCount) {}
      
      typename RawVector::const_iterator mIt;
      size_t mEntriesPerMono;		     
    };


    // *** Constructors and assignment

    MonoVector(const MonoMonoid& monoid): mMonoid(monoid) {}
    MonoVector(const MonoVector& v): mMonos(v.mMonos), mMonoid(v.monoid()) {}
    MonoVector(MonoVector&& v):
      mMonos(std::move(v.mMonos)), mMonoid(v.monoid()) {}

    MonoVector& operator=(const MonoVector& v) {
      MATHICGB_ASSERT(monoid() == v.monoid());
      mMonos = v.mMonos;
      return *this;
    }

    MonoVector& operator=(MonoVector&& v) {
      MATHICGB_ASSERT(monoid() == v.monoid());
      mMonos = std::move(v.mMonos);
      return *this;      
    }


    // *** Iterators

    const_iterator begin() const {
      return const_iterator(mMonos.begin(), mMonoid.entryCount());
    }

    const_iterator end() const {
      return const_iterator(mMonos.end(), mMonoid.entryCount());
    }

    const_iterator cbegin() const {return begin();}
    const_iterator cend() const {return end();}

    // *** Operators
    
    /// Returns true if *this and v contain the same monomials in the same
    /// order. This ought to be a free-standing function, but it cannot be,
    /// because template argument deduction cannot deduce a type T from
    /// something of the form A<T>::MonoVector.
    bool operator==(const MonoVector& v) const {
      MATHICGB_ASSERT(monoid() == v.monoid());
      return mMonos == v.mMonos;
    }

    /// As !(*this == v).
    bool operator!=(const MonoVector& v) const {
      MATHICGB_ASSERT(monoid() == v.monoid());
      return !(*this == v);
    }


    // *** Size and capacity

    size_t size() const {return mMonos.size() / monoid().entryCount();}
    bool empty() const {return mMonos.empty();}
    size_t capacity() const {return mMonos.capacity() / monoid().entryCount();}
    bool atCapacity() const {return size() == capacity();}


    // *** Element access

    ConstMonoRef front() const {
      MATHICGB_ASSERT(!empty());
      return *begin();
    }

    MonoRef back() {
      MATHICGB_ASSERT(!empty());
      const auto offset = mMonos.size() - monoid().entryCount();
      return *MonoPtr(mMonos.data() + offset);
    }

    ConstMonoRef back() const {
      MATHICGB_ASSERT(!empty());
      const auto offset = mMonos.size() - monoid().entryCount();
      return *ConstMonoPtr(mMonos.data() + offset);
    }


    // *** Modifiers

    void reserve(size_t count) {
      mMonos.reserve(count * monoid().entryCount());
    }

    /// Appends the identity.
    void push_back() {
      const auto offset = mMonos.size();
      mMonos.resize(offset + monoid().entryCount());
      MATHICGB_ASSERT(monoid().isIdentity(back()));
      MATHICGB_ASSERT(monoid().debugValid(back()));
    }

    void push_back(ConstMonoRef mono) {
      MATHICGB_ASSERT(monoid().debugValid(mono));
      const auto offset = mMonos.size();
      mMonos.resize(offset + monoid().entryCount());
      monoid().copy(mono, *MonoPtr(mMonos.data() + offset));
      MATHICGB_ASSERT(monoid().debugValid(back()));
      MATHICGB_ASSERT(monoid().equal(back(), mono));
    }

    template<class Monoid>
    void push_back(
      const Monoid& monoidMono,
      typename Monoid::ConstMonoRef mono
    ) {
      MATHICGB_ASSERT(monoidMono.debugValid(mono));
      const auto offset = mMonos.size();
      mMonos.resize(offset + monoid().entryCount());
      monoid().copy(monoidMono, mono, *MonoPtr(mMonos.data() + offset));
      MATHICGB_ASSERT(monoid().debugValid(back()));
      MATHICGB_ASSERT(monoid().equal(monoidMono, mono, back()));
    }

    void swap(MonoVector& v) {
      MATHICGB_ASSERT(monoid() == v.monoid());
      mMonos.swap(v.mMonos);
    }

    void clear() {mMonos.clear();}


    // *** Other

    size_t memoryBytesUsed() const {
      return mMonos.capacity() * sizeof(mMonos.front());
    }

    const MonoMonoid& monoid() const {return mMonoid;}

  private:
    RawVector mMonos;
    const MonoMonoid& mMonoid;
  };

  bool debugValid(ConstMonoRef mono) const {
    MATHICGB_ASSERT(debugOrderValid(mono));
    MATHICGB_ASSERT(debugHashValid(mono));
    return true;
  }

private:
  void operator=(MonoMonoid&); // not available

  // Grants access to other template instantiations.
  template<class E2, bool HC2, bool SH2, bool SO2>
  friend class MonoMonoid;

  // The main point here is to grant access to rawPtr().
  friend class Mono;
  friend class MonoRef;
  friend class ConstMonoRef;
  friend class MonoPtr;
  friend class ConstMonoPtr;
  friend class MonoVector;
  friend class MonoPool;

  friend class PolyHashTable;

  typedef typename Base::Gradings Gradings;

  bool debugAssertValid() const {
#ifdef MATHICGB_DEBUG
    // ** Order checks
    MATHICGB_ASSERT(orderIndexBegin() == exponentsIndexEnd());
    const auto storedDegrees = StoreOrder * gradingCount();
    MATHICGB_ASSERT(orderIndexEnd() == orderIndexBegin() + storedDegrees);
    MATHICGB_ASSERT(orderIndexEnd() <= entryCount());
    if (orderIndexEnd() + StoreHash == 0) {
      MATHICGB_ASSERT(entryCount() == 1);
    } else {
      MATHICGB_ASSERT(entryCount() == orderIndexEnd() + StoreHash);
    }

    MATHICGB_ASSERT(isLexBaseOrder() || varCount() == 0 || gradingCount() >= 1);

    MATHICGB_ASSERT(gradings().size() == gradingCount() * varCount());
    if (orderIsTotalDegreeRevLex()) {
      MATHICGB_ASSERT(!isLexBaseOrder());
      MATHICGB_ASSERT(gradingCount() == 1);
    }

    if (componentGradingIndex() != Order::ComponentAfterBaseOrder) {
      MATHICGB_ASSERT(componentGradingIndex() < gradingCount());
      for (VarIndex var = 0; var < varCount(); ++var) {
        const auto index = gradingsIndex(componentGradingIndex(), var);
        MATHICGB_ASSERT(gradings()[index] == 0);
      }
    }

    // ** Hash checks
    if (StoreHash) {
      MATHICGB_ASSERT(hashIndex() < entryCount());
      MATHICGB_ASSERT(hashIndex() == orderIndexEnd());
    }
    MATHICGB_ASSERT(hashCoefficients().size() == varCount());
#endif
    return true;
  }

  template<class MonoidA, class MonoidB>
  bool debugLcmCheck(
    const MonoidA& monoidA,
    typename MonoidA::ConstMonoRef a,
    const MonoidB& monoidB,
    typename MonoidB::ConstMonoRef b
  ) const {
    MATHICGB_ASSERT(monoidA.varCount() == varCount());
    MATHICGB_ASSERT(monoidB.varCount() == varCount());
    MATHICGB_ASSERT
      ((std::is_same<Exponent, typename MonoidA::Exponent>::value));
    MATHICGB_ASSERT
      ((std::is_same<Exponent, typename MonoidB::Exponent>::value));
    MATHICGB_ASSERT
      (HasComponent == (MonoidA::HasComponent || MonoidB::HasComponent));
    MATHICGB_ASSERT(monoidA.debugValid(a));
    MATHICGB_ASSERT(monoidB.debugValid(b));
    MATHICGB_ASSERT(
      !HasComponent ||
      !MonoidA::HasComponent ||
      !MonoidB::HasComponent ||
      monoidA.component(a) == monoidB.component(b)
  );

    return true;
  }

  static MonoPtr toMonoPtr(Exponent* raw) {return MonoPtr(raw);}
  static ConstMonoPtr toMonoPtr(const Exponent* raw) {
    return ConstMonoPtr(raw);
  }

  // *** Accessing fields of a monomial
  template<class M>
  static auto rawPtr(M&& m) -> decltype(m.internalRawPtr()) {
    return m.internalRawPtr();
  }

  Exponent* ptr(MonoRef& m, const VarIndex index) const {
    MATHICGB_ASSERT(index <= entryCount());
    return rawPtr(m) + index;
  }

  const Exponent* ptr(ConstMonoRef& m, const VarIndex index) const {
    MATHICGB_ASSERT(index <= entryCount());
    return rawPtr(m) + index;
  }

  Exponent& access(MonoRef& m, const VarIndex index) const {
    MATHICGB_ASSERT(index < entryCount());
    return rawPtr(m)[index];
  }

  const Exponent& access(ConstMonoRef& m, const VarIndex index) const {
    MATHICGB_ASSERT(index < entryCount());
    return rawPtr(m)[index];
  }

  // *** Implementation of monomial ordering

  using Base::gradingsOppositeRowIndex;
  using Base::gradingsIndex;
  using Base::reverseGradings;
  using Base::negateGradings;

  bool debugOrderValid(ConstMonoRef mono) const {
#ifdef MATHICGB_DEBUG
    if (!StoreOrder)
      return true;
    // Check the order data of mono
    const auto degrees = ptr(mono, orderIndexBegin());
    for (VarIndex grading = 0; grading < gradingCount(); ++grading) {
      MATHICGB_ASSERT(degrees[grading] == computeDegree(mono, grading));
    }
#endif
    return true;
  }

  void setOrderData(MonoRef mono) const {
    if (!StoreOrder)
      return;

    const auto degrees = ptr(mono, orderIndexBegin());
    for (VarIndex grading = 0; grading < gradingCount(); ++grading)
      degrees[grading] = computeDegree(mono, grading);
    MATHICGB_ASSERT(debugOrderValid(mono));
  }

  void updateOrderData(
    const VarIndex var,
    const Exponent oldExponent,
    const Exponent newExponent,
    MonoRef mono
  ) const {
    if (!StoreOrder)
      return;

    MATHICGB_ASSERT(var < varCount());
    if (orderIsTotalDegreeRevLex())
      rawPtr(mono)[orderIndexBegin()] -= newExponent - oldExponent;
    else {
      MATHICGB_ASSERT(gradings().size() == gradingCount() * varCount());
      const auto degrees = ptr(mono, orderIndexBegin());
      for (VarIndex grading = 0; grading < gradingCount(); ++grading) {
        const auto index = gradingsIndex(grading, var);
        degrees[grading] += gradings()[index] * ( newExponent - oldExponent);
      }
    }
    MATHICGB_ASSERT(debugOrderValid(mono));
  }

  void updateOrderComponent(const VarIndex newComponent, MonoRef mono) const {
    if (componentGradingIndex() != Order::ComponentAfterBaseOrder)
      ptr(mono, orderIndexBegin())[componentGradingIndex()] =
        static_cast<Exponent>(newComponent);
  }

  Exponent computeDegree(ConstMonoRef mono, VarIndex grading) const {
    MATHICGB_ASSERT(grading < gradingCount());

    Exponent degree = 0;
    if (orderIsTotalDegreeRevLex()) {
      MATHICGB_ASSERT(grading == 0);
      for (auto var = size_t(0); var < varCount(); ++var)
        degree -= exponent(mono, var);
    } else if (HasComponent && componentGradingIndex() == grading)
      return component(mono);
    else {
      MATHICGB_ASSERT(gradings().size() == gradingCount() * varCount());
      for (auto var = size_t(0); var < varCount(); ++var) {
        const auto index = gradingsIndex(grading, var);
        degree += exponent(mono, var) * gradings()[index];
      }
    }
    return degree;
  }


  // *** Implementation of hash value computation

  bool debugHashValid(ConstMonoRef mono) const {
    if (!StoreHash)
      return true;

    // We cannot call hash() here since it calls this method.
    // todo: we cannot make this check right now because the legacy
    // integration with PolyRing can create monomials with unset hash.
    // MATHICGB_ASSERT(rawPtr(mono)[hashIndex()] == computeHash(mono));
    return true;
  }

  HashValue computeHash(ConstMonoRef mono) const {
    HashValue hash = HasComponent ? component(mono) : 0;
    for (VarIndex var = 0; var < varCount(); ++var) {
      hash +=
        static_cast<HashValue>(exponent(mono, var)) * hashCoefficients()[var];
    }

    // Hash values are stored as exponents. If the cast to an exponent
    // changes the value, then we need computeHashValue to match that
    // change by casting to an exponent and back. Otherwise the computed
    // hash value will not match a hash value that has been stored.
    return static_cast<HashValue>(static_cast<Exponent>(hash));
  }

  void setHash(MonoRef mono) const {
    if (!StoreHash)
      return;
    rawPtr(mono)[hashIndex()] = computeHash(mono);
    MATHICGB_ASSERT(debugHashValid(mono));
  }

  void updateHashComponent(
    const Exponent oldComponent,
    const Exponent newComponent,
    MonoRef mono
  ) const {
    if (!StoreHash)
      return;
    rawPtr(mono)[hashIndex()] += newComponent - oldComponent;
    MATHICGB_ASSERT(debugHashValid(mono));
  }

  void updateHashExponent(
    const VarIndex var,
    const Exponent oldExponent,
    const Exponent newExponent,
    MonoRef mono
  ) const {
    if (!StoreHash)
      return;
    MATHICGB_ASSERT(var < varCount());
    rawPtr(mono)[hashIndex()] +=
      (newExponent - oldExponent) * hashCoefficients()[var];
    MATHICGB_ASSERT(debugHashValid(mono));
  }


  // *** Code determining the layout of monomials in memory
  // Layout in memory:
  //   [component] [exponents...] [order data...] [hash]

  VarIndex componentIndex() const {
    //static_assert(HasComponent, "");
    return 0;
  }

  VarIndex exponentsIndexBegin() const {return HasComponent;}
  VarIndex exponentsIndexEnd() const {return exponentsIndexBegin() + varCount();}
  VarIndex lastExponentIndex() const {return exponentsIndexEnd() - 1;}

  using Base::orderIndexBegin;
  using Base::orderIndexEnd;
  using Base::hashIndex;
  using Base::orderIsTotalDegreeRevLex;
  using Base::gradings;
  using Base::isLexBaseOrder;
  using Base::componentGradingIndex;

  VarIndex entriesIndexBegin() const {return 0;}
  VarIndex entriesIndexEnd() const {return entryCount();}
  VarIndex beforeEntriesIndexBegin() const {return entriesIndexBegin() - 1;}
  VarIndex lastEntryIndex() const {return entriesIndexEnd() - 1;}

  using Base::hashCoefficients;

  mutable MonoPool mPool;
};

/// Returns true if a and b are the same object.
template<class E, bool HC, bool SH, bool SO>
bool operator==(
  const MonoMonoid<E,HC,SH,SO>& a,
  const MonoMonoid<E,HC,SH,SO>& b
) {
  return &a == &b;
}

/// As !(a == b).
template<class E, bool HC, bool SH, bool SO>
bool operator!=(
  const MonoMonoid<E,HC,SH,SO>& a,
  const MonoMonoid<E,HC,SH,SO>& b
) {
  return !(a == b);
}

MATHICGB_NAMESPACE_END
#endif
