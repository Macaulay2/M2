// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_STATIC_MONO_LOOKUP_GUARD
#define MATHICGB_STATIC_MONO_LOOKUP_GUARD

#include "SigPolyBasis.hpp"
#include "PolyBasis.hpp"
#include "MonoLookup.hpp"
#include "PolyRing.hpp"
#include "mathic/mathic.h"
#include <type_traits>
#include <string>
#include <vector>

MATHICGB_NAMESPACE_BEGIN

/// Data structure for performing queries on a set of monomials.
/// This is static in the sense that the interface is not virtual.
template<
  /// Will use mathic::DivList or mathic::KDTree depending on this value.
  bool UseKDTree,

  /// Thing to store along with each monomial in the data structure.
  class Data,

  /// Indicate whether elements should be allowed to be removed from the
  /// data structure. There can be a slight performance benefit from
  /// disallowing removal.
  bool AllowRemovals,

  /// Whether to use bit vectors of features to speed up divisibility
  /// checks. This is usually a big speed up.
  bool UseDivMask
>
class StaticMonoLookup;

template<bool UseKDTree, class Data, bool AR, bool DM>
class StaticMonoLookup {
private:
  typedef PolyRing::Monoid Monoid;
  typedef Monoid::ConstMonoRef ConstMonoRef;
  typedef Monoid::ConstMonoPtr ConstMonoPtr;
  typedef MonoLookup::EntryOutput EntryOutput;

  /// Configuration for a Mathic KDTree or DivList.
  class Configuration {
  public:
    Configuration(const Monoid& monoid): mMonoid(monoid) {}
    const Monoid& monoid() const {return mMonoid;}

    typedef int Exponent;
    typedef ConstMonoRef Monomial;

    class Entry {
    public:
      Entry(): mEntry() {}

      template<class D>
      Entry(ConstMonoRef mono, D&& data):
        mEntry(mono.ptr(), std::forward<D>(data)) {}

      ConstMonoRef mono() const {return *mEntry.first;}
      const Data& data() const {return mEntry.second;}
      Data& data() {return mEntry.second;}

    private:
      // Store in a std::pair so that if Data has no members then no space
      // will be used on it. Here we are depending on std::pair to be using
      // the empty base class optimization.
      std::pair<ConstMonoPtr, Data> mEntry;
    };

    Exponent getExponent(const Monomial& m, size_t var) const {
      return monoid().exponent(m, var);
    }

    Exponent getExponent(const Entry& e, size_t var) const {
      return monoid().exponent(e.mono(), var);
    }

    bool divides(const Monomial& a, const Monomial& b) const {
      return monoid().dividesWithComponent(a, b);
    }

    bool divides(const Entry& a, const Monomial& b) const {
      return monoid().dividesWithComponent(a.mono(), b);
    }

    bool divides(const Monomial& a, const Entry& b) const {
      return monoid().dividesWithComponent(a, b.mono());
    }

    bool divides(const Entry& a, const Entry& b) const {
      return monoid().dividesWithComponent(a.mono(), b.mono());
    }

    bool getSortOnInsert() const {return false;}
    template<class A, class B>
    bool isLessThan(const A& a, const B& b) const {
      MATHICGB_ASSERT(false);
      return false;
    }

    size_t getVarCount() const {return monoid().varCount();}

    static const bool UseTreeDivMask = DM;
    static const bool UseLinkedList = false;
    static const bool UseDivMask = DM;
    static const size_t LeafSize = 1;
    static const bool PackedTree = true;
    static const bool AllowRemovals = AR;

    bool getUseDivisorCache() const {return true;}
    bool getMinimizeOnInsert() const {return false;}

    bool getDoAutomaticRebuilds() const {return UseDivMask;}
    double getRebuildRatio() const {return 0.5;}
    size_t getRebuildMin() const {return 50;}

  private:
    const Monoid& mMonoid;
  };

public:
  typedef typename std::conditional<
    UseKDTree,
    mathic::KDTree<Configuration>,
    mathic::DivList<Configuration>
  >::type BaseLookup;
  typedef typename BaseLookup::Entry Entry;

  static_assert
    (!Configuration::UseTreeDivMask || Configuration::UseDivMask, "");

  StaticMonoLookup(const Monoid& monoid): mLookup(Configuration(monoid)) {}

  const Monoid& monoid() const {return mLookup.getConfiguration().monoid();}

  template<class Lambda>
  class LambdaWrap {
  public:
    LambdaWrap(Lambda& lambda): mLambda(lambda) {}
    bool proceed(const Entry& entry) const {return mLambda(entry);}
  private:
    Lambda& mLambda;
  };

  template<class Lambda>
  static LambdaWrap<Lambda> lambdaWrap(Lambda& lambda) {
    return LambdaWrap<Lambda>(lambda);
  }

  template<class Lambda>
  void forAll(Lambda&& lambda) const {
    const auto wrap = [&](const Entry& entry){
      lambda(entry);
      return true;
    };
    auto outerWrap = lambdaWrap(wrap);
    mLookup.forAll(outerWrap);
  }

  // *** Signature specific functionality

  size_t regularReducer(
    ConstMonoRef sig,
    ConstMonoRef mono,
    const SigPolyBasis& sigBasis,
    const bool preferSparseReducers
  ) const {
    SigPolyBasis::StoredRatioCmp ratioCmp(sig, mono, sigBasis);
    const auto& basis = sigBasis.basis();

    auto reducer = size_t(-1);
    auto proceed = [&](const Entry& e) {
      if (ratioCmp.compare(e.data()) != GT)
        return true;

      if (reducer != size_t(-1)) {
        if (preferSparseReducers) {
          const auto newTermCount = basis.poly(e.data()).termCount();
          const auto oldTermCount = basis.poly(reducer).termCount();
          if (newTermCount > oldTermCount)
            return true; // what we already have is sparser
          // resolve ties by picking oldest
          if (newTermCount == oldTermCount && e.data() > reducer)
            return true;
        } else { // pick oldest
          if (e.data() > reducer)
            return true; // what we already have is older
        }
      }
      reducer = e.data();
      return true;
    };
    auto wrap = lambdaWrap(proceed);
    mLookup.findAllDivisors(mono, wrap);
    return reducer;
  }

  void lowBaseDivisors(
    std::vector<size_t>& divisors,
    const size_t maxDivisors,
    const size_t newGenerator,
    const SigPolyBasis& basis
  ) const {
    MATHICGB_ASSERT(newGenerator < basis.size());
    auto proceed = [&](const Entry& entry) {
      if (entry.data() >= newGenerator)
        return true;
      for (size_t j = 0; j <= divisors.size(); ++j) {
        if (j == divisors.size()) {
          divisors.push_back(entry.data());
          break;
        }
        int cmp = basis.ratioCompare(entry.data(), divisors[j]);
        if (cmp == EQ && (entry.data() < divisors[j]))
          cmp = GT; // prefer minimum index to ensure deterministic behavior
        if (cmp == GT) {
          divisors.insert(divisors.begin() + j, entry.data());
          break;
        }
      }
      if (divisors.size() > maxDivisors)
        divisors.pop_back();
      MATHICGB_ASSERT(divisors.size() <= maxDivisors);
      return true;
    };
    divisors.clear();
    divisors.reserve(maxDivisors + 1);
    auto wrap = lambdaWrap(proceed);
    mLookup.findAllDivisors(basis.signature(newGenerator), wrap);
  }

  size_t highBaseDivisor(
    const size_t newGenerator,
    const SigPolyBasis& basis
  ) const {
    MATHICGB_ASSERT(newGenerator < basis.size());
    auto highDivisor = size_t(-1);
    auto proceed = [&](const Entry& entry) {
      if (entry.data() >= newGenerator)
        return true;
      if (highDivisor != size_t(-1)) {
        int cmp = basis.ratioCompare(highDivisor, entry.data());
        if (cmp == LT)
          return true;
        if (cmp == EQ && (entry.data() > highDivisor))
          return true; // prefer minimum index to ensure deterministic behavior
      }
      highDivisor = entry.data();
      return true;
    };
    auto wrap = lambdaWrap(proceed);
    mLookup.findAllDivisors(basis.leadMono(newGenerator), wrap);
    return highDivisor;
  }

  size_t minimalLeadInSig(
    ConstMonoRef sig,
    const SigPolyBasis& basis
  ) const {
    // Given signature sig, we want to minimize (S/G)g where
    // g and G are the lead term and signature taken over basis elements
    // whose signature G divide S. The code here instead maximizes G/g,
    // which is equivalent and also faster since the basis has a data
    // structure to accelerate comparisons between the ratio of
    // signature to lead term.
    //
    // In case of ties, we select the sparser elements. If there is
    // still a tie, we select the basis element with the largest
    // signature. There can be no further ties since all basis
    // elements have distinct signatures.
    auto minLeadGen = size_t(-1);
    auto proceed = [&](const Entry& entry) {
      if (minLeadGen != size_t(-1)) {
        const int ratioCmp = basis.ratioCompare(entry.data(), minLeadGen);
        if (ratioCmp == LT)
          return true;
        if (ratioCmp == EQ) {
          // If same lead monomial in signature, pick the one with fewer terms
          // as that one might be less effort to reduce.
          const size_t minTerms = basis.poly(minLeadGen).termCount();
          const size_t terms = basis.poly(entry.data()).termCount();
          if (minTerms > terms)
            return true;
          if (minTerms == terms) {
            // If same number of terms, pick the one with larger signature
            // before being multiplied into the same signature. That one
            // might be more reduced as the constraint on regular reduction
            // is less. Also, as no two generators have same signature, this
            // ensures deterministic behavior.
            const auto minSig = basis.signature(minLeadGen);
            const auto genSig = basis.signature(entry.data());
            const auto sigCmp = basis.monoid().compare(minSig, genSig);
            if (basis.monoid().lessThan(genSig, minSig))
              return true;
          }
        }
      }
      minLeadGen = entry.data();
      return true;
    };
    auto wrap = lambdaWrap(proceed);
    mLookup.findAllDivisors(sig, wrap);
    return minLeadGen;
  }

  // *** Classic GB specific functionality

  size_t classicReducer(
    ConstMonoRef mono,
    const PolyBasis& basis,
    const bool preferSparseReducers
  ) const {
    auto reducer = size_t(-1);
    auto proceed = [&](const Entry& entry) {
      if (reducer == size_t(-1)) {
        reducer = entry.data();
        return true;
      }
      if (preferSparseReducers) {
        const auto oldTermCount = basis.poly(reducer).termCount();
        const auto newTermCount = basis.poly(entry.data()).termCount();
        if (oldTermCount > newTermCount) {
          reducer = entry.data(); // prefer sparser
          return true;
        }
        if (oldTermCount < newTermCount)
          return true;
      } // break ties by age

      if (reducer > entry.data())
        reducer = entry.data(); // prefer older
      return true;
    };
    auto wrap = lambdaWrap(proceed);
    mLookup.findAllDivisors(mono, wrap);
    return reducer;
  }

  // *** General functionality

  const Entry* divisor(ConstMonoRef mono) const {
    return mLookup.findDivisor(mono);
  }

  void divisors(ConstMonoRef mono, EntryOutput& consumer) const {
    auto proceed = [&](const Entry& e) {return consumer.proceed(e.data());};
    auto wrap = lambdaWrap(proceed);
    mLookup.findAllDivisors(mono, wrap);
  }

  void multiples(ConstMonoRef mono, EntryOutput& consumer) const {
    auto proceed = [&](const Entry& e) {return consumer.proceed(e.data());};
    auto wrap = lambdaWrap(proceed);
    mLookup.findAllMultiples(mono, wrap);
  }

  void removeMultiples(ConstMonoRef mono) {
    mLookup.removeMultiples(mono);
  }

  void remove(ConstMonoRef mono) {mLookup.removeElement(mono);}

  size_t size() const {return mLookup.size();}

  std::string getName() const {return mLookup.getName();}
  const PolyRing& ring() const {return mLookup.configuration().ring();}

  size_t getMemoryUse() const {return mLookup.getMemoryUse();}

  template<class D>
  void insert(ConstMonoRef mono, D&& data) {
    mLookup.insert(Entry(mono, std::forward<D>(data)));
  }

private:
  BaseLookup mLookup;
};

/// Function for creating statically compiled classes that use
/// StaticMonoLookup based on run-time values.
///
/// The type Functor must have an interface that is compatible with the
/// following example.
///
/// template<bool UseKDTree, bool AllowRemovals, bool UseDivMask>
/// struct Functor {
///   static ReturnType make(const Params& params) {
///     // do your thing
///   }
/// };
template<
  template<bool, bool, bool> class Functor,
  class ReturnType,
  class Params
>
ReturnType staticMonoLookupMake(
  int type,
  bool allowRemovals,
  Params&& params
) {
  switch (type) {
  case 1:
    if (allowRemovals)
      return Functor<0, 1, 1>::make(std::forward<Params>(params));
    else
      return Functor<0, 0, 1>::make(std::forward<Params>(params));
    
  case 2:
    if (allowRemovals)
      return Functor<1, 1, 1>::make(std::forward<Params>(params));
    else
      return Functor<1, 0, 1>::make(std::forward<Params>(params));

  case 3:
    if (allowRemovals)
      return Functor<0, 1, 0>::make(std::forward<Params>(params));
    else
      return Functor<0, 0, 0>::make(std::forward<Params>(params));

  case 4:
    if (allowRemovals)
      return Functor<1, 1, 0>::make(std::forward<Params>(params));
    else
      return Functor<1, 0, 0>::make(std::forward<Params>(params));

  default:
    MATHICGB_ASSERT_NO_ASSUME(false);
    throw std::runtime_error("Unknown code for monomial data structure");
  }  
}

MATHICGB_NAMESPACE_END
#endif
