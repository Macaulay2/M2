// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_MONO_PROCESSOR_GUARD
#define MATHICGB_MONO_PROCESSOR_GUARD

#include "Basis.hpp"

MATHICGB_NAMESPACE_BEGIN

/// Does pre- and post-processing of monomials to implement module monomial
/// orders not directly supported by the monoid.
template<class Monoid>
class MonoProcessor;

template<class M>
class MonoProcessor {
public:
  typedef M Monoid;
  typedef typename Monoid::VarIndex VarIndex;
  typedef typename Monoid::Component Component;
  typedef typename Monoid::MonoVector MonoVector;
  typedef typename Monoid::MonoRef MonoRef;
  typedef typename Monoid::ConstMonoRef ConstMonoRef;
  typedef typename Monoid::ConstMonoPtr ConstMonoPtr;

  MonoProcessor(
    const Monoid& monoid,
    const bool componentsAscendingDesired,
    const bool schreyering
  ):
    mComponentsAscendingDesired(componentsAscendingDesired),
    mComponentCount(0),
    mSchreyering(schreyering),
    mSchreyerMultipliersMemory(monoid)
  {}

  void setComponentsAscendingDesired(bool value) {
    mComponentsAscendingDesired = value;
  }
  bool componentsAscendingDesired() const {return mComponentsAscendingDesired;}

  void setSchreyering(bool value) {mSchreyering = true;}
  bool schreyering() const {return mSchreyering;}

  void setSchreyerMultipliers(const Basis& basis) {
    MonoVector schreyer(monoid());
    for (size_t gen = 0; gen < basis.size(); ++gen)
      schreyer.push_back(basis.getPoly(gen)->leadMono());
    setSchreyerMultipliers(std::move(schreyer));
  }

  void setSchreyerMultipliers(MonoVector&& moduleAdjustments) {
    MATHICGB_ASSERT(moduleAdjustments.monoid() == monoid());
    MATHICGB_ASSERT(mSchreyerMultipliersMemory.empty() ||
      mSchreyerMultipliersMemory.size() == componentCount());
    mSchreyerMultipliersMemory = std::move(moduleAdjustments);

    mSchreyerMultipliers.clear();
    for (
      auto it = mSchreyerMultipliersMemory.begin();
      it != mSchreyerMultipliersMemory.end();
      ++it
    ) {
      // in the absence of a separate monoid for (non-module) monomials,
      // at least we can check that the component is zero.
      MATHICGB_ASSERT(this->monoid().component(*it) == 0);

      // todo: there should be a better way of indexing into a
      // MonoVector.
      mSchreyerMultipliers.emplace_back((*it).ptr());
    }
  }

  void preprocess(MonoRef mono) const {
    if (hasSchreyerMultipliers())
      monoid().multiplyInPlace(moduleAdjustment(mono), mono);
    if (needToReverseComponents())
      reverseComponent(mono);
  }

  void postprocess(MonoRef mono) const {
    if (needToReverseComponents())
      reverseComponent(mono);
    if (hasSchreyerMultipliers()) {
      MATHICGB_ASSERT(monoid().divides(moduleAdjustment(mono), mono));
      monoid().divideInPlace(moduleAdjustment(mono), mono);
    }
  }

  bool processingNeeded() const {
    return needToReverseComponents() || hasSchreyerMultipliers();
  }

  bool needToReverseComponents() const {
    return Monoid::HasComponent &&
      componentsAscendingDesired() != monoid().componentsAscending();
  }

  bool hasSchreyerMultipliers() const {
    return !mSchreyerMultipliers.empty();
  }

  void setComponentCount(Component count) {mComponentCount = count;}
  Component componentCount() const {return mComponentCount;}
  const Monoid& monoid() const {return mSchreyerMultipliersMemory.monoid();}

private:
  void operator==(const MonoProcessor&) const; // not available

  void reverseComponent(MonoRef mono) const {
    const auto component = monoid().component(mono);
    const auto newComponent = mComponentCount - 1 - component;
    monoid().setComponent(newComponent, mono);
  }

  ConstMonoRef moduleAdjustment(ConstMonoRef mono) const {
    MATHICGB_ASSERT(hasSchreyerMultipliers());
    const auto component = monoid().component(mono);
    MATHICGB_ASSERT(component < componentCount());
    MATHICGB_ASSERT(mSchreyerMultipliers.size() == componentCount());
    return *mSchreyerMultipliers[component];
  }

  bool mOrderFromLeft;
  bool mComponentsAscendingDesired;
  Component mComponentCount;
  bool mSchreyering;
  MonoVector mSchreyerMultipliersMemory;
  std::vector<ConstMonoPtr> mSchreyerMultipliers;
};

MATHICGB_NAMESPACE_END
#endif
