#include "EGB.hpp"

EMatrix *EGroebnerComputation::reduceMatrix(const EMatrix *m) const
{
  return m->reduceByGB(this);
}

EMatrix *EGroebnerComputation::reduceMatrix(const EMatrix *m, EMatrix * & result_lift) const
{
  return m->reduceByGB(this, result_lift);
}

EDeclaredGB *EDeclaredGB::make(EMatrix *gens, EMatrix *gb, EMatrix *change, EMatrix *syz)
{
  return new EDeclaredGB(gens,gb,change,syz);
}

EDeclaredGB::EDeclaredGB(EMatrix *gens, EMatrix *gb, EMatrix *change, EMatrix *syz)
  : gens(gens), gb(gb), change(change), syz(syz)
{
  bump_up(gens);
  bump_up(gb);
  bump_up(change);
  bump_up(syz);
}

const EFreeModule *EDeclaredGB::getTarget() const
{
  return gb->getTarget();
}

const EFreeModule *EDeclaredGB::getSource() const
{
  return change->getTarget();
}

EVector EDeclaredGB::reduceVector(const EVector &v, int multType) const
{
  return v.clone();
}

EVector EDeclaredGB::reduceVector(const EVector &v, int multType, EVector &result_lift) const
{
  result_lift = v.getFreeModule()->zero();
  return v.clone();
}
