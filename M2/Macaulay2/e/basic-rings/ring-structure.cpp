#include "basic-rings/ring-structure.hpp"
#include "rings/ring.hpp"
#include "basic-rings/aring-glue.hpp"

namespace M2 {

namespace {

/// Direct edges in the canonical-map DAG.
/// directEdge(a, b) iff there is an atomic canonical map a → b.
/// Edges: ZZ→QQ, ZZ→PrimeField, QQ→RealField, RealField→ComplexField,
///        RealField→RealInterval, ComplexField→ComplexInterval,
///        RealInterval→ComplexInterval, PrimeField→FiniteField.
bool directEdge(RingStructure a, RingStructure b)
{
  using S = RingStructure;
  return (a == S::ZZ          && (b == S::QQ || b == S::PrimeField))            ||
         (a == S::QQ          &&  b == S::RealField)                             ||
         (a == S::RealField   && (b == S::ComplexField || b == S::RealInterval)) ||
         (a == S::ComplexField  && b == S::ComplexInterval)                      ||
         (a == S::RealInterval  && b == S::ComplexInterval)                      ||
         (a == S::PrimeField    && b == S::FiniteField);
}

constexpr int numRingStructures = 8;  // excludes Unknown
constexpr RingStructure allStructures[numRingStructures] = {
    RingStructure::ZZ,          RingStructure::QQ,
    RingStructure::RealField,   RingStructure::ComplexField,
    RingStructure::RealInterval, RingStructure::ComplexInterval,
    RingStructure::PrimeField,  RingStructure::FiniteField
};

// Returns n for GF(p^n) — the dimension as a vector space over GF(p).
// Only meaningful when structureOf(R->ringID()) == FiniteField.
long gfDimension(const Ring* R)
{
  switch (R->ringID()) {
    case ring_GFFlintZech:
      return static_cast<const ConcreteRing<ARingGFFlint>*>(R)->ring().dimension();
    case ring_GFFlintBig:
      return static_cast<const ConcreteRing<ARingGFFlintBig>*>(R)->ring().dimension();
    case ring_GFM2:
      return static_cast<long>(
          static_cast<const ConcreteRing<ARingGFM2>*>(R)->ring().dimension());
    default:
      return 0;
  }
}

}  // namespace

RingStructure structureOf(RingID id)
{
  switch (id) {
    case ring_ZZ:         case ring_ZZFlint:     return RingStructure::ZZ;
    case ring_QQ:         case ring_QQFlint:     return RingStructure::QQ;
    case ring_RR:         case ring_RRR:         return RingStructure::RealField;
    case ring_CC:         case ring_CCC:         return RingStructure::ComplexField;
    case ring_RRi:                               return RingStructure::RealInterval;
    case ring_CCi:                               return RingStructure::ComplexInterval;
    case ring_ZZp:        case ring_ZZpFfpack:
    case ring_ZZpFlint:   case ring_tower_ZZp:   return RingStructure::PrimeField;
    case ring_GFM2:       case ring_GFFlintBig:
    case ring_GFFlintZech:                       return RingStructure::FiniteField;
    default:                                     return RingStructure::Unknown;
  }
}

bool structurePrecedes(RingStructure a, RingStructure b)
{
  if (a == RingStructure::Unknown || b == RingStructure::Unknown) return false;
  if (a == b) return true;
  // BFS over the 8-node DAG.
  bool reachable[numRingStructures] = {};
  for (int i = 0; i < numRingStructures; ++i)
    if (directEdge(a, allStructures[i])) reachable[i] = true;
  bool changed = true;
  while (changed) {
    changed = false;
    for (int i = 0; i < numRingStructures; ++i) {
      if (!reachable[i]) continue;
      for (int j = 0; j < numRingStructures; ++j) {
        if (!reachable[j] && directEdge(allStructures[i], allStructures[j])) {
          reachable[j] = true;
          changed = true;
        }
      }
    }
  }
  for (int i = 0; i < numRingStructures; ++i)
    if (allStructures[i] == b && reachable[i]) return true;
  return false;
}

bool hasCanonicalMap(const Ring* R, const Ring* S)
{
  auto a = structureOf(R->ringID());
  auto b = structureOf(S->ringID());
  if (!structurePrecedes(a, b)) return false;
  // Finite-field families require matching characteristic.
  if (a == RingStructure::PrimeField || a == RingStructure::FiniteField)
    if (R->characteristic() != S->characteristic()) return false;
  // GF(p^m) → GF(p^n) requires m | n.
  if (a == RingStructure::FiniteField && b == RingStructure::FiniteField) {
    long dimR = gfDimension(R), dimS = gfDimension(S);
    if (dimR <= 0 || dimS % dimR != 0) return false;
  }
  return true;
}

}  // namespace M2

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
