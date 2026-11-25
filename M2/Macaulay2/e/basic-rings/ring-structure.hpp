#ifndef M2_BASIC_RINGS_RING_STRUCTURE_HPP_
#define M2_BASIC_RINGS_RING_STRUCTURE_HPP_

#include "basic-rings/aring.hpp"  // for M2::RingID

class Ring;

namespace M2 {

/// Mathematical structure of a ring, collapsing precision and backend.
/// RR and RRR both collapse to RealField; all ZZp backends collapse to
/// PrimeField; etc. Unknown covers ring_old (polynomial rings, fraction
/// fields, etc.) for which no structural order is defined.
enum class RingStructure : int {
  ZZ              = 0,
  QQ              = 1,
  RealField       = 2,
  ComplexField    = 3,
  RealInterval    = 4,
  ComplexInterval = 5,
  PrimeField      = 6,
  FiniteField     = 7,
  Unknown         = 8
};

/// Maps a concrete RingID to its mathematical structure.
RingStructure structureOf(RingID id);

/// True iff there is a canonical ring map from structure a to structure b
/// (reflexive-transitive closure of the canonical-map DAG).
/// Returns false when either argument is Unknown.
bool structurePrecedes(RingStructure a, RingStructure b);

/// True iff there exists a canonical ring map R → S.
/// Combines the structural partial order with characteristic matching (for
/// finite-field families) and dimension divisibility (for GF → GF).
bool hasCanonicalMap(const Ring* R, const Ring* S);

}  // namespace M2

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
