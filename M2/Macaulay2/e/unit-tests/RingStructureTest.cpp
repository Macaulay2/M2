#include <gtest/gtest.h>
#include "basic-rings/ring-structure.hpp"

using M2::RingStructure;
using M2::structurePrecedes;
using M2::structureOf;
using M2::RingID;

// structureOf: verify RingID → RingStructure mapping

TEST(StructureOf, CharZeroTower)
{
  EXPECT_EQ(structureOf(M2::ring_ZZ),       RingStructure::ZZ);
  EXPECT_EQ(structureOf(M2::ring_ZZFlint),  RingStructure::ZZ);
  EXPECT_EQ(structureOf(M2::ring_QQ),       RingStructure::QQ);
  EXPECT_EQ(structureOf(M2::ring_QQFlint),  RingStructure::QQ);
  EXPECT_EQ(structureOf(M2::ring_RR),       RingStructure::RealField);
  EXPECT_EQ(structureOf(M2::ring_RRR),      RingStructure::RealField);
  EXPECT_EQ(structureOf(M2::ring_CC),       RingStructure::ComplexField);
  EXPECT_EQ(structureOf(M2::ring_CCC),      RingStructure::ComplexField);
  EXPECT_EQ(structureOf(M2::ring_RRi),      RingStructure::RealInterval);
  EXPECT_EQ(structureOf(M2::ring_CCi),      RingStructure::ComplexInterval);
}

TEST(StructureOf, FiniteFields)
{
  EXPECT_EQ(structureOf(M2::ring_ZZp),          RingStructure::PrimeField);
  EXPECT_EQ(structureOf(M2::ring_ZZpFfpack),    RingStructure::PrimeField);
  EXPECT_EQ(structureOf(M2::ring_ZZpFlint),     RingStructure::PrimeField);
  EXPECT_EQ(structureOf(M2::ring_tower_ZZp),    RingStructure::PrimeField);
  EXPECT_EQ(structureOf(M2::ring_GFM2),         RingStructure::FiniteField);
  EXPECT_EQ(structureOf(M2::ring_GFFlintBig),   RingStructure::FiniteField);
  EXPECT_EQ(structureOf(M2::ring_GFFlintZech),  RingStructure::FiniteField);
}

TEST(StructureOf, Unknown)
{
  EXPECT_EQ(structureOf(M2::ring_old),     RingStructure::Unknown);
}

// structurePrecedes: reflexivity

TEST(StructurePrecedes, Reflexive)
{
  EXPECT_TRUE(structurePrecedes(RingStructure::ZZ,             RingStructure::ZZ));
  EXPECT_TRUE(structurePrecedes(RingStructure::QQ,             RingStructure::QQ));
  EXPECT_TRUE(structurePrecedes(RingStructure::RealField,      RingStructure::RealField));
  EXPECT_TRUE(structurePrecedes(RingStructure::ComplexField,   RingStructure::ComplexField));
  EXPECT_TRUE(structurePrecedes(RingStructure::RealInterval,   RingStructure::RealInterval));
  EXPECT_TRUE(structurePrecedes(RingStructure::ComplexInterval,RingStructure::ComplexInterval));
  EXPECT_TRUE(structurePrecedes(RingStructure::PrimeField,     RingStructure::PrimeField));
  EXPECT_TRUE(structurePrecedes(RingStructure::FiniteField,    RingStructure::FiniteField));
}

// structurePrecedes: valid canonical maps (promote direction)

TEST(StructurePrecedes, CharZeroTowerPromote)
{
  using S = RingStructure;
  // ZZ maps to everything
  EXPECT_TRUE(structurePrecedes(S::ZZ, S::QQ));
  EXPECT_TRUE(structurePrecedes(S::ZZ, S::RealField));
  EXPECT_TRUE(structurePrecedes(S::ZZ, S::ComplexField));
  EXPECT_TRUE(structurePrecedes(S::ZZ, S::RealInterval));
  EXPECT_TRUE(structurePrecedes(S::ZZ, S::ComplexInterval));
  EXPECT_TRUE(structurePrecedes(S::ZZ, S::PrimeField));
  EXPECT_TRUE(structurePrecedes(S::ZZ, S::FiniteField));

  // QQ chain
  EXPECT_TRUE(structurePrecedes(S::QQ, S::RealField));
  EXPECT_TRUE(structurePrecedes(S::QQ, S::ComplexField));
  EXPECT_TRUE(structurePrecedes(S::QQ, S::RealInterval));
  EXPECT_TRUE(structurePrecedes(S::QQ, S::ComplexInterval));

  // RealField chain
  EXPECT_TRUE(structurePrecedes(S::RealField, S::ComplexField));
  EXPECT_TRUE(structurePrecedes(S::RealField, S::RealInterval));
  EXPECT_TRUE(structurePrecedes(S::RealField, S::ComplexInterval));

  // ComplexField → ComplexInterval
  EXPECT_TRUE(structurePrecedes(S::ComplexField, S::ComplexInterval));

  // RealInterval → ComplexInterval
  EXPECT_TRUE(structurePrecedes(S::RealInterval, S::ComplexInterval));

  // PrimeField → FiniteField
  EXPECT_TRUE(structurePrecedes(S::PrimeField, S::FiniteField));
}

// structurePrecedes: invalid maps — the "nonsense" cases

TEST(StructurePrecedes, InvalidMaps)
{
  using S = RingStructure;
  // QQ does not map to ZZ (no ring hom QQ → ZZ)
  EXPECT_FALSE(structurePrecedes(S::QQ, S::ZZ));

  // QQ does not map to finite-characteristic rings
  EXPECT_FALSE(structurePrecedes(S::QQ, S::PrimeField));
  EXPECT_FALSE(structurePrecedes(S::QQ, S::FiniteField));

  // RealField does not map down to QQ or ZZ
  EXPECT_FALSE(structurePrecedes(S::RealField, S::QQ));
  EXPECT_FALSE(structurePrecedes(S::RealField, S::ZZ));

  // ComplexField does not map to RealField (non-trivial hom impossible)
  EXPECT_FALSE(structurePrecedes(S::ComplexField, S::RealField));

  // Interval types do not map to non-interval real/complex
  EXPECT_FALSE(structurePrecedes(S::RealInterval,   S::RealField));
  EXPECT_FALSE(structurePrecedes(S::ComplexInterval, S::ComplexField));

  // FiniteField does not map to char-0 rings
  EXPECT_FALSE(structurePrecedes(S::FiniteField, S::ZZ));
  EXPECT_FALSE(structurePrecedes(S::FiniteField, S::QQ));
  EXPECT_FALSE(structurePrecedes(S::FiniteField, S::RealField));

  // Unknown blocks everything
  EXPECT_FALSE(structurePrecedes(S::Unknown, S::ZZ));
  EXPECT_FALSE(structurePrecedes(S::ZZ,      S::Unknown));
  EXPECT_FALSE(structurePrecedes(S::Unknown, S::Unknown));
}
