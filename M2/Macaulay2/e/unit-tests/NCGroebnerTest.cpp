#include <gtest/gtest.h>

#include <algorithm>
#include <memory>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#include "MemoryBlock.hpp"
#include "interface/ring.h"

#include "rings/poly.hpp"
#include "basic-rings/aring-glue.hpp"
#include "NCAlgebras/FreeAlgebra.hpp"
#include "NCAlgebras/FreeAlgebraQuotient.hpp"
#include "NCAlgebras/WordTable.hpp"
#include "NCAlgebras/NCGroebner.hpp"
#include "NCAlgebras/OverlapTable.hpp"
#include "NCAlgebras/SuffixTree.hpp"
#include "NCAlgebras/NCReduction.hpp"
#include "monomials/monordering.hpp"
#include "monoid.hpp"

#include "unit-tests/util-polyring-creation.hpp"

namespace {

const std::vector<int> monom1 {2, 0, 1};                  // cab
const std::vector<int> monom2 {2, 2};                     // cc
const std::vector<int> monom3 {1, 0, 1, 0};               // baba
const std::vector<int> word {2, 0, 1, 2, 2, 1, 0, 1, 0};  // cabccbaba

void checkPolynomialHeap(HeapType type)
{
  FreeAlgebra* A = FreeAlgebra::create(
      globalQQ, {"x", "y", "z"}, degreeRing(1), {1, 1, 1}, {}, {1});
  ASSERT_NE(A, nullptr);
  FreeAlgebraElement x(A), y(A), z(A);
  A->var(*x, 0);
  A->var(*y, 1);
  A->var(*z, 2);
  {
    // Repeated reads must preserve the heap; removing terms empties it in
    // order.
    SCOPED_TRACE("heap: merge, repeat read, and remove");
    auto heap = makePolynomialHeap(type, *A);
    heap->addPolynomial(*(x + y));
    heap->addPolynomial(*(y + z));
    const auto expected = x + y + y + z;
    EXPECT_EQ(FreeAlgebraElement(A, *heap->value()), expected);
    EXPECT_EQ(FreeAlgebraElement(A, *heap->value()), expected);
    heap->removeLeadTerm();
    EXPECT_EQ(FreeAlgebraElement(A, *heap->value()), y + y + z);
    heap->removeLeadTerm();
    EXPECT_EQ(FreeAlgebraElement(A, *heap->value()), z);
    heap->removeLeadTerm();
    EXPECT_TRUE(heap->isZero());
    EXPECT_TRUE(A->is_zero(*heap->value()));
  }
  {
    // Opposite polynomials cancel without relying on an earlier removal
    // sequence.
    SCOPED_TRACE("heap: complete cancellation");
    auto heap = makePolynomialHeap(type, *A);
    const auto value = x + y + y + z;
    heap->addPolynomial(*value);
    heap->addPolynomial(*(-value));
    EXPECT_TRUE(heap->isZero());
    EXPECT_TRUE(A->is_zero(*heap->value()));
  }
}

void checkOverlappingHeapTerms(HeapType type)
{
  FreeAlgebra* A = FreeAlgebra::create(
      globalQQ, {"x", "y", "z"}, degreeRing(1), {1, 1, 1}, {}, {1});
  ASSERT_NE(A, nullptr);
  FreeAlgebraElement x(A), y(A), z(A);
  A->var(*x, 0);
  A->var(*y, 1);
  A->var(*z, 2);
  {
    // These degree-four inputs combine shared terms after the leading terms
    // cancel.
    SCOPED_TRACE("heap: overlapping degree-four terms");
    auto heap = makePolynomialHeap(type, *A);
    auto f = y * z * x * z - y * z * y * y - y * z * z * x - z * x * y * z +
             z * x * z * y - z * y * y * y - z * z * x * y - z * z * y * x -
             z * z * z * z;
    auto g = -y * z * x * z - y * z * y * y - y * z * z * x;
    const auto expected = -y * z * y * y - y * z * y * y - y * z * z * x -
                          y * z * z * x - z * x * y * z + z * x * z * y -
                          z * y * y * y - z * z * x * y - z * z * y * x -
                          z * z * z * z;
    heap->addPolynomial(*f);
    heap->addPolynomial(*g);
    EXPECT_EQ(FreeAlgebraElement(A, *heap->value()), expected);
  }
}

TEST(MemoryBlock, tryit)
{
  // Allocation growth and shrinking must preserve every still-live stored
  // value.
  MemoryBlock storage;
  struct Allocation
  {
    int* begin;
    int* end;
    int sequence;
  };
  std::vector<Allocation> allocations;
  for (int sequence = 0; sequence < 1000; ++sequence)
    {
      SCOPED_TRACE(::testing::Message() << "allocation " << sequence);
      const size_t size = 4 + (32343 * sequence) % 10;
      auto range = storage.allocateArray<int>(size);
      const size_t expectedSize = sequence % 93 == 0 ? 4 : size;
      if (sequence % 93 == 0)
        range = storage.shrinkLastAllocate(
            range.first, range.second, range.first + 4);
      ASSERT_EQ(range.second - range.first, expectedSize);
      for (int offset = 0; offset < expectedSize; ++offset)
        range.first[offset] = 100 * sequence + offset;
      allocations.push_back({range.first, range.second, sequence});
    }
  for (const auto& allocation : allocations)
    {
      SCOPED_TRACE(::testing::Message()
                   << "allocation " << allocation.sequence);
      for (int* value = allocation.begin; value != allocation.end; ++value)
        EXPECT_EQ(*value,
                  100 * allocation.sequence + (value - allocation.begin));
    }
}

TEST(NCReduction, tryit)
{
  // Both queue-backed reduction heaps preserve addition and cancellation.
  for (HeapType type :
       {HeapType::NaiveGeobucket, HeapType::NaiveDedupGeobucket})
    {
      SCOPED_TRACE(getHeapName(type));
      checkPolynomialHeap(type);
    }
}

TEST(NCReduction, TrivialPolynomialHeap)
{
  // The simple heap obeys the same polynomial contract as the queue-based
  // heaps.
  checkPolynomialHeap(HeapType::Trivial);
  checkOverlappingHeapTerms(HeapType::Trivial);
}

TEST(NCReduction, NaiveDedupPolynomialHeap)
{
  // Deduplicating terms preserves multiplicities and cancels opposite
  // coefficients.
  checkPolynomialHeap(HeapType::NaiveDedupGeobucket);
}

TEST(NCReduction, NaivePolynomialHeap)
{
  // The geobucket heap merges terms in order and preserves their coefficients.
  checkPolynomialHeap(HeapType::NaiveGeobucket);
  checkOverlappingHeapTerms(HeapType::NaiveGeobucket);
}

// Disabled because the deduplicating heap retains opposite leading terms in
// its returned polynomial. Re-enable when the result is canonical and agrees
// with the other heap implementations.
// https://github.com/Macaulay2/M2/issues/4700
TEST(NCReduction, DISABLED_deduplicatingHeapCancellation)
{
  // Equal leading words with opposite coefficients must cancel in the returned
  // polynomial.
  checkOverlappingHeapTerms(HeapType::NaiveDedupGeobucket);
}

TEST(MonomialOrdering, create)
{
  // Joined orderings retain their variable counts and ordering categories.
  auto mo1 = MonomialOrderings::Lex(5);
  auto mo2 = MonomialOrderings::GroupLex(4);
  auto mo3 = MonomialOrderings::join({mo1, mo2});
  std::string answer3 {
      "MonomialOrder => {\n    Lex => 5,\n    GroupLex => 4\n    }"};
  EXPECT_EQ(answer3, MonomialOrderings::toString(mo3));
  EXPECT_EQ(9, rawNumberOfVariables(mo3));
  EXPECT_TRUE(moIsLex(mo1));

  auto mo4 = MonomialOrderings::GRevLex({3, 2, 5, 7});
  EXPECT_TRUE(moIsGRevLex(mo4));
  auto mo5 = MonomialOrderings::GRevLex2({1, 1, 1, 1});
  EXPECT_TRUE(moIsGRevLex(mo5));
  auto mo6 {MonomialOrderings::join({MonomialOrderings::GRevLex(3),
                                     MonomialOrderings::GRevLex2(4),
                                     MonomialOrderings::GRevLex4(5),
                                     MonomialOrderings::GroupLex(3)})};
  EXPECT_EQ(rawNumberOfVariables(mo6), 15);
}

TEST(FreeAlgebra, create)
{
  // A weighted free algebra can be constructed over the rational field.
  ASSERT_NE(degreeRing(1), nullptr);
  ASSERT_FALSE(error());

  FreeAlgebra* A = FreeAlgebra::create(
      globalQQ, {"x", "y", "z"}, degreeRing(1), {1, 2, 3}, {}, {1});
  ASSERT_NE(A, nullptr);
}

TEST(FreeAlgebra, polyarithmetic)
{
  // Known noncommutative words check arithmetic, leading terms, and
  // normalization.
  FreeAlgebra* A = FreeAlgebra::create(
      globalQQ, {"x", "y", "z"}, degreeRing(1), {1, 2, 3}, {}, {1});
  ASSERT_NE(A, nullptr);
  FreeAlgebraElement x(A), y(A), z(A);
  A->var(*x, 0);
  A->var(*y, 1);
  A->var(*z, 2);

  {
    // Mixed generators expose word ordering and distribution errors.
    SCOPED_TRACE("arithmetic: mixed generators");
    auto f = x + y;
    auto g = y + z;
    EXPECT_EQ(x + y, y + x);
    EXPECT_FALSE(f == g) << "distinct sums must differ";
    EXPECT_EQ(x * (y + z), x * y + x * z);
    EXPECT_EQ((f * g) * f, f * (g * f));
    EXPECT_EQ(f ^ 2, f * f);
  }
  {
    // A nonempty word preserves letter order; an empty word is one.
    SCOPED_TRACE("from_word: nonempty and empty words");
    FreeAlgebraElement word(A), empty(A), one(A);
    A->from_word(*word, {1, 2, 1, 0, 1});
    A->from_word(*empty, std::vector<int> {});
    A->from_long(*one, 1);
    EXPECT_EQ(word, y * z * y * x * y);
    EXPECT_EQ(empty, one);
    EXPECT_TRUE(A->is_unit(*one));
    EXPECT_EQ(word ^ 0, one);
  }
  {
    // A negative coefficient agrees with subtraction and additive inversion.
    SCOPED_TRACE("signs: subtraction, negate and coefficient multiplication");
    FreeAlgebraElement difference(A), negated(A), scaled(A);
    auto f = x + y;
    A->subtract(*difference, *x, *y);
    A->negate(*negated, *f);
    A->mult_by_coeff(*scaled, *f, A->coefficientRing()->from_long(-1));
    EXPECT_EQ(difference, x - y);
    EXPECT_EQ(negated, -x - y);
    EXPECT_EQ(scaled, -x - y);
  }
  {
    // Appending a lower term retains the leading term; full subtraction is
    // zero.
    SCOPED_TRACE("terms: leading word, append and cancellation");
    auto f = x + y;
    FreeAlgebraElement lead(A), difference(A);
    A->lead_term_as_poly(*lead, *f);
    A->add_to_end(*f, *z);
    A->subtract(*difference, *f, *(x + y + z));
    EXPECT_EQ(lead, x);
    EXPECT_EQ(f, x + y + z);
    EXPECT_TRUE(A->is_zero(*difference));
    A->setZero(*f);
    EXPECT_TRUE(A->is_zero(*f));
  }
  {
    // Left and right word multiplication must not exchange the two sides.
    SCOPED_TRACE("term multiplication: distinct left and right words");
    auto f = x + y;
    FreeAlgebraElement left(A), right(A), both(A);
    const auto one = A->coefficientRing()->from_long(1);
    A->mult_by_term_left(*left, *f, one, Word(monom2));
    A->mult_by_term_right(*right, *f, one, Word(monom3));
    A->mult_by_term_left_and_right(*both, *f, one, Word(monom2), Word(monom3));
    EXPECT_EQ(left, z * z * f);
    EXPECT_EQ(right, f * y * x * y * x);
    EXPECT_EQ(both, z * z * f * y * x * y * x);
  }
  {
    // A negative leading coefficient is removed by monic normalization.
    SCOPED_TRACE("makeMonic: negative leading coefficient");
    auto f = -x - y;
    FreeAlgebraElement monic(A);
    A->makeMonic(*monic, *f);
    EXPECT_EQ(monic, x + y);
  }
}

TEST(FreeAlgebra, quotientArithmetic)
{
  // Anticommuting generators reduce a mixed word with the expected sign.
  FreeAlgebra* Q = FreeAlgebra::create(
      globalQQ, {"x", "y", "z"}, degreeRing(1), {1, 2, 3}, {}, {1});
  ASSERT_NE(Q, nullptr);
  FreeAlgebraElement X(Q), Y(Q), Z(Q), F(Q), G(Q), H(Q);
  Q->var(*X, 0);
  Q->var(*Y, 1);
  Q->var(*Z, 2);
  F = X * Y + Y * X;
  G = X * Z + Z * X;
  H = Y * Z + Z * Y;

  auto GB = std::unique_ptr<ConstPolyList>(new ConstPolyList);
  GB->push_back(&*F);
  GB->push_back(&*G);
  GB->push_back(&*H);
  EXPECT_TRUE(GB->size() == 3);

  FreeAlgebraQuotient quotient(*Q, *GB, -1);
  auto* A = &quotient;

  FreeAlgebraQuotientElement x(A), y(A), z(A), f(A), g(A), h(A);

  // check if things reduce properly
  A->var(*x, 0);
  A->var(*y, 1);
  A->var(*z, 2);
  A->setZero(*f);
  A->setZero(*g);
  A->setZero(*h);
  f = x * y * x * z * x * y * x * z;
  g = x * x * x * x * y * y * z * z;
  A->negate(*h, *g);
  EXPECT_TRUE(f == h);
}

TEST(FreeAlgebra, comparisons)
{
  // The configured ordering distinguishes the first two generators
  // consistently.
  FreeAlgebra* A = FreeAlgebra::create(
      globalQQ, {"x", "y", "z"}, degreeRing(1), {1, 2, 3}, {}, {1});
  ASSERT_NE(A, nullptr);
  FreeAlgebraElement x(A), y(A), z(A), f(A), g(A), h(A);
  A->var(*x, 0);
  A->var(*y, 1);
  A->var(*z, 2);
  EXPECT_TRUE(A->compare_elems(*x, *y) == GT);
  EXPECT_TRUE(A->compare_elems(*y, *x) == LT);
  EXPECT_TRUE(A->compare_elems(*x, *x) == EQ);
}

TEST(FreeAlgebra, spairs)
{
  // Leading-word slices identify the overlap used to construct an S-polynomial.
  FreeAlgebra* A = FreeAlgebra::create(
      globalQQ, {"x", "y", "z"}, degreeRing(1), {3, 2, 1}, {3, 2, 1}, {1});
  ASSERT_NE(A, nullptr);
  FreeAlgebraElement x(A), y(A), z(A), f(A), g(A), h(A);
  A->var(*x, 0);
  A->var(*y, 1);
  A->var(*z, 2);
  f = x * y * x + z * y * z;
  Word leadWord = A->lead_word(*f);
  Word leadWordPrefix = A->lead_word_prefix(*f, 2);
  Word leadWordSuffix = A->lead_word_suffix(*f, 1);
  // One heft word and one length word precede the three-letter leading word.
  EXPECT_TRUE((*f).cbegin().monom().begin() + 2 == leadWord.begin() &&
              (*f).cbegin().monom().begin() + 5 == leadWord.end());
  EXPECT_TRUE((*f).cbegin().monom().begin() + 2 == leadWordPrefix.begin() &&
              (*f).cbegin().monom().begin() + 4 == leadWordPrefix.end());
  EXPECT_TRUE((*f).cbegin().monom().begin() + 3 == leadWordSuffix.begin() &&
              (*f).cbegin().monom().begin() + 5 == leadWordSuffix.end());

  PolyList polyList {&*f};
  *g = *(NCGroebner::createOverlapPoly(*A, polyList, 0, 2, 0));
  h = f * y * x - x * y * f;
  EXPECT_TRUE(g == h);
}

TEST(OverlapTable, insertion)
{
  // Inserted overlaps remain pending at their degrees and contribute to the
  // total size.
  OverlapTable overlapTable;
  overlapTable.insert(3, false, std::make_tuple(1, 2, 3, true));
  overlapTable.insert(3, false, std::make_tuple(1, 2, 1, true));
  overlapTable.insert(2, false, std::make_tuple(1, 1, 1, true));
  EXPECT_FALSE(overlapTable.isFinished());
  EXPECT_TRUE(overlapTable.isFinished(1));
  EXPECT_FALSE(overlapTable.isFinished(3));
  EXPECT_TRUE(overlapTable.size() == 3);
}

TEST(NCGroebner, sorting)
{
  // The overlap table orders degree first, then the generator flag.
  OverlapTable table;
  for (const auto& key : {std::make_pair(2, true),
                          std::make_pair(1, false),
                          std::make_pair(2, false),
                          std::make_pair(1, true)})
    table.insert(key.first, key.second, std::make_tuple(0, 1, 0, true));
  const std::vector<std::pair<int, bool>> expected = {
      {1, false}, {1, true}, {2, false}, {2, true}};
  std::vector<std::pair<int, bool>> actual;
  for (const auto& entry : table.overlapMap()) actual.push_back(entry.first);
  EXPECT_EQ(actual, expected);
}

TEST(WordTable, create)
{
  // Each inserted word receives one entry in the table.
  WordTable W;

  EXPECT_EQ(monom1.size(), 3);
  EXPECT_EQ(monom2.size(), 2);

  W.insert(Word(monom1));
  W.insert(Word(monom2));
  W.insert(Word(monom3));

  EXPECT_EQ(W.monomialCount(), 3);
}

TEST(WordTable, insert)
{
  // Subword lookup returns the inserted word indices and starting positions.
  WordTable W;

  EXPECT_EQ(monom1.size(), 3);
  EXPECT_EQ(monom2.size(), 2);

  W.insert(Word(monom1));
  W.insert(Word(monom2));
  W.insert(Word(monom3));

  std::vector<std::pair<int, int>> matches;
  W.subwords(Word(word), matches);

  ASSERT_EQ(matches.size(), 3);
  EXPECT_EQ(matches[0], std::make_pair(0, 0));
  EXPECT_EQ(matches[1], std::make_pair(1, 3));
  EXPECT_EQ(matches[2], std::make_pair(2, 5));
}

TEST(WordTable, simpleSubwords)
{
  // A shorter word cannot contain a longer stored pattern.
  std::vector<int> monom1 {1, 1};  // yy
  std::vector<int> word {1};       // y

  WordTable W;
  W.insert(Word(monom1));

  std::vector<std::pair<int, int>> matches;
  W.subwords(Word(word), matches);

  EXPECT_EQ(matches.size(), 0);
}

TEST(WordTable, subwords)
{
  // Overlapping occurrences are all reported with their pattern indices.
  std::vector<int> monom1 {1, 0, 1, 2};  // babc
  std::vector<int> monom2 {1, 0, 2, 2};  // bacc
  std::vector<int> monom3 {1, 0, 1, 0};  // baba
  std::vector<int> monom4 {1, 0};        // ba
  std::vector<int> word {1, 0, 1, 0, 2, 2, 1, 0, 1, 2};

  WordTable W;

  EXPECT_EQ(monom1.size(), 4);
  EXPECT_EQ(monom2.size(), 4);
  EXPECT_EQ(monom3.size(), 4);
  EXPECT_EQ(monom4.size(), 2);
  EXPECT_EQ(word.size(), 10);

  W.insert(Word(monom1));
  W.insert(Word(monom2));
  W.insert(Word(monom3));
  W.insert(Word(monom4));

  std::vector<std::pair<int, int>> matches;
  W.subwords(Word(word), matches);

  ASSERT_EQ(matches.size(), 6);
  EXPECT_EQ(matches[0], std::make_pair(0, 6));
  EXPECT_EQ(matches[1], std::make_pair(1, 2));
  EXPECT_EQ(matches[2], std::make_pair(2, 0));
  EXPECT_EQ(matches[3], std::make_pair(3, 0));
  EXPECT_EQ(matches[4], std::make_pair(3, 2));
  EXPECT_EQ(matches[5], std::make_pair(3, 6));
}

TEST(WordTable, prefix_suffix)
{
  // Prefix and suffix lookup distinguish full matches, boundary matches, and
  // absent patterns.
  std::vector<int> monom1 {1, 0, 1, 2};  // babc
  std::vector<int> monom2 {1, 0, 2, 2};  // bacc
  std::vector<int> monom3 {1, 0, 1, 0};  // baba
  std::vector<int> word {1, 0, 1, 0, 2, 2, 1, 0, 1, 2};
  std::vector<int> word2 {1, 0, 1, 1, 2, 2, 1, 1, 1, 2};

  WordTable W;

  EXPECT_EQ(monom1.size(), 4);
  EXPECT_EQ(monom2.size(), 4);
  EXPECT_EQ(monom3.size(), 4);
  EXPECT_EQ(word.size(), 10);

  W.insert(Word(monom1));
  W.insert(Word(monom2));
  W.insert(Word(monom3));

  // Full words, boundary occurrences and absence have distinct expected
  // indices.
  struct Case
  {
    const char* name;
    std::vector<int> input;
    int prefix;
    int suffix;
  };
  for (const auto& sample : {Case {"boundary occurrences", word, 2, 0},
                             Case {"first full pattern", monom1, 0, 0},
                             Case {"second full pattern", monom2, 1, 1},
                             Case {"absent", word2, -1, -1}})
    {
      SCOPED_TRACE(sample.name);
      int prefix = -1, suffix = -1;
      EXPECT_EQ(W.isPrefix(Word(sample.input), prefix), sample.prefix >= 0);
      EXPECT_EQ(W.isSuffix(Word(sample.input), suffix), sample.suffix >= 0);
      if (sample.prefix >= 0) EXPECT_EQ(prefix, sample.prefix);
      if (sample.suffix >= 0) EXPECT_EQ(suffix, sample.suffix);
    }
}

template <typename PatternStore>
void checkSklyaninPatterns()
{
  // Sklyanin leading words exercise overlapping patterns and repeated subwords.
  // X,Y Z are the 3 variables

  std::vector<int> m0 {2, 0};               // ZX
  std::vector<int> m1 {2, 1};               // ZY
  std::vector<int> m2 {2, 2};               // ZZ
  std::vector<int> m3 {1, 1, 0};            // YYX
  std::vector<int> m4 {1, 1, 2};            // YYZ
  std::vector<int> m5 {1, 0, 1, 1};         // YXYY
  std::vector<int> m6 {1, 1, 1, 1};         // YYYY
  std::vector<int> m7 {1, 0, 1, 0, 0};      // YXYXX
  std::vector<int> m8 {1, 0, 1, 0, 1};      // YXYXY
  std::vector<int> m9 {1, 0, 1, 0, 2};      // YXYXZ
  std::vector<int> m10 {1, 0, 0, 1, 0, 0};  // YXXYXX
  std::vector<int> m11 {1, 0, 0, 1, 0, 2};  // YXXYXZ
  std::vector<int> m12 {1, 0, 0, 1, 1, 1};  // YXXYYY

  std::vector<Overlap> overlaps;
  std::vector<std::pair<int, int>> matches;

  PatternStore W;
  // The first three words have three one-letter right overlaps.
  {
    SCOPED_TRACE("Sklyanin: ZX, ZY and ZZ initial overlaps");
    W.insert(Word(m0), overlaps);
    EXPECT_EQ(0, overlaps.size());

    W.insert(Word(m1), overlaps);
    EXPECT_EQ(0, overlaps.size());

    W.insert(Word(m2), overlaps);
    std::vector<Overlap> ans {std::make_tuple(2, 1, 0, true),
                              std::make_tuple(2, 1, 1, true),
                              std::make_tuple(2, 1, 2, true)};
    std::sort(ans.begin(), ans.end());
    std::sort(overlaps.begin(), overlaps.end());

    ASSERT_EQ(overlaps, ans);
    overlaps.clear();
    W.leftOverlaps(overlaps);
    EXPECT_EQ(0, overlaps.size());
  }
  // Add longer patterns before querying their shared YY occurrences.
  W.insert(Word(m3));
  W.insert(Word(m4));
  W.insert(Word(m5));
  W.insert(Word(m6));
  W.insert(Word(m7));
  W.insert(Word(m8));
  W.insert(Word(m9));
  W.insert(Word(m10));
  W.insert(Word(m11));
  W.insert(Word(m12));

  {
    SCOPED_TRACE("superwords: repeated YY occurrences");
    matches.clear();
    W.superwords(Word(std::vector<int> {1, 1}), matches);
    std::vector<std::pair<int, int>> ans2 {std::make_pair(3, 0),
                                           std::make_pair(4, 0),
                                           std::make_pair(5, 2),
                                           std::make_pair(6, 0),
                                           std::make_pair(6, 1),
                                           std::make_pair(6, 2),
                                           std::make_pair(12, 3),
                                           std::make_pair(12, 4)};
    std::sort(ans2.begin(), ans2.end());
    std::sort(matches.begin(), matches.end());
    EXPECT_EQ(ans2, matches);
  }

  // Interior occurrences make both mixed words nontrivial superwords.
  {
    SCOPED_TRACE("superwords: interior occurrence");
    EXPECT_TRUE(W.isNontrivialSuperword(
        Word(std::vector<int> {1, 2, 1, 1, 2, 1, 1}), 6, 6));
    EXPECT_TRUE(W.isNontrivialSuperword(
        Word(std::vector<int> {1, 1, 2, 1, 1, 2, 1, 1, 2}), 4, 4));
  }

  // Five patterns occur, including an overlapping longer pattern.
  {
    SCOPED_TRACE("subwords: ZZXYYXYXYY");
    matches.clear();
    W.subwords(Word(std::vector<int> {2, 2, 0, 1, 1, 0, 1, 0, 1, 1}),
               matches);  // ZZXYYXYXYY
    std::vector<std::pair<int, int>> ans3 {
        {0, 1}, {2, 0}, {3, 3}, {5, 6}, {8, 4}};
    std::sort(ans3.begin(), ans3.end());
    std::sort(matches.begin(), matches.end());
    EXPECT_EQ(ans3, matches);
  }
}

TEST(WordTable, skylanin)
{
  // Sklyanin leading words exercise overlapping patterns and repeated subwords.
  checkSklyaninPatterns<WordTable>();
}

// Disabled because this pattern set loses the baba self-overlap and leaves
// expected suffix links unset. Re-enable when these invariants and the later
// pattern-set assertions pass.
// https://github.com/Macaulay2/M2/issues/4701
TEST(SuffixTree, DISABLED_suffixtree1)
{
  // Known pattern sets check suffix links, overlap counts, and repeated subword
  // matches.
  {
    // A small pattern set checks the root links and two self-overlaps.
    SCOPED_TRACE("suffix links: cc, cab, baba");
    auto suffixTree = std::make_unique<SuffixTree>();
    EXPECT_TRUE(suffixTree->numPatterns() == 0);

    Label sLabel {1, 2, 3, 2, 1};
    Label tLabel {1, 2, 3, 4, 1};
    Label resultLabel {1, 2, 3};
    Word sWord(sLabel);
    Word tWord(tLabel);
    Word resultWord(resultLabel);
    EXPECT_TRUE(suffixTree->sharedPrefix(sWord, tWord) == Word(resultWord));

    std::vector<Overlap> rightOverlaps {};

    auto monList1 =
        std::vector<Label> {Label {2, 2}, Label {2, 0, 1}, Label {1, 0, 1, 0}};
    suffixTree->insert(monList1, rightOverlaps);

    Label xLabel {0};
    Label yLabel {1};
    Label zLabel {2};
    Label yxLabel {1, 0};
    Word xWord(xLabel);
    Word yWord(yLabel);
    Word zWord(zLabel);
    Word yxWord(yxLabel);
    auto xNode =
        std::get<0>(suffixTree->extendedLocus(suffixTree->mRoot, xWord));
    auto yNode =
        std::get<0>(suffixTree->extendedLocus(suffixTree->mRoot, yWord));
    auto zNode =
        std::get<0>(suffixTree->extendedLocus(suffixTree->mRoot, zWord));
    auto yxNode =
        std::get<0>(suffixTree->extendedLocus(suffixTree->mRoot, yxWord));
    ASSERT_NE(xNode, nullptr);
    ASSERT_NE(yNode, nullptr);
    ASSERT_NE(zNode, nullptr);
    ASSERT_NE(yxNode, nullptr);
    EXPECT_EQ(xNode->suffixLink(), suffixTree->mRoot);
    EXPECT_EQ(yNode->suffixLink(), suffixTree->mRoot);
    EXPECT_EQ(zNode->suffixLink(), suffixTree->mRoot);
    EXPECT_EQ(yxNode->suffixLink(), xNode);
    auto retval = std::vector<Overlap> {std::make_tuple(0, 1, 0, true),
                                        std::make_tuple(2, 2, 2, true)};
    ASSERT_EQ(rightOverlaps, retval);
  }
  {
    // These 47 leading words exercise repeated suffixes at several depths.
    SCOPED_TRACE("overlaps: 47 Sklyanin patterns");
    std::vector<Overlap> rightOverlaps;
    auto suffixTree2 = std::make_unique<SuffixTree>();
    auto monList2 =
        std::vector<Label> {Label {2, 0},
                            Label {2, 1},
                            Label {2, 2},
                            Label {1, 1, 0},
                            Label {1, 1, 2},
                            Label {1, 0, 1, 1},
                            Label {1, 1, 1, 1},
                            Label {1, 0, 1, 0, 0},
                            Label {1, 0, 1, 0, 1},
                            Label {1, 0, 1, 0, 2},
                            Label {1, 0, 0, 1, 0, 0},
                            Label {1, 0, 0, 1, 0, 2},
                            Label {1, 0, 0, 1, 1, 1},
                            Label {1, 0, 0, 0, 1, 0, 1},
                            Label {1, 0, 0, 0, 1, 1, 1},
                            Label {1, 0, 0, 1, 0, 1, 0},
                            Label {1, 0, 0, 1, 0, 1, 2},
                            Label {1, 0, 0, 0, 0, 1, 1, 1},
                            Label {1, 0, 0, 0, 1, 0, 0, 0},
                            Label {1, 0, 0, 0, 1, 0, 0, 1},
                            Label {1, 0, 0, 0, 1, 0, 0, 2},
                            Label {1, 0, 0, 0, 0, 0, 1, 1, 1},
                            Label {1, 0, 0, 0, 0, 1, 0, 0, 0},
                            Label {1, 0, 0, 0, 0, 1, 0, 0, 2},
                            Label {1, 0, 0, 0, 0, 1, 0, 1, 0},
                            Label {1, 0, 0, 0, 0, 1, 0, 1, 2},
                            Label {1, 0, 0, 0, 0, 0, 1, 0, 0, 1},
                            Label {1, 0, 0, 0, 0, 0, 1, 0, 0, 2},
                            Label {1, 0, 0, 0, 0, 0, 1, 0, 1, 0},
                            Label {1, 0, 0, 0, 0, 0, 1, 0, 1, 2},
                            Label {1, 0, 0, 0, 0, 1, 0, 0, 1, 0},
                            Label {1, 0, 0, 0, 0, 1, 0, 0, 1, 1},
                            Label {1, 0, 0, 0, 0, 1, 0, 0, 1, 2},
                            Label {1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 2},
                            Label {1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0},
                            Label {1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 2},
                            Label {1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0},
                            Label {1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1},
                            Label {1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2},
                            Label {1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 2},
                            Label {1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 2},
                            Label {1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0},
                            Label {1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1},
                            Label {1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2},
                            Label {1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0},
                            Label {1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1},
                            Label {1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 2}};
    suffixTree2->insert(monList2, rightOverlaps);
    EXPECT_EQ(596, rightOverlaps.size());
    EXPECT_EQ(47, suffixTree2->numPatterns());

    // All occurrences, and the first single match, use pattern/position pairs.
    {
      SCOPED_TRACE("subwords: five occurrences in ZZXYYXYXYY");
      const Label query {2, 2, 0, 1, 1, 0, 1, 0, 1, 1};
      std::vector<std::pair<int, int>> actual;
      suffixTree2->subwords(Word(query), actual);
      std::sort(actual.begin(), actual.end());
      const std::vector<std::pair<int, int>> expected {
          {0, 1}, {2, 0}, {3, 3}, {5, 6}, {8, 4}};
      EXPECT_EQ(actual, expected);
      std::pair<int, int> first {-1, -1};
      EXPECT_TRUE(suffixTree2->subword(Word(query), first));
      EXPECT_EQ(first, std::make_pair(2, 0));
    }

    auto superwordsOutput = std::vector<std::pair<int, int>> {};
    Label yyLabel {1, 1};
    Word yyWord(yyLabel);
    suffixTree2->superwords(yyWord, superwordsOutput);
    auto correctSuperwords =
        std::vector<std::pair<int, int>> {std::make_pair(12, 4),
                                          std::make_pair(45, 10),
                                          std::make_pair(14, 5),
                                          std::make_pair(31, 8),
                                          std::make_pair(3, 0),
                                          std::make_pair(17, 6),
                                          std::make_pair(4, 0),
                                          std::make_pair(21, 7),
                                          std::make_pair(5, 2),
                                          std::make_pair(17, 5),
                                          std::make_pair(6, 0),
                                          std::make_pair(12, 3),
                                          std::make_pair(21, 6),
                                          std::make_pair(14, 4),
                                          std::make_pair(6, 1),
                                          std::make_pair(6, 2)};
    std::sort(superwordsOutput.begin(), superwordsOutput.end());
    std::sort(correctSuperwords.begin(), correctSuperwords.end());
    EXPECT_EQ(superwordsOutput, correctSuperwords);

    auto leftOverlapsOutput = std::vector<std::pair<int, int>> {};
    Label yyxLabel {1, 1, 0};
    Word yyxWord(yyxLabel);
    suffixTree2->leftOverlaps(yyxWord, leftOverlapsOutput);
    auto correctLeftOverlaps = std::vector<std::pair<int, int>> {
        std::make_pair(1, 1),   std::make_pair(6, 2),   std::make_pair(6, 3),
        std::make_pair(5, 3),   std::make_pair(5, 2),   std::make_pair(8, 4),
        std::make_pair(12, 4),  std::make_pair(12, 5),  std::make_pair(14, 5),
        std::make_pair(14, 6),  std::make_pair(13, 6),  std::make_pair(19, 7),
        std::make_pair(17, 6),  std::make_pair(17, 7),  std::make_pair(21, 7),
        std::make_pair(21, 8),  std::make_pair(31, 8),  std::make_pair(31, 9),
        std::make_pair(26, 9),  std::make_pair(37, 10), std::make_pair(45, 10),
        std::make_pair(45, 11), std::make_pair(42, 11)};
    std::sort(leftOverlapsOutput.begin(), leftOverlapsOutput.end());
    std::sort(correctLeftOverlaps.begin(), correctLeftOverlaps.end());
    EXPECT_EQ(leftOverlapsOutput, correctLeftOverlaps);
  }
  {
    // A matching prefix alone must not produce a full subword match.
    SCOPED_TRACE("subword: YYX inserted, YYZ absent");
    std::vector<Overlap> rightOverlaps;
    const Label yyxLabel {1, 1, 0};
    const Word yyxWord(yyxLabel);
    auto suffixTree3 = std::make_unique<SuffixTree>();
    auto monList3 =
        std::vector<Label> {Label {2, 0}, Label {2, 1}, Label {2, 2}};
    auto leftOverlapsOutput2 = std::vector<Overlap> {};
    std::pair<int, int> o;
    suffixTree3->insert(monList3, rightOverlaps);
    suffixTree3->leftOverlaps(leftOverlapsOutput2);
    EXPECT_EQ(leftOverlapsOutput2.size(), 0);

    Label yyzLabel {1, 1, 2};
    Word yyzWord(yyzLabel);
    suffixTree3->insert(yyxWord, rightOverlaps);
    EXPECT_FALSE(suffixTree3->subword(yyzWord, o));
  }
  {
    // Inserting a longer word preserves both occurrences in an extended query.
    SCOPED_TRACE("subwords: long inserted word and trailing suffix");
    std::vector<Overlap> rightOverlaps;
    auto monList4 = std::vector<Label> {Label {0, 0},
                                        Label {0, 1},
                                        Label {0, 2},
                                        Label {1, 1, 2},
                                        Label {1, 1, 0},
                                        Label {1, 1, 1, 1},
                                        Label {1, 2, 1, 1},
                                        Label {1, 2, 1, 2, 1},
                                        Label {1, 2, 1, 2, 0},
                                        Label {1, 2, 1, 2, 2},
                                        Label {1, 2, 2, 1, 1, 1},
                                        Label {1, 2, 2, 1, 2, 2},
                                        Label {1, 2, 2, 1, 2, 0},
                                        Label {1, 2, 2, 1, 2, 1, 0},
                                        Label {1, 2, 2, 1, 2, 1, 2},
                                        Label {1, 2, 2, 2, 1, 2, 1},
                                        Label {1, 2, 2, 2, 1, 1, 1},
                                        Label {1, 2, 2, 2, 1, 2, 2, 2},
                                        Label {1, 2, 2, 2, 1, 2, 2, 1},
                                        Label {1, 2, 2, 2, 1, 2, 2, 0},
                                        Label {1, 2, 2, 2, 2, 1, 1, 1}};
    rightOverlaps.clear();
    auto suffixTree4 = std::make_unique<SuffixTree>();
    suffixTree4->insert(monList4, rightOverlaps);
    Label bigLabel {1, 2, 2, 2, 2, 1, 2, 1, 0};
    Word bigWord(bigLabel);
    suffixTree4->insert(bigWord, rightOverlaps);
    auto subwordsOutput2 = std::vector<std::pair<int, int>> {};
    Label bigLabel2 {1, 2, 2, 2, 2, 1, 2, 1, 0, 0};
    Word bigWord2(bigLabel2);
    suffixTree4->subwords(bigWord2, subwordsOutput2);
    EXPECT_EQ(subwordsOutput2.size(), 2);
  }
}

// Disabled because inserting ZX, ZY, ZZ omits the overlaps of ZZ with ZX and
// ZY. Re-enable when the suffix tree satisfies the same overlap and occurrence
// assertions as WordTable.
// https://github.com/Macaulay2/M2/issues/4701
TEST(SuffixTree, DISABLED_suffixtree2)
{
  // The suffix tree must find the same Sklyanin overlaps and occurrences as the
  // word table.
  checkSklyaninPatterns<SuffixTree>();
}

}  // namespace

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests
// runNCGroebnerTest  " indent-tabs-mode: nil End:
