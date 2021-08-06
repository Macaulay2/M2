// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "mathicgb/stdinc.h"

#include "mathicgb/Poly.hpp"
#include "mathicgb/Basis.hpp"
#include "mathicgb/ModuleMonoSet.hpp"
#include "mathicgb/io-util.hpp"
#include "mathicgb/SigPolyBasis.hpp"
#include "mathicgb/SignatureGB.hpp"
#include "mathicgb/ClassicGBAlg.hpp"
#include "mathicgb/mtbb.hpp"
#include "mathicgb/MathicIO.hpp"
#include "mathicgb/Scanner.hpp"
#include "ideals.hpp"
#include <cstdio>
#include <string>
#include <iostream>
#include <sstream>
#include <memory>
#include <gtest/gtest.h>

using namespace mgb;

TEST(IO, ideal) {
  const char* idealA_fromStr_format = 
"32003 6 \
1 1 1 1 1 1 1 \
3 \
-bc+ad \
-b2+af \
-bc2+a2e \
";

  std::unique_ptr<Basis> I = basisParseFromString(idealA_fromStr_format);
  EXPECT_EQ("  -bc+ad\n  -b2+af\n  -bc2+a2e\n", toString(I.get()));
}

void testGB(
  std::string idealStr,
  std::string sigBasisStr,
  std::string syzygiesStr,
  std::string initialIdealStr,
  size_t nonSingularReductions
) {
  // Put the contents of pict.out into allPairsTest as a string. This
  // works because pict.out does not have any commas and we do not
  // care about whitespace. pict.out contains a set of tests such that
  // all pairs of parameters are covered by at least one test. See
  // pict.in for details.
#define MATHICGB_ESCAPE_MULTILINE_STRING(str) #str
char const allPairsTests[] = MATHICGB_ESCAPE_MULTILINE_STRING(
spairQueue	reducerType	divLookup	monTable	buchberger	postponeKoszul	useBaseDivisors	autoTailReduce	autoTopReduce	preferSparseReducers	useSingularCriterionEarly	sPairGroupSize	threadCount
0	25	4	1	1	0	0	0	0	0	0	1	1
3	11	3	2	0	1	1	0	0	1	1	100	2
1	9	1	4	1	0	0	1	1	1	0	2	8
2	21	2	3	1	0	0	1	1	0	0	10	2
1	9	2	4	0	1	1	0	0	0	1	1	2
2	7	1	3	0	0	1	0	0	1	1	1	8
0	21	4	1	0	1	0	0	0	1	1	100	1
3	26	3	2	1	0	0	0	1	1	0	10	1
3	26	3	1	1	0	0	1	1	0	0	1	8
0	25	4	2	1	0	0	1	1	1	0	0	8
0	14	1	1	0	1	1	0	0	0	1	10	8
2	22	4	4	1	0	0	1	0	0	0	10	1
1	14	4	3	0	1	1	0	0	1	0	2	1
2	10	2	2	0	1	1	0	0	0	1	2	2
3	17	2	1	0	1	1	0	0	0	1	0	2
0	18	1	2	0	1	1	0	0	1	1	1	2
0	23	2	3	1	0	0	1	1	1	0	10	1
1	10	3	4	1	0	0	1	1	0	0	100	8
2	19	3	3	0	1	1	0	0	0	1	0	1
0	26	4	1	1	0	0	0	0	1	0	2	2
3	13	1	4	1	0	0	1	0	1	0	2	1
1	15	2	1	1	0	0	1	0	0	0	10	8
0	21	3	4	1	0	0	1	1	0	0	2	8
3	10	4	3	0	0	1	0	0	1	1	1	1
1	12	1	2	0	0	1	0	0	0	1	10	8
2	19	1	1	1	0	0	1	1	1	0	100	8
1	19	4	2	1	0	0	0	1	1	0	10	2
1	16	1	4	1	0	0	0	1	1	0	0	1
1	26	2	3	1	0	0	1	0	0	0	100	8
2	16	4	3	0	1	1	0	0	0	1	1	2
0	7	2	1	1	0	0	1	1	0	0	10	2
3	9	4	2	0	1	0	0	0	0	1	10	1
1	17	3	3	1	0	0	1	1	1	0	10	1
1	23	1	2	0	1	1	0	0	0	1	1	2
2	14	3	4	1	0	0	1	1	1	0	100	2
1	7	4	4	1	0	0	1	1	0	0	2	1
1	13	4	3	0	1	1	0	0	0	1	1	2
3	23	4	1	0	0	1	0	0	0	1	0	8
3	7	3	2	0	1	1	0	0	1	1	0	2
0	17	1	2	1	0	0	1	1	0	0	100	8
0	10	1	1	0	1	1	0	0	1	0	10	8
0	12	2	4	1	0	0	1	1	1	0	1	2
2	12	4	1	1	0	0	1	1	1	0	0	1
1	18	4	1	1	0	0	1	1	0	0	2	8
1	22	1	1	0	1	1	0	0	1	1	2	2
1	21	1	2	0	0	1	0	0	0	1	0	1
2	11	4	4	1	0	0	1	1	0	0	10	8
0	15	3	3	0	1	1	0	0	1	1	1	2
2	23	3	4	1	0	0	0	0	0	0	2	8
2	17	4	4	0	1	0	0	0	0	1	2	1
0	13	2	2	1	0	0	1	1	1	0	0	8
2	13	3	1	0	1	1	0	0	0	1	100	2
0	9	3	1	1	0	0	1	0	1	0	0	8
0	20	1	3	0	1	0	0	0	1	1	2	8
0	11	1	3	1	0	0	0	1	1	0	1	1
2	8	1	4	1	0	0	1	0	1	0	2	1
1	20	2	1	1	0	0	1	1	0	0	100	2
0	22	2	3	0	0	0	0	0	0	1	100	8
1	13	4	4	1	0	0	0	0	1	0	10	8
0	16	3	2	0	1	0	0	0	1	1	2	8
3	22	3	2	0	0	0	0	0	1	1	0	8
1	8	2	1	0	1	1	0	0	0	1	10	8
0	19	2	4	0	1	1	0	0	0	1	1	8
1	11	2	1	1	0	0	1	0	0	0	0	2
3	15	1	2	1	0	0	0	1	0	0	100	1
1	17	4	3	0	1	0	0	0	1	1	1	1
3	24	2	4	0	1	1	0	0	1	1	1	8
2	24	3	3	1	0	0	1	1	0	0	2	1
3	8	3	2	1	0	0	1	1	0	0	100	2
3	21	3	4	0	0	0	0	0	0	0	1	8
2	26	1	4	1	0	0	1	1	0	0	0	2
3	14	2	2	0	1	1	0	0	1	1	0	1
0	8	4	3	0	0	1	0	0	0	1	1	1
2	9	1	3	1	0	0	1	0	1	0	100	2
3	12	3	3	0	1	1	0	0	0	1	100	8
0	7	1	4	1	0	0	1	1	1	0	100	8
0	10	4	2	0	0	1	0	0	0	1	0	2
1	24	4	1	0	0	1	0	0	0	1	0	2
3	16	2	1	0	0	0	0	0	1	1	10	1
3	18	2	3	0	1	1	0	0	1	1	0	1
2	16	3	4	0	1	1	0	0	0	1	100	8
0	11	1	3	0	0	1	0	0	0	1	2	1
2	14	2	2	0	1	0	0	0	0	1	1	8
2	12	4	3	1	0	0	1	1	1	0	2	8
0	8	1	4	0	0	1	0	0	0	1	0	1
3	25	2	3	1	0	0	1	0	1	0	2	2
2	15	4	4	1	0	0	1	1	0	0	2	1
3	20	3	2	1	0	0	1	1	0	0	0	1
3	23	4	4	0	1	1	0	0	0	1	100	2
2	25	3	4	1	0	0	0	0	1	0	100	2
1	25	1	2	1	0	0	0	1	1	0	10	8
0	24	1	2	0	0	1	0	0	1	1	10	1
1	15	4	3	1	0	0	1	1	1	0	0	2
1	24	3	4	0	1	0	0	0	0	1	100	1
2	20	4	4	0	1	1	0	0	1	0	1	1
1	22	4	3	1	0	0	0	1	0	0	1	2
2	18	3	4	0	0	0	0	0	0	0	100	8
3	19	2	4	0	0	0	0	0	0	1	2	1
2	18	1	1	1	0	0	0	1	0	0	10	1
3	16	2	3	1	0	0	1	1	1	0	10	8
2	20	4	1	0	1	1	0	0	0	1	10	8
);
  std::istringstream tests(allPairsTests);
  // skip the initial line with the parameter names.
  {
    char const* params[] = {
      "spairQueue", "reducerType", "divLookup", "monTable",
      "buchberger", "postponeKoszul", "useBaseDivisors", "autoTailReduce",
      "autoTopReduce", "preferSparseReducers", "useSingularCriterionEarly",
      "sPairGroupSize", "threadCount"};

    std::string paramName;
    size_t const paramCount = sizeof(params) / sizeof(*params);
    for (size_t i = 0; i < paramCount; ++i) {
      tests >> paramName;
      // This assert will fire if you changed the order of the
      // parameters, renamed a parameter, removed a parameter or added
      // a parameter. Unless all you did was to rename a parameter,
      // don't just update the params array that the assert is based
      // on - you also need to update the code below that parses the
      // pict output because it depends on the order of the
      // parameters.
      MATHICGB_ASSERT(paramName == params[i]);
    }
  }

  while (true) {
    // parse a line of the pict file

    int spairQueue;
    tests >> spairQueue;
    if (!tests)
      break; // no more tests
    MATHICGB_ASSERT(0 <= spairQueue && spairQueue <= 3);

    int reducerType;
    tests >> reducerType;
    MATHICGB_ASSERT(0 <= reducerType && reducerType <= 30);

    int divLookup;
    tests >> divLookup;
    MATHICGB_ASSERT(1 <= divLookup && divLookup <= 4);

    int monTable;
    tests >> monTable;
    MATHICGB_ASSERT(1 <= monTable && monTable <= 4);
    
    int buchberger;
    tests >> buchberger;
    MATHICGB_ASSERT(0 <= buchberger && buchberger <= 1);

    int postponeKoszul;
    tests >> postponeKoszul;
    MATHICGB_ASSERT(0 <= postponeKoszul && postponeKoszul <= 1);

    int useBaseDivisors;
    tests >> useBaseDivisors;
    MATHICGB_ASSERT(0 <= useBaseDivisors && useBaseDivisors <= 1);

    int autoTailReduce;
    tests >> autoTailReduce;
    MATHICGB_ASSERT(0 <= autoTailReduce && autoTailReduce <= 1);

    int autoTopReduce;
    tests >> autoTopReduce;
    MATHICGB_ASSERT(0 <= autoTopReduce && autoTopReduce <= 1);

    int preferSparseReducers;
    tests >> preferSparseReducers;
    MATHICGB_ASSERT(0 <= preferSparseReducers && preferSparseReducers <= 1);

    int useSingularCriterionEarly;
    tests >> useSingularCriterionEarly;
    MATHICGB_ASSERT(0 <= useSingularCriterionEarly);
    MATHICGB_ASSERT(useSingularCriterionEarly <= 1);

    int sPairGroupSize;
    tests >> sPairGroupSize;
    MATHICGB_ASSERT(0 <= sPairGroupSize);

    int threadCount;
    tests >> threadCount;
    MATHICGB_ASSERT(0 <= threadCount);

    // Rule out combinations of parameter values that do not make sense.
    // These are asserts because pict should have already removed these
    // combinations.
    MATHICGB_ASSERT(buchberger || !autoTopReduce);
    MATHICGB_ASSERT(buchberger || !autoTailReduce);
    MATHICGB_ASSERT(buchberger || reducerType != 25);
    MATHICGB_ASSERT(buchberger || reducerType != 26);
    MATHICGB_ASSERT(!buchberger || !postponeKoszul);
    MATHICGB_ASSERT(!buchberger || !useBaseDivisors);
    MATHICGB_ASSERT(!buchberger || !useSingularCriterionEarly);

    // check that we have a valid reducer type
    Reducer::ReducerType red = Reducer::ReducerType(reducerType);
    MATHICGB_ASSERT(static_cast<int>(red) == reducerType);

    std::istringstream inStream(idealStr);

    Scanner in(inStream);
    auto p = MathicIO<>().readRing(true, in);
    auto& ring = *p.first;
    auto& processor = p.second;
    auto basis = MathicIO<>().readBasis(ring, false, in);
    if (processor.schreyering())
      processor.setSchreyerMultipliers(basis);

    MATHICGB_ASSERT(Reducer::makeReducerNullOnUnknown(red, ring).get() != 0);

    mgb::mtbb::task_scheduler_init scheduler(threadCount);
    if (buchberger) {
      const auto reducer = Reducer::makeReducer
        (Reducer::reducerType(reducerType), ring);

      ClassicGBAlgParams params;
      params.reducer = reducer.get();
      params.monoLookupType = divLookup;
      params.preferSparseReducers = preferSparseReducers;
      params.sPairQueueType = spairQueue;
      params.breakAfter = 0;
      params.printInterval = 0;
      params.sPairGroupSize = sPairGroupSize;
      params.reducerMemoryQuantum = 100 * 1024;
      params.useAutoTopReduction = autoTopReduce;
      params.useAutoTailReduction = autoTailReduce;
      params.callback = nullptr;

      auto gb = computeGBClassicAlg(std::move(basis), params);

      Basis initialIdeal(gb.ring());
      for (size_t i = 0; i < gb.size(); ++i) {
        auto poly = make_unique<Poly>(gb.ring());
        auto leadTerm = gb.getPoly(i)->leadTerm();
        leadTerm.coef = gb.ring().field().one();
        poly->append(leadTerm);
        initialIdeal.insert(std::move(poly));
      }
      initialIdeal.sort();
      EXPECT_EQ(initialIdealStr, toString(&initialIdeal))
        << reducerType << ' ' << divLookup << ' '
        << monTable << ' ' << postponeKoszul << ' ' << useBaseDivisors;
    } else {
      SignatureGB alg(
        std::move(basis),
        std::move(processor),
        Reducer::reducerType(reducerType),
        divLookup,
        monTable,
        postponeKoszul,
        useBaseDivisors,
        preferSparseReducers,
        useSingularCriterionEarly,
        spairQueue
      );
      alg.computeGrobnerBasis();
      EXPECT_EQ(sigBasisStr, toString(alg.getGB(), 1))
        << reducerType << ' ' << divLookup << ' '
        << monTable << ' ' << ' ' << postponeKoszul << ' '
        << useBaseDivisors;
      EXPECT_EQ(syzygiesStr, toString(alg.getSyzTable()))
        << reducerType << ' ' << divLookup << ' '
        << monTable << ' ' << ' ' << postponeKoszul << ' '
        << useBaseDivisors;
      /*EXPECT_EQ(nonSingularReductions, alg.getSigReductionCount() - alg.getSingularReductionCount())
        << reducerType << ' ' << divLookup << ' '
        << monTable << ' ' << ' ' << postponeKoszul << ' '
        << useBaseDivisors;*/
    }
  }
}

TEST(GB, small) {
  testGB(smallIdealComponentLastDescending(),
         idealSmallBasis, idealSmallSyzygies, idealSmallInitial, 7);
}

TEST(GB, liu_0_1) {
  testGB(liuIdealComponentLastDescending(), liu_gb_strat0_free1,
    liu_syzygies_strat0_free1, liu_initial_strat0_free1, 13);
}

TEST(GB, weispfennig97_0_4) {
  testGB(weispfennig97IdealComponentLast(true),
         weispfennig97_gb_strat0_free4,
         weispfennig97_syzygies_strat0_free4, weispfennig97_initial_strat0_free4, 31);
}

TEST(GB, weispfennig97_0_5) {
  testGB(weispfennig97IdealComponentLast(false),
         weispfennig97_gb_strat0_free5,
         weispfennig97_syzygies_strat0_free5, weispfennig97_initial_strat0_free5, 27);
}

TEST(GB, gerdt93_0_1) {
  testGB(gerdt93IdealComponentLast(false, false), gerdt93_gb_strat0_free1,
         gerdt93_syzygies_strat0_free1, gerdt93_initial_strat0_free1, 9);
}

TEST(GB, gerdt93_0_2) {
  testGB(gerdt93IdealComponentMiddle(true), gerdt93_gb_strat0_free2,
         gerdt93_syzygies_strat0_free2, gerdt93_initial_strat0_free2, 7);
}

TEST(GB, gerdt93_0_3) {
  testGB(gerdt93IdealComponentMiddle(false), gerdt93_gb_strat0_free3,
         gerdt93_syzygies_strat0_free3, gerdt93_initial_strat0_free3, 9);
}

TEST(GB, gerdt93_0_4) {
  testGB(gerdt93IdealComponentLast(true, true), gerdt93_gb_strat0_free4,
         gerdt93_syzygies_strat0_free4, gerdt93_initial_strat0_free4, 7);
}

TEST(GB, gerdt93_0_5) {
  testGB(gerdt93IdealComponentLast(false, true), gerdt93_gb_strat0_free5,
         gerdt93_syzygies_strat0_free5, gerdt93_initial_strat0_free5, 7);
}

TEST(GB, gerdt93_0_6) {
  testGB(gerdt93IdealComponentFirst(true), gerdt93_gb_strat0_free6,
         gerdt93_syzygies_strat0_free6, gerdt93_initial_strat0_free6, 7);
}

TEST(GB, gerdt93_0_7) {
  testGB(gerdt93IdealComponentFirst(false), gerdt93_gb_strat0_free7,
         gerdt93_syzygies_strat0_free7, gerdt93_initial_strat0_free7, 9);
}
