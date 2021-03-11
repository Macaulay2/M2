#include "mathic/PairQueue.h"
#include <gtest/gtest.h>

#include <string>
#include <sstream>
#include <set>

namespace {
  class PQConf {
  public:
	PQConf(int id): mId(id) {}
	int id() const {return mId;}

	typedef std::string PairData;
	void computePairData(size_t col, size_t row, std::string& str) {
	  str = pairString(col, row);
	}

	typedef bool CompareResult;
	bool compare(int colA, int rowA, const std::string& strA,
				 int colB, int rowB, const std::string& strB) const {
	  MATHIC_ASSERT(pairString(colA, rowA) == strA);
	  MATHIC_ASSERT(pairString(colB, rowB) == strB);
	  return strA > strB;
	}
	bool cmpLessThan(bool v) const {return v;}

	std::string pairString(size_t col, size_t row) const {
	  std::ostringstream out;
	  out << col << row << mId;
	  return out.str();
	}

  private:
	int mId;
  };
}

namespace mathic {
  namespace PairQueueNamespace {
	template<>
	struct SupportRetirement<PQConf> {
	  static bool const value = false;
	};
  }
}

TEST(PairQueue, RetirementSetToFalse) {
  ASSERT_FALSE(mathic::PairQueue<PQConf>::SupportRetirement);
  mathic::PairQueue<PQConf> pq(PQConf(421));
  mathic::PairQueue<PQConf>::Index* null = 0;
  pq.addColumnDescending(null, null);
  ASSERT_FALSE(pq.retired(0));
}

TEST(PairQueue, NoOp) {
  mathic::PairQueue<PQConf> pq(PQConf(421));
  ASSERT_EQ(421, pq.configuration().id());
};

TEST(PairQueue, emptyAndPairCountAndColumnCount) {
  mathic::PairQueue<PQConf> pq(PQConf(1));

  ASSERT_TRUE(pq.empty());
  ASSERT_EQ(0, pq.pairCount());
  ASSERT_EQ(0, pq.columnCount());
  size_t const columnCount = 7;
  mathic::PairQueue<PQConf>::Index rows[columnCount - 1] = {0,1,2,3,4,5};
  // add full columns
  for (size_t i = 0; i < columnCount; ++i) {
	// single-digit indicies are sorted just by their numerical value
	pq.addColumnDescending(rows, rows + i);
	ASSERT_EQ(i == 0, pq.empty());
	ASSERT_EQ((i * (i + 1)) / 2, pq.pairCount()) << "i = " << i;
	ASSERT_EQ(i + 1, pq.columnCount());
  }
  size_t pairCount = (columnCount * (columnCount - 1)) / 2;
  ASSERT_FALSE(pq.empty());
  ASSERT_EQ(pairCount, pq.pairCount());
  ASSERT_EQ(columnCount, pq.columnCount());

  // add empty column
  pq.addColumnDescending(rows, rows);
  ASSERT_FALSE(pq.empty());
  ASSERT_EQ(pairCount, pq.pairCount());
  ASSERT_EQ(columnCount + 1, pq.columnCount());

  // add a 2-element column
  pq.addColumnDescending(rows, rows + 2);
  pairCount += 2;
  ASSERT_FALSE(pq.empty());
  ASSERT_EQ(pairCount, pq.pairCount());
  ASSERT_EQ(columnCount + 2, pq.columnCount());

  for (size_t i = 0; i < pairCount; ++i) {
	ASSERT_EQ(pairCount - i, pq.pairCount());
	ASSERT_FALSE(pq.empty());
	pq.pop();
  }
  ASSERT_TRUE(pq.empty());
  ASSERT_EQ(0, pq.pairCount());
  ASSERT_EQ(columnCount + 2, pq.columnCount());

  pq.addColumnDescending(rows, rows); // empty column
  ASSERT_TRUE(pq.empty()); // still empty
}

// test that the queue orders the pairs correctly
TEST(PairQueue, Ordering) {
  mathic::PairQueue<PQConf> pq(PQConf(9));
  typedef mathic::PairQueue<PQConf>::Index Index;

  // Put some pairs in. Note that the pairdata string does not
  // distinguish all pairs and that, according to the pairdata,
  //   (11,0) < (11,10) = (111,0) < (11,5)
  // so the order that pairs are extracted mix up columns 11 and 111.

  for (size_t col = 0; col < 112; ++col) {
	Index const* begin = 0;
	Index const* end = 0;
	if (col == 1) {
	  Index const rows[] = {0};
	  begin = rows;
	  end = rows + sizeof(rows) / sizeof(rows[0]);
      pq.addColumnDescending(begin, end);
	} else if (col == 11) {
	  Index const rows[] = {0, 10, 5};
	  begin = rows;
	  end = rows + sizeof(rows) / sizeof(rows[0]);
      pq.addColumnDescending(begin, end);
	} else if (col == 13) {
	  Index const rows[] = {12, 3, 7};
	  begin = rows;
	  end = rows + sizeof(rows) / sizeof(rows[0]);
      pq.addColumnDescending(begin, end);
	} else if (col == 111) {
	  Index const rows[] = {0, 100};
	  begin = rows;
	  end = rows + sizeof(rows) / sizeof(rows[0]);
      pq.addColumnDescending(begin, end);
	} else
      pq.addColumnDescending(begin, end);
  }
  std::ostringstream out;
  std::string lastPd;
  while (!pq.empty()) {
	// Extract top pair and top pairdata and check that doing that in
	// either order works.
	std::pair<size_t, size_t> p;
	std::string pd;
	if ((pq.pairCount() % 2) == 0) {
	  pd = pq.topPairData();
	  p = pq.topPair();
	} else {
	  p = pq.topPair();
	  pd = pq.topPairData();
	}
	if (p.first == 11 && p.second == 5) {
	  // test adding a column after top() but before pop()
	  MATHIC_ASSERT(pq.columnCount() == 112);
	  Index const rows[] = {0, 111};
	  Index const* begin = rows;
	  Index const* end = rows + sizeof(rows) / sizeof(rows[0]);
	  pq.addColumnDescending(begin, end);
	  ASSERT_EQ((std::make_pair<size_t, size_t>(112, 0)), pq.topPair());
	  ASSERT_EQ("11209", pq.topPairData());
	  pq.pop();
	  ASSERT_EQ("1121119", pq.topPairData());
	  ASSERT_EQ((std::make_pair<size_t, size_t>(112, 111)), pq.topPair());
	  pq.pop();
	  // should be back at same place now
	  ASSERT_EQ(pd, pq.topPairData());
	  ASSERT_EQ(p, pq.topPair());

	  // test adding a column that only becomes the top later
	  while (pq.columnCount() < 200)
		pq.addColumnDescending(rows, rows);
	  pq.addColumnDescending(begin, end); 

	  // still at same place
	  ASSERT_EQ(pd, pq.topPairData());
	  ASSERT_EQ(p, pq.topPair());
	}


	//	ASSERT_TRUE(lastPd <= pd);
	ASSERT_EQ(pq.configuration().pairString(p.first, p.second), pd);
	out << ' ' << pd;
	pq.pop();
	lastPd = pd;
	// test adding a column that has already been 
	if (p.first == 11 && p.second == 5) {
	  Index rows[] = {0, 111};
	  Index const* begin = rows;
	  Index const* end = rows + sizeof(rows) / sizeof(rows[0]);
	}
  }
  ASSERT_EQ(" 109 1109 11109 11109 1111009 1159 13129 1339 1379 20009 2001119",
			out.str());
}

// test that the change to large indices works correctly
TEST(PairQueue, LargeIndices) {
  mathic::PairQueue<PQConf> pq(PQConf(7));
  typedef mathic::PairQueue<PQConf>::Index Index;
  Index const rows[] = {0, 100000}; // 100000 is more than fits in 16 bits
  MATHIC_ASSERT(rows[1] > 64000); // this test assumes that index has >=32 bits
  pq.addColumnDescending(rows, rows);
  pq.addColumnDescending(rows, rows + 1);
  for (size_t i = 2; i < 100100; ++i)
	pq.addColumnDescending(rows, rows);
  ASSERT_EQ(100100, pq.columnCount());
  pq.addColumnDescending(rows, rows + sizeof(rows) / sizeof(rows[0]));
  ASSERT_EQ((std::make_pair<size_t, size_t>(100100, 0)), pq.topPair());
  ASSERT_EQ("10010007", pq.topPairData());
  pq.pop();
  ASSERT_EQ((std::make_pair<size_t, size_t>(100100, 100000)), pq.topPair());
  ASSERT_EQ("1001001000007", pq.topPairData());
  pq.pop();
  ASSERT_EQ((std::make_pair<size_t, size_t>(1, 0)), pq.topPair());
  ASSERT_EQ("107", pq.topPairData());
  pq.pop();
}

namespace {
  // This configuration uses static variables to track construction
  // and deconstruction of PairData. Therefore this configuration
  // should only be used in one test so that tests can still run in
  // parallel.
  class PQConDeconCounterConf {
  public:
	// better not have two of these objects around at the same time!
	PQConDeconCounterConf(): owningQueue(0) {mLive.clear();}

	// This is the queue that this is the configuration of. You have
	// to set it after constructing the queue. It has to be void* as
	// we can't instantiate the type now before we specialize the
	// parameters like mathic::PairQueueNamespace::SupportRetirement.
	void* owningQueue;

	class PairData {
	  friend class PQConDeconCounterConf;
	private:
	  // only the configuration should call any method on PairData, in
	  // this case even including the constructor and destructor
	  // because we specialized
	  // mathic::PairQueueNamespace::constructPairData and
	  // mathic::PairQueueNamespace::destructPairData (see below).
	  PairData() {makeLive();}
	  ~PairData() {makeDead();}

	  bool live() const {
		return mLive.find(this) != mLive.end();
	  }

	  void makeLive() {
		// Could not do this directly in the constructor as gcc 4.5.3
		// would complain "error: returning a value from a
		// constructor", even though that should not be happening.

		// assert this not already live
		ASSERT_TRUE(mLive.insert(this).second);
	  }

	  void makeDead() {
		// Could not do this directly in the destructor as gcc 4.5.3
		// would complain "error: returning a value from a
		// destructor", even though that should not be happening.

		// assert this live
		ASSERT_EQ(1, mLive.erase(this));
	  }

	  size_t row;
	};

	void construct(void* memory) {
	  new (memory) PairData();
	}

	void destruct(PairData* pd) {
	  pd->~PairData();
	}

	void computePairData(size_t col, size_t row, PairData& pd);

	typedef bool CompareResult;
	bool compare(int colA, int rowA, const PairData& pdA,
				 int colB, int rowB, const PairData& pdB) const {
	  MATHIC_ASSERT(pdA.live());
	  MATHIC_ASSERT(pdB.live());
	  return pdA.row > pdB.row;
	}
	bool cmpLessThan(bool v) const {return v;}

    static size_t liveCount() {return mLive.size();}

  private:
	static std::set<PairData const*> mLive;
  };
  std::set<PQConDeconCounterConf::PairData const*> PQConDeconCounterConf::mLive;
}

namespace mathic {
  namespace PairQueueNamespace {
	template<>
	struct ConstructPairDataFunction<PQConDeconCounterConf> {
	  static void function
	  (void* memory, Index col, Index row, PQConDeconCounterConf& conf) {
		PQConDeconCounterConf::PairData* pd =
		  static_cast<PQConDeconCounterConf::PairData*>(memory);
		conf.construct(pd);
		conf.computePairData(col, row, *pd);
	  }
	};

	template<>
	struct DestructPairDataFunction<PQConDeconCounterConf> {
	  static void function
	  (PQConDeconCounterConf::PairData* pd,
	   Index col, Index row, PQConDeconCounterConf& conf) {
		conf.destruct(pd);
	  }
	};

	template<>
	struct SupportRetirement<PQConDeconCounterConf> {
	  static bool const value = true;
	};
  }
}

namespace {
  inline void PQConDeconCounterConf::computePairData
	(size_t col, size_t row, PairData& pd) {
	MATHIC_ASSERT(pd.live());
	MATHIC_ASSERT(!static_cast<mathic::PairQueue<PQConDeconCounterConf>*>
				  (owningQueue)->retired(col));
	MATHIC_ASSERT(!static_cast<mathic::PairQueue<PQConDeconCounterConf>*>
				  (owningQueue)->retired(row));
	pd.row = row;
  }
}

// check that all PairQueue properly constructs and deconstructs
// all PairData objects.
TEST(PairQueue, ConDeconOfPairData) {
  size_t const columnCount = 7;
  mathic::PairQueue<PQConDeconCounterConf>::Index
	rows[columnCount - 1] = {0,1,2,3,4,5};

  // check that PairData get cleaned up for a PairQueue that ends up empty.
  {
	mathic::PairQueue<PQConDeconCounterConf> pq((PQConDeconCounterConf()));
	pq.configuration().owningQueue = &pq;
	for (size_t i = 0; i < columnCount; ++i)
	  pq.addColumnDescending(rows, rows + i);
	pq.addColumnDescending(rows, rows);
	pq.addColumnDescending(rows, rows + 2);
	while (!pq.empty()) {
	  pq.topPairData(); // just to cause more constuctions/deconstructions
	  pq.pop();
	}
  }
  ASSERT_EQ(0, PQConDeconCounterConf::liveCount());

  // check that PairData get cleaned up for a PairQueue that has not
  // been pop'ed at all.
  {
	mathic::PairQueue<PQConDeconCounterConf> pq((PQConDeconCounterConf()));
	pq.configuration().owningQueue = &pq;
	for (size_t i = 0; i < columnCount; ++i)
	  pq.addColumnDescending(rows, rows + i);
	pq.addColumnDescending(rows, rows);
	pq.addColumnDescending(rows, rows + 2);
  }
  ASSERT_EQ(0, PQConDeconCounterConf::liveCount());

  // check that PairData get cleaned up for a PairQueue that has been
  // pop'ed but is not full.
  {
	mathic::PairQueue<PQConDeconCounterConf> pq((PQConDeconCounterConf()));
	pq.configuration().owningQueue = &pq;
	for (size_t i = 0; i < columnCount; ++i)
	  pq.addColumnDescending(rows, rows + i);
	pq.addColumnDescending(rows, rows);
	pq.addColumnDescending(rows, rows + 2);
	for (size_t i = 0; i < 3u; ++i)
	  pq.pop();
  }
  ASSERT_EQ(0, PQConDeconCounterConf::liveCount());
}

TEST(PairQueue, RetirementSetToTrue) {
  // This test gives an undesigned symbol in the linker now
  // ASSERT_TRUE(mathic::PairQueue<PQConDeconCounterConf>::SupportRetirement);
  mathic::PairQueue<PQConDeconCounterConf> pq((PQConDeconCounterConf()));
  pq.configuration().owningQueue = &pq;
  mathic::PairQueue<PQConf>::Index* null = 0;
  pq.addColumnDescending(null, null);
  ASSERT_TRUE(pq.empty());
  ASSERT_FALSE(pq.retired(0));
  pq.retireIndex(0);
  ASSERT_TRUE(pq.retired(0));
  ASSERT_TRUE(pq.empty());
}

TEST(PairQueue, Retirement) {
  typedef mathic::PairQueue<PQConDeconCounterConf>::Index Index;
  mathic::PairQueue<PQConDeconCounterConf> pq((PQConDeconCounterConf()));
  pq.configuration().owningQueue = &pq;
  Index const* null = 0;
  pq.addColumnDescending(null, null);

  {
	Index const rows[] = {0};
	pq.addColumnDescending(rows, rows + sizeof(rows) / sizeof(*rows));
  }
  {std::pair<size_t, size_t> p(1, 0); ASSERT_EQ(p, pq.topPair());}
  
  // retire higher component of the top pair
  pq.retireIndex(1);
  ASSERT_TRUE(pq.empty());
  
  {
	Index const rows[] = {0};
	pq.addColumnDescending(rows, rows + sizeof(rows) / sizeof(*rows));
  }
  {std::pair<size_t, size_t> p(2, 0); ASSERT_EQ(p, pq.topPair());}
  {
	Index const rows[] = {0,2};
	pq.addColumnDescending(rows, rows + sizeof(rows) / sizeof(*rows));
  }
  ASSERT_EQ(0, pq.topPair().second);
  {
	Index const rows[] = {0,2,3};
	pq.addColumnDescending(rows, rows + sizeof(rows) / sizeof(*rows));
  }
  ASSERT_EQ(0, pq.topPair().second);

  // retire lower component of top pair
  pq.retireIndex(0);
  ASSERT_FALSE(pq.empty());
  ASSERT_EQ(2, pq.topPair().second);

  // retire component not involved in top pair
  if (pq.topPair().first == 3) {
	pq.retireIndex(4);
	{std::pair<size_t, size_t> p(3, 2); ASSERT_EQ(p, pq.topPair());}
  } else {
	pq.retireIndex(3);
	{std::pair<size_t, size_t> p(4, 2); ASSERT_EQ(p, pq.topPair());}
  }
  ASSERT_FALSE(pq.empty());
  pq.pop();
  ASSERT_TRUE(pq.empty());
}
