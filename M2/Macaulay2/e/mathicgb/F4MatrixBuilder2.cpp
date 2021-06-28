// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "F4MatrixBuilder2.hpp"

#include "LogDomain.hpp"
#include "F4MatrixProjection.hpp"

MATHICGB_DEFINE_LOG_DOMAIN(
  F4MatrixBuild2,
  "Displays statistics about F4 matrix construction."
);

MATHICGB_DEFINE_LOG_DOMAIN(
  F4MatrixSizes,
  "Displays row and column count for each F4 matrix construction."
);

MATHICGB_NAMESPACE_BEGIN

class F4MatrixBuilder2::Builder {
public:
  typedef PolyRing::Field Field;

  typedef PolyRing::Monoid Monoid;
  typedef Monoid::Mono Mono;
  typedef Monoid::MonoRef MonoRef;
  typedef Monoid::ConstMonoRef ConstMonoRef;
  typedef Monoid::MonoPtr MonoPtr;
  typedef Monoid::ConstMonoPtr ConstMonoPtr;

  typedef SparseMatrix::ColIndex ColIndex;
  typedef SparseMatrix::Scalar Scalar;
  typedef MonomialMap<ColIndex> Map;
  typedef SparseMatrix::RowIndex RowIndex;

  /// Initializes the set of add-this-row tasks.
  void initializeRowsToReduce(std::vector<RowTask>& tasks) {
    // If aF-bG is an S-pair that is added as a bottom row in the matrix, and
    // we do not already have a reducer for the common leading term of aF and
    // bG, then we can instead set bG as a top/reducer/pivot row and put aF
    // as a bottom row. This way, we do not need to add a reducer for that
    // column if that monomial turns out to be used. We can also use the
    // faster code path for adding a single multiple like aF to the matrix
    // instead of the more complicated path that adds the difference between
    // two multiples aF-bG.
    //
    // If bG is already the top/reducer/pivot row for its leading monomial,
    // then we can simply replace a bottom row aF-bG with aF directly, since
    // the first computation that will happen will be to calculate aF-bG.
    // This way we get to use the much faster code in the matrix reducer for
    // making that reduction/Gaussian-elimination step. More importantly, we
    // have 1 row less to construct.
    //
    // The above two cases turn rows specified like aF-bG into rows specified
    // more simply as just aF. Note that we can interchange aF and bG in these
    // arguments with no problems.
    // 
    // These two cases will apply to all
    // S-pairs with a given leading monomial if and only if there is a single
    // bG that appears in all of those S-pairs. The absolute best case is
    // if that single common bG is at the same time the most preferable
    // (sparsest or oldest) reducer for that monomial. It is not trivial to
    // realize this (in fact I should write a paper it), but the particular
    // way that S-pair elimination works in MathicGB ensures that this best
    // case is always the case. So the code below will get rid of all the
    // S-pairs aF-bG and replace them with single bottom rows aF.
    // However, if the S-pair elimination code is ever changed, that may
    // no longer be the case, so this code is still supposed to work even
    // if some S-pairs are not gotten rid of. Still, there is an assert
    // to flag it if the S-pair elimination is accidentally broken so that it
    // no longer gives this guarantee.


    // Bring S-pairs with the same leading monomial together by ordering
    // them according in increasing order of monomial. Put the non-S-pairs
    // together at the front.
    auto cmp = [&](const RowTask& a, const RowTask& b) {
      if (a.sPairPoly == nullptr)
        return b.sPairPoly != nullptr;
      else
        return monoid().lessThan(*a.desiredLead, *b.desiredLead);
    };
    mgb::mtbb::parallel_sort(tasks.begin(), tasks.end(), cmp);

    const auto taskCount = tasks.size();
    for (size_t i = 0; i < taskCount;) {
      if (tasks[i].sPairPoly == 0) {
        ++i;
        continue;
      }
      MATHICGB_ASSERT(tasks[i].desiredLead != nullptr);
      MATHICGB_ASSERT(ColReader(mMap).find(*tasks[i].desiredLead).first == 0);

      // Create column for the lead term that cancels in the S-pair
      if (mIsColumnToLeft.size() >= std::numeric_limits<ColIndex>::max())
        throw std::overflow_error("Too many columns in matrix.");
      const auto newIndex = static_cast<ColIndex>(mIsColumnToLeft.size());
      const auto inserted =
        mMap.insert(std::make_pair(tasks[i].desiredLead, newIndex));
      mIsColumnToLeft.push_back(true);
      const auto& mono = inserted.first.second;

      // Schedule the two parts of the S-pair as separate rows. This adds a row
      // while creating the column in the hash table without adding a reducer
      // removes a row, effectively making this operation equivalent to taking
      // half of the S-pair and using it as a reducer.
      auto desiredLead = monoid().alloc();
      monoid().copy(*mono, *desiredLead);
      RowTask newTask = {desiredLead.release(), tasks[i].sPairPoly, nullptr};

      // Now we can strip off any part of an S-pair with the same cancelling lead
      // term that equals a or b since those rows are in the matrix.
      const auto* const a = tasks[i].poly;
      const auto* const b = tasks[i].sPairPoly;

      tasks[i].sPairPoly = 0;

      tasks.push_back(newTask);
      for (++i; i < taskCount; ++i) {
        auto& task = tasks[i];
        if (tasks[i].sPairPoly == 0)
          continue;
        MATHICGB_ASSERT(tasks[i].desiredLead != nullptr);
        if (!monoid().equal(*tasks[i].desiredLead, *mono))
          break;
        if (tasks[i].poly == a || tasks[i].poly == b) {
          tasks[i].poly = tasks[i].sPairPoly;
          tasks[i].sPairPoly = 0;
        } else if (tasks[i].sPairPoly == a || tasks[i].sPairPoly == b) {
          tasks[i].sPairPoly = 0;
        } else
          MATHICGB_ASSERT(false);
      }
    }
  }

  void buildMatrixAndClear(std::vector<RowTask>& tasks, QuadMatrix& quadMatrix) {
    MATHICGB_ASSERT(&quadMatrix.ring() == &ring());
    MATHICGB_LOG_TIME(F4MatrixBuild2) <<
      "\n***** Constructing matrix *****\n";

    if (tasks.empty()) {
      quadMatrix.clear();
      return;
    }

    initializeRowsToReduce(tasks);

    // The MonoRef's cannot be Mono's since enumerable_thread_specific
    // apparently requires the stored data type to be copyable and
    // Mono is not copyable.
    struct ThreadData {
      MonoRef tmp1;
      MonoRef tmp2;
      F4ProtoMatrix block;
    };

    mgb::mtbb::enumerable_thread_specific<ThreadData> threadData([&](){  
      // We need to grab a lock since monoid isn't internally synchronized.
      mgb::mtbb::mutex::scoped_lock guard(mCreateColumnLock);
      ThreadData data = {
        *monoid().alloc().release(),
        *monoid().alloc().release()
      };
      return data;
    });

    // Construct the matrix as pre-blocks
    mgb::mtbb::parallel_do(tasks.begin(), tasks.end(),
      [&](const RowTask& task, TaskFeeder& feeder)
    {
      auto& data = threadData.local();
      const auto& poly = *task.poly;

      // It is perfectly permissible for task.sPairPoly to be non-null. The
      // assert is there because of an interaction between S-pair
      // elimination/choice and the use of halves of S-pairs as reducers. The
      // current effect of these is that *all* S-pairs have a component split
      // off so that sPairPoly is always null (this is non-trivial to
      // realize). So if this assert goes off, you've messed that interaction
      // up somehow or you are using this class in some new way. So you can
      // remove the assert if necessary.
      MATHICGB_ASSERT(task.sPairPoly == 0);

      if (task.sPairPoly != 0) {
        monoid().colons(
          poly.leadMono(),
          task.sPairPoly->leadMono(),
          data.tmp2,
          data.tmp1
        );
        appendRowSPair
          (poly, data.tmp1, *task.sPairPoly, data.tmp2, data.block, feeder);
        return;
      }
      if (task.desiredLead == nullptr)
        monoid().setIdentity(data.tmp1);
      else
        monoid().divide(poly.leadMono(), *task.desiredLead, data.tmp1);
      appendRow(data.tmp1, *task.poly, data.block, feeder);
    });
    MATHICGB_ASSERT(!threadData.empty()); // as tasks empty causes early return

    // Free the monomials from all the tasks
    for (const auto& task : tasks)
      if (task.desiredLead != nullptr)
        monoid().freeRaw(*task.desiredLead.castAwayConst());
    tasks.clear();

    // Move the proto-matrices across all threads into the projection.
    F4MatrixProjection projection
      (ring(), static_cast<ColIndex>(mMap.entryCount()));
    const auto end = threadData.end();
    for (auto& data : threadData) {
      monoid().freeRaw(data.tmp1);
      monoid().freeRaw(data.tmp2);
      projection.addProtoMatrix(std::move(data.block));
    }

    // Sort columns by monomial and tell the projection of the resulting order
    MonomialMap<ColIndex>::Reader reader(mMap);
    typedef std::pair<ColIndex, ConstMonoPtr> IndexMono;
    auto toPtr = [](std::pair<ColIndex, ConstMonoRef> p) {
      return std::make_pair(p.first, &p.second);
    };
    std::vector<IndexMono> columns;
    std::transform
      (reader.begin(), reader.end(), std::back_inserter(columns), toPtr);

    const auto cmp = [&](const IndexMono& a, const IndexMono b) {
      return monoid().lessThan(*b.second, *a.second);
    };
    mgb::mtbb::parallel_sort(columns.begin(), columns.end(), cmp);

    const auto colEnd = columns.end();
    for (auto it = columns.begin(); it != colEnd; ++it) {
      const auto p = *it;
      projection.addColumn(p.first, *p.second, mIsColumnToLeft[p.first]);
    }

    quadMatrix = projection.makeAndClear(mMemoryQuantum);

    MATHICGB_LOG(F4MatrixSizes) 
      << "F4[" 
      << mathic::ColumnPrinter::commafy(quadMatrix.rowCount())
      << " by "
      << mathic::ColumnPrinter::commafy(
           quadMatrix.computeLeftColCount() + quadMatrix.computeRightColCount())
      << "]" << std::endl;

#ifdef MATHICGB_DEBUG
    for (size_t side = 0; side < 2; ++side) {
      auto& monos = side == 0 ?
        quadMatrix.leftColumnMonomials : quadMatrix.rightColumnMonomials;
      for (auto it = monos.begin(); it != monos.end(); ++it) {
        MATHICGB_ASSERT(!it->isNull());
      }
    }
    for (RowIndex row = 0; row < quadMatrix.topLeft.rowCount(); ++row) {
      MATHICGB_ASSERT(quadMatrix.topLeft.entryCountInRow(row) > 0);
      MATHICGB_ASSERT(quadMatrix.topLeft.leadCol(row) == row);
    }
    MATHICGB_ASSERT(quadMatrix.debugAssertValid());
#endif
  }

  const PolyRing& ring() const {return mBasis.ring();}
  const Monoid& monoid() const {return ring().monoid();}
  const Field& field() const {return ring().field();}

  Builder(const PolyBasis& basis, const size_t memoryQuantum):
    mMemoryQuantum(memoryQuantum),
    mTmp(basis.ring().monoid().alloc()),
    mMap(basis.ring()),
    mBasis(basis)
  {
    // This assert has to be _NO_ASSUME since otherwise the compiler will
    // assume that the error checking branch here cannot be taken and optimize
    // it away.
    const Scalar maxScalar = std::numeric_limits<Scalar>::max();
    MATHICGB_ASSERT_NO_ASSUME(ring().charac() <= maxScalar);
    if (ring().charac() > maxScalar)
      mathic::reportInternalError("F4MatrixBuilder2: too large characteristic.");
  }

  typedef const Map::Reader ColReader;
  typedef std::vector<monomial> Monomials;

  typedef mgb::mtbb::parallel_do_feeder<RowTask> TaskFeeder;

  /// Creates a column with monomial label monoA * monoB and schedules a new
  /// row to reduce that column if possible. If such a column already
  /// exists, then a new column is not inserted. In either case, returns
  /// the column index and column monomial corresponding to monoA * monoB.
  ///
  /// createColumn can be used simply to search for an existing column, but
  /// since createColumn incurs locking overhead, this is not a good idea.
  /// Note that createColumn has to work correctly for pre-existing columns
  /// because the only way to be *certain* that no other thread has inserted
  /// the column of interest is to grab a lock, and the lock being grabbed
  /// is being grabbed inside createColumn.
  MATHICGB_NO_INLINE
  std::pair<ColIndex, ConstMonoRef> createColumn(
    ConstMonoRef monoA,
    ConstMonoRef monoB,
    TaskFeeder& feeder
  ) {
    mgb::mtbb::mutex::scoped_lock lock(mCreateColumnLock);
    // see if the column exists now after we have synchronized
    {
      const auto found(ColReader(mMap).findProduct(monoA, monoB));
      if (found.first != 0)
        return std::make_pair(*found.first, *found.second);
    }

    // The column really does not exist, so we need to create it
    monoid().multiply(monoA, monoB, mTmp);
    if (!monoid().hasAmpleCapacity(*mTmp))
      mathic::reportError("Monomial exponent overflow in F4MatrixBuilder2.");

    // look for a reducer of mTmp
    const size_t reducerIndex = mBasis.classicReducer(*mTmp);
    const bool insertLeft = (reducerIndex != static_cast<size_t>(-1));

    // Create the new left or right column
    if (mIsColumnToLeft.size() >= std::numeric_limits<ColIndex>::max())
      throw std::overflow_error("Too many columns in QuadMatrix");
    const auto newIndex = static_cast<ColIndex>(mIsColumnToLeft.size());
    const auto inserted = mMap.insert(std::make_pair(mTmp.ptr(), newIndex));
    mIsColumnToLeft.push_back(insertLeft);

    // schedule new task if we found a reducer
    if (insertLeft) {
      RowTask task = {};
      task.poly = &mBasis.poly(reducerIndex);
      task.desiredLead = inserted.first.second;
      feeder.add(task);
    }

    return std::make_pair(*inserted.first.first, *inserted.first.second);
  }


  /// Append multiple * poly to block, creating new columns as necessary.
  void appendRow(
    ConstMonoRef multiple,
    const Poly& poly,
    F4ProtoMatrix& block,
    TaskFeeder& feeder
  ) {
    const auto begin = poly.begin();
    const auto end = poly.end();
    const auto count = poly.termCount();
    MATHICGB_ASSERT(count < std::numeric_limits<ColIndex>::max());
    auto indices = block.makeRowWithTheseScalars(poly);

    auto it = begin;
    if ((count % 2) == 1) {
      ColReader reader(mMap);
      const auto col = findOrCreateColumn
        (it.mono(), multiple, reader, feeder);
	  MATHICGB_ASSERT(it.coef() < std::numeric_limits<Scalar>::max());
      MATHICGB_ASSERT(!field().isZero(it.coef()));
      *indices = col.first;
      ++indices;
      ++it;
    }
  updateReader:
    ColReader colMap(mMap);
    while (it != end) {
	  MATHICGB_ASSERT(it.coef() < std::numeric_limits<Scalar>::max());
      MATHICGB_ASSERT(!field().isZero(it.coef()));
      const auto scalar1 = static_cast<Scalar>(it.coef());
      const auto mono1 = it.mono();

      auto it2 = it;
      ++it2;
	  MATHICGB_ASSERT(it2.coef() < std::numeric_limits<Scalar>::max());
      MATHICGB_ASSERT(!field().isZero(it2.coef()));
      const auto scalar2 = static_cast<Scalar>(it2.coef());
      const auto mono2 = it2.mono();

      const auto colPair = colMap.findTwoProducts(mono1, mono2, multiple);
      if (colPair.first == 0 || colPair.second == 0) {
        createColumn(mono1, multiple, feeder);
        createColumn(mono2, multiple, feeder);
        goto updateReader;
      }

      *indices = *colPair.first;
      ++indices;
      *indices = *colPair.second;
      ++indices;

      it = ++it2;
    }
  }

  /// Append poly*multiply - sPairPoly*sPairMultiply to block, creating new
  /// columns as necessary.
  void appendRowSPair(
    const Poly& poly,
    ConstMonoRef multiply,
    const Poly& sPairPoly,
    ConstMonoRef sPairMultiply,
    F4ProtoMatrix& block,
    TaskFeeder& feeder
  ) {
    MATHICGB_ASSERT(!poly.isZero());
    auto itA = poly.begin();
    const auto endA = poly.end();

    MATHICGB_ASSERT(!sPairPoly.isZero());
    auto itB = sPairPoly.begin();
    const auto endB = sPairPoly.end();

    // skip leading terms since they cancel
    MATHICGB_ASSERT(itA.coef() == itB.coef());
    ++itA;
    ++itB;

    // @todo: handle overflow of termCount addition here
    MATHICGB_ASSERT(poly.termCount() + sPairPoly.termCount() - 2 <=
      std::numeric_limits<ColIndex>::max());
    const auto maxCols =
      static_cast<ColIndex>(poly.termCount() + sPairPoly.termCount() - 2);
    auto row = block.makeRow(maxCols);
    const auto indicesBegin = row.first;

    const ColReader colMap(mMap);

    auto mulA = multiply;
    auto mulB = sPairMultiply;
    while (itB != endB && itA != endA) {
      const auto colA = findOrCreateColumn
        (itA.mono(), mulA, colMap, feeder);
      const auto colB = findOrCreateColumn
        (itB.mono(), mulB, colMap, feeder);
      const auto cmp = monoid().compare(colA.second, colB.second);

      coefficient coeff = 0;
      ColIndex col;
      if (cmp != LT) {
        coeff = itA.coef();
        col = colA.first;
        ++itA;
      }
      if (cmp != GT) {
        coeff = ring().coefficientSubtract(coeff, itB.coef());
        col = colB.first;
        ++itB;
      }
      MATHICGB_ASSERT(coeff < std::numeric_limits<Scalar>::max());
      if (coeff != 0) {
        *row.first++ = col;
        *row.second++ = static_cast<Scalar>(coeff);
      }
    }

    for (; itA != endA; ++itA) {
      const auto colA = findOrCreateColumn
        (itA.mono(), mulA, colMap, feeder);
      *row.first++ = colA.first;
      *row.second++ = static_cast<Scalar>(itA.coef());
    }

    for (; itB != endB; ++itB) {
      const auto colB = findOrCreateColumn
        (itB.mono(), mulB, colMap, feeder);
      const auto negative = ring().coefficientNegate(itB.coef());
      *row.first = colB.first;
      ++row.first;
      *row.second = static_cast<Scalar>(negative);
      ++row.second;
    }

    const auto toRemove =
      maxCols - static_cast<ColIndex>(row.first - indicesBegin);
    block.removeLastEntries(block.rowCount() - 1, toRemove);
  }

  /// As createColumn, except with much better performance in the common
  /// case that the column for monoA * monoB already exists. In particular,
  /// no lock is grabbed in that case.
  MATHICGB_NO_INLINE
  std::pair<ColIndex, ConstMonoRef> findOrCreateColumn(
    ConstMonoRef monoA,
    ConstMonoRef monoB,
    TaskFeeder& feeder
  ) {
    const auto col = ColReader(mMap).findProduct(monoA, monoB);
    if (col.first != 0)
      return std::make_pair(*col.first, *col.second);
    return createColumn(monoA, monoB, feeder);
  }

  /// As the overload that does not take a ColReader parameter, except with
  /// better performance in the common case that the column already exists
  /// and colMap is up-to-date.
  MATHICGB_INLINE
  std::pair<ColIndex, ConstMonoRef> findOrCreateColumn(
    ConstMonoRef monoA,
    ConstMonoRef monoB,
    const ColReader& colMap,
    TaskFeeder& feeder
  ) {
    const auto col = colMap.findProduct(monoA, monoB);
    if (col.first == 0) {
      // The reader may be out of date, so try again with a fresh reader.
      return findOrCreateColumn(monoA, monoB, feeder);
    }
    return std::make_pair(*col.first, *col.second);
  }

  /// The split into left and right columns is not done until the whole matrix
  /// has been constructed. This vector keeps track of which side each column
  /// should go to once we do the split. char is used in place of bool because
  /// the specialized bool would just be slower for this use case. See
  /// http://isocpp.org/blog/2012/11/on-vectorbool .
  std::vector<char> mIsColumnToLeft;

  /// How much memory to allocate every time more memory is needed.
  const size_t mMemoryQuantum;

  /// If you want to modify the columns, you need to grab this lock first.
  mgb::mtbb::mutex mCreateColumnLock;

  /// A monomial for temporary scratch calculations. Protected by
  /// mCreateColumnLock.
  Mono mTmp;

  /// Mapping from monomials to column indices.
  Map mMap;

  /// The basis that supplies reducers.
  const PolyBasis& mBasis;
};

F4MatrixBuilder2::F4MatrixBuilder2(
  const PolyBasis& basis,
  const size_t memoryQuantum
):
  mMemoryQuantum(memoryQuantum),
  mBasis(basis)
{}

void F4MatrixBuilder2::addSPolynomialToMatrix(
  const Poly& polyA,
  const Poly& polyB
) {
  MATHICGB_ASSERT(!polyA.isZero());
  MATHICGB_ASSERT(polyA.isMonic());
  MATHICGB_ASSERT(!polyB.isZero());
  MATHICGB_ASSERT(polyB.isMonic());

  auto desiredLead = monoid().alloc();
  monoid().lcm(polyA.leadMono(), polyB.leadMono(), *desiredLead);
  RowTask task = {desiredLead.release(), &polyA, &polyB};
  mTodo.push_back(task);
}

void F4MatrixBuilder2::addPolynomialToMatrix(const Poly& poly) {
  if (poly.isZero())
    return;

  RowTask task = {};
  task.poly = &poly;
  mTodo.push_back(task);
}

void F4MatrixBuilder2::addPolynomialToMatrix(
  ConstMonoRef multiple,
  const Poly& poly
) {
  if (poly.isZero())
    return;

  auto desiredLead = monoid().alloc();
  monoid().multiply(poly.leadMono(), multiple, desiredLead);
  RowTask task = {desiredLead.release(), &poly, nullptr};
  mTodo.push_back(task);
}

void F4MatrixBuilder2::buildMatrixAndClear(QuadMatrix& quadMatrix) {
  Builder builder(mBasis, mMemoryQuantum);
  builder.buildMatrixAndClear(mTodo, quadMatrix);
}

MATHICGB_NAMESPACE_END
