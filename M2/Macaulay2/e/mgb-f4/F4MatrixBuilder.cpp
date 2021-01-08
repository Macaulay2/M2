// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "F4MatrixBuilder.hpp"

#include "LogDomain.hpp"

MATHICGB_DEFINE_LOG_DOMAIN(
  F4MatrixBuild,
  "Displays statistics about F4 matrix construction."
);

MATHICGB_NAMESPACE_BEGIN

MATHICGB_NO_INLINE
auto F4MatrixBuilder::findOrCreateColumn(
  ConstMonoRef monoA,
  ConstMonoRef monoB,
  TaskFeeder& feeder
) -> std::pair<QuadMatrixBuilder::LeftRightColIndex, ConstMonoRef> {
  const auto col = ColReader(mMap).findProduct(monoA, monoB);
  if (col.first != 0)
    return std::make_pair(*col.first, *col.second);
  return createColumn(monoA, monoB, feeder);
}

MATHICGB_INLINE
auto F4MatrixBuilder::findOrCreateColumn(
  ConstMonoRef monoA,
  ConstMonoRef monoB,
  const ColReader& colMap,
  TaskFeeder& feeder
) -> std::pair<QuadMatrixBuilder::LeftRightColIndex, ConstMonoRef> {
  const auto col = colMap.findProduct(monoA, monoB);
  if (col.first == 0)
    return findOrCreateColumn(monoA, monoB, feeder);
  return std::make_pair(*col.first, *col.second);
}

MATHICGB_NO_INLINE
void F4MatrixBuilder::createTwoColumns(
  ConstMonoRef monoA1,
  ConstMonoRef monoA2,
  ConstMonoRef monoB,
  TaskFeeder& feeder
) {
  createColumn(monoA1, monoB, feeder);
  createColumn(monoA2, monoB, feeder);
}

F4MatrixBuilder::F4MatrixBuilder(
  const PolyBasis& basis,
  const size_t memoryQuantum
):
  mLeftColCount(0),
  mRightColCount(0),
  mTmp(basis.ring().monoid().alloc()),
  mBasis(basis),
  mMonomialsLeft(),
  mMonomialsRight(),
  mBuilder(basis.ring(), mMap, mMonomialsLeft, mMonomialsRight, memoryQuantum),
  mMap(basis.ring())
{
  // This assert to be _NO_ASSUME since otherwise the compiler will assume that
  // the error checking branch here cannot be taken and optimize it away.
  const Scalar maxScalar = std::numeric_limits<Scalar>::max();
  MATHICGB_ASSERT_NO_ASSUME(ring().charac() <= maxScalar);
  if (ring().charac() > maxScalar)
    mathic::reportInternalError("F4MatrixBuilder: too large characteristic.");
}

void F4MatrixBuilder::addSPolynomialToMatrix(
  const Poly& polyA,
  const Poly& polyB
) {
  MATHICGB_ASSERT(!polyA.isZero());
  MATHICGB_ASSERT(polyA.isMonic());
  MATHICGB_ASSERT(!polyB.isZero());
  MATHICGB_ASSERT(polyB.isMonic());

  RowTask task;
  task.addToTop = false;
  task.poly = &polyA;
  task.sPairPoly = &polyB;
  mTodo.push_back(task);
}

void F4MatrixBuilder::addPolynomialToMatrix(const Poly& poly) {
  if (poly.isZero())
    return;

  RowTask task = {};
  task.addToTop = false;
  task.poly = &poly;
  mTodo.push_back(task);
}

void F4MatrixBuilder::addPolynomialToMatrix
  (ConstMonoRef multiple, const Poly& poly)
{
  if (poly.isZero())
    return;

  auto desiredLead = monoid().alloc();
  monoid().multiply(poly.leadMono(), multiple, desiredLead);
  RowTask task = {};
  task.addToTop = false;
  task.poly = &poly;
  task.desiredLead = desiredLead.release();

  MATHICGB_ASSERT(task.sPairPoly == 0);
  mTodo.push_back(task);
}

void F4MatrixBuilder::buildMatrixAndClear(QuadMatrix& matrix) {
  MATHICGB_ASSERT(&matrix.ring() == &ring());
  MATHICGB_LOG_TIME(F4MatrixBuild) <<
    "\n***** Constructing matrix *****\n";

  if (mTodo.empty()) {
    matrix.clear();
    return;
  }

  // todo: prefer sparse/old reducers among the inputs.

  // Process pending rows until we are done. Note that the methods
  // we are calling here can add more pending items.

  struct ThreadData {
    QuadMatrixBuilder builder;
    MonoRef tmp1;
    MonoRef tmp2;
  };

  mgb::mtbb::enumerable_thread_specific<ThreadData> threadData([&](){  
    mgb::mtbb::mutex::scoped_lock guard(mCreateColumnLock);
    ThreadData data = {
      QuadMatrixBuilder(
        ring(),
        mMap,
        mMonomialsLeft,
        mMonomialsRight,
        mBuilder.memoryQuantum()
      ),
      *monoid().alloc().release(),
      *monoid().alloc().release()
    };
    return data;
  });

  mgb::mtbb::parallel_do(mTodo.begin(), mTodo.end(),
    [&](const RowTask& task, TaskFeeder& feeder)
  {
    auto& data = threadData.local();
    QuadMatrixBuilder& builder = data.builder;
    const Poly& poly = *task.poly;

    if (task.sPairPoly != 0) {
      MATHICGB_ASSERT(!task.addToTop);
      monoid().colons(
        poly.leadMono(),
        task.sPairPoly->leadMono(),
        data.tmp2,
        data.tmp1
      );
      appendRowBottom
        (poly, data.tmp1, *task.sPairPoly, data.tmp2, data.builder, feeder);
      return;
    }
    if (task.desiredLead == nullptr)
      monoid().setIdentity(data.tmp1);
    else
      monoid().divide(poly.leadMono(), *task.desiredLead, data.tmp1);
    if (task.addToTop)
      appendRowTop(data.tmp1, *task.poly, builder, feeder);
    else
      appendRowBottom
        (data.tmp1, false, poly.begin(), poly.end(), builder, feeder);
  });
  MATHICGB_ASSERT(!threadData.empty()); // as mTodo empty causes early return
  // Free the monomials from all the tasks
  const auto todoEnd = mTodo.end();
  for (auto& mono : mTodo)
    if (!mono.desiredLead.isNull())
      monoid().freeRaw(*mono.desiredLead.castAwayConst());
  mTodo.clear();

  auto& builder = threadData.begin()->builder;
  for (auto& data : threadData) {
    if (&data.builder != &builder)
      builder.takeRowsFrom(data.builder.buildMatrixAndClear());
    monoid().freeRaw(data.tmp1);
    monoid().freeRaw(data.tmp2);
  }
  matrix = builder.buildMatrixAndClear();
  threadData.clear();

  {
    ColReader reader(mMap);
    matrix.leftColumnMonomials.clear();
    matrix.rightColumnMonomials.clear();
    const auto end = reader.end();
    for (auto it = reader.begin(); it != end; ++it) {
      const auto p = *it;
      auto copy = monoid().alloc();
      monoid().copy(p.second, copy);
      auto& monos = p.first.left() ?
        matrix.leftColumnMonomials : matrix.rightColumnMonomials;
      const auto index = p.first.index();
      if (monos.size() <= index)
        monos.resize(index + 1);
      MATHICGB_ASSERT(monos[index].isNull());
      monos[index] = copy.release();
    }
  }
#ifdef MATHICGB_DEBUG
  for (size_t side = 0; side < 2; ++side) {
    auto& monos = side == 0 ?
      matrix.leftColumnMonomials : matrix.rightColumnMonomials;
    for (auto it = monos.begin(); it != monos.end(); ++it) {
      MATHICGB_ASSERT(!it->isNull());
    }
  }
#endif
  matrix.sortColumnsLeftRightParallel();
  mMap.clearNonConcurrent();
}

auto F4MatrixBuilder::createColumn(
  ConstMonoRef monoA,
  ConstMonoRef monoB,
  TaskFeeder& feeder
) -> std::pair<F4MatrixBuilder::LeftRightColIndex, ConstMonoRef> {
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
    mathic::reportError("Monomial exponent overflow in F4MatrixBuilder.");

  // look for a reducer of mTmp
  const size_t reducerIndex = mBasis.classicReducer(*mTmp);
  const bool insertLeft = (reducerIndex != static_cast<size_t>(-1));

  // Create the new left or right column
  auto& colCount = insertLeft ? mLeftColCount : mRightColCount;
  if (colCount == std::numeric_limits<ColIndex>::max())
    throw std::overflow_error("Too many columns in QuadMatrix");
  const auto inserted = mMap.insert
    (std::make_pair(mTmp.ptr(), LeftRightColIndex(colCount, insertLeft)));
  ++colCount;
  MATHICGB_ASSERT(inserted.second);
  MATHICGB_ASSERT(inserted.first.first != 0);

  // schedule new task if we found a reducer
  if (insertLeft) {
    RowTask task = {};
    task.addToTop = true;
    task.poly = &mBasis.poly(reducerIndex);
    task.desiredLead = inserted.first.second;
    feeder.add(task);
  }

  return std::make_pair(*inserted.first.first, *inserted.first.second);
}

void F4MatrixBuilder::appendRowBottom(
  ConstMonoRef multiple,
  const bool negate,
  const Poly::ConstTermIterator begin,
  const Poly::ConstTermIterator end,
  QuadMatrixBuilder& builder,
  TaskFeeder& feeder
) {
  // todo: eliminate the code-duplication between here and appendRowTop.
  MATHICGB_ASSERT(&builder != 0);

  auto it = begin;
updateReader:
  // Use an on-stack const reader to make it as obvious as possible to the
  // optimizer's alias analysis that the pointer inside the reader never
  // changes inside the loop.
  const ColReader reader(mMap);
  for (; it != end; ++it) {
    const auto col = reader.findProduct(it.mono(), multiple);
    if (col.first == 0) {
      createColumn(it.mono(), multiple, feeder);
      goto updateReader;
    }

    const auto origScalar = it.coef();
    MATHICGB_ASSERT(!ring().field().isZero(origScalar));
    const auto maybeNegated =
      negate ? field().negativeNonZero(origScalar) : origScalar;
	MATHICGB_ASSERT(maybeNegated < std::numeric_limits<Scalar>::max());
    builder.appendEntryBottom(*col.first, static_cast<Scalar>(maybeNegated));
  }
  builder.rowDoneBottomLeftAndRight();
}

void F4MatrixBuilder::appendRowTop(
  ConstMonoRef multiple,
  const Poly& poly,
  QuadMatrixBuilder& builder,
  TaskFeeder& feeder
) {
  MATHICGB_ASSERT(&poly != 0);
  MATHICGB_ASSERT(&builder != 0);

  auto it = poly.begin();
  const auto end = poly.end();
  if ((poly.termCount() % 2) == 1) {
    ColReader reader(mMap);
    const auto col = findOrCreateColumn
      (it.mono(), multiple, reader, feeder);
	MATHICGB_ASSERT(it.coef() < std::numeric_limits<Scalar>::max());
    MATHICGB_ASSERT(!field().isZero(it.coef()));
    builder.appendEntryTop
      (col.first, static_cast<Scalar>(it.coef()));
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
      createTwoColumns(mono1, mono2, multiple, feeder);
      goto updateReader;
    }

    builder.appendEntryTop(*colPair.first, scalar1);
    builder.appendEntryTop(*colPair.second, scalar2);
    it = ++it2;
  }
  builder.rowDoneTopLeftAndRight();
}

void F4MatrixBuilder::appendRowBottom(
  const Poly& poly,
  ConstMonoRef multiply,
  const Poly& sPairPoly,
  ConstMonoRef sPairMultiply,
  QuadMatrixBuilder& builder,
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

  const ColReader colMap(mMap);

  const auto mulA = multiply;
  const auto mulB = sPairMultiply;
  while (true) {
    // Watch out: we are depending on appendRowBottom to finish the row, so
    // if you decide not to call that function in case
    // (itA == itA && itB == endB) then you need to do that yourself.
    if (itB == endB) {
      appendRowBottom(mulA, false, itA, endA, builder, feeder);
      break;
    }
    if (itA == endA) {
      appendRowBottom(mulB, true, itB, endB, builder, feeder);
      break;
    }

    coefficient coeff = 0;
    LeftRightColIndex col;
    const auto colA = findOrCreateColumn
      (itA.mono(), mulA, colMap, feeder);
    const auto colB = findOrCreateColumn
      (itB.mono(), mulB, colMap, feeder);
    const auto cmp = monoid().compare(colA.second, colB.second);
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
    if (!field().isZero(coeff))
      builder.appendEntryBottom(col, static_cast<Scalar>(coeff));
  }
}

MATHICGB_NAMESPACE_END
