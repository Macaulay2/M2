// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "F4Reducer.hpp"

#include "F4MatrixBuilder.hpp"
#include "F4MatrixBuilder2.hpp"
#include "F4MatrixReducer.hpp"
#include "QuadMatrix.hpp"
#include "LogDomain.hpp"
#include "CFile.hpp"
#include <iostream>
#include <limits>

MATHICGB_DEFINE_LOG_DOMAIN(
  F4MatrixRows,
  "Count number of rows in F4 matrices."
);

MATHICGB_DEFINE_LOG_DOMAIN(
  F4MatrixTopRows,
  "Count number of top (reducer) rows in F4 matrices."
);

MATHICGB_DEFINE_LOG_DOMAIN(
  F4MatrixBottomRows,
  "Count number of bottom (reducee) rows in F4 matrices."
);

MATHICGB_DEFINE_LOG_DOMAIN(
  F4MatrixEntries,
  "Count number of non-zero entries in F4 matrices."
);

MATHICGB_DEFINE_LOG_ALIAS(
  "F4Detail",
  "F4MatrixEntries,F4MatrixBottomRows,F4MatrixTopRows,F4MatrixRows,"
  "F4MatrixReduce,F4"
);

MATHICGB_DEFINE_LOG_ALIAS(
  "F4",
  "F4MatrixSizes,F4MatrixBuild,F4MatrixBuild2,"
  "F4MatReduceTop,F4RedBottomRight"
);

#include "Reducer.hpp"
#include "PolyRing.hpp"
#include <string>

MATHICGB_NAMESPACE_BEGIN

void f4ReducerDependency() {}

class F4Reducer : public Reducer {
public:
  enum Type {
    OldType,
    NewType
  };

  F4Reducer(const PolyRing& ring, Type type);

  virtual unsigned int preferredSetSize() const;

  /// Store all future matrices to file-1.mat, file-2.mat and so on.
  /// Matrices with less than minEntries non-zero entries are not stored.
  /// If file is an empty string then no matrices are stored. If this method
  /// is never called then no matrices are stored.
  void writeMatricesTo(std::string file, size_t minEntries);

  virtual std::unique_ptr<Poly> classicReduce
    (const Poly& poly, const PolyBasis& basis);

  virtual std::unique_ptr<Poly> classicTailReduce
    (const Poly& poly, const PolyBasis& basis);

  virtual std::unique_ptr<Poly> classicReduceSPoly
    (const Poly& a, const Poly& b, const PolyBasis& basis);

  virtual void classicReduceSPolySet(
    std::vector<std::pair<size_t, size_t> >& spairs,
    const PolyBasis& basis,
    std::vector<std::unique_ptr<Poly> >& reducedOut
  );

  virtual void classicReducePolySet(
    const std::vector<std::unique_ptr<Poly> >& polys,
    const PolyBasis& basis,
    std::vector<std::unique_ptr<Poly> >& reducedOut
  );

  virtual std::unique_ptr<Poly> regularReduce(
    ConstMonoRef sig,
    ConstMonoRef multiple,
    size_t basisElement,
    const SigPolyBasis& basis
  );

  virtual void setMemoryQuantum(size_t quantum);

  virtual std::string description() const;
  virtual size_t getMemoryUse() const;

  const PolyRing& ring() const {return mRing;}
  const Monoid& monoid() const {return mRing.monoid();}

private:
  void saveMatrix(const QuadMatrix& matrix);

  Type mType;
  std::unique_ptr<Reducer> mFallback;
  const PolyRing& mRing;
  size_t mMemoryQuantum;
  std::string mStoreToFile; /// stem of file names to save matrices to
  size_t mMinEntryCountForStore; /// don't save matrices with fewer entries
  size_t mMatrixSaveCount; // how many matrices have been saved
};

F4Reducer::F4Reducer(const PolyRing& ring, Type type):
  mType(type),
  mFallback(Reducer::makeReducer(Reducer::Reducer_Geobucket_Hashed, ring)),
  mRing(ring),
  mMemoryQuantum(0),
  mStoreToFile(""),
  mMinEntryCountForStore(0),
  mMatrixSaveCount(0) {
}

unsigned int F4Reducer::preferredSetSize() const {
  return 100000;
}

void F4Reducer::writeMatricesTo(std::string file, size_t minEntries) {
  mStoreToFile = std::move(file);
  mMinEntryCountForStore = minEntries;
  mMatrixSaveCount = 0;
}

std::unique_ptr<Poly> F4Reducer::classicReduce
  (const Poly& poly, const PolyBasis& basis)
{
  if (tracingLevel >= 2)
    std::cerr <<
      "F4Reducer: Using fall-back reducer for single classic reduction\n";

  return mFallback->classicReduce(poly, basis);
}

std::unique_ptr<Poly> F4Reducer::classicTailReduce
  (const Poly& poly, const PolyBasis& basis)
{
  if (tracingLevel >= 2)
    std::cerr <<
      "F4Reducer: Using fall-back reducer for single classic tail reduction\n";

  return mFallback->classicTailReduce(poly, basis);
}

std::unique_ptr<Poly> F4Reducer::classicReduceSPoly(
  const Poly& a,
  const Poly& b,
  const PolyBasis& basis
) {
  if (tracingLevel >= 2)
    std::cerr << "F4Reducer: "
      "Using fall-back reducer for single classic S-pair reduction\n";
  return mFallback->classicReduceSPoly(a, b, basis);
}

void F4Reducer::classicReduceSPolySet(
  std::vector<std::pair<size_t, size_t>>& spairs,
  const PolyBasis& basis,
  std::vector<std::unique_ptr<Poly>>& reducedOut
) {
  if (spairs.size() <= 1) {
    if (tracingLevel >= 2)
      std::cerr << "F4Reducer: Using fall-back reducer for "
        << spairs.size() << " S-pairs.\n";
    mFallback->classicReduceSPolySet(spairs, basis, reducedOut);
    return;
  }
  reducedOut.clear();

  MATHICGB_ASSERT(!spairs.empty());
  if (tracingLevel >= 2)
    std::cerr << "F4Reducer: Reducing " << spairs.size() << " S-polynomials.\n";

  SparseMatrix reduced;
  QuadMatrix::Monomials monomials;
  {
    QuadMatrix qm(basis.ring());
    {
      if (mType == OldType) {
        F4MatrixBuilder builder(basis, mMemoryQuantum);
        for (const auto& spair : spairs)
          builder.addSPolynomialToMatrix
            (basis.poly(spair.first), basis.poly(spair.second));
        builder.buildMatrixAndClear(qm);
      } else {
        F4MatrixBuilder2 builder(basis, mMemoryQuantum);
        for (const auto& spair : spairs)
          builder.addSPolynomialToMatrix
            (basis.poly(spair.first), basis.poly(spair.second));
        builder.buildMatrixAndClear(qm);
      }
    }
    MATHICGB_LOG_INCREMENT_BY(F4MatrixRows, qm.rowCount());
    MATHICGB_LOG_INCREMENT_BY(F4MatrixTopRows, qm.topLeft.rowCount());
    MATHICGB_LOG_INCREMENT_BY(F4MatrixBottomRows, qm.bottomLeft.rowCount());
    MATHICGB_LOG_INCREMENT_BY(F4MatrixEntries, qm.entryCount());
    saveMatrix(qm);
    reduced = F4MatrixReducer(basis.ring().charac()).
      reducedRowEchelonFormBottomRight(qm);
    monomials = std::move(qm.rightColumnMonomials);
    for (auto& mono : qm.leftColumnMonomials)
      monoid().freeRaw(mono.castAwayConst());
  }

  for (SparseMatrix::RowIndex row = 0; row < reduced.rowCount(); ++row) {
    auto p = make_unique<Poly>(basis.ring());
    reduced.rowToPolynomial(row, monomials, *p);
    reducedOut.push_back(std::move(p));
  }
  for (auto& mono : monomials)
    monoid().freeRaw(mono.castAwayConst());
}

void F4Reducer::classicReducePolySet(
  const std::vector< std::unique_ptr<Poly> >& polys,
  const PolyBasis& basis,
  std::vector< std::unique_ptr<Poly> >& reducedOut
) {
  if (polys.size() <= 1) {
    if (tracingLevel >= 2)
      std::cerr << "F4Reducer: Using fall-back reducer for "
                << polys.size() << " polynomials.\n";
    mFallback->classicReducePolySet(polys, basis, reducedOut);
    return;
  }

  reducedOut.clear();

  MATHICGB_ASSERT(!polys.empty());
  if (tracingLevel >= 2)
    std::cerr << "F4Reducer: Reducing " << polys.size() << " polynomials.\n";

  SparseMatrix reduced;
  QuadMatrix::Monomials monomials;
  {
    QuadMatrix qm(ring());
    {
      if (mType == OldType) {
        F4MatrixBuilder builder(basis, mMemoryQuantum);
        for (const auto& poly : polys)
          builder.addPolynomialToMatrix(*poly);
        builder.buildMatrixAndClear(qm);
      } else {
        F4MatrixBuilder2 builder(basis, mMemoryQuantum);
        for (const auto& poly : polys)
          builder.addPolynomialToMatrix(*poly);
        builder.buildMatrixAndClear(qm);
      }
    }
    MATHICGB_LOG_INCREMENT_BY(F4MatrixRows, qm.rowCount());
    MATHICGB_LOG_INCREMENT_BY(F4MatrixTopRows, qm.topLeft.rowCount());
    MATHICGB_LOG_INCREMENT_BY(F4MatrixBottomRows, qm.bottomLeft.rowCount());
    MATHICGB_LOG_INCREMENT_BY(F4MatrixEntries, qm.entryCount());
    saveMatrix(qm);
    reduced = F4MatrixReducer(basis.ring().charac()).
      reducedRowEchelonFormBottomRight(qm);
    monomials = std::move(qm.rightColumnMonomials);
    for (auto& mono : qm.leftColumnMonomials)
      monoid().freeRaw(mono.castAwayConst());
  }

  if (tracingLevel >= 2 && false)
    std::cerr << "F4Reducer: Extracted " << reduced.rowCount()
              << " non-zero rows\n";

  for (SparseMatrix::RowIndex row = 0; row < reduced.rowCount(); ++row) {
    auto p = make_unique<Poly>(basis.ring());
    reduced.rowToPolynomial(row, monomials, *p);
    reducedOut.push_back(std::move(p));
  }
  for (auto& mono : monomials)
    monoid().freeRaw(mono.castAwayConst());
}

std::unique_ptr<Poly> F4Reducer::regularReduce(
  ConstMonoRef sig,
  ConstMonoRef multiple,
  size_t basisElement,
  const SigPolyBasis& basis
) {
  if (tracingLevel >= 2)
    std::cerr <<
      "F4Reducer: Using fall-back reducer for single regular reduction\n";
  auto p = mFallback->regularReduce(sig, multiple, basisElement, basis);
  return p;
}

void F4Reducer::setMemoryQuantum(size_t quantum) {
  mMemoryQuantum = quantum;
}

std::string F4Reducer::description() const {
  return "F4 reducer";
}

size_t F4Reducer::getMemoryUse() const {
  return 0; // @todo: implement
}

void F4Reducer::saveMatrix(const QuadMatrix& matrix) {
  if (mStoreToFile.empty())
    return;
  const auto entryCount = matrix.entryCount();
  if (mMinEntryCountForStore > entryCount)
    return;
  ++mMatrixSaveCount;
  std::ostringstream fileName;
  fileName << mStoreToFile << '-' << mMatrixSaveCount << ".qmat";
  if (tracingLevel > 2)
    std::cerr << "F4Reducer: Saving matrix to " << fileName.str() << '\n';

  CFile file(fileName.str(), "wb");
  matrix.write
    (static_cast<SparseMatrix::Scalar>(mRing.charac()), file.handle());
}

std::unique_ptr<Reducer> makeF4Reducer(
 const PolyRing& ring,
 bool oldType,
 std::string file,
 size_t minEntries
) {
  auto reducer = oldType ?
    make_unique<F4Reducer>(ring, F4Reducer::OldType) :
    make_unique<F4Reducer>(ring, F4Reducer::NewType);
  reducer->writeMatricesTo(file, minEntries);
  return std::move(reducer);
}

MATHICGB_REGISTER_REDUCER(
  "F4Old",
  Reducer_F4_Old,
  (make_unique<F4Reducer>(ring, F4Reducer::OldType))
);

MATHICGB_REGISTER_REDUCER(
  "F4New",
  Reducer_F4_New,
  (make_unique<F4Reducer>(ring, F4Reducer::NewType))
);

MATHICGB_NAMESPACE_END
