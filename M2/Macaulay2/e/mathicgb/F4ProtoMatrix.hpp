// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_F4_PROTO_MATRIX_GUARD
#define MATHICGB_F4_PROTO_MATRIX_GUARD

#include "PolyRing.hpp"
#include "SparseMatrix.hpp"
#include "Poly.hpp"

MATHICGB_NAMESPACE_BEGIN

class F4ProtoMatrix {
public:
  typedef uint32 RowIndex;
  typedef uint32 ColIndex;
  typedef SparseMatrix::Scalar Scalar;
  typedef coefficient ExternalScalar;
  typedef Poly::ConstCoefIterator ExternalConstCoefIterator;

  struct Row {
    Row(): indices(), scalars(), externalScalars(), entryCount() {}

    const ColIndex* indices;
    const Scalar* scalars;
    ExternalConstCoefIterator externalScalars;
    ColIndex entryCount;
  };

  RowIndex rowCount() const {return static_cast<RowIndex>(mRows.size());}

  Row row(const RowIndex row) const;

  ColIndex* makeRowWithTheseScalars(const Poly& scalars);

  std::pair<ColIndex*, Scalar*> makeRow(ColIndex entryCount);

  void removeLastEntries(const RowIndex row, const ColIndex count);

private:
  struct InternalRow {
    size_t indicesBegin;
    size_t scalarsBegin;
    ColIndex entryCount;
    bool scalarsStoredExternally;
    ExternalConstCoefIterator externalScalars;
  };

  std::vector<ColIndex> mIndices;
  std::vector<Scalar> mScalars;
  std::vector<InternalRow> mRows;
};

MATHICGB_NAMESPACE_END
#endif
    