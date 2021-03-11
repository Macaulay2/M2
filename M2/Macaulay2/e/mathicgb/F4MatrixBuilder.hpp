// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_F4_MATRIX_BUILDER_GUARD
#define MATHICGB_F4_MATRIX_BUILDER_GUARD

#include "QuadMatrixBuilder.hpp"
#include "Poly.hpp"
#include "PolyRing.hpp"
#include "PolyBasis.hpp"
#include "QuadMatrix.hpp"
#include "mtbb.hpp"
#include <vector>

MATHICGB_NAMESPACE_BEGIN

/// Class for constructing an F4 matrix. This class is reponsible for
/// figuring out what matrix to build and then it uses QuadMatrixBuilder
/// to create that matrix.
///
/// @todo: this class does not offer exception guarantees. It's just not
/// very workable without an RAII monomial handle, so add one of those
/// before fixing this.
class F4MatrixBuilder {
private:
  typedef QuadMatrixBuilder::ColIndex ColIndex;
  typedef QuadMatrixBuilder::LeftRightColIndex LeftRightColIndex;
  typedef QuadMatrixBuilder::Scalar Scalar;
  typedef QuadMatrixBuilder::Map Map;
  typedef QuadMatrixBuilder::Monomials Monomials;

public:
  typedef PolyRing::Field Field;

  typedef PolyRing::Monoid Monoid;
  typedef Monoid::Mono Mono;
  typedef Monoid::MonoRef MonoRef;
  typedef Monoid::ConstMonoRef ConstMonoRef;
  typedef Monoid::MonoPtr MonoPtr;
  typedef Monoid::ConstMonoPtr ConstMonoPtr;


  /// memoryQuantum is how much to increase the memory size by each time the
  /// current amount of memory is exhausted. A value of 0 indicates to start
  /// small and double the quantum at each exhaustion.
  F4MatrixBuilder(const PolyBasis& basis, size_t memoryQuantum = 0);

  /** Schedules a row representing the S-polynomial between polyA and
    polyB to be added to the matrix. No ownership is taken, but polyA
    and polyB must remain valid until the matrix is constructed.

    Currently, the two monomials must be monic, though this is just
    because they always happen to be monic so there was no reason to
    support the non-monic case. */
  void addSPolynomialToMatrix(const Poly& polyA, const Poly& polyB);

  /** Schedules a row representing multiple*poly to be added to the
    matrix. No ownership is taken, but poly must remain valid until
    the matrix is constructed. multiple is copied so it need not
    remain valid. */
  void addPolynomialToMatrix(ConstMonoRef multiple, const Poly& poly);

  /// as the overload with a multiple, just letting multiple be the
  /// identity.
  void addPolynomialToMatrix(const Poly& poly);

  /** Builds an F4 matrix to the specifications given. Also clears the
    information in this object.

    The right columns are ordered by decreasing monomial of each
    column according to the order from the basis. The left columns are
    ordered in some way so that the first entry in each top row (the
    pivot) has a lower index than any other entries in that row.

    The matrix contains a reducer/pivot for every monomial that can be
    reduced by the basis and that is present in the matrix. There is
    no guarantee that the bottom part of the matrix contains rows that
    exactly correspond to the polynomials that have been scheduled to
    be added to the matrix. It is only guaranteed that the whole matrix has
    the same row-space as though that had been the case. */
  void buildMatrixAndClear(QuadMatrix& matrix);

  const PolyRing& ring() const {return mBuilder.ring();}
  const Monoid& monoid() const {return ring().monoid();}
  const Field& field() const {return ring().field();}

private:
  typedef const MonomialMap<LeftRightColIndex>::Reader ColReader;

  /// Represents the task of adding a row to the matrix. If sPairPoly is null
  /// then the row to add is multiply * poly. Otherwise, the row to add is
  ///   multiply * poly - sPairMultiply * sPairPoly
  /// where sPairMultiply makes the lead terms cancel.
  struct RowTask {
    bool addToTop; // add the row to the bottom if false
    ConstMonoPtr desiredLead; // multiply monomial onto poly to get this lead
    const Poly* poly;
    const Poly* sPairPoly;
    ConstMonoPtr sPairMultiply;
  };
  typedef mtbb::parallel_do_feeder<RowTask> TaskFeeder;

  /// Creates a column with monomial label x and schedules a new row to
  /// reduce that column if possible. Here x is monoA if monoB is
  /// null and otherwise x is the product of monoA and monoB.
  MATHICGB_NO_INLINE
  std::pair<LeftRightColIndex, ConstMonoRef>
  createColumn(ConstMonoRef monoA, ConstMonoRef monoB, TaskFeeder& feeder);

  void appendRowTop(
    ConstMonoRef multiple,
    const Poly& poly,
    QuadMatrixBuilder& builder,
    TaskFeeder& feeder
  );

  void appendRowBottom(
    const Poly& poly,
    ConstMonoRef multiply,
    const Poly& sPairPoly,
    ConstMonoRef sPairMultiply,
    QuadMatrixBuilder& builder,
    TaskFeeder& feeder
  );

  void appendRowBottom(
    ConstMonoRef multiple,
    bool negate,
    Poly::ConstTermIterator begin,
    Poly::ConstTermIterator end,
    QuadMatrixBuilder& builder,
    TaskFeeder& feeder
  );

  MATHICGB_NO_INLINE
  std::pair<QuadMatrixBuilder::LeftRightColIndex, ConstMonoRef>
  findOrCreateColumn(
    ConstMonoRef monoA,
    ConstMonoRef monoB,
    TaskFeeder& feeder
  );
  
  MATHICGB_INLINE
  std::pair<QuadMatrixBuilder::LeftRightColIndex, ConstMonoRef>
  findOrCreateColumn(
    ConstMonoRef monoA,
    ConstMonoRef monoB,
    const ColReader& colMap,
    TaskFeeder& feeder
  );

  MATHICGB_NO_INLINE
  void createTwoColumns(
    ConstMonoRef monoA1,
    ConstMonoRef monoA2,
    ConstMonoRef monoB,
    TaskFeeder& feeder
  );

  mgb::mtbb::mutex mCreateColumnLock;
  ColIndex mLeftColCount;
  ColIndex mRightColCount;
  Mono mTmp;
  const PolyBasis& mBasis;
  Monomials mMonomialsLeft;
  Monomials mMonomialsRight;
  QuadMatrixBuilder mBuilder;
  Map mMap;
  std::vector<RowTask> mTodo;
};

MATHICGB_NAMESPACE_END

#endif
