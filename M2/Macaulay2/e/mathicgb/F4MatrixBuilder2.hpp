// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_F4_MATRIX_BUILDER_2_GUARD
#define MATHICGB_F4_MATRIX_BUILDER_2_GUARD

#include "SparseMatrix.hpp"
#include "Poly.hpp"
#include "PolyRing.hpp"
#include "PolyBasis.hpp"
#include "QuadMatrix.hpp"
#include "MonomialMap.hpp"
#include "F4ProtoMatrix.hpp"
#include "mtbb.hpp"
#include <vector>

MATHICGB_NAMESPACE_BEGIN

/// Class for constructing an F4 matrix.
///
/// @todo: this class does not offer exception guarantees. It's just not
/// very workable without an RAII monomial handle or a scope exit
/// functionality, so add one of those before fixing this.
class F4MatrixBuilder2 {
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
  F4MatrixBuilder2(const PolyBasis& basis, size_t memoryQuantum = 0);

  /// Schedules a row representing the S-polynomial between polyA and
  /// polyB to be added to the matrix. No ownership is taken, but polyA
  /// and polyB must remain valid until the matrix is constructed.
  ///
  /// Currently, the two monomials must be monic, though this is just
  /// because they happen always to be monic so there was no reason to
  /// support the non-monic case.
  void addSPolynomialToMatrix(const Poly& polyA, const Poly& polyB);

  /// Schedules a row representing multiple*poly to be added to the
  /// matrix. No ownership is taken, but poly must remain valid until
  /// the matrix is constructed. multiple is copied, so it need not
  /// remain valid.
  void addPolynomialToMatrix(ConstMonoRef multiple, const Poly& poly);

  /// As the overload with a multiple, where the multiple is 1.
  void addPolynomialToMatrix(const Poly& poly);

  /// Builds an F4 matrix to the specifications given. Also clears the
  /// information in this object.
  ///
  /// The right columns are in order of strictly decreasing monomial.
  /// The left columns are ordered in some way so that the leading non-zero
  /// entry in each top row has the maximal column monomial out of all
  /// non-zero entries in that row.
  ///
  /// The monomials that can be reduced by some element of the basis go on
  /// the left while the remaining monomials go on the right. The upper left
  /// matrix is upper triangular, thus having a reducer/pivot row for every
  /// column.
  ///
  /// There is no guarantee that the bottom part of the matrix contains rows
  /// that exactly correspond to the polynomials that have been scheduled to
  /// be added to the matrix. It is only guaranteed that the whole matrix has
  /// the same row-space as though that had been the case.
  void buildMatrixAndClear(QuadMatrix& matrix);

  const PolyRing& ring() const {return mBasis.ring();}
  const Monoid& monoid() const {return ring().monoid();}
  const Field& field() const {return ring().field();}

private:
  /// Represents the task of adding a row to the matrix. If sPairPoly is null
  /// then the row to add is multiply * poly. Otherwise, the row to add is
  ///   multiply * poly - sPairMultiply * sPairPoly
  /// where multiply and sPairMultiply are such that the leading terms become
  /// desiredLead.
  struct RowTask {
    ConstMonoPtr desiredLead; // multiply a monomial onto poly to get this lead
    const Poly* poly;
    const Poly* sPairPoly;
  };

  class Builder;

  /// How much memory to allocate every time more memory is needed.
  const size_t mMemoryQuantum;

  /// The basis that supplies reducers.
  const PolyBasis& mBasis;

  /// Stores the rows that have been scheduled to be added.
  std::vector<RowTask> mTodo;
};

MATHICGB_NAMESPACE_END
#endif
