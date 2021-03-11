// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_MONO_ORDER_GUARD
#define MATHICGB_MONO_ORDER_GUARD

#include <vector>
#include <algorithm>

MATHICGB_NAMESPACE_BEGIN

/// Class used to describe an monomial order and/or a module monomial
/// order. Use this class to construct a monoid. The monoid does the
/// actual comparisons. Monomials must be preprocessed by MonoProcessor -
/// otherwise the ordering may not be correct. MonoProcessor also offers
/// additional parameters for making orders.
template<class Weight>
class MonoOrder;

template<class W>
class MonoOrder {
public:
  typedef W Weight;
  typedef size_t VarIndex;
  typedef std::vector<Weight> Gradings;

  static const size_t ComponentAfterBaseOrder = static_cast<size_t>(-1);

  enum BaseOrder {
    /// Lexicographic order with x_0 < x_1 < ... < x_n.
    LexBaseOrderFromRight = 0,

    /// Reverse lexicographic order with x_0 > x_1 > ... > x_n.
    RevLexBaseOrderFromRight = 1,

    /// Lexicographic order with x_0 > x_1 > ... > x_n.
    LexBaseOrderFromLeft = 2,

    /// Reverse lexicographic order with x_0 < x_1 < ... < x_n.
    RevLexBaseOrderFromLeft = 3
  };

  /// The specified base order is graded by the gradings matrix.
  ///
  /// The layout of the gradings matrix is row-major. For comparisons,
  /// the degree with respect to the first row is considered first,
  /// then the degree with respect to the second row and so on. The
  /// base order is used as a tie-breaker. The gradings vector can be
  /// empty. The order must be a monomial order - in particular, 1
  /// must be strictly less than all other monomials.
  ///
  /// For module monomials, the component is considered too. When the
  /// component is considered depends on componentBefore. If
  /// componentBefore == 0 then the component is considered before
  /// anything else. If componentBefore < gradingCount(), then the
  /// component is considered before the grading with index
  /// componentBefore and after the grading with index componentBefore
  /// - 1. If componentBefore == gradingCount(), then the component is
  /// considered after all gradings and before the base order. If
  /// componentBefore = ComponentAfterBaseOrder then the component is
  /// considered after everything else, including afte the base order.
  MonoOrder(
    const VarIndex varCount,
    Gradings&& gradings,
    const BaseOrder baseOrder = RevLexBaseOrderFromRight,
    const size_t componentBefore = ComponentAfterBaseOrder,
    const bool componentsAscendingDesired = true,
    const bool schreyering = true
  ):
    mVarCount(varCount),
    mGradings(std::move(gradings)),
    mBaseOrder(baseOrder),
    mComponentBefore(componentBefore),
    mSchreyering(schreyering),
    mComponentsAscendingDesired(componentsAscendingDesired)
  {
    MATHICGB_ASSERT(debugAssertValid());
  }

  /// Same as MonoOrder(varCount, varOrder, gradings, componentBefore)
  /// where gradings has a single row of varCount 1's.
  MonoOrder(
    const VarIndex varCount,
    const BaseOrder baseOrder = RevLexBaseOrderFromRight,
    const size_t componentBefore = ComponentAfterBaseOrder,
    const bool componentsAscendingDesired = true,
    const bool schreyering = true
  ):
    mVarCount(varCount),
    mGradings(varCount, 1),
    mBaseOrder(baseOrder),
    mComponentBefore(componentBefore),
    mComponentsAscendingDesired(componentsAscendingDesired),
    mSchreyering(schreyering)
  {
    MATHICGB_ASSERT(debugAssertValid());
  }

  VarIndex varCount() const {return mVarCount;}

  VarIndex componentBefore() const {return mComponentBefore;}

  /// Returns the number of rows in the grading vector.
  size_t gradingCount() const {
    return varCount() == 0 ? 0 : mGradings.size() / varCount();
  }

  /// Returns the grading matrix in row-major layout.
  const Gradings& gradings() const {return mGradings;}

  /// Returns true if the grading matrix is a single row of 1's.
  bool isTotalDegree() const {
    if (varCount() == 0 || mGradings.size() != varCount())
      return false;
    for (VarIndex var = 0; var < varCount(); ++var)
      if (mGradings[var] != 1)
        return false;
    return true;
  }

  BaseOrder baseOrder() const {return mBaseOrder;}

  bool hasFromLeftBaseOrder() const {
    return
      baseOrder() == LexBaseOrderFromLeft ||
      baseOrder() == RevLexBaseOrderFromLeft;
  }

  bool hasLexBaseOrder() const {
    return
      baseOrder() == LexBaseOrderFromLeft ||
      baseOrder() == LexBaseOrderFromRight;
  }

  /// Returns true if the order is a monomial order. A monomial order
  /// is a total order on monomials where a>b => ac>bc for all
  /// monomials a,b,c and where the order is a well order. Only the
  /// well order property could currently fail. It is equivalent to
  /// stating that x>1 for all variables x.
  bool isMonomialOrder() const {
    for (VarIndex var = 0; var < varCount(); ++var) {
      // Check that x_var > 1.
      for (size_t grading = 0; ; ++grading) {
        if (grading == gradingCount()) {
          // The column was entirely zero, so x_var > 1 if and only if the
          // base ordering is lex.
          if (!hasLexBaseOrder())
            return false;
          break;
        }
        const auto index = grading * varCount() + var;
        MATHICGB_ASSERT(index < mGradings.size());
        if (mGradings[index] != 0) {
          // We have found the first non-zero weight in this column,
          // so x_var > 1 if and only if this weight is positive.
          if (mGradings[index] < 0)
            return false;
          break;
        }
      }
    }
    return true;
  }

  bool componentsAscendingDesired() const {return mComponentsAscendingDesired;}
  bool schreyering() const {return mSchreyering;}

private:
  bool debugAssertValid() {
#ifdef MATHICGB_DEBUG
    MATHICGB_ASSERT(mGradings.size() == gradingCount() * varCount());
    MATHICGB_ASSERT(
      mComponentBefore == ComponentAfterBaseOrder ||
      mComponentBefore <= gradingCount()
    );
    MATHICGB_ASSERT(
      mBaseOrder == LexBaseOrderFromLeft ||
      mBaseOrder == RevLexBaseOrderFromLeft ||
      mBaseOrder == LexBaseOrderFromRight ||
      mBaseOrder == RevLexBaseOrderFromRight 
    );
#endif
    return true;
  }

  const VarIndex mVarCount;

  const Gradings mGradings;
  const BaseOrder mBaseOrder;
  const size_t mComponentBefore;
  const bool mSchreyering;
  const bool mComponentsAscendingDesired;
};

MATHICGB_NAMESPACE_END
#endif
