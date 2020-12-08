/* Copyright 2014, Michael E. Stillman */

#include <iostream>

#include "interface/monomial-ordering.h"
#include "res-f4-m2-interface.hpp"
#include "res-f4-computation.hpp"
#include "res-schreyer-frame.hpp"

#include "matrix.hpp"
#include "exceptions.hpp"

class ResolutionComputation;
class MutableMatrix;

long nres = 0;
long nres_destruct = 0;

/** createF4Res
 * The only function to create an (F4) resolution computation
 * The constructor for this class is private.  This function
 * provides all of the logic, and throws an exception if
 * there is a problem.
 */
ResolutionComputation* createF4Res(const Matrix* groebnerBasisMatrix,
                                   int max_level,
                                   int strategy)
{
  // We expect the following to hold:
  // the ring of groebnerBasisMatrix is a PolynomialRing, but not:
  //   quotient ring
  //   Weyl algebra
  // We assume also that the matrix is homogeneous.
  // If any of these are incorrect, an error message is provided, and
  // null is returned.
  const PolynomialRing* origR =
      groebnerBasisMatrix->get_ring()->cast_to_PolynomialRing();
  if (origR == 0)
    {
      ERROR("expected polynomial ring");
      return nullptr;
    }
  if (origR->is_quotient_ring())
    {
      ERROR("cannot use res(...,FastNonminimal=>true) for quotient rings");
      return nullptr;
    }
  if (origR->is_weyl_algebra())
    {
      ERROR("cannot use res(...,FastNonminimal=>true) over Weyl algebras");
      return nullptr;
    }
  if (origR->is_solvable_algebra())
    {
      ERROR(
          "cannot use res(...,FastNonminimal=>true) over non-commutative "
          "algebras");
      return nullptr;
    }
  //  if (!groebnerBasisMatrix->is_homogeneous())
  //    {
  //      ERROR(
  //          "cannot use res(...,FastNonminimal=>true) with inhomogeneous input");
  //      return nullptr;
  //    }
  //  if (origR->getMonoid()->get_degree_ring()->n_vars() != 1)
  //    {
  //      ERROR("expected singly graded with positive degrees for the variables");
  //      return nullptr;
  //    }
  if (!origR->getMonoid()->primary_degrees_of_vars_positive())
    {
      ERROR("expected the degree of each variable to be positive");
      return nullptr;
    }
  // Still to check:
  //   (a) coefficients are ZZ/p, for p in range.

  const Ring* K = origR->getCoefficients();
  ResGausser* KK = ResGausser::newResGausser(K);
  if (KK == 0)
    {
      ERROR(
          "cannot use res(...,FastNonminimal=>true) with this type of "
          "coefficient ring");
      return nullptr;
    }

  auto mo = origR->getMonoid()->getMonomialOrdering();  // mon ordering
  auto motype = MonomialOrderingType::Weights;
  if (moIsLex(mo))
    motype = MonomialOrderingType::Lex;
  else if (moIsGRevLex(mo))
    motype = MonomialOrderingType::GRevLex;

  auto MI = new ResMonoid(origR->n_vars(),
                          origR->getMonoid()->getPrimaryDegreeVector(),
                          origR->getMonoid()->getFirstWeightVector(),
                          motype);
  ResPolyRing* R;
  if (origR->is_skew_commutative())
    {
      R = new ResPolyRing(KK, MI, origR->getMonoid(), &(origR->getSkewInfo()));
    }
  else
    {
      R = new ResPolyRing(KK, MI, origR->getMonoid());
    }
  auto result = new F4ResComputation(origR, R, groebnerBasisMatrix, max_level);

  // Set level 0
  // take the columns of the matrix, and insert them into mComp
  const FreeModule* F = groebnerBasisMatrix->rows();
  int maxdeg = 0;
  if (F->rank() > 0)
    {
      maxdeg = F->primary_degree(0);
      for (int j = 1; j < F->rank(); j++)
        if (maxdeg < F->primary_degree(j)) maxdeg = F->primary_degree(j);
    }

  // Set level 0
  // take the info from F, place it into mComp
  SchreyerFrame& frame = result->frame();
  for (int i = 0; i < F->rank(); i++)
    {
      res_packed_monomial elem =
          frame.monomialBlock().allocate(MI->max_monomial_size());
      MI->one(i, elem);
      frame.insertLevelZero(elem, F->primary_degree(i), maxdeg);
    }
  frame.endLevel();

  // At this point, we want to sort the columns of groebnerBasisMatrix.
  Matrix* leadterms = groebnerBasisMatrix->lead_term();
  M2_arrayint pos = leadterms->sort(1 /* ascending degree */,
                                    -1 /* descending monomial order */);

  std::vector<poly> input_polys;
  for (int i = 0; i < groebnerBasisMatrix->n_cols(); i++)
    {
      poly f;
      ResF4toM2Interface::from_M2_vec(*R, F, groebnerBasisMatrix->elem(i), f);
      input_polys.emplace_back(std::move(f));
    }

  // Set level 1.
  for (int j = 0; j < F->rank(); j++)
    {
      // Only insert the ones whose lead monomials are in component j:
      for (int i = 0; i < pos->len; i++)
        {
          int loc = pos->array[i];
          poly& f = input_polys[loc];
          if (f.len == 0) continue;
          if (MI->get_component(f.monoms.data()) != j) continue;
          res_packed_monomial elem =
              frame.monomialBlock().allocate(MI->max_monomial_size());
          MI->copy(f.monoms.data(), elem);
          // the following line grabs f.
          if (!frame.insertLevelOne(
                  elem, groebnerBasisMatrix->cols()->primary_degree(loc), f))
            {
              ERROR(
                  "input polynomials/vectors were computed in a non-compatible "
                  "monomial order");
              // TODO: clean up.
              return nullptr;
            }
        }
    }
  frame.endLevel();
  // frame.show(0);
  // Remove matrix:
  delete leadterms;

  return result;
}

F4ResComputation::F4ResComputation(const PolynomialRing* origR,
                                   ResPolyRing* R,
                                   const Matrix* gbmatrix,
                                   int max_level)

    : mOriginalRing(*origR),
      mInputGroebnerBasis(*gbmatrix),
      mRing(R),
      mComp(new SchreyerFrame(*mRing, max_level))
{
  //  mComp.reset(new SchreyerFrame(*mRing, max_level)); // might need
  //  gbmatrix->rows() too
  nres++;
}

F4ResComputation::~F4ResComputation()
{
  // The following lines should not be required.
  //  mComp.reset(); mComp = nullptr;
  nres_destruct++;
}

void F4ResComputation::start_computation() { mComp->start_computation(stop_); }

int F4ResComputation::complete_thru_degree() const
// The computation is complete up through this degree.
{
  throw exc::engine_error("complete_thru_degree not implemented");
}

M2_arrayint F4ResComputation::get_betti(int type) const
// type is documented under rawResolutionBetti, in engine.h
{
  return mComp->getBetti(type);
}

M2_arrayint F4ResComputation::minimal_betti(M2_arrayint slanted_degree_limit,
                                            M2_arrayint length_limit)
{
  bool stop_after_degree = (slanted_degree_limit->len == 1);
  int top_slanted_degree = slanted_degree_limit->array[0];
  int new_length_limit = (length_limit->len == 1 ? length_limit->array[0]
                                                 : frame().maxLevel() - 1);

  BettiDisplay B = frame().minimalBettiNumbers(
      stop_after_degree, top_slanted_degree, new_length_limit);
  return B.getBetti();
}

void F4ResComputation::text_out(buffer& o) const
{
  o << "F4 resolution computation" << newline;
}

const Matrix /* or null */* F4ResComputation::get_matrix(int level)
{
  if (mOriginalRing.getCoefficientRing()->is_QQ())
    {
      std::cout << "setting error message, returning null" << std::endl;
      ERROR("cannot creat differential over this ring");
      return nullptr;
    }
  const FreeModule* tar = get_free(level - 1);
  const FreeModule* src = get_free(level);
  return ResF4toM2Interface::to_M2_matrix(*mComp, level, tar, src);
}

MutableMatrix /* or null */* F4ResComputation::get_matrix(int level, int degree)
{
  if (mOriginalRing.getCoefficientRing()->is_QQ())
    {
      ERROR("cannot creat differential over this ring");
      return nullptr;
    }
  return ResF4toM2Interface::to_M2_MutableMatrix(
      *mComp, mOriginalRing.getCoefficientRing(), level, degree);
}

const FreeModule /* or null */* F4ResComputation::get_free(int lev)
{
  if (lev < 0 or lev > mComp->maxLevel())
    return mOriginalRing.make_FreeModule(0);
  if (lev == 0) return mInputGroebnerBasis.rows();
  return ResF4toM2Interface::to_M2_freemodule(&mOriginalRing, mInputGroebnerBasis.rows(), *mComp, lev);
}

MutableMatrix /* or null */* F4ResComputation::get_mutable_matrix(const Ring* R,
                                                                  int level)
{
  return ResF4toM2Interface::to_M2_MutableMatrix(frame(), R, level);
}

MutableMatrix /* or null */* F4ResComputation::get_mutable_matrix(
    const Ring* KK,
    int slanted_degree,
    int level)
{
  return ResF4toM2Interface::to_M2_MutableMatrix(
      frame(), KK, level, slanted_degree);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
