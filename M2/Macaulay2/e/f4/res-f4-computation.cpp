/* Copyright 2014, Michael E. Stillman */

#include "res-f4-m2-interface.hpp"
#include "res-f4-computation.hpp"
#include "res-f4.hpp"
#include "res-schreyer-frame.hpp"

#include "matrix.hpp"
#include "../exceptions.hpp"

#include <iostream>


/** createF4Res
 * The only function to create an (F4) resolution computation
 * The constructor for this class is private.  This function 
 * provides all of the logic, and throws an exception if
 * there is a problem.
 */
ResolutionComputation* createF4Res(const Matrix* groebnerBasisMatrix,
                                   int max_level,
                                   int strategy
                                   )
{
  // We expect the following to hold:
  // the ring of groebnerBasisMatrix is a PolynomialRing, but not:
  //   quotient ring
  //   Weyl algebra
  // We assume also that the matrix is homogeneous.
  // If any of these are incorrect, an error message is provided, and
  // null is returned.
  const PolynomialRing *origR = groebnerBasisMatrix->get_ring()->cast_to_PolynomialRing();
  if (origR == 0)
    {
      ERROR("expected polynomial ring");
      return nullptr;
    }
  if (origR->is_quotient_ring())
    {
      ERROR("cannot use nonminimalResolution for quotient rings");
      return nullptr;
    }
  if (origR->is_weyl_algebra())
    {
      ERROR("cannot use nonminimalResolution over Weyl algebras");
      return nullptr;
    }
  if (origR->is_solvable_algebra())
    {
      ERROR("cannot use nonminimalResolution over non-commutative algebras");
      return nullptr;
    }
  if (!groebnerBasisMatrix->is_homogeneous())
    {
      ERROR("cannot use nonminimalResolution with inhomogeneous input");
      return nullptr;
    }
  if (origR->getMonoid()->get_degree_ring()->n_vars() != 1)
    {
      ERROR("expected singly graded with positive degrees for the variables");
      return nullptr;
    }
  if (!origR->getMonoid()->primary_degrees_of_vars_positive())
    {
      ERROR("expected the degree of each variable to be positive");
      return nullptr;
    }
  // Still to check:
  //   (a) coefficients are ZZ/p, for p in range.
  
  const Ring *K = origR->getCoefficients();
  if (!K->isFinitePrimeField())
    {
      ERROR("currently, nonminimalResolution requires finite prime fields");
      return nullptr;
    }

  ResGausser *KK = ResGausser::newResGausser(static_cast<int>(K->characteristic()));
  if (KK == 0)
    {
      ERROR("cannot use Algorithm => 4 with this type of coefficient ring");
      return nullptr;
    }
  auto MI = new ResMonoid(origR->n_vars(), origR->getMonoid()->primary_degree_of_vars(), origR->getMonoid()->getMonomialOrdering());
  ResPolyRing* R;
  if (origR->is_skew_commutative())
    {
      R = new ResPolyRing(*KK, *MI, & (origR->getSkewInfo()));
    }
  else
    {
      R = new ResPolyRing(*KK, *MI);
    }
  auto result = new F4ResComputation(origR,
                                     R,
                                     groebnerBasisMatrix,
                                     max_level);

  // Set level 0
  // take the info from F, place it into mComp
  const FreeModule* F = groebnerBasisMatrix->rows();
  SchreyerFrame& frame = result->frame();
  for (int i=0; i<F->rank(); i++)
    {
      packed_monomial elem = frame.monomialBlock().allocate(MI->max_monomial_size());
      MI->one(i, elem);
      frame.insertLevelZero(elem, F->primary_degree(i));
    }
  frame.endLevel();

  // At this point, we want to sort the columns of groebnerBasisMatrix.
  Matrix *leadterms = groebnerBasisMatrix->lead_term();
  M2_arrayint pos = leadterms->sort(1 /* ascending degree */, -1 /* descending monomial order */);

  std::vector<poly> input_polys;
  for (int i=0; i<groebnerBasisMatrix->n_cols(); i++)
    {
      poly f;
      ResF4toM2Interface::from_M2_vec(*R, F, groebnerBasisMatrix->elem(i), f);
      input_polys.emplace_back(f);
    }

  // Set level 1
  // take the columns of the matrix, and insert them into mComp
  for (int j=0; j<F->rank(); j++)
    {
      // Only insert the ones whose lead monomials are in component j:
      for (int i=0; i<pos->len; i++)
        {
          int loc = pos->array[i];
          poly&f = input_polys[loc];
          if (f.len == 0) continue;
          if (MI->get_component(f.monoms) != j)
            continue;
          packed_monomial elem = frame.monomialBlock().allocate(MI->max_monomial_size());
          MI->copy(f.monoms, elem);
          // the following line grabs f.
          frame.insertLevelOne(elem, groebnerBasisMatrix->cols()->primary_degree(loc), f);
        }
    }
  frame.endLevel();
  //  frame.show(0);

  // Remove matrix:
  delete leadterms;

#if 0  
  int mylevel = 2;
  while (mylevel <= max_level and frame.computeNextLevel() > 0)
    {
      //      frame.show(0);
      mylevel++;
    }
#endif
  return result;
}

F4ResComputation::F4ResComputation(const PolynomialRing* origR,
                                   ResPolyRing* R,
                                   const Matrix* gbmatrix,
                                   int max_level)

  : mOriginalRing(*origR),
    mRing(R),
    mInputGroebnerBasis(*gbmatrix)
{
  std::cout << "making a nonminimalResolution" << std::endl;
  mComp.reset(new SchreyerFrame(*mRing, max_level)); // might need gbmatrix->rows() too
}

F4ResComputation::~F4ResComputation()
{
  remove_res();
}

void F4ResComputation::start_computation()
{
  mComp->start_computation(stop_);
}

int F4ResComputation::complete_thru_degree() const
    // The computation is complete up through this degree.

{
  throw exc::engine_error("complete_thru_degree not implemented");
}

void F4ResComputation::remove_res()
{
  mComp.reset(); mComp = nullptr;
}

M2_arrayint F4ResComputation::get_betti(int type) const
  // type is documented under rawResolutionBetti, in engine.h  
{
  return mComp->getBetti(type);
}

void F4ResComputation::text_out(buffer &o) const
{
  o << "F4 resolution computation" << newline;
}


const Matrix /* or null */ *F4ResComputation::get_matrix(int level) 
{
  const FreeModule* F = get_free(level-1);
  return ResF4toM2Interface::to_M2_matrix(*mComp, level, F);
}

MutableMatrix /* or null */ *F4ResComputation::get_matrix(int level, int degree)
{
  return ResF4toM2Interface::to_M2_MutableMatrix(mOriginalRing.getCoefficientRing(), *mComp, level, degree);
}

const FreeModule /* or null */ *F4ResComputation::get_free(int lev) 
{
  if (lev < 0 or lev > mComp->maxLevel()) return mOriginalRing.make_FreeModule(0);
  if (lev == 0) return mInputGroebnerBasis.rows();
  return mOriginalRing.make_FreeModule(static_cast<int>(mComp->level(lev).size()));
  // TODO: this should return a schreyer order free module, or at least a graded one
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
