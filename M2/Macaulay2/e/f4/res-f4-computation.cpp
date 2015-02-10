/* Copyright 2014, Michael E. Stillman */

#include "res-f4-m2-interface.hpp"
#include "res-f4-computation.hpp"
#include "res-f4.hpp"

#include "matrix.hpp"

#include <memory>
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
  std::cout << "warnings: (1) The monomial order in the ring should use Weights: the first weight vector is the degree" << std::endl;
  std::cout << "          (2) If the target is not the ring, then the GB should be sorted so that comp1 comes first, then comp2, etc" << std::endl;

  const PolynomialRing *origR = groebnerBasisMatrix->get_ring()->cast_to_PolynomialRing();
  if (origR == 0)
    {
      ERROR("expected polynomial ring");
      return nullptr;
    }
  const Ring *K = origR->getCoefficients();

  ResF4Mem *Mem = new ResF4Mem; // Used both for ResGausser and F4Res
  ResGausser *KK = ResGausser::newResGausser(K, Mem);
  if (KK == 0)
    {
      ERROR("cannot use Algorithm => 4 with this type of coefficient ring");
      delete Mem;
      return nullptr;
    }
  auto MI = new MonomialInfo(origR->n_vars(), origR->getMonoid()->getMonomialOrdering());
  ResPolyRing R(*KK, *MI);
  auto result = new F4ResComputation(origR,
                                     groebnerBasisMatrix,
                                     Mem,
                                     KK,
                                     MI,
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

  // Set level 1
  // take the columns of the matrix, and insert them into mComp
  for (int i=0; i<groebnerBasisMatrix->n_cols(); i++)
    {
      packed_monomial elem = frame.monomialBlock().allocate(MI->max_monomial_size());
      poly f;
      ResF4toM2Interface::from_M2_vec(R, F, groebnerBasisMatrix->elem(i), f);
      MI->copy(f.monoms, elem);
      frame.insertLevelOne(elem, f);
    }
  frame.endLevel();
  frame.show(0);

  while (frame.computeNextLevel() > 0) { }
  frame.show(0);

  //  result->start_computation();
  return result;
}

F4ResComputation::F4ResComputation(const PolynomialRing* R,
                                   const Matrix* gbmatrix,
                                   ResF4Mem* Mem,
                                   const ResGausser* KK,
                                   const MonomialInfo* MI,
                                   int max_level)

  : mOriginalRing(*R),
    mRing(*KK, *MI),
    mInputGroebnerBasis(*gbmatrix),
    mMem(Mem) // we own this.  mResGausser and mComp just use it
{
  mComp.reset(new F4Res(Mem, mRing, max_level)); // might need gbmatrix->rows() too
}

F4ResComputation::~F4ResComputation()
{
  remove_res();
}

const Matrix /* or null */ *F4ResComputation::get_matrix(int level) 
{
  const FreeModule* F = get_free(level-1);
  return ResF4toM2Interface::to_M2_matrix(*mComp, level, F);
}

MutableMatrix /* or null */ *F4ResComputation::get_matrix(int level, int degree)
{
  return ResF4toM2Interface::to_M2_MutableMatrix(*mComp, level, degree);
}

const FreeModule /* or null */ *F4ResComputation::get_free(int lev) 
{
  if (lev < 0) return mOriginalRing.make_FreeModule(0);
  if (lev == 0) return mInputGroebnerBasis.rows();
  return mOriginalRing.make_FreeModule(mComp->frame().level(lev).size());
  // TODO: this should return a schreyer order free module, or at least a graded one
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
