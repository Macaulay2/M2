// Copyright 2005 Michael E. Stillman.

#include "f4/f4-computation.hpp"

#include "buffer.hpp"              // for buffer
#include "error.h"                 // for ERROR
#include "f4/f4-m2-interface.hpp"  // for F4toM2Interface
#include "f4/f4-mem.hpp"           // for F4Mem
#include "f4/f4-types.hpp"         // for gb_array, gbelem
#include "f4/f4.hpp"               // for F4GB
#include "f4/moninfo.hpp"          // for MonomialInfo
#include "matrix-con.hpp"          // for MatrixConstructor
#include "matrix.hpp"              // for Matrix
#include "mem.hpp"                 // for stash
#include "monoid.hpp"              // for Monoid
#include "polyring.hpp"            // for PolynomialRing
#include "ring.hpp"                // for Ring
#include "ringelem.hpp"            // for vec
#include "text-io.hpp"             // for emit
#include "VectorArithmetic.hpp"    // for VectorArithmetic, CoeffVector

class Computation;
class RingElement;

GBComputation *createF4GB(const Matrix *m,
                          M2_bool collect_syz,
                          int n_rows_to_keep,
                          M2_arrayint gb_weights,
                          int strategy,
                          M2_bool use_max_degree,
                          int max_degree)
{
  const PolynomialRing *R = m->get_ring()->cast_to_PolynomialRing();
  const Ring *K = R->getCoefficients();
  F4Mem *Mem = new F4Mem;
  auto vectorArithmetic = new VectorArithmetic(K);
  // TODO: code here used to detect whether R, K is a valid ring here

  GBComputation *G;
  G = new F4Computation(vectorArithmetic,
                        Mem,
                        m,
                        collect_syz,
                        n_rows_to_keep,
                        gb_weights,
                        strategy,
                        use_max_degree,
                        max_degree);
  return G;
}

F4Computation::F4Computation(const VectorArithmetic* VA,
                             F4Mem *Mem0,
                             const Matrix *m,
                             M2_bool collect_syz,
                             int n_rows_to_keep,
                             M2_arrayint gb_weights,
                             int strategy,
                             M2_bool use_max_degree,
                             int max_degree)
  : mFreeModule(m->rows()),
    mVectorArithmetic(VA),
    mMemoryBlock(Mem0)
{
  // Note: the F4Mem which K0 uses should be mMemoryBlock. ??TODO: no longer valid, still containing useful info?
  mOriginalRing = m->get_ring()->cast_to_PolynomialRing();
  mMonoid = new MonomialInfo(mOriginalRing->n_vars(),
                        mOriginalRing->getMonoid()->getMonomialOrdering());


  mF4GB = new F4GB(mVectorArithmetic,
                   Mem0,
                   mMonoid,
                   m->rows(),
                   collect_syz,
                   n_rows_to_keep,
                   gb_weights,
                   strategy,
                   use_max_degree,
                   max_degree);

  F4toM2Interface::from_M2_matrix(mVectorArithmetic, mMonoid, m, gb_weights, mF4GB->get_generators());
  mF4GB->new_generators(0, m->n_cols() - 1);
}

F4Computation::~F4Computation() { delete mMemoryBlock; }
/*************************
 ** Top level interface **
 *************************/

void F4Computation::start_computation() { mF4GB->start_computation(stop_); }
Computation /* or null */ *F4Computation::set_hilbert_function(
    const RingElement *hf)
{
  mF4GB->set_hilbert_function(hf);
  return this;
}

const Matrix /* or null */ *F4Computation::get_gb()
{
  const gb_array &gb = mF4GB->get_gb();
  MatrixConstructor result(mFreeModule, 0);
  for (int i = 0; i < gb.size(); i++)
    {
      vec v = F4toM2Interface::to_M2_vec(
          mVectorArithmetic, mMonoid, gb[i]->f, mFreeModule);
      result.append(v);
    }
  return result.to_matrix();
}

const Matrix /* or null */ *F4Computation::get_mingens()
{
#if 0
//   MatrixConstructor mat(_F,0);
//   for (VECTOR(gbelem *)::iterator i = gb.begin();
//        i != gb.end();
//        i++)
//     if ((*i)->minlevel == ELEM_POSSIBLE_MINGEN)
//       mat.append(mOriginalRing->translate_gbvector_to_vec(_F, (*i)->g.f));
//   return mat.to_matrix();
#endif
  return nullptr;
}

const Matrix /* or null */ *F4Computation::get_change() { return 0; }
const Matrix /* or null */ *F4Computation::get_syzygies() { return 0; }
const Matrix /* or null */ *F4Computation::get_initial(int nparts) { return 0; }
const Matrix /* or null */ *F4Computation::matrix_remainder(const Matrix *m)
{
  return nullptr;
}

M2_bool F4Computation::matrix_lift(
    const Matrix *m,
    const Matrix /* or null */ **result_remainder,
    const Matrix /* or null */ **result_quotient)
{
  *result_remainder = nullptr;
  *result_quotient = nullptr;
  ERROR("rawGBMatrixLift not implemented for LinearAlgebra GB's yets");
  return false;
}

int F4Computation::contains(const Matrix *m)
// Return -1 if every column of 'm' reduces to zero.
// Otherwise return the index of the first column that
// does not reduce to zero.
{
  return 0;
}

int F4Computation::complete_thru_degree() const
// The computation is complete up through this degree.
{
  return 0;
}

void F4Computation::text_out(buffer &o) const
/* This displays statistical information, and depends on the
   M2_gbTrace value */
{
  // TODO: what to display?
}

void F4Computation::show() const  // debug display
{
  buffer o;
  stash::stats(o);
  emit(o.str());

  mMemoryBlock->show();
  // f4->show();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
