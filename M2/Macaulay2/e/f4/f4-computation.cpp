// Copyright 2005 Michael E. Stillman.

#include "f4/f4-computation.hpp"

#include "buffer.hpp"              // for buffer
#include "error.h"                 // for ERROR
#include "f4/f4-m2-interface.hpp"  // for F4toM2Interface
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
#include "util.hpp"                // for M2_arrayint_to_stdvector
#include "VectorArithmetic.hpp"    // for VectorArithmetic, ElementArray

class Computation;
class RingElement;

GBComputation *createF4GB(const Matrix *m,
                          M2_bool collect_syz,
                          int n_rows_to_keep,
                          M2_arrayint gb_weights,
                          int strategy,
                          M2_bool use_max_degree,
                          int max_degree,
                          int numThreads)
{
  const PolynomialRing *R = m->get_ring()->cast_to_PolynomialRing();
  if (R == nullptr)
    {
      ERROR("internal error: expected a polynomial ring for `Algorithm => LinearAlgebra` groebner basis");
      return nullptr;
    }
  const Ring *K = R->getCoefficients();

  // TODO: code here used to detect whether R, K is a valid ring here
  if (not R->is_commutative_ring())
    {
      ERROR("expected commutative polynomial ring for `Algorithm => LinearAlgebra` groebner basis");
      return nullptr;
    }
  if (R->is_quotient_ring())
    {
      ERROR("can't use quotient polynomial rings with `Algorithm => LinearAlgebra` groebner basis");
      return nullptr;
    }
  if (not m->is_homogeneous())
    {
      ERROR("expected homogeneous input for `Algorithm => LinearAlgebra` groebner basis");
      return nullptr;
    }
  if (not K->isFinitePrimeField() and not K->isGaloisField())
    {
      ERROR("expected coefficient ring to be a finite field for `Algorithm => LinearAlgebra` groebner basis");
      return nullptr;
    }
  auto vectorArithmetic = new VectorArithmetic(K);

  return new F4Computation(vectorArithmetic,
                           m,
                           collect_syz,
                           n_rows_to_keep,
                           gb_weights,
                           strategy,
                           use_max_degree,
                           max_degree,
                           numThreads);
}

F4Computation::F4Computation(const VectorArithmetic* VA,
                             const Matrix *m,
                             M2_bool collect_syz,
                             int n_rows_to_keep,
                             M2_arrayint gb_weights,
                             int strategy,
                             M2_bool use_max_degree,
                             int max_degree,
                             int numThreads)
  : mFreeModule(m->rows()),
    mVectorArithmetic(VA)
{
  mOriginalRing = m->get_ring()->cast_to_PolynomialRing();

  std::vector<int> heftDegrees = M2_arrayint_to_stdvector<int> (gb_weights);
  std::vector<int> moduleHeftDegrees;
  for (int j = 0; j < mFreeModule->rank(); ++j)
    {
      moduleHeftDegrees.push_back(mFreeModule->primary_degree(j));
      
    }
  mMonoid = new MonomialInfo(mOriginalRing->n_vars(),
                             mOriginalRing->getMonoid()->getMonomialOrdering(),
                             heftDegrees,
                             moduleHeftDegrees);

  if (m->n_rows() >= 2)
    {
      if (mMonoid->componentLocation() != -1 or
          mMonoid->positionUp() != 1)
        {
          delete mMonoid;
          throw exc::engine_error("must use Position=>Up at end of monomial order for Algorithm=>LinearAlgebra groebner basis");
        }
    }
  
  mF4GB = new F4GB(mVectorArithmetic,
                   mMonoid,
                   m->rows(),
                   collect_syz,
                   n_rows_to_keep,
                   gb_weights,
                   strategy,
                   use_max_degree,
                   max_degree,
                   numThreads);

  F4toM2Interface::from_M2_matrix(mVectorArithmetic, mMonoid, m, mF4GB->get_generators());
  mF4GB->new_generators(0, m->n_cols() - 1);
}

F4Computation::~F4Computation() = default;
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
  GBSorter C(mMonoid, gb);
  auto gbsize = gb.size();
  int *gb_order = new int[gbsize];
  for (int i = 0; i < gbsize; i++)
    {
      gb_order[i] = i;
    }

  std::stable_sort(gb_order, gb_order + gbsize, C);

  // Order: increasing in monomial order
  MatrixConstructor result(mFreeModule, 0);
  for (int i = 0; i < gbsize; i++)
    {
      int which = gb_order[i];
      vec v = F4toM2Interface::to_M2_vec(
        mVectorArithmetic, mMonoid, gb[which]->f, mFreeModule);
      result.append(v);
    }
  return result.to_matrix();
}

const Matrix /* or null */ *F4Computation::get_mingens()
{
  ERROR("Minimal generators for all `Algorithm => LinearAlgebra` Groebner bases not computed");
  return nullptr;
}

const Matrix /* or null */ *F4Computation::get_change()
{
  ERROR("Change of basis matrix for all `Algorithm => LinearAlgebra` Groebner bases not computed");
  return nullptr;
}

const Matrix /* or null */ *F4Computation::get_syzygies()
{
  ERROR("Syzygies for `Algorithm => LinearAlgebra` Groebner bases not computed");
  return nullptr;
}

const Matrix /* or null */ *F4Computation::get_initial(int nparts)
{
  ERROR("Lead terms for `Algorithm => LinearAlgebra`: use `leadTerm gens gb` instead of `leadTerm gb`");
  return nullptr;
}

const Matrix /* or null */ *F4Computation::matrix_remainder(const Matrix *m)
{
  ERROR("No special algorithm for computing matrix remainder for `Algorithm => LinearAlgebra");
  return nullptr;
}

M2_bool F4Computation::matrix_lift(
    const Matrix *m,
    const Matrix /* or null */ **result_remainder,
    const Matrix /* or null */ **result_quotient)
{
  *result_remainder = nullptr;
  *result_quotient = nullptr;
  ERROR("No special algorithm for computing matrix remainder and lift for `Algorithm => LinearAlgebra");  
  return false;
}

int F4Computation::contains(const Matrix *m)
// Return -1 if every column of 'm' reduces to zero.
// Otherwise return the index of the first column that
// does not reduce to zero.
{
  ERROR("No special algorithm for computing containment for `Algorithm => LinearAlgebra");  
  return 0;
}

int F4Computation::complete_thru_degree() const
// The computation is complete up through this degree.
{
  // TODO!
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

  // f4->show();
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
