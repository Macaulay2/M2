// Copyright 2004 Michael E. Stillman.

#include "comp-gb.hpp"

#include "gb-homog2.hpp"
#include "gb-sugarless.hpp"
#include "gb-toric.hpp"
#include "gauss.hpp"
#include "hermite.hpp"
#include "gb-default.hpp"
#include "gbweight.hpp"
#include "comp-gb-proxy.hpp"
#include "text-io.hpp"
#include "finalize.hpp"
#include "util.hpp"

GBComputation *createF4GB(const Matrix *m,
                          M2_bool collect_syz,
                          int n_rows_to_keep,
                          M2_arrayint gb_weights,
                          int strategy,
                          M2_bool use_max_degree,
                          int max_degree,
                          int numThreads);

// Found in "gb-f4/GBF4Interface.hpp"
GBComputation *createGBF4Interface(const Matrix *m,
                                  const std::vector<int>& weights,
                                  int strategy,
                                  int numThreads
                                  );

GBComputation::~GBComputation() {}
void GBComputation::text_out(buffer &o) const
{
  o << "-- a raw Groebner basis computation --";
}

GBComputation *GBComputation::choose_gb(const Matrix *m,
                                        M2_bool collect_syz,
                                        int n_rows_to_keep,
                                        M2_arrayint gb_weights,
                                        M2_bool use_max_degree,
                                        int max_degree,
                                        int algorithm,
                                        int strategy,
                                        int numThreads,
                                        int max_reduction_count)
{
  const Ring *R1 = m->get_ring();
  const PolynomialRing *R2 = R1->cast_to_PolynomialRing();

  std::vector<int> weights; // used in createParallelF4GB
  if (R2 == nullptr)
    {
      // Look for the correct computation type here.
      if (R1 == globalZZ)
        {
          return new HermiteComputation(m, collect_syz, n_rows_to_keep);
        }
      if (R1->is_field())
        {
          return new GaussElimComputation(m, collect_syz, n_rows_to_keep);
        }
#ifdef DEVELOPMENT
#warning "handle non polynomial rings"
#endif
      ERROR("GB computation for non-polynomial rings not yet re-implemented");
      return nullptr;
    }

//  const PolynomialRing *R = R2->get_flattened_ring();
// bool is_graded = (R->is_graded() && m->is_homogeneous());
// bool ring_is_base = R->is_basic_ring();
// bool base_is_ZZ = R->getCoefficientRing()->is_ZZ();
#ifdef DEVELOPMENT
#warning "NOT QUITE!!  Need to know if it is ZZ or QQ"
#warning "unused variables commented out"
#endif
  // bool base_is_field = !R->getCoefficientRing()->is_ZZ();

  GBComputation *result;

  switch (algorithm)
    {
      case 4:
        result = GBinhom_comp::create(m,
                                      collect_syz,
                                      n_rows_to_keep,
                                      gb_weights,
                                      strategy,
                                      use_max_degree,
                                      max_degree);
        break;
      case 5:
        result = GB_comp::create(m,
                                 collect_syz,
                                 n_rows_to_keep,
                                 gb_weights,
                                 strategy,
                                 use_max_degree,
                                 max_degree);
        break;
      case 6:
        result = createF4GB(m,
                            collect_syz,
                            n_rows_to_keep,
                            gb_weights,
                            strategy,
                            use_max_degree,
                            max_degree,
                            numThreads);
        break;
      case 7:
        result = binomialGB_comp::create(m,
                                         collect_syz,
                                         n_rows_to_keep,
                                         gb_weights,
                                         strategy,
                                         use_max_degree,
                                         max_degree);
        break;
      case 8:
        ERROR("Algorithm => Test has been removed from M2");
        return nullptr;
      case 9:
        // new GBF4 algorithm
        weights = M2_arrayint_to_stdvector<int>(gb_weights);
        result = createGBF4Interface(m,
                            weights,
                            strategy,
                            numThreads);
        break;
      default:
        result = gbA::create(m,
                             collect_syz,
                             n_rows_to_keep,
                             gb_weights,
                             strategy,
                             use_max_degree,
                             max_degree,
                             max_reduction_count);
        break;
    }
  intern_GB(result);
  return result != nullptr ? new GBProxy(result) : nullptr;

#if 0
//   if (is_graded)
//     return GB_comp::create(m,
//                         collect_syz,
//                         n_rows_to_keep,
//                         strategy,
//                         use_max_degree,
//                         max_degree);
//
//   return 0;
#endif
#if 0
//   if (base_is_ZZ)
//     {
//       if (ring_is_base)
//      {
//
//        return HermiteComputation::create(m,
//                                          collect_syz,
//                                          collect_change,
//                                          n_rows_to_keep);
//        return 0;
//      }
//       // Question: should we separate between the graded, nongraded versions?
//       return GBZZ::create(m,
//                        collect_syz,
//                        collect_change,
//                        n_rows_to_keep);
//       return 0;
//     }
//   else
//     {
//       // Base is a field
//       if (ring_is_base)
//      {
//        return GaussElimComputation::create(m,
//                                            collect_syz,
//                                            collect_change,
//                                            n_rows_to_keep);
//        // This should be fraction free
//        return 0;
//      }
//       // Also allow the user to choose between them.
//       if (is_graded)
//      return GB_comp::create(m,
//                             collect_syz,
//                             n_rows_to_keep,
//                             strategy,
//                             use_max_degree,
//                             max_degree);
//       return GB_inhom_comp::create(m,
//                                 collect_syz,
//                                 collect_change,
//                                 n_rows_to_keep,
//                                 strategy);
//       return 0;
//     }
#endif
}

Computation /* or null */ *GBComputation::set_hilbert_function(
    const RingElement *h)
// The default version returns an error saying that Hilbert functions cannot be
// used.
{
  ERROR("Hilbert function use is not implemented for this GB algorithm");
  return nullptr;
}

const Matrix /* or null */ *GBComputation::get_parallel_lead_terms(
    M2_arrayint w)
{
  ERROR(
      "Cannot compute parallel lead terms for this kind of Groebner "
      "computation");
  return nullptr;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
