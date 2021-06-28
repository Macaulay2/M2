// Copyright 2004 Michael E. Stillman.

#include "text-io.hpp"
#include "comp-res.hpp"
#include "res-a1.hpp"
#include "res-a0.hpp"
#include "res-a2.hpp"
#include "finalize.hpp"
#include "schreyer-resolution/res-f4-computation.hpp"
#include "NCResolutions/nc-res-computation.hpp"

#include <iostream>

ResolutionComputation::ResolutionComputation() {}
ResolutionComputation::~ResolutionComputation() {}
ResolutionComputation *ResolutionComputation::choose_res(
    const Matrix *m,
    M2_bool resolve_cokernel,
    int max_level,
    M2_bool use_max_slanted_degree,
    int max_slanted_degree,
    int algorithm,
    int strategy)
{
  // The following modification is because some algorithms do not work if
  // max_level is 0.
  // github issue (crash, #368).
  if (max_level <= 0) max_level = 1;

  const Ring *R = m->get_ring();
  ResolutionComputation *C = nullptr;
  int origsyz;
  // First, we need to check that m is homogeneous, and that
  // the heft values of the variables are all positive.
  // All of these algorithms also assume that R is a polynomial ring.

  const M2FreeAlgebraOrQuotient *NCP = R->cast_to_M2FreeAlgebraOrQuotient();
  if (NCP != nullptr)
    {
      if (M2_gbTrace > 0) emit_line("NC resolution");
      C = createNCRes(m, max_level, strategy);
      return C;
    }
  const PolynomialRing *P = R->cast_to_PolynomialRing();
  if (P == nullptr)
    {
      ERROR("engine resolution strategies all require a polynomial base ring");
      return nullptr;
    }
  const Ring* K = P->getCoefficientRing();
  if (K->get_precision() != 0)
    {
      ERROR("free resolutions over polynomial rings with RR or CC coefficients not yet implemented");
      return nullptr;
    }
  if (!P->getMonoid()->primary_degrees_of_vars_positive())
    {
      ERROR(
          "engine resolution strategies all require a Heft vector which is "
          "positive for all variables");
      return nullptr;
    }
  if (algorithm < 4 and !m->is_homogeneous())
    {
      ERROR("engine resolution strategies require a homogeneous module");
      return nullptr;
    }

  switch (algorithm)
    {
      case 1:
        if (!resolve_cokernel)
          {
            ERROR(
                "resolution Strategy=>1 cannot resolve a cokernel with a given "
                "presentation: use Strategy=>2 or Strategy=>3 instead");
            return nullptr;
          }
        if (!R->is_commutative_ring())
          {
            ERROR(
                "use resolution Strategy=>2 or Strategy=>3 for non commutative "
                "polynomial rings");
            return nullptr;
          }
        if (M2_gbTrace > 0) emit_line("resolution Strategy=>1");
        C = new res_comp(m, max_level, strategy);
        break;
      case 0:
        if (!resolve_cokernel)
          {
            ERROR(
                "resolution Strategy=>0 cannot resolve a cokernel with a given "
                "presentation: use Strategy=>2 or Strategy=>3 instead");
            return nullptr;
          }
        if (!R->is_commutative_ring())
          {
            ERROR(
                "use resolution Strategy=>2 or Strategy=>3 for non commutative "
                "polynomial rings");
            return nullptr;
          }
        if (M2_gbTrace > 0) emit_line("resolution Strategy=>0");
        C = new res2_comp(
            m, max_level, use_max_slanted_degree, max_slanted_degree, strategy);
        break;
      case 2:
        origsyz = m->n_cols();
        if (M2_gbTrace > 0) emit_line("resolution Strategy=>2");
        C = new gbres_comp(m, max_level + 1, origsyz, strategy);
        break;
      case 3:
        origsyz = m->n_cols();
        if (M2_gbTrace > 0) emit_line("resolution Strategy=>3");
        C = new gbres_comp(
            m, max_level + 1, origsyz, strategy | STRATEGY_USE_HILB);
        break;
      case 4:
      case 5:
        if (!resolve_cokernel)
          {
            ERROR(
                "resolution Strategy=>4 cannot resolve a cokernel with a given "
                "presentation: use Strategy=>2 or Strategy=>3 instead");
            return nullptr;
          }
        if (!P->is_skew_commutative() and !R->is_commutative_ring())
          {
            ERROR(
                "use resolution Strategy=>2 or Strategy=>3 for non commutative "
                "polynomial rings");
            return nullptr;
          }
        if (M2_gbTrace > 0) emit_line("resolution Strategy=>4 (res-f4)");
        C = createF4Res(m, max_level, strategy);
        if (C == nullptr) return nullptr;
        break;
    }
  if (C == nullptr)
    {
      ERROR("unknown resolution algorithm");
      return nullptr;
    }
  intern_res(C);
  return C;
}

void ResolutionComputation::betti_init(int lo, int hi, int len, int *&bettis)
{
  int z = (hi - lo + 1) * (len + 1);
  bettis = newarray_atomic_clear(int, z);
}

M2_arrayint ResolutionComputation::betti_make(int lo,
                                              int hi,
                                              int len,
                                              int *bettis)
{
  int d, lev;
  int hi1 = hi + 1;
  int len1 = len + 1;

  // Reset 'hi1' to reflect the top degree that occurs
  for (d = hi; d >= lo; d--)
    {
      for (lev = 0; lev <= len; lev++)
        if (bettis[lev + (len + 1) * (d - lo)] > 0)
          {
            hi1 = d;
            break;
          }
      if (hi1 <= hi) break;
    }
  if (hi1 > hi) hi1 = hi;

  // Reset 'len1' to reflect the top level that occurs
  for (lev = len; lev >= 0; lev--)
    {
      for (d = lo; d <= hi1; d++)
        if (bettis[lev + (len + 1) * (d - lo)] > 0)
          {
            len1 = lev;
            break;
          }
      if (len1 <= len) break;
    }
  if (len1 > len) len1 = len;

  int totallen = (hi1 - lo + 1) * (len1 + 1);
  M2_arrayint result = M2_makearrayint(3 + totallen);

  result->array[0] = lo;
  result->array[1] = hi1;
  result->array[2] = len1;

  int next = 3;
  for (d = lo; d <= hi1; d++)
    for (lev = 0; lev <= len1; lev++)
      result->array[next++] = bettis[lev + (len + 1) * (d - lo)];

  return result;
}

void ResolutionComputation::betti_display(buffer &o, M2_arrayint ar)
{
  int *a = ar->array;
  int total_sum = 0;
  int lo = a[0];
  int hi = a[1];
  int len = a[2] + 1;
  o << "total  ";
  for (int lev = 0; lev < len; lev++)
    {
      int sum = 0;
      for (int d = lo; d <= hi; d++) sum += a[len * (d - lo) + lev + 3];
      total_sum += sum;
      o.put(sum, 6);
      o << ' ';
    }
  o << " [" << total_sum << "]" << newline;
  for (int d = lo; d <= hi; d++)
    {
      o.put(d, 5);
      o << ": ";
      for (int lev = 0; lev < len; lev++)
        {
          int c = a[len * (d - lo) + lev + 3];
          if (c != 0)
            o.put(c, 6);
          else
            o << "     -";
          o << " ";
        }
      o << newline;
    }
}

MutableMatrix /* or null */ *ResolutionComputation::get_matrix(int level,
                                                               int degree)
{
  // the default version gives an error that it isn't defined
  ERROR("this function not defined for this resolution type");
  return 0;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
