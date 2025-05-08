// Copyright 1995 Michael E. Stillman

#include "interface/freemodule.h"

#include "buffer.hpp"
#include "error.h"
#include "exceptions.hpp"
#include "freemod.hpp"
#include "monoid.hpp"
#include "newdelete.hpp"
#include "ring.hpp"

class Matrix;

const Ring *IM2_FreeModule_ring(const FreeModule *F) { return F->get_ring(); }
int IM2_FreeModule_rank(const FreeModule *F) { return F->rank(); }
M2_string IM2_FreeModule_to_string(const FreeModule *F)
{
  buffer o;
  F->text_out(o);
  return o.to_string();
}

unsigned int rawFreeModuleHash(const FreeModule *F) { return F->hash(); }
const FreeModule /* or null */ *IM2_FreeModule_make(const Ring *R, int rank)
{
  try
    {
      if (rank < 0)
        {
          ERROR("freemodule rank must be non-negative");
          return nullptr;
        }
      return R->make_FreeModule(rank);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const FreeModule /* or null */ *IM2_FreeModule_make_degs(const Ring *R,
                                                         M2_arrayint degs)
{
  try
    {
      auto D = R->degree_monoid();
      unsigned int eachdeg = D->n_vars();
      if (eachdeg == 0)
        {
          ERROR("rawFreeModule: degree rank 0, but sequence of degrees given");
          return nullptr;
        }
      unsigned int rank = degs->len / eachdeg;
      if (rank * eachdeg != degs->len)
        {
          ERROR("inappropriate number of degrees");
          return nullptr;
        }
      monomial deg = D->make_one();
      FreeModule *F = R->make_FreeModule();
      for (unsigned int i = 0; i < rank; i++)
        {
          D->from_expvector(degs->array + i * eachdeg, deg);
          F->append(deg);
        }
      return F;
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const FreeModule /* or null */ *IM2_FreeModule_make_schreyer(const Matrix *m)
{
  try
    {
      return FreeModule::make_schreyer(m);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

M2_arrayint IM2_FreeModule_get_degrees(const FreeModule *F)
{
  auto D = F->get_ring()->degree_monoid();
  auto n = D->n_vars();
  auto r = F->rank();
  M2_arrayint result = M2_makearrayint(r * n);
  for (int i = 0; i < r; i++)
    D->to_expvector(F->degree(i), result->array + i * n);
  return result;
}

const Matrix *IM2_FreeModule_get_schreyer(const FreeModule *F)
{
  return F->get_induced_order();
}

M2_bool IM2_FreeModule_is_equal(const FreeModule *F, const FreeModule *G)
/* Determines if F and G are the same graded module.  If one has a
   Schreyer order and one does not, but their ranks and degrees are the
   same, then they are considered equal by this routine. */
{
  return F->is_equal(G);
}

const FreeModule /* or null */ *IM2_FreeModule_sum(const FreeModule *F,
                                                   const FreeModule *G)
{
  return F->direct_sum(G);
}

const FreeModule /* or null */ *IM2_FreeModule_tensor(const FreeModule *F,
                                                      const FreeModule *G)
{
  try
    {
      return F->tensor(G);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const FreeModule /* or null */ *IM2_FreeModule_dual(const FreeModule *F)
{
  try
    {
      return F->transpose();
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const FreeModule *IM2_FreeModule_symm(int n, const FreeModule *F)
{
  try
    {
      return F->symm(n);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const FreeModule *IM2_FreeModule_exterior(int n, const FreeModule *F)
{
  try
    {
      return F->exterior(n);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const FreeModule *IM2_FreeModule_submodule(const FreeModule *F,
                                           M2_arrayint selection)
{
  try
    {
      return F->sub_space(selection);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

M2_arrayintOrNull rawFreeModuleSelectByDegrees(const FreeModule *F,
                                               M2_arrayint lo,
                                               M2_arrayint hi)
{
  try
    {
      return F->select_by_degrees(lo, hi);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
