// Copyright 1995 Michael E. Stillman

#include "interface/freemodule.h"

#include "buffer.hpp"
#include "error.h"
#include "exceptions.hpp"
#include "free-modules/freemod.hpp"
#include "monoid.hpp"
#include "newdelete.hpp"
#include "rings/ring.hpp"

class Matrix;

const Ring *rawFreeModuleRing(const FreeModule *F) { return F->get_ring(); }
int rawFreeModuleRank(const FreeModule *F) { return F->rank(); }
M2_string rawFreeModuleToString(const FreeModule *F)
{
  buffer o;
  F->text_out(o);
  return o.to_string();
}

unsigned int rawFreeModuleHash(const FreeModule *F) { return F->hash(); }
const FreeModule /* or null */ *rawFreeModuleMake(const Ring *R, int rank)
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

const FreeModule /* or null */ *rawFreeModuleMakeDegs(const Ring *R,
                                                         M2_arrayint degs)
{
  try
    {
      return R->make_FreeModule(degs->len, degs->array);
  } catch (const exc::engine_error& e)
    {
      ERROR(e.what());
      return nullptr;
  }
}

const FreeModule /* or null */ *rawFreeModuleMakeSchreyer(const Matrix *m)
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

M2_arrayint rawFreeModuleGetDegrees(const FreeModule *F)
{
  auto D = F->get_ring()->degree_monoid();
  auto n = D->n_vars();
  auto r = F->rank();
  M2_arrayint result = M2_makearrayint(r * n);
  for (int i = 0; i < r; i++)
    D->to_expvector(F->degree(i), result->array + i * n);
  return result;
}

const Matrix *rawFreeModuleGetSchreyer(const FreeModule *F)
{
  return F->get_induced_order();
}

M2_bool rawFreeModuleIsEqual(const FreeModule *F, const FreeModule *G)
/* Determines if F and G are the same graded module.  If one has a
   Schreyer order and one does not, but their ranks and degrees are the
   same, then they are considered equal by this routine. */
{
  return F->is_equal(G);
}

const FreeModule /* or null */ *rawFreeModuleSum(const FreeModule *F,
                                                   const FreeModule *G)
{
  return F->direct_sum(G);
}

const FreeModule /* or null */ *rawFreeModuleTensor(const FreeModule *F,
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

const FreeModule /* or null */ *rawFreeModuleDual(const FreeModule *F)
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

const FreeModule *rawFreeModuleSymm(int n, const FreeModule *F)
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

const FreeModule *rawFreeModuleExterior(int n, const FreeModule *F)
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

const FreeModule *rawFreeModuleSubmodule(const FreeModule *F,
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
