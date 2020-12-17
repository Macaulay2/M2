// (c) 1994-2002 Michael E. Stillman

#include "interface/monoid.h"

#include <cstdio>
#include <vector>

#include "buffer.hpp"
#include "error.h"
#include "interface/monomial-ordering.h"
#include "monoid.hpp"
#include "ring.hpp"

class PolynomialRing;

Monoid* IM2_Monoid_trivial()
{
  return Monoid::get_trivial_monoid();  // Set up in IM2_initialize()
}

engine_RawMonoidOrNull IM2_Monoid_make(const MonomialOrdering* mo,
                                       M2_ArrayString names,
                                       const Ring* deg_ring,
                                       M2_arrayint degs,
                                       M2_arrayint hefts)
{
  const PolynomialRing* P = deg_ring->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
  return Monoid::create(mo, names, P, degs, hefts);
}

unsigned int rawMonoidHash(const Monoid* M) { return M->hash(); }
M2_string IM2_Monoid_to_string(const Monoid* M)
{
  buffer o;
  M->text_out(o);
  return o.to_string();
}

// Many monomial ordering routines are in monordering.c
// Here are some that use c++ features, so cannot be there.
// @todo Make monordering.{h,c} into a c++ class.

static void write_row(std::vector<int>& grading,
                      int nvars,
                      int which,
                      int value)
{
  for (int i = 0; i < nvars; i++)
    if (i == which)
      grading.push_back(value);
    else
      grading.push_back(0);
}
static void write_weights(std::vector<int>& grading,
                          int nvars,
                          int firstvar,
                          int* wts,
                          int nwts)
// place nvars ints into grading:  0 ... 0 wts[0] wts[1] ... wts[nwts-1] 0 ....
// 0
// where wts[0] is in the 'firstvar' location.  If wts is NULL, treat it as the
// vector with nwts '1's.
{
  for (int i = 0; i < firstvar; i++) grading.push_back(0);
  if (wts == 0)
    for (int i = 0; i < nwts; i++) grading.push_back(1);
  else
    for (int i = 0; i < nwts; i++) grading.push_back(wts[i]);
  for (int i = firstvar + nwts; i < nvars; i++) grading.push_back(0);
}

bool monomialOrderingToMatrix(
    const struct MonomialOrdering& mo,
    std::vector<int>& mat,
    bool& base_is_revlex,
    int& component_direction,     // -1 is Down, +1 is Up, 0 is not present
    int& component_is_before_row  // -1 means: at the end. 0 means before the
                                  // order.
    // and r means considered before row 'r' of the matrix.
    )
{
  // a false return value means an error has occurred.
  int nvars = rawNumberOfVariables(&mo);
  base_is_revlex = true;
  enum LastBlock { LEX, REVLEX, WEIGHTS, NONE };
  LastBlock last = NONE;
  int nwts = 0;  // local var used in MO_WEIGHTS section
  int nrows = 0;
  int firstvar = 0;
  component_direction = 0;
  component_is_before_row =
      -2;                   // what should the default value be?  Probably: -1.
  size_t last_element = 0;  // The vector 'mat' will be resized back to this
                            // value if the last part of the order is lex or
                            // revlex.
  for (int i = 0; i < mo.len; i++)
    {
      mon_part p = mo.array[i];
      switch (p->type)
        {
          case MO_LEX:
          case MO_LEX2:
          case MO_LEX4:
            // printf("lex %d\n", p->nvars);
            last_element = mat.size();
            for (int j = 0; j < p->nvars; j++)
              {
                write_row(mat, nvars, firstvar + j, 1);
              }
            last = LEX;
            firstvar += p->nvars;
            nrows += p->nvars;
            break;
          case MO_GREVLEX:
          case MO_GREVLEX2:
          case MO_GREVLEX4:
            // printf("grevlex %d %ld\n", p->nvars, p->wts);
            write_weights(mat, nvars, firstvar, p->wts, p->nvars);
            last_element = mat.size();
            for (int j = p->nvars - 1; j >= 1; --j)
              {
                write_row(mat, nvars, firstvar + j, -1);
              }
            last = REVLEX;
            firstvar += p->nvars;
            nrows += p->nvars;
            break;
          case MO_GREVLEX_WTS:
          case MO_GREVLEX2_WTS:
          case MO_GREVLEX4_WTS:
            // printf("grevlex_wts %d %ld\n", p->nvars, p->wts);
            write_weights(mat, nvars, firstvar, p->wts, p->nvars);
            last_element = mat.size();
            for (int j = p->nvars - 1; j >= 1; --j)
              {
                write_row(mat, nvars, firstvar + j, -1);
              }
            last = REVLEX;
            firstvar += p->nvars;
            nrows += p->nvars;
            break;
          case MO_REVLEX:
            // printf("revlex %d\n", p->nvars);
            last_element = mat.size();
            for (int j = p->nvars - 1; j >= 0; --j)
              {
                write_row(mat, nvars, firstvar + j, -1);
              }
            last = REVLEX;
            firstvar += p->nvars;
            nrows += p->nvars;
            break;
          case MO_WEIGHTS:
            // printf("matsize= %d weights %d p->wts=%lu\n", mat.size(),
            // p->nvars, p->wts);
            nwts = (p->nvars > nvars ? nvars : p->nvars);
            write_weights(mat, nvars, 0, p->wts, nwts);
            nrows++;
            last_element = mat.size();
            last = WEIGHTS;
            break;
          case MO_LAURENT:
          case MO_LAURENT_REVLEX:
          case MO_NC_LEX:
            return false;
            break;
          case MO_POSITION_UP:
            component_direction = 1;
            component_is_before_row = nrows;
            break;
          case MO_POSITION_DOWN:
            component_direction = -1;
            component_is_before_row = nrows;
            break;
          default:
            // DO nothing
            break;
        }
    }
  if (last == LEX)
    {
      // last block was lex, so use lex tie-breaker
      mat.resize(last_element);
      if (nrows == component_is_before_row) component_is_before_row = -1;
      base_is_revlex = false;
    }
  else if (last == REVLEX)
    {
      // last block was revlex, so use revlex tie-breaker
      if (nrows == component_is_before_row) component_is_before_row = -1;
      mat.resize(last_element);
    }
  else
    {
      // last block is a weight vector, so use revlex as the tie-breaker.
      // nothing to change here.
    }
  return true;
}

M2_arrayint rawMonomialOrderingToMatrix(const struct MonomialOrdering* mo)
{
  bool base;
  std::vector<int> mat;
  M2_arrayint result = 0;
  int component_is_before_row = 0;
  int component_direction = 0;
  if (monomialOrderingToMatrix(
          *mo, mat, base, component_direction, component_is_before_row))
    {
      int top = static_cast<int>(mat.size());
      result = M2_makearrayint(top + 3);
      for (int i = 0; i < top; i++) result->array[i] = mat[i];
      result->array[top] = (base ? 1 : 0);
      result->array[top + 1] = component_direction;
      result->array[top + 2] = component_is_before_row;
    }
  return result;
}

int rawMonoidNumberOfBlocks(const Monoid* M)
{
  return M->num_parts();
}
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
