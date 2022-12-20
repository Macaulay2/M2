// (c) 1995 Michael E. Stillman

#include "ExponentList.hpp"

#include <assert.h>      // for assert

#include "buffer.hpp"    // for buffer
#include "error.h"       // for ERROR
#include "overflow.hpp"  // for add, mult

#define MAX_VAR 2147483647
#define MIN_EXP -2147483647
#define MAX_EXP 2147483647

static bool check_var(int v, int e)
{
  if (v < 0 || v > MAX_VAR)
    {
      ERROR("Monomial expects variable number in range 0..%d", MAX_VAR);
      return false;
    }
  if (e < MIN_EXP || e > MAX_EXP || e == 0)
    {
      ERROR("Monomial expects non-zero exponents in range %d..%d",
            MIN_EXP,
            MAX_EXP);
      return false;
    }
  return true;
}

template <>
void varpower::from_arrayint(M2_arrayint m, Vector& result)
{
  // TODO: should result be cleared?
  result.resize(m->len + 1);
  int *result_vp = result.data();
  *result_vp++ = result.size();
  int *melems = m->array;

  for (int i = 0; i < m->len; i += 2) // FIXME: reconcile
    {
      int v = *melems++;
      int e = *melems++;
      check_var(v, e);
      *result_vp++ = v;
      *result_vp++ = e;
    }
}

template <>
M2_arrayint varpower::to_arrayint(ConstExponents vp)
{
  int len = length(vp);
  M2_arrayint result = M2_makearrayint(len); // FIXME: reconcile
  for (int i = 0; i < len; i++) result->array[i] = *vp++;
  return result;
}

template <>
void varpower::to_expvector(int n, ConstExponents a, exponents_t result)
{
  for (int j = 0; j < n; j++) result[j] = 0;
  for (index_varpower i = a; i.valid(); ++i)
    {
      int v = i.var();
      int e = i.exponent();
      assert(v < n);
      result[v] = e;
    }
}

template <>
void varpower::from_expvector(int n, ConstExponents a, Vector& result)
{
  int len = 0;
  for (int i = 0; i < n; i++)
    if (a[i] != 0) len++;

  result.resize(2 * len + 1);
  Exponents result_vp = result.data();
  *result_vp++ = result.size(); // FIXME: reconcile
  for (int i = n - 1; i >= 0; i--)
    if (a[i] != 0)
      {
        *result_vp++ = i;
        *result_vp++ = a[i];
      }
}

template <>
void varpower::mult(ConstExponents a, ConstExponents b, Vector& result)
{
  // TODO: should result be cleared?
  result.resize(length(a) + length(b));  // potential length
  Exponents result_vp = result.data();
  Exponents orig_result_vp = result_vp;
  result_vp++;

  index_varpower i = a;
  index_varpower j = b;

  // merge the two varpowers to staticVP
  int va = (i.valid() ? i.var() : -1);
  int vb = (j.valid() ? j.var() : -1);
  for (;;)
    {
      if (va > vb)
        {
          *result_vp++ = va;
          *result_vp++ = i.exponent();
          ++i;
          va = (i.valid() ? i.var() : -1);
        }
      else if (vb > va)
        {
          *result_vp++ = vb;
          *result_vp++ = j.exponent();
          ++j;
          vb = (j.valid() ? j.var() : -1);
        }
      else
        {
          if (va == -1) break;
          int z = safe::add(i.exponent(), j.exponent());
          if (z != 0)
            {
              *result_vp++ = va;
              *result_vp++ = z;
            }
          ++i;
          ++j;
          va = (i.valid() ? i.var() : -1);
          vb = (j.valid() ? j.var() : -1);
        }
    }
  int newlen = static_cast<int>(result_vp - orig_result_vp);
  *orig_result_vp = newlen;
  result.resize(newlen);
}

template <>
void varpower::quotient(ConstExponents a,
                        ConstExponents b,
                        Vector& result)
// return a:b
{
  // TODO: should result be cleared?
  result.resize(length(a));  // potential length
  Exponents result_vp = result.data();
  Exponents orig_result_vp = result_vp;
  result_vp++;

  index_varpower i = a;
  index_varpower j = b;

  int va = (i.valid() ? i.var() : -1);
  int vb = (j.valid() ? j.var() : -1);
  for (;;)
    {
      if (va > vb)
        {
          *result_vp++ = va;
          *result_vp++ = i.exponent();
          ++i;
          va = (i.valid() ? i.var() : -1);
        }
      else if (vb > va)
        {
          ++j;
          vb = (j.valid() ? j.var() : -1);
        }
      else
        {
          if (va == -1) break;
          int ea = i.exponent();
          int eb = j.exponent();
          if (ea > eb)
            {
              *result_vp++ = va;
              *result_vp++ = ea - eb;  // overflow cannot occur
            }
          ++i;
          ++j;
          va = (i.valid() ? i.var() : -1);
          vb = (j.valid() ? j.var() : -1);
        }
    }
  *orig_result_vp = static_cast<int>(result_vp - orig_result_vp);
}

template <>
void varpower::power(ConstExponents a, int n, Vector& result)
{
  if (n == 0)
    {
      result.push_back(1);
      return;
    }
  result.resize(length(a));
  int *result_vp = result.data();
  *result_vp++ = result.size();
  for (index_varpower i = a; i.valid(); ++i)
    {
      *result_vp++ = i.var();
      *result_vp++ = safe::mult(i.exponent(), n);
    }
}

template <>
bool varpower::divides(ConstExponents b, ConstExponents a)
// FIXME: Note the switch in order of parameters.  Does b divide a?
{
  index_varpower i = a;
  index_varpower j = b;
  int va = (i.valid() ? i.var() : -1);
  int vb = (j.valid() ? j.var() : -1);
  for (;;)
    {
      if (va > vb)
        {
          ++i;
          va = (i.valid() ? i.var() : -1);
        }
      else if (va < vb)
        return false;
      else
        {
          if (va == -1) return true;
          int ea = i.exponent();
          int eb = j.exponent();
          if (ea < eb) return false;
          ++i;
          ++j;
          va = (i.valid() ? i.var() : -1);
          vb = (j.valid() ? j.var() : -1);
        }
    }
}

template <>
void varpower::monsyz(ConstExponents a, ConstExponents b, Vector& sa, Vector& sb)
// sa, sb are set so that a * sa = b * sb
// and sa, sb have disjoint support.
{
  sa.resize(length(a));
  sb.resize(length(b));
  int *result_vp1 = sa.data();
  int *result_vp2 = sa.data();
  int *orig_result_vp1 = result_vp1;
  int *orig_result_vp2 = result_vp2;
  result_vp1++;
  result_vp2++;

  index_varpower i = a;
  index_varpower j = b;

  int va = (i.valid() ? i.var() : -1);
  int vb = (j.valid() ? j.var() : -1);
  for (;;)
    {
      if (va > vb)
        {
          // va should be placed into sb
          *result_vp2++ = va;
          *result_vp2++ = i.exponent();
          ++i;
          va = (i.valid() ? i.var() : -1);
        }
      else if (va < vb)
        {
          // vb should be placed into sa
          *result_vp1++ = vb;
          *result_vp1++ = j.exponent();
          ++j;
          vb = (j.valid() ? j.var() : -1);
        }
      else
        {
          if (va == -1) break;
          int ea = i.exponent();
          int eb = j.exponent();
          if (ea < eb)
            {
              *result_vp1++ = va;
              *result_vp1++ = eb - ea;
            }
          else if (ea > eb)
            {
              *result_vp2++ = va;
              *result_vp2++ = ea - eb;
            }
          ++i;
          ++j;
          va = (i.valid() ? i.var() : -1);
          vb = (j.valid() ? j.var() : -1);
        }
    }
  *orig_result_vp1 = static_cast<int>(result_vp1 - orig_result_vp1);
  *orig_result_vp2 = static_cast<int>(result_vp2 - orig_result_vp2);
}

template <>
void varpower::lcm(ConstExponents a, ConstExponents b, Vector& result)
{
  // TODO: should result be cleared?
  result.resize(length(a) + length(b));  // potential length
  Exponents result_vp = result.data();
  Exponents orig_result_vp = result_vp;
  result_vp++;

  index_varpower i = a;
  index_varpower j = b;

  // merge the two varpowers to staticVP
  int va = (i.valid() ? i.var() : -1);
  int vb = (j.valid() ? j.var() : -1);
  for (;;)
    {
      if (va > vb)
        {
          *result_vp++ = va;
          *result_vp++ = i.exponent();
          ++i;
          va = (i.valid() ? i.var() : -1);
        }
      else if (vb > va)
        {
          *result_vp++ = vb;
          *result_vp++ = j.exponent();
          ++j;
          vb = (j.valid() ? j.var() : -1);
        }
      else
        {
          if (va == -1) break;
          int ea = i.exponent();
          int eb = j.exponent();
          if (ea < eb) ea = eb;
          *result_vp++ = va;
          *result_vp++ = ea;
          ++i;
          ++j;
          va = (i.valid() ? i.var() : -1);
          vb = (j.valid() ? j.var() : -1);
        }
    }
  *orig_result_vp = static_cast<int>(result_vp - orig_result_vp);
}

template <>
void varpower::gcd(ConstExponents a, ConstExponents b, Vector& result)
{
  // TODO: should result be cleared?
  result.resize(std::min(length(a), length(b)));  // potential length
  int *result_vp = result.data();
  int *orig_result_vp = result_vp;
  result_vp++;

  index_varpower i = a;
  index_varpower j = b;

  // merge the two varpowers to staticVP
  int va = (i.valid() ? i.var() : -1);
  int vb = (j.valid() ? j.var() : -1);
  for (;;)
    {
      if (va > vb)
        {
          ++i;
          va = (i.valid() ? i.var() : -1);
        }
      else if (vb > va)
        {
          ++j;
          vb = (j.valid() ? j.var() : -1);
        }
      else
        {
          if (va == -1) break;
          int ea = i.exponent();
          int eb = j.exponent();
          if (ea > eb) ea = eb;
          *result_vp++ = va;
          *result_vp++ = ea;
          ++i;
          ++j;
          va = (i.valid() ? i.var() : -1);
          vb = (j.valid() ? j.var() : -1);
        }
    }
  *orig_result_vp = static_cast<int>(result_vp - orig_result_vp);
}

template <>
void varpower::erase(ConstExponents a, ConstExponents b, Vector& result)
// divide a by b^infinity
{
  // TODO: should result be cleared?
  result.resize(length(a));
  int *result_vp = result.data();
  int *orig_result_vp = result_vp;
  result_vp++;

  index_varpower i = a;
  index_varpower j = b;

  int va = (i.valid() ? i.var() : -1);
  int vb = (j.valid() ? j.var() : -1);
  for (;;)
    {
      if (va > vb)
        {
          *result_vp++ = va;
          *result_vp++ = i.exponent();
          ++i;
          va = (i.valid() ? i.var() : -1);
        }
      else if (vb > va)
        {
          ++j;
          vb = (j.valid() ? j.var() : -1);
        }
      else
        {
          if (va == -1) break;
          ++i;
          ++j;
          va = (i.valid() ? i.var() : -1);
          vb = (j.valid() ? j.var() : -1);
        }
    }

  *orig_result_vp = static_cast<int>(result_vp - orig_result_vp);
}

template <>
void varpower::radical(ConstExponents a, Vector& result)
{
  // TODO: should result be cleared?
  // length of result is the same as that of a
  result.resize(length(a));
  int *result_vp = result.data();
  *result_vp++ = result.size();
  for (index_varpower i = a; i.valid(); ++i)
    {
      *result_vp++ = i.var();  // var
      *result_vp++ = 1;        // exponent
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
