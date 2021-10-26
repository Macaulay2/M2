// (c) 1995 Michael E. Stillman

#include "varpower.hpp"
#include "error.h"
#include "overflow.hpp"

#define MAX_VAR 2147483647
#define MIN_EXP -2147483647
#define MAX_EXP 2147483647

unsigned int varpower::computeHashValue(const int *vp)
{
  unsigned int hashval = *vp;
  index_varpower i = vp;
  for (; i.valid(); ++i)
    {
      int v = i.var();
      int e = i.exponent();
      hashval = 4624296 * hashval + 2341 * v + e;
    }
  return hashval;
}

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

void varpower::elem_text_out(buffer &o, const int *a, bool p_one)
{
  index_varpower i = a;
  if (!i.valid())
    {
      if (p_one) o << "1";
    }
  else
    for (; i.valid(); ++i)
      {
        int v = i.var();
        int e = i.exponent();
        if (v < 26)
          o << char('a' + v);
        else if (v < 52)
          o << char('A' + v - 26);
        else
          o << "x[" << v << "]";
        if (e > 1)
          o << e;
        else if (e < 0)
          o << "^(" << e << ")";
      }
}

void varpower::elem_text_out(buffer &o,
                             const int *a,
                             M2_ArrayString varnames,
                             bool p_one)
{
  index_varpower i = a;
  if (!i.valid())
    {
      if (p_one) o << "1";
    }
  else
    for (; i.valid(); ++i)
      {
        int v = i.var();
        int e = i.exponent();
        if (varnames->len < v)
          o << ".";
        else
          o << varnames->array[v];
        int single = (varnames->array[v]->len == 1);
        if (e > 1 && single)
          o << e;
        else if (e > 1)
          o << "^" << e;
        else if (e < 0)
          o << "^(" << e << ")";
        //      if (i > 0) o << "*";
      }
}

bool varpower::is_one(const int *a) { return *a == 1; }
bool varpower::is_equal(const int *a, const int *b)
{
  if (*a != *b++) return false;
  int len = *a++;
  for (int i = 1; i < len; i++)
    if (*a++ != *b++) return false;
  return true;
}

int varpower::topvar(const int *a)
{
  assert(*a > 1);
  return a[1];
}

// Used in 2 places
void varpower::one(intarray &result) { result.append(1); }
// Mostly used to make skew vars...
void varpower::var(int v, int e, intarray &result)
{
  if (e == 0)
    result.append(1);
  else
    {
      check_var(v, e);  // Sets ERROR if a problem...
      result.append(3);
      result.append(v);
      result.append(e);
    }
}

void varpower::from_arrayint(M2_arrayint m, intarray &result)
{
  int *result_vp = result.alloc(m->len + 1);
  *result_vp++ = m->len + 1;
  int *melems = m->array;

  for (int i = 0; i < m->len; i += 2)
    {
      int v = *melems++;
      int e = *melems++;
      check_var(v, e);
      *result_vp++ = v;
      *result_vp++ = e;
    }
}

M2_arrayint varpower::to_arrayint(const int *vp)
{
  int len = *vp;
  M2_arrayint result = M2_makearrayint(len);
  for (int i = 0; i < len; i++) result->array[i] = *vp++;
  return result;
}

int *varpower::copy(const int *vp, intarray &result)
{
  return result.copy(*vp, vp);
}

void varpower::to_ntuple(int n, const int *a, int *result_exponents)
{
  int *t = result_exponents;
  for (int j = 0; j < n; j++) t[j] = 0;
  for (index_varpower i = a; i.valid(); ++i)
    {
      int v = i.var();
      int e = i.exponent();
      if (v < n) t[v] = e;
    }
}

void varpower::from_ntuple(int n, const int *a, intarray &result)
{
  int len = 0;
  for (int i = 0; i < n; i++)
    if (a[i] != 0) len++;
  int result_len = 2 * len + 1;
  int *result_vp = result.alloc(result_len);

  *result_vp++ = result_len;
  for (int i = n - 1; i >= 0; i--)
    if (a[i] != 0)
      {
        *result_vp++ = i;
        *result_vp++ = a[i];
      }
}

int varpower::simple_degree(const int *a)
{
  int deg = 0;
  for (index_varpower i = a; i.valid(); ++i) deg += i.exponent();
  return deg;
}

void varpower::mult(const int *a, const int *b, intarray &result)
{
  int len = *a + *b;  // potential length
  int *result_vp = result.alloc(len);
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
  result.shrink(newlen);
}

void varpower::quotient(const int *a, const int *b, intarray &result)
// return a:b
{
  int *result_vp = result.alloc(*a);
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

void varpower::power(const int *a, int n, intarray &result)
{
  if (n == 0)
    {
      result.append(1);
      return;
    }
  int *result_vp = result.alloc(*a);
  *result_vp++ = *a;
  for (index_varpower i = a; i.valid(); ++i)
    {
      *result_vp++ = i.var();
      *result_vp++ = safe::mult(i.exponent(), n);
    }
}

bool varpower::divides(const int *b, const int *a)
// (Note the switch in order of parameters.  Does b divide a?
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

void varpower::monsyz(const int *a, const int *b, intarray &sa, intarray &sb)
// sa, sb are set so that a * sa = b * sb
// and sa, sb have disjoint support.
{
  int *result_vp1 = sa.alloc(*b);
  int *result_vp2 = sa.alloc(*a);
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

void varpower::lcm(const int *a, const int *b, intarray &result)
{
  int len = *a + *b;  // potential length
  int *result_vp = result.alloc(len);
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

void varpower::gcd(const int *a, const int *b, intarray &result)
{
  int len = *a;  // potential length
  if (*b < *a) len = *b;
  int *result_vp = result.alloc(len);
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

void varpower::erase(const int *a, const int *b, intarray &result)
// divide a by b^infinity
{
  int *result_vp = result.alloc(*a);
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

void varpower::radical(const int *a, intarray &result)
{
  // length of result is the same as that of a
  int *result_vp = result.alloc(*a);
  *result_vp++ = *a;
  for (index_varpower i = a; i.valid(); ++i)
    {
      *result_vp++ = i.var();  // var
      *result_vp++ = 1;        // exponent
    }
}

bool varpower::is_pure_power(const int *a, int &v, int &e)
// if a is a pure power, then set v, e so that v^e is a.
// otherwise return false.
{
  if (*a != 3) return false;
  v = a[1];
  e = a[2];
  return true;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
