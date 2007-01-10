// (c) 1995 Michael E. Stillman

#include "varpower.hpp"
#include "text-io.hpp"
#include "overflow.hpp"

struct varpower_monomial
{
  int len;
  struct vp {
    int var;
    int exponent;
  } pairs[1];

  varpower_monomial() : len(0) {}
};

#define MAX_VAR 2147483647
#define MIN_EXP -2147483647
#define MAX_EXP 2147483647

static varpower_monomial staticFirst_;
static varpower_monomial *staticVP = &staticFirst_;

static varpower_monomial static2_;
static varpower_monomial *staticVP2 = &static2_;

static void insure_space(int len)
{
  if (len > staticVP->len)
    {
      staticVP = GETMEM_ATOMIC(varpower_monomial *, 
			       sizeof(int) + 2*len*sizeof(varpower_monomial::vp));
      staticVP->len = 2*len;
    }
}

static void insure_space2(int len)
{
  if (len > staticVP2->len)
    {
      staticVP2 = GETMEM_ATOMIC(varpower_monomial *, 
			       sizeof(int) + 2*len*sizeof(varpower_monomial::vp));
      staticVP2->len = 2*len;
    }
}

static void copy_to(varpower_monomial *m, int len, intarray &result)
{
  // the resulting size will be 2*len+1
  int newlen = 2*len+1;
  int *t = result.alloc(newlen);
  *t++ = newlen;
  varpower_monomial::vp *pairs = m->pairs;
  for (int i=0; i<len; i++)
    {
      *t++ = pairs->var;
      *t++ = pairs->exponent;
      pairs++;
    }
}

static bool check_var(int v, int e)
{
  if (v < 0 || v > MAX_VAR)
    {
      ERROR("Monomial expects variable number in range 0..%d",MAX_VAR);
      return false;
    }
  if (e < MIN_EXP || e > MAX_EXP || e == 0)
    {
      ERROR("Monomial expects non-zero exponents in range %d..%d",MIN_EXP,MAX_EXP);
      return false;
    }
  return true;
}

// TO REMOVE
#if 0
// int varpower::var(int n)
// {
//   return n >> 16;
// }
// 
// // TO REMOVE
// int varpower::exponent(int n)
// {
//   int e = 0x0000ffff & n;
//   if (e >= 0x00008000) e |= 0xffff0000;
//   return e;
// //  int res = n % (1 << 16);
// //  return (res > (1 << 15) ? -(res - (1 << 15)) : res);
// }
// 
// // TO REMOVE
// int check_exponent_pair(int v, int e)
// {
//   if (e > MAX_EXP || e < MIN_EXP)
//     ERROR("monomial overflow");
//   return (v << 16) | (0x0000ffff & e);
// }
// 
// // TO REMOVE
// int checked_pair(int v, int e)
// {
//   if (e > MAX_EXP || e < MIN_EXP)
//     ERROR("monomial overflow");
//   return (v << 16) | (0x0000ffff & e);
// }
// 
// // TO REMOVE
// int varpower::pair(int v, int e)
// {
//   return (v << 16) | (0x0000ffff & e);
// }
// 
// int varpower::degree_of(int n, const int *a)
// {
//   int len = *a++;
//   for (int i=0; i<len; i++)
//     if (var(a[i]) == n) return exponent(a[i]);
//   return 0;
// }
// 
// int varpower::compare(const int *a, const int *b)
//     // return EQ, LT, or GT for a == b, a < b, or a > b.
// {
//   int alen = *a++ - 1;
//   int blen = *b++ - 1;
//   if (alen > blen)
//     {
//       for (int i=0; i<blen; i++)
// 	{
// 	  int c = *a++ - *b++;
// 	  if (c == 0) continue;
// 	  if (c > 0) return GT;
// 	  return LT;
// 	}
//       return GT;
//     }
//   for (int i=0; i<alen; i++)
//     {
//       int c = *a++ - *b++;
//       if (c == 0) continue;
//       if (c > 0) return GT;
//       return LT;
//     }
//   if (alen == blen)
//     return EQ;
//   return LT;
// }  
#endif





void varpower::elem_text_out(buffer &o, const int *a)
{
  index_varpower i = a;
  if (!i.valid())
    o << "1";
  else
    for ( ; i.valid(); ++i)
      {
	int v = i.var();
	int e = i.exponent();
	if (v < 26) o << char('a' + v);
	else if (v < 52) o << char('A' + v - 26);
	else o << "x[" << v << "]";
	if (e > 1) o << e;
	else if (e < 0) o << "^(" << e << ")";
      }
}

void varpower::elem_text_out(buffer &o, const int *a, 
			     M2_stringarray varnames)
{
  index_varpower i = a;
  if (!i.valid())
    {
      if (p_one) o << "1";
    }
  else
    for ( ; i.valid(); ++i)
      {
	int v = i.var();
	int e = i.exponent();
	if (varnames->len < v)
	  o << ".";
	else
	  o << varnames->array[v];
	int single = (varnames->array[v]->len == 1);
	if (e > 1 && single) o << e;
	else if (e > 1) o << "^" << e;
	else if (e < 0) o << "^(" << e << ")";
//	if (i > 0) o << "*";
      }
}


bool varpower::is_one(const int *a) { return *a == 1; }

bool varpower::is_equal(const int *a, const int *b)
{
  if (*a != *b++) return false;
  int len = *a++;
  for (int i=1; i<len; i++)
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
      check_var(v,e); // Sets ERROR if a problem...
      result.append(3);
      result.append(v);
      result.append(e);
    }
}

void varpower::from_arrayint(M2_arrayint m, intarray &result)
{
  int len = m->len/2;
  insure_space(len);
  int *melems = m->array;
  varpower_monomial::vp *pairs = staticVP->pairs;
  for (int i=0; i<len; i++)
    {
      int v = *melems++;
      int e = *melems++;
      check_var(v,e);
      pairs->var = v;
      pairs->exponent = e;
      pairs++;
    }
  copy_to(staticVP, len, result);
}

M2_arrayint varpower::to_arrayint(const int *vp)
{
  int len = *vp;
  M2_arrayint result = makearrayint(len);
  for (int i=0; i<len; i++)
    result->array[i] = *vp++;
  return result;
}

int * varpower::copy(const int *vp, intarray &result)
{
  return result.copy(*vp, vp);
}

void varpower::to_ntuple(int n, const int *a, int *result_exponents)
{
  int *t = result_exponents;
  for (int j=0; j<n; j++) t[j] = 0;
  for (index_varpower i = a; i.valid(); ++i)
    {
      int v = i.var();
      int e = i.exponent();
      if (v < n) t[v] = e;
    }
}

void varpower::from_ntuple(int n, const int *a, intarray &result)
{
  insure_space(n);
  varpower_monomial::vp *pairs = staticVP->pairs;
  for (int i=n-1; i>=0; i--)
    if (a[i] != 0)
      {
	pairs->var = i;
	pairs->exponent = a[i];
	pairs++;
      }
  int len = pairs - staticVP->pairs;
  copy_to(staticVP, len, result);
}

int varpower::simple_degree(const int *a)
{
  int deg = 0;
  for (index_varpower i = a; i.valid(); ++i)
    deg += i.exponent();
  return deg;
}

void varpower::mult(const int *a, const int *b, intarray &result)
{
  insure_space(*a + *b);
  varpower_monomial::vp *pairs = staticVP->pairs;
  index_varpower i = a;
  index_varpower j = b;

  // merge the two varpowers to staticVP
  int va = (i.valid() ? i.var() : -1);
  int vb = (j.valid() ? j.var() : -1);
  for (;;)
    {
      if (va > vb)
	{
	  pairs->var = va;
	  pairs->exponent = i.exponent();
	  ++pairs;
	  ++i;
	  va = (i.valid() ? i.var() : -1);
	}
      else if (vb > va)
	{
	  pairs->var = vb;
	  pairs->exponent = j.exponent();
	  ++pairs;
	  ++j;
	  vb = (j.valid() ? j.var() : -1);
	}
      else 
	{
	  if (va == -1) break;
	  long x = i.exponent();
	  long y = j.exponent();
	  long z = x+y;
	  if ((x < 0) == (y < 0) && (x < 0) != (z < 0))
	    {
	      ERROR("monomial overflow");
	    }
	  else
	    {
	      if (z != 0)
		{
		  pairs->var = va;
		  pairs->exponent = z;
		  pairs++;
		}
	    }
	  ++i; ++j;
	  va = (i.valid() ? i.var() : -1);
	  vb = (j.valid() ? j.var() : -1);
	}
    }
  copy_to(staticVP, pairs - staticVP->pairs, result);
}

void varpower::quotient(const int *a, const int *b, intarray &result)
    // return a:b
{
  insure_space(*a + *b);
  varpower_monomial::vp *pairs = staticVP->pairs;
  index_varpower i = a;
  index_varpower j = b;

  int va = (i.valid() ? i.var() : -1);
  int vb = (j.valid() ? j.var() : -1);
  for (;;)
    {
      if (va > vb)
	{
	  pairs->var = va;
	  pairs->exponent = i.exponent();
	  ++pairs;
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
	      pairs->var = va;
	      pairs->exponent = ea-eb; // overflow cannot occur
	      pairs++;
	    }
	  ++i; ++j;
	  va = (i.valid() ? i.var() : -1);
	  vb = (j.valid() ? j.var() : -1);
	}
    }
  copy_to(staticVP, pairs - staticVP->pairs, result);
}

void varpower::power(const int *a, int n, intarray &result)
{
  if (n == 0) 
    {
      result.append(1); 
      return;
    }
  insure_space(*a);
  varpower_monomial::vp *pairs = staticVP->pairs;
  for (index_varpower i = a; i.valid(); ++i)
    {
      pairs->var = i.var();
      pairs++->exponent = safe::mult(i.exponent(),n);
    }
  copy_to(staticVP, pairs - staticVP->pairs, result);
}

bool varpower::divides(const int *b, const int *a)
    // (Note the switch in order of paramters.  Does b divide a?
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
	  if (ea < eb)
	    return false;
	  ++i; ++j;
	  va = (i.valid() ? i.var() : -1);
	  vb = (j.valid() ? j.var() : -1);
	}
    }
}

void varpower::monsyz(const int *a, const int *b, 
		      intarray &sa, intarray &sb)
// sa, sb are set so that a * sa = b * sb
// and sa, sb have disjoint support.
{
  index_varpower i = a;
  index_varpower j = b;
  insure_space(*b);
  insure_space2(*a);
  varpower_monomial::vp *sa1 = staticVP->pairs;
  varpower_monomial::vp *sb1 = staticVP2->pairs;

  int va = (i.valid() ? i.var() : -1);
  int vb = (j.valid() ? j.var() : -1);
  for (;;)
    {
      if (va > vb)
	{
	  // va should be placed into sb
	  sb1->var = va;
	  sb1->exponent = i.exponent();
	  sb1++;
	  ++i;
	  va = (i.valid() ? i.var() : -1);
	}
      else if (va < vb)
	{
	  // vb should be placed into sa
	  sa1->var = vb;
	  sa1->exponent = j.exponent();
	  sa1++;
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
	      sa1->var = va;
	      sa1->exponent = eb-ea;
	      sa1++;
	    }
	  else if (ea > eb)
	    {
	      sb1->var = va;
	      sb1->exponent = ea-eb;
	      sb1++;
	    }
	  ++i; ++j;
	  va = (i.valid() ? i.var() : -1);
	  vb = (j.valid() ? j.var() : -1);
	}
    }
  copy_to(staticVP, sa1-staticVP->pairs, sa);
  copy_to(staticVP2, sb1-staticVP2->pairs, sb);
}

void varpower::lcm(const int *a, const int *b, intarray &result)
{
  insure_space(*a + *b);
  varpower_monomial::vp *pairs = staticVP->pairs;
  index_varpower i = a;
  index_varpower j = b;

  // merge the two varpowers to staticVP
  int va = (i.valid() ? i.var() : -1);
  int vb = (j.valid() ? j.var() : -1);
  for (;;)
    {
      if (va > vb)
	{
	  pairs->var = va;
	  pairs->exponent = i.exponent();
	  ++pairs;
	  ++i;
	  va = (i.valid() ? i.var() : -1);
	}
      else if (vb > va)
	{
	  pairs->var = vb;
	  pairs->exponent = j.exponent();
	  ++pairs;
	  ++j;
	  vb = (j.valid() ? j.var() : -1);
	}
      else 
	{
	  if (va == -1) break;
	  int ea = i.exponent();
	  int eb = j.exponent();  
	  if (ea < eb) ea = eb;
	  pairs->var = va;
	  pairs->exponent = ea;
	  pairs++;
	  ++i; ++j;
	  va = (i.valid() ? i.var() : -1);
	  vb = (j.valid() ? j.var() : -1);
	}
    }
  copy_to(staticVP, pairs - staticVP->pairs, result);
}

void varpower::gcd(const int *a, const int *b, intarray &result)
{
  insure_space(*a + *b);
  varpower_monomial::vp *pairs = staticVP->pairs;
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
	  pairs->var = va;
	  pairs->exponent = ea;
	  pairs++;
	  ++i; ++j;
	  va = (i.valid() ? i.var() : -1);
	  vb = (j.valid() ? j.var() : -1);
	}
    }
  copy_to(staticVP, pairs - staticVP->pairs, result);
}

void varpower::erase(const int *a, const int *b, intarray &result)
    // divide a by b^infinity
{
  insure_space(*a + *b);
  varpower_monomial::vp *pairs = staticVP->pairs;
  index_varpower i = a;
  index_varpower j = b;

  int va = (i.valid() ? i.var() : -1);
  int vb = (j.valid() ? j.var() : -1);
  for (;;)
    {
      if (va > vb)
	{
	  pairs->var = va;
	  pairs->exponent = i.exponent();
	  ++pairs;
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
	  ++i; ++j;
	  va = (i.valid() ? i.var() : -1);
	  vb = (j.valid() ? j.var() : -1);
	}
    }
  copy_to(staticVP, pairs - staticVP->pairs, result);
}

void varpower::radical(const int *a, intarray &result)
{
  insure_space(*a);
  varpower_monomial::vp *pairs = staticVP->pairs;
  for (index_varpower i = a; i.valid(); ++i)
    {
      pairs->var = i.var();
      pairs->exponent = 1;
      pairs++;
    }
  copy_to(staticVP, pairs - staticVP->pairs, result);
}



// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
