// (c) 1995 Michael E. Stillman

#include "varpower.hpp"
#include "text_io.hpp"

#define MAX_VAR 32767
#define MIN_EXP -32768
#define MAX_EXP 32767

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

int varpower::max_mon_size(int n)
{
  return n+1;
}

void varpower::elem_text_out(buffer &o, const int *a)
{
  int len = *a++ - 1;
  if (len == 0)
    o << "1";
  else
    for (int i=len-1; i>=0; i--)
      {
	int v = var(a[i]);
	int e = exponent(a[i]);
	if (v < 26) o << char('a' + v);
	else if (v < 52) o << char('A' + v - 26);
	else o << "x[" << v << "]";
	if (e > 1) o << e;
	else if (e < 0) o << "^(" << e << ")";
      }
}

void varpower::elem_text_out(buffer &o, const int *a, 
			     const M2_stringarray varnames)
{
  int len = *a++ - 1;
  if (len == 0)
    { if (p_one) o << "1"; }
  else
    for (int i=len-1; i>=0; i--)
      {
	unsigned int v = var(a[i]);
	int e = exponent(a[i]);
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

int varpower::var(int n)
{
  return n >> 16;
}

int varpower::exponent(int n)
{
  int e = 0x0000ffff & n;
  if (e >= 0x00008000) e |= 0xffff0000;
  return e;
//  int res = n % (1 << 16);
//  return (res > (1 << 15) ? -(res - (1 << 15)) : res);
}

int varpower::pair(int v, int e)
{
  return (v << 16) | (0x0000ffff & e);
}

int check_exponent_pair(int v, int e)
{
  if (e > MAX_EXP || e < MIN_EXP)
    ERROR("monomial overflow");
  return (v << 16) | (0x0000ffff & e);
}

int checked_pair(int v, int e)
{
  if (e > MAX_EXP || e < MIN_EXP)
    ERROR("monomial overflow");
  return (v << 16) | (0x0000ffff & e);
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

bool varpower::is_nonneg(const int *) { return true; }

int varpower::topvar(const int *a)
{
  assert(*a > 1);
  return var(a[1]);
}

void varpower::one(intarray &result) { result.append(1); }

void varpower::var(int v, int e, intarray &result)
{
  if (e == 0)
    result.append(1);
  else
    {
      check_var(v,e); // Sets ERROR if a problem...
      result.append(2);
      result.append(pair(v,e));
    }
}

void varpower::from_arrayint(M2_arrayint m, intarray &result)
{
  int len = m->len;
  result.append(len/2+1);
  for (int i=0; i<len; i+=2)
    {
      int v = m->array[i];
      int e = m->array[i+1];
      check_var(v,e);
      result.append(pair(v,e));
    }
}

M2_arrayint varpower::to_arrayint(const int *vp)
{
  int len = 2*((*vp)-1);
  M2_arrayint result = makearrayint(len);
  for (int i=0; i<len; i += 2)
    {
      vp++;
      result->array[i] = var(*vp);
      result->array[i+1] = exponent(*vp);
    }
  return result;
}
int * varpower::copy(const int *vp, intarray &result)
{
  return result.copy(*vp, vp);
}

void varpower::to_varpower(const int *a, intarray &result)
{
  int len = *a;
  int *t = result.alloc(len);
  for (int i=0; i<len; i++)
    *t++ = *a++;
}

void varpower::from_varpower(const int *a, intarray &result)
{
  to_varpower(a,result);
}

void varpower::to_ntuple(int n, const int *a, intarray &result)
{
  int i;
  int *t = result.alloc(n);
  for (i=0; i<n; i++) t[i] = 0;
  int len = *a++ - 1;
  for (i=0; i<len; i++)
    {
      int v = var(*a);
      if (v < n) t[v] = exponent(*a);
      a++;
    }
}

void varpower::from_ntuple(int n, const int *a, intarray &result)
{
  int csize = result.start();
  for (int i=n-1; i>=0; i--)
    if (a[i] != 0)
      result.append(checked_pair(i,a[i]));
  result[csize] = result.length() - csize;
}

int varpower::simple_degree(const int *a)
{
  int deg = 0;
  int len = *a++;
  for (int i=1; i<len; i++)
    deg += exponent(*a++);
  return deg;
}

int varpower::degree_of(int n, const int *a)
{
  int len = *a++;
  for (int i=0; i<len; i++)
    if (var(a[i]) == n) return exponent(a[i]);
  return 0;
}

int varpower::compare(const int *a, const int *b)
    // return EQ, LT, or GT for a == b, a < b, or a > b.
{
  int alen = *a++ - 1;
  int blen = *b++ - 1;
  if (alen > blen)
    {
      for (int i=0; i<blen; i++)
	{
	  int c = *a++ - *b++;
	  if (c == 0) continue;
	  if (c > 0) return GT;
	  return LT;
	}
      return GT;
    }
  for (int i=0; i<alen; i++)
    {
      int c = *a++ - *b++;
      if (c == 0) continue;
      if (c > 0) return GT;
      return LT;
    }
  if (alen == blen)
    return EQ;
  return LT;
}  

void varpower::mult(const int *a, const int *b, intarray &result)
{
  int csize = result.start();
  int alen = *a++;
  int blen = *b++;
  int va = (--alen > 0 ? var(*a) : -1);
  int vb = (--blen > 0 ? var(*b) : -1);
  for (;;)
    {
      if (va > vb)
	{
	  result.append(*a++);
	  va = (--alen > 0 ? var(*a) : -1);
	}
      else if (va < vb)
	{
	  result.append(*b++);
	  vb = (--blen > 0 ? var(*b) : -1);
	}
      else
	{
	  if (va == -1) break;
	  int e = exponent(*a) + exponent(*b);
	  if (e != 0)
	    result.append(checked_pair(va, e));
	  a++; b++;
	  va = (--alen > 0 ? var(*a) : -1);
	  vb = (--blen > 0 ? var(*b) : -1);
	}
    }
  result[csize] = result.length() - csize;
}

void varpower::quotient(const int *a, const int *b, intarray &result)
    // divide a by b.
{
  int csize = result.start();
  int alen = *a++;
  int blen = *b++;
  int va = (--alen > 0 ? var(*a) : -1);
  int vb = (--blen > 0 ? var(*b) : -1);
  for (;;)
    {
      if (va > vb)
	{
	  result.append(*a++);
	  va = (--alen > 0 ? var(*a) : -1);
	}
      else if (va < vb)
	{
	  b++;
	  vb = (--blen > 0 ? var(*b) : -1);
	}
      else
	{
	  if (va == -1) break;
	  int ea = exponent(*a);
	  int eb = exponent(*b);
	  if (ea > eb)
	    {
	      int e = ea-eb;
	      result.append(pair(va, e)); // overflow cannot occur
	    }
	  a++; b++;
	  va = (--alen > 0 ? var(*a) : -1);
	  vb = (--blen > 0 ? var(*b) : -1);
	}
    }
  result[csize] = result.length() - csize;
}

void varpower::power(const int *a, int n, intarray &result)
{
  if (n == 0) 
    {
      result.append(1); 
      return;
    }
  int len = *a;
  int *t = result.alloc(len);
  *t++ = *a++;
  for (int i=0; i<len; i++)
    {
      *t++ = checked_pair(var(*a), n * exponent(*a));
      a++;
    }
}

bool varpower::divides(const int *b, const int *a)
    // (Note the switch in order of paramters.  Does b divide a?
{
  int alen = *a++;
  int blen = *b++;
  if (alen < blen) return false;
  int va = (--alen > 0 ? var(*a) : -1);
  int vb = (--blen > 0 ? var(*b) : -1);
  for (;;)
    {
      if (va > vb)
	{
	  a++;
	  va = (--alen > 0 ? var(*a) : -1);
	}
      else if (va < vb)
	return false;
      else
	{
	  if (va == -1) return true;
	  if (exponent(*a) < exponent(*b)) return false;
	  a++; b++;
	  va = (--alen > 0 ? var(*a) : -1);
	  vb = (--blen > 0 ? var(*b) : -1);
	}
    }
}

void varpower::monsyz(const int *a, const int *b, 
		      intarray &sa, intarray &sb)
{
  int sasize = sa.start();
  int sbsize = sb.start();
  int alen = *a++;
  int blen = *b++;
  int va = (--alen > 0 ? var(*a) : -1);
  int vb = (--blen > 0 ? var(*b) : -1);
  for (;;)
    {
      if (va > vb)
	{
	  sb.append(*a++);
	  va = (--alen > 0 ? var(*a) : -1);
	}
      else if (va < vb)
	{
	  sa.append(*b++);
	  vb = (--blen > 0 ? var(*b) : -1);
	}
      else
	{
	  if (va == -1) break;
	  int v = exponent(*a) - exponent(*b);
	  if (v > 0)
	    sb.append(pair(va, v));
	  else if (v < 0)
	    sa.append(pair(va, -v));
	  a++; b++;
	  va = (--alen > 0 ? var(*a) : -1);
	  vb = (--blen > 0 ? var(*b) : -1);
	}
    }
  sa[sasize] = sa.length() - sasize;
  sb[sbsize] = sb.length() - sbsize;
}

void varpower::lcm(const int *a, const int *b, intarray &result)
{
  int csize = result.start();
  int alen = *a++;
  int blen = *b++;
  int va = (--alen > 0 ? var(*a) : -1);
  int vb = (--blen > 0 ? var(*b) : -1);
  for (;;)
    {
      if (va > vb)
	{
	  result.append(*a++);
	  va = (--alen > 0 ? var(*a) : -1);
	}
      else if (va < vb)
	{
	  result.append(*b++);
	  vb = (--blen > 0 ? var(*b) : -1);
	}
      else
	{
	  if (va == -1) break;
	  int ae = exponent(*a);
	  int be = exponent(*b);
	  if (be > ae) ae = be;
	  result.append(pair(va, ae));
	  a++; b++;
	  va = (--alen > 0 ? var(*a) : -1);
	  vb = (--blen > 0 ? var(*b) : -1);
	}
    }
  result[csize] = result.length() - csize;
}

void varpower::gcd(const int *a, const int *b, intarray &result)
{
  int csize = result.start();
  int alen = *a++;
  int blen = *b++;
  int va = (--alen > 0 ? var(*a) : -1);
  int vb = (--blen > 0 ? var(*b) : -1);
  for (;;)
    {
      if (va > vb)
	{
	  a++;
	  va = (--alen > 0 ? var(*a) : -1);
	}
      else if (va < vb)
	{
	  b++;
	  vb = (--blen > 0 ? var(*b) : -1);
	}
      else
	{
	  if (va == -1) break;
	  int ae = exponent(*a);
	  int be = exponent(*b);
	  if (be < ae) ae = be;
	  result.append(pair(va, ae));
	  a++; b++;
	  va = (--alen > 0 ? var(*a) : -1);
	  vb = (--blen > 0 ? var(*b) : -1);
	}
    }
  result[csize] = result.length() - csize;
}

void varpower::erase(const int *a, const int *b, intarray &result)
    // divide a by b^infinity
{
  int csize = result.start();
  int alen = *a++;
  int blen = *b++;
  int va = (--alen > 0 ? var(*a) : -1);
  int vb = (--blen > 0 ? var(*b) : -1);
  for (;;)
    {
      if (va > vb)
	{
	  result.append(*a++);
	  va = (--alen > 0 ? var(*a) : -1);
	}
      else if (va < vb)
	{
	  b++;
	  vb = (--blen > 0 ? var(*b) : -1);
	}
      else
	{
	  if (va == -1) break;
	  a++; b++;
	  va = (--alen > 0 ? var(*a) : -1);
	  vb = (--blen > 0 ? var(*b) : -1);
	}
    }
  result[csize] = result.length() - csize;
}

void varpower::radical(const int *a, intarray &result)
{
  int len = *a;
  int *t = result.alloc(len);
  *t++ = *a++;
  for (int i=1; i<len; i++)
    *t++ = pair(var(*a++), 1);
}


