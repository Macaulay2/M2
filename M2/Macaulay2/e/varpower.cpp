// (c) 1995 Michael E. Stillman

#include "varpower.hpp"
#include "bin_io.hpp"
#include "text_io.hpp"

extern ostrstream *gError;

int varpower::max_mon_size(int n)
{
  return n+1;
}

void varpower::elem_text_out(ostream &o, const int *a)
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

void varpower::elem_text_out(ostream &o, const int *a, 
			     const array<char *> &varnames)
{
  int len = *a++ - 1;
  if (len == 0)
    { if (p_one) o << "1"; }
  else
    for (int i=len-1; i>=0; i--)
      {
	int v = var(a[i]);
	int e = exponent(a[i]);
	o << varnames[v];
	int single = (varnames[v][1] == '\0');
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
      result.append(2);
      result.append(pair(v,e));
    }
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
      result.append(pair(i,a[i]));
  result[csize] = result.length() - csize;
}

void varpower::from_binary(char *&s, int &len, intarray &result)
{
  // This routine expects input in the form:
  // [nvarpowers, v0, e0, v1, e1, ...]
  // where v0 < v1 < v2 < ...
  // and each ei != 0, but could be negative.
  int nvarpowers = bin_int_in(s,len);
  int lastlen = result.length();
  int *m = result.alloc(nvarpowers+1);
  *m++ = nvarpowers+1;
  int v, e, oldv = 0;
  for (int i=0; i<nvarpowers; i++)
    {
      v = bin_int_in(s,len);
      e = bin_int_in(s,len);
      if ((v < 0) || (i > 0 && v <= oldv))
	{
	  *gError << "badly formed monomial";
	  result.shrink(lastlen);
	  result.append(1);
	  return;
	}
      m[nvarpowers-1-i] = pair(v,e);
      oldv = v;
    }
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
	  if (c > 0) return LT;
	  return GT;
	}
      return LT;
    }
  for (int i=0; i<alen; i++)
    {
      int c = *a++ - *b++;
      if (c == 0) continue;
      if (c > 0) return LT;
      return GT;
    }
  if (alen == blen)
    return EQ;
  return GT;
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
	  result.append(pair(va, exponent(*a) + exponent(*b)));
	  a++; b++;
	  va = (--alen > 0 ? var(*a) : -1);
	  vb = (--blen > 0 ? var(*b) : -1);
	}
    }
  result[csize] = result.length() - csize;
}

void varpower::divide(const int *a, const int *b, intarray &result)
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
	  int e = exponent(*a) - exponent(*b);
	  if (e > 0)
	    result.append(pair(va, e));
	  a++; b++;
	  va = (--alen > 0 ? var(*a) : -1);
	  vb = (--blen > 0 ? var(*b) : -1);
	}
    }
  result[csize] = result.length() - csize;
}

void varpower::power(const int *a, int n, intarray &result)
{
  if (n <= 0) 
    {
      result.append(1); 
      return;
    }
  int len = *a;
  int *t = result.alloc(len);
  *t++ = *a++;
  for (int i=0; i<len; i++)
    {
      *t++ = pair(var(*a), n * exponent(*a));
      a++;
    }
}

int varpower::divides(const int *b, const int *a)
    // (Note the switch in order of paramters.  Does b divide a?
{
  int alen = *a++;
  int blen = *b++;
  if (alen < blen) return 0;
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
	return 0;
      else
	{
	  if (va == -1) return 1;
	  if (exponent(*a) < exponent(*b)) return 0;
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
	  result.append(pair(va, ::max(exponent(*a), exponent(*b))));
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
	  result.append(pair(va, ::min(exponent(*a), exponent(*b))));
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

void varpower::elem_bin_out(ostream &o, const int *a)
{
  int npowers = *a++ - 1;
  bin_int_out(o, npowers);
  for (int i=npowers-1; i>=0; i--)
    {
      bin_int_out(o, var(a[i]));
      bin_int_out(o, exponent(a[i]));
    }
}

