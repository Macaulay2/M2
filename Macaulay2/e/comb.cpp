// Copyright 1997 by Michael E. Stillman

#include "comb.hpp"
#include "text_io.hpp"

array_class < array<int> > comb::tab;

typedef unsigned long int ulong;

inline ulong range_safe_add(ulong a, ulong b)
{
  ulong c = a + b;
  if (c < a)
    {
      emit_line("ulong integer addition overflow");
      exit(-1);
    }
  return c;
}

void comb::expand(int nn, int dd)
{
  int n,d;

  for (d = tab.length(); d<=dd; d++)
    tab[d][0] = 0;
  for (n = tab[0].length(); n<=nn; n++) 
    tab[0][n] = 1;
  tab[0][0] = 1;

#if 0
//   // This is, I believe, the buggy version...
//   for (n = tab.length(); n<=nn; n++) 
//     tab[0][n] = 1;
//   for (d = tab[0].length(); d<=dd; d++)
//     tab[d][0] = 0;
//   tab[0][0] = 1;
#endif

  for(d = 1; d<=dd; d++)
    for(n = tab[d].length(); n<=nn; n++)
      tab[d][n] = range_safe_add(tab[d][n-1],tab[d-1][n-1]);
}

void comb::text_out(buffer &o)
{
  o << "length = " << tab.length() << newline;
  for(int i=0; i<tab.length(); i++)
    {
      o << '[' << tab[i].length() << "] ";
      for(int j=0; j < tab[i].length(); j++)
	o << tab[i][j] << ' ';
      o << newline;
    }
}

int comb::encode(int *a, int p)
{
  int result = 0;

  expand(a[p-1],p);
  for (int i=0; i<p; i++)
    result += binom(a[i],i+1);

  return result;
}

void comb::decode(int b, int *result, int p)
{
  int tmp = b;
  expand(0,p);
  int bot = 0, len = d_len(p) - 1, bit, i;

  while (binom(len++,p) <= tmp)
    expand(len,p);

  for (i=p; i>0; i--)
    {
      while(bit = len % 2, len >>= 1)
	{
	  if (binom(bot+len, i) <= tmp)
	    {
	      bot += len;
	      len += bit;
	    }
	}
      result[i-1] = bot;
      tmp -= binom(bot, i);
      len = bot+1;
      bot = 0;
    }
}

bool comb::increment(int p, int n, int *s)
{
  for (int i=0; i<p; i++)
    {
      // Attempt to increment this one element
      if ((i < p-1 && s[i]+1 < s[i+1])
	  || (i == p-1 && s[i]+1 < n))
	{
	  s[i]++;
	  for (int j=0; j<i; j++)
	    s[j] = j;
	  return true;
	}
    }
  return false;
}

bool comb::valid_subset(int p, int n, const int *s)
{
  // Return 1 iff s is a valid p-subset of 0..n-1.
  if (s[0] < 0 || s[0] > n-p) return false;
  for (int i=1; i<p; i++)
    if (s[i] <= s[i-1] || s[i] > n-p+i)
      return false;
  return true;
}

int comb::mult_subsets(int p, const int *s, int q, const int *t, int *&result)
{
  // Places the sorted value of [s0..s(p-1),t0..t(p-1)]
  // int 'result', and returns the sign of the permutation
  // required.  0 is returned if s and t have a common
  // element, in which case, the value in 'result'
  // is undefined.

  int a = 0;
  int b = 0;
  int c = 0;
  int sign = 0;
  if (p == 0 && q == 0) return 1;
  for (;;)
    {
      if (a >= p)
	{
	  while (b < q) 
	    result[c++] = t[b++];
	  break;
	}
      else if (b >= q)
	{
	  while (a < p) 
	    result[c++] = s[a++];
	  break;
	}
      if (s[a] > t[b])
	{
	  sign += p-a;
	  result[c++] = t[b++];
	}
      else if (s[a] < t[b])
	{
	  result[c++] = s[a++];
	}
      else return 0;
    }
  if ((sign % 2) == 0) return 1;
  return -1;
}

void pcombtab()
{
  printf("comb tab has length %d\n", comb::tab.length());
  for (int i=0; i<comb::tab.length(); i++)
    {
      printf("[%d] ", comb::tab[i].length());
      for (int j=0; j<comb::tab[i].length(); j++)
	printf("%d ",comb::tab[i][j]);
      printf("\n");
    }
}
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
