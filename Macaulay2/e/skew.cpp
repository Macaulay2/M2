#include "skew.hpp"
#include "newdelete.hpp"

SkewMultiplication::SkewMultiplication()
  : _n_vars(0),
    _n_skew(0),
    _skew_list(0),
    _skew_exp(0),
    _SKEW1(0),
    _SKEW2(0)
{
}

SkewMultiplication::SkewMultiplication(int nvars, int nskew, int * skew_list)
  : _n_vars(nvars),
    _n_skew(nskew),
    _skew_list(skew_list),
    _skew_exp(0),
    _SKEW1(0),
    _SKEW2(0)
{
  _skew_exp = newarray_atomic(bool,nvars);
  for (int i=0; i<nvars; i++)
    _skew_exp[i] = false;
  for (int v=0; v<nskew; v++)
    _skew_exp[skew_list[v]] = true;

  _SKEW1 = newarray_atomic(int,nskew);
  _SKEW2 = newarray_atomic(int,nskew);
}

static int sort_sign(int a, int *v1, int b, int *v2)
{
  if (a == 0 || b == 0) return 1;
  int result = 0; // number of sign switches
  a--;
  b--;
  for (;;)
    {
      if (v1[a] < v2[b])
	{
	  b--;
	  if (b < 0)
	    {
	      return (result % 2 == 0 ? 1 : -1);
	    }
	}
      else if (v1[a] > v2[b])
	{
	  result += b+1;
	  a--;
	  if (a < 0)
	    {
	      return (result % 2 == 0 ? 1 : -1);
	    }
	}
      else 
	return 0;
    }
}

int SkewMultiplication::skew_vars(const int *exp, int *result) const
    // The number s of skew variables in 'exp' is returned, and their
    // indices are placed in result[0], ..., result[s-1].
    // The space that 'result' points to MUST hold at least n_skew ints
{
  int i;
  int next = 0;
  for (i=0; i<_n_skew; i++)
    {
      int v = _skew_list[i];
      if (exp[v] > 0)
	result[next++] = v;
    }
  return next;
}

int SkewMultiplication::mult_sign(const int *exp1, const int *exp2) const
{
  int a = skew_vars(exp1, _SKEW1);
  int b = skew_vars(exp2, _SKEW2);
  return sort_sign(a,_SKEW1, b, _SKEW2);
}

int SkewMultiplication::diff(const int *exp1, const int *exp2, int *result) const
      // exp1 acting as a differential operator on exp2 is s * result, s = 0, 1, or -1.
{
  for (int i=0; i<_n_vars; i++)
    {
      int cmp = exp2[i] - exp1[i];
      if (cmp < 0) return 0;
      result[i] = cmp;
    }
  int a = skew_vars(result, _SKEW1);
  int b = skew_vars(exp1, _SKEW2);
  int sign = sort_sign(a,_SKEW1, b, _SKEW2);
  int c = b % 4;
  if (c == 2 || c == 3) sign = -sign;
  return sign;
}

int SkewMultiplication::divide(const int *exp1, const int *exp2, int *result) const
    // If the result is s (1,or -1), then exp1 = s * result * exp2
{
  for (int i=0; i<_n_vars; i++)
    {
      int cmp = exp1[i] - exp2[i];
      if (cmp < 0) return 0;
      result[i] = cmp;
    }
  int sign = mult_sign(result,exp2);
  return sign;
}

bool SkewMultiplication::exp_is_zero(const int *exp) const
    // Return whether any skew variable in the exponent vector has exponent >= 2
{
  for (int i=0; i<_n_skew; i++)
    {
      int v = _skew_list[i];
      if (exp[v] >= 2) return true;
    }
  return false;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
