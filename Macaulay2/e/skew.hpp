#ifndef __skew_hpp_
#define __skew_hpp_
class SkewMultiplication
{
public:
  int _n_vars;
  int _n_skew;
  int * _skew_list; // An array 0.._n_skew-1, listing the indices of the skew comm variables.
  bool * _skew_exp; // 0.._n_vars-1

  int *_SKEW1;
  int *_SKEW2;

  SkewMultiplication();
  SkewMultiplication(int nvars, int nskew, int * skew_list);
  ~SkewMultiplication() {}

  int n_skew_vars() const { return _n_skew; }
  bool is_skew_var(int i) const { return _skew_exp[i]; }
  int skew_variable(int i) const { return _skew_list[i]; }

  int skew_vars(const int *exp, int *result) const;
  // The number s of skew variables in 'exp' is returned, and their
  // indices are placed in result[0], ..., result[s-1].
  // The space that 'result' points to MUST hold at least 'nskew' ints.

  int mult_sign(const int *exp1, const int *exp2) const;
  int diff(const int *exp1, const int *exp2, int *result) const;
  int divide(const int *exp1, const int *exp2, int *result) const;
  bool exp_is_zero(const int *exp) const;
};

#endif

#if 0



  int n_skew;			// If multiplication is skew commutative
				// (this is just used by rings, NOT by
				// monoid multiplication, although this may affect
				// implementation).
  
  int *skew_vars;		// 0..nvars-1: skew_vars[v] = 1 iff v has odd (skew)degree
  int *skew_list;		// 0..n_skew-1: skew_list[i] = (skew var in range 0..nvars-1)



  int *skew_mvars;
  int *skew_nvars;		// To save ALOT of allocations...

  bool is_skew() const;
  int is_skew_var(int v) const;
  int skew_mult_sign(const int *m, const int *n) const;
  int exp_skew_mult_sign(const int *exp1, const int *exp2) const;
  int skew_mult(const int *m, const int *n, int *result) const;
  int skew_divide(const int *m, const int *n, int *result) const;
      // If the result is s (1,or -1), then m = s * n * result
  int skew_diff(const int *m, const int *n, int *result) const;
      // m acting as a differential operator on n is s * result, s = 0, 1, or -1.
  int exp_skew_vars(const int *exp, int *result) const;
      // The number s of skew variables in 'exp' is returned, and their
      // indices are placed in result[0], ..., result[s-1].
  int skew_vars(const int *m, int *result) const;
      // The number s of skew variables in 'm' is returned, and their
      // indices are placed in result[0], ..., result[s-1].
  bool skew_is_zero(const int *exp) const;
      // Return whether any skew variable in the exponent vector has exponent >= 2


/*________________ From monoid.cpp moninfo creation ________________*/
  if (skewvariables == 0 || skewvariables->len == 0)
    {
      n_skew = 0;
      skew_vars = NULL;
      skew_list = NULL;
    }
  else
    {
      skew_vars = newarray_atomic(int,nvars);
      skew_list = newarray_atomic(int,nvars);
      n_skew = skewvariables->len;
      for (int j=0; j<nvars; j++)
	{
	  skew_vars[j] = skew_list[j] = 0;
	}
      for (int i=0; i<n_skew; i++)
	{
	  if (skewvariables->array[i] >= 0 && skewvariables->array[i] < nvars)
	    {
	      if (skew_vars[skewvariables->array[i]] != 0)
		n_skew--;
	      else
		{
		  skew_list[i] = skewvariables->array[i];
		  skew_vars[skew_list[i]] = 1;
		}
	    }
	  else
	    n_skew--;
	}
    }

  skew_mvars = newarray_atomic(int,nvars);
  skew_nvars = newarray_atomic(int,nvars);

  deletearray(skew_mvars);
  deletearray(skew_nvars);

  // Set the skew variable information needed.
  M2_arrayint skewvars = 0;
  int is_skew = options->array[2]; // should be 0, 1 or 2.
  if (is_skew == 1)
    {
      // Put the variables which have first degree odd into skewvars.
      int nskew = 0;
      if (eachdeg != 0)
	for (unsigned int i=0; i<nvars; i++)
	  if ((degs->array[i * eachdeg] % 2) != 0)
	    nskew++;

      skewvars = makearrayint(nskew);
      int next = 0;
      if (eachdeg != 0)
	for (unsigned int i=0; i<nvars; i++)
	  if ((degs->array[i * eachdeg] % 2) != 0)
	    skewvars->array[next++] = i;
    }
  else if (is_skew >= 2)
    {
      // The variables appear in 'opts'
      skewvars = makearrayint(options->len-3);
      for (unsigned int i=3; i<options->len; i++)
	skewvars->array[i-3] = options->array[i];
    }


bool Monoid::is_skew() const
{
  return (moninfo->n_skew > 0);
}

int Monoid::is_skew_var(int v) const
{
  return (moninfo->skew_vars[v]);
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

int Monoid::exp_skew_mult_sign(const int *exp1, const int *exp2) const
{
  int a = exp_skew_vars(exp1, skew_mvars);
  int b = exp_skew_vars(exp2, skew_nvars);
  return sort_sign(a,skew_mvars, b, skew_nvars);
}

int Monoid::skew_mult_sign(const int *m, const int *n) const
{
  int a = skew_vars(m, skew_mvars);
  int b = skew_vars(n, skew_nvars);
  return sort_sign(a,skew_mvars, b, skew_nvars);
}

int Monoid::skew_mult(const int *m, const int *n, int *result) const
{
  int sign = skew_mult_sign(m,n);
  mult(m,n,result);
  return sign;
}

int Monoid::skew_divide(const int *m, const int *n, int *result) const
    // If the result is s (1,or -1), then m = s * result * n
{
  divide(m,n,result);
  int sign = skew_mult_sign(result,n);
  return sign;
}

int Monoid::skew_diff(const int *m, const int *n, int *result) const
      // m acting as a differential operator on n is s * result, s = 0, 1, or -1.
{
  divide(n,m,result);
  int a = skew_vars(result, skew_mvars);
  int b = skew_vars(m, skew_nvars);
  int sign = sort_sign(a,skew_mvars, b, skew_nvars);
  int c = b % 4;
  if (c == 2 || c == 3) sign = -sign;
  return sign;
}

int Monoid::exp_skew_vars(const int *exp, int *result) const
    // The number s of skew variables in 'm' is returned, and their
    // indices are placed in result[0], ..., result[s-1].
    // The space that 'result' points to MUST hold at least 'nvars' ints.
{
  int i;
  int next = 0;
  for (i=0; i<moninfo->n_skew; i++)
    {
      int v = moninfo->skew_list[i];
      if (exp[v] > 0)
	result[next++] = v;
    }
  return next;
}
int Monoid::skew_vars(const int *m, int *result) const
    // The number s of skew variables in 'm' is returned, and their
    // indices are placed in result[0], ..., result[s-1].
    // The space that 'result' points to MUST hold at least 'nvars' ints.
{
  to_expvector(m, result);
  return exp_skew_vars(result,result); // This aliasing is ok...
}

bool Monoid::skew_is_zero(const int *exp) const
    // Return whether any skew variable in the exponent vector has exponent >= 2
{
  for (int i=0; i<moninfo->n_skew; i++)
    {
      int v = moninfo->skew_list[i];
      if (exp[v] >= 2) return true;
    }
  return false;
}
#endif


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
