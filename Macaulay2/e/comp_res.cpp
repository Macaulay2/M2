// Copyright 2004 Michael E. Stillman.

#include "text_io.hpp"
#include "comp_res.hpp"
#include "res.hpp"
#include "res2.hpp"
#include "gb2.hpp"

ResolutionComputation::ResolutionComputation()
{
}

ResolutionComputation::~ResolutionComputation()
{
}

ResolutionComputation *ResolutionComputation::choose_res(const Matrix *m,
							 M2_bool resolve_cokernel,
							 int max_level,
							 M2_bool use_max_slanted_degree,
							 int max_slanted_degree,
							 int algorithm,
							 int strategy
							 )
{
  int origsyz;
  switch (algorithm) {
  case 1 : 
    if (!resolve_cokernel)
      {
	ERROR("resolution Strategy=>1 cannot resolve a cokernel with a given presentation: use Strategy=>2 or Strategy=>3 instead");
	return 0;
      }
    if (gbTrace > 0) emit_line("resolution Strategy=>1");
    return new res_comp(m, max_level, strategy);
  case 0: 
    if (!resolve_cokernel)
      {
	ERROR("resolution Strategy=>0 cannot resolve a cokernel with a given presentation: use Strategy=>2 or Strategy=>3 instead");
	return 0;
      }
    if (gbTrace > 0) emit_line("resolution Strategy=>0");
    return new res2_comp(m, max_level, use_max_slanted_degree, max_slanted_degree, strategy);
  case 2 : 
    origsyz = m->n_cols();
    if (gbTrace > 0) emit_line("resolution Strategy=>2");
    return new gbres_comp(m, max_level+1, origsyz, strategy);
  case 3: 
    origsyz = m->n_cols();
    if (gbTrace > 0) emit_line("resolution Strategy=>3");
    return new gbres_comp(m, max_level+1, origsyz, strategy | STRATEGY_USE_HILB);

  }

  ERROR("unknown resolution algorithm");
  return 0;
}

void ResolutionComputation::betti_init(int lo, int hi, int len, int *&bettis) const
{
  int z = (hi-lo+1) * (len+1);
  bettis = newarray(int,z);
  for (int i=0; i<z; i++)
    bettis[i] = 0;
}

M2_arrayint ResolutionComputation::betti_make(int lo, int hi, int len, int *bettis) const
{
  int d, lev;
  int hi1 = hi+1;
  int len1 = len+1;

  // Reset 'hi1' to reflect the top degree that occurs
  for (d=hi; d >= lo; d--)
    {
      for (lev=0; lev<=len; lev++)
	if (bettis[lev+(len+1)*(d-lo)] > 0)
	  {
	    hi1 = d;
	    break;
	  }
      if (hi1 <= hi) break;
    }
  if (hi1 > hi) hi1 = hi;

  // Reset 'len1' to reflect the top level that occurs
  for (lev=len; lev>=0; lev--)
    {
      for (d=lo; d<=hi1; d++)
	if (bettis[lev+(len+1)*(d-lo)] > 0)
	  {
	    len1 = lev;
	    break;
	  }
      if (len1 <= len) break;
    }
  if (len1 > len) len1 = len;

  int totallen = (hi1-lo+1)*(len1+1);
  M2_arrayint result = makearrayint(3 + totallen);

  result->array[0] = lo;
  result->array[1] = hi1;
  result->array[2] = len1;

  int next = 3;
  for (d=lo; d<=hi1; d++)
    for (lev=0; lev<=len1; lev++)
      result->array[next++] = bettis[lev+(len+1)*(d-lo)];

  return result;
}

void ResolutionComputation::betti_display(buffer &o, M2_arrayint ar) const
{
  int *a = ar->array;
  int total_sum = 0;
  int lo = a[0];
  int hi = a[1];
  int len = a[2]+1;
  o << "total  ";
  for (int lev=0; lev<len; lev++)
    {
      int sum = 0;
      for (int d=lo; d<=hi; d++)
	sum += a[len*(d-lo)+lev+3];
      total_sum += sum;
      o.put(sum, 6);
      o << ' ';
    }
  o << " [" << total_sum << "]" << newline;
  for (int d=lo; d<=hi; d++)
    {
      o.put(d, 5);
      o << ": ";
      for (int lev=0; lev<len; lev++)
	{
	  int c = a[len*(d-lo) + lev + 3];
	  if (c != 0)
	    o.put(c, 6);
	  else
	    o << "     -";
	  o << " ";
	}
      o << newline;
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
