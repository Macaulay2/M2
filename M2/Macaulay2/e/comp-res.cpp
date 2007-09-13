// Copyright 2004 Michael E. Stillman.

#include "text-io.hpp"
#include "comp-res.hpp"
#include "res-a1.hpp"
#include "res-a0.hpp"
#include "res-a2.hpp"

static int nfinalized = 0;
static int nremoved = 0;

//////////////////////////////////////////////
// EngineResolutionComputation ///////////////
//////////////////////////////////////////////
EngineResolutionComputation::EngineResolutionComputation(ResolutionComputation *C0)
  : C(C0)
{
}

EngineResolutionComputation *EngineResolutionComputation::create(ResolutionComputation *C0)
{
  EngineResolutionComputation *E = new EngineResolutionComputation(C0);
  intern_computation(E);
  return E;
}

EngineResolutionComputation::~EngineResolutionComputation()
{
  destroy();
}

void EngineResolutionComputation::destroy()
{
  if (C) delete C;
}

void EngineResolutionComputation::start_computation()
{
  if (C == 0) return;
  long deg;
  //  ComputationStatusCode ret = C->compute(stop_,deg);
  //  set_status(ret);
}

long EngineResolutionComputation::complete_thru_degree() const
{
  return C->complete_thru_degree();
}
//////////////////////////////////////////////

void remove_res(void *p, void *cd)
{
  ResolutionComputation *G = static_cast<ResolutionComputation *>(p);
  nremoved++;
  if (gbTrace>=3)
    fprintf(stderr, "\nremoving res %d at %p\n",nremoved, G);
  G->remove_res();
}

void intern_res(ResolutionComputation *G)
{
  GC_REGISTER_FINALIZER(G,remove_res,0,0,0);
  nfinalized++;
  if (gbTrace>=3)
    fprintf(stderr, "\n   -- registering res %d at %p\n", nfinalized, (void *)G);
}

ResolutionComputation::ResolutionComputation()
{
}

ResolutionComputation::~ResolutionComputation()
{
}

void ResolutionComputation::remove_res()
{
  // This is the default behavior: doing nothing 
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
  const Ring *R = m->get_ring();
  ResolutionComputation *C = 0;
  int origsyz;
  switch (algorithm) {
  case 1 : 
    if (!resolve_cokernel)
      {
	ERROR("resolution Strategy=>1 cannot resolve a cokernel with a given presentation: use Strategy=>2 or Strategy=>3 instead");
	return 0;
      }
    if (!R->is_commutative_ring())
      {
	ERROR("use resolution Strategy=>2 or Strategy=>3 for non commutative polynomial rings");
	return 0;
      }
    if (gbTrace > 0) emit_line("resolution Strategy=>1");
    C = new res_comp(m, max_level, strategy);
    break;
  case 0: 
    if (!resolve_cokernel)
      {
	ERROR("resolution Strategy=>0 cannot resolve a cokernel with a given presentation: use Strategy=>2 or Strategy=>3 instead");
	return 0;
      }
    if (!R->is_commutative_ring())
      {
	ERROR("use resolution Strategy=>2 or Strategy=>3 for non commutative polynomial rings");
	return 0;
      }
    if (gbTrace > 0) emit_line("resolution Strategy=>0");
    C = new res2_comp(m, max_level, use_max_slanted_degree, max_slanted_degree, strategy);
    break;
  case 2 : 
    origsyz = m->n_cols();
    if (gbTrace > 0) emit_line("resolution Strategy=>2");
    C = new gbres_comp(m, max_level+1, origsyz, strategy);
    break;
  case 3: 
    origsyz = m->n_cols();
    if (gbTrace > 0) emit_line("resolution Strategy=>3");
    C = new gbres_comp(m, max_level+1, origsyz, strategy | STRATEGY_USE_HILB);
    break;
  }

  if (C == 0)
    {
      ERROR("unknown resolution algorithm");
      return 0;
    }
  intern_res(C);
  return C;
}

void ResolutionComputation::betti_init(int lo, int hi, int len, int *&bettis) const
{
  int z = (hi-lo+1) * (len+1);
  bettis = newarray_atomic_clear(int,z);
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
