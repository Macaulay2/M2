// Copyright 2004 Michael E. Stillman.

#include "comp_res.hpp"
#include "res.hpp"

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
  switch (algorithm) {
  case 1 : 
    if (!resolve_cokernel)
      {
	ERROR("resolution algorithm 1 cannot resolve a cokernel with a given presentation: use algorithm 2 or 3 instead");
	return 0;
      }
    return new res_comp(m, max_level, strategy);
  }
}

#if 0
  // This is here just to make writing the above routine easier...
void cmd_res(object &om, object &oalg, object &olength, object &odegree, object &ostrategy)
{
  Matrix m = om->cast_to_Matrix();
  int alg = oalg->int_of();
  int maxlev = olength->int_of();
  int strategy = ostrategy->int_of();
  intarray *deg = odegree->intarray_of();
  bool usedeg;
  int maxdeg, origsyz;
  res2_comp *p0;
  res_comp *p1;
  gbres_comp *p2;
  if (deg->length() > 0)
    {
      usedeg = (deg->length() > 0);
      maxdeg = (*deg)[0];
    }
  else
    {
      usedeg = 0;
      maxdeg = 0;
    }
  switch (alg) {
  case 0:
    p0 = new res2_comp(m, maxlev, usedeg, maxdeg, strategy);
    gStack.insert(p0);
    break;
  case 1:
    p1 = new res_comp(m, maxlev, strategy);
    gStack.insert(p1);
    break;
  case 2:
    origsyz = m.n_cols();
    p2 = new gbres_comp(m, maxlev+1, origsyz, strategy);
    gStack.insert(p2);
    break;
  case 3:
    origsyz = m.n_cols();
    p2 = new gbres_comp(m, maxlev+1, origsyz, strategy | USE_HILB);
    gStack.insert(p2);
    break;
  default:
    gError << "Unknown algorithm for computing resolutions";
    break;
  }
}
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
