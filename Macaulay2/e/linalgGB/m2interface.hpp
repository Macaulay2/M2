// Copyright 2005 Michael E. Stillman

#ifndef __m2interface_hpp_
#define __m2interface_hpp_

class GBComputation;
class Matrix;

GBComputation *createLinearAlgebraGB(const Matrix *m,
				     M2_bool collect_syz,
				     int n_rows_to_keep,
				     M2_arrayint gb_weights,
				     int strategy,
				     M2_bool use_max_degree,
				     int max_degree);

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
//  End:


