// Copyright 2005 Michael E. Stillman.

#include <map>
#include <vector>
#include <ctime>

#include "../text_io.hpp"
#include "../debug.hpp"
#include "../z_mod_p.hpp"
#include "../matrix.hpp"
#include "../mat.hpp"
#include "../matrixcon.hpp"
#include "../dmat-LU.hpp"

#include "F4.hpp"

static clock_t clock_sort_columns = 0;
static clock_t clock_gauss = 0;
static clock_t clock_make_matrix = 0;

template<typename CoeffRing, typename MonomialInfo>
F4<CoeffRing,MonomialInfo>::F4(const RingType *K0,
			       const Matrix *m, 
			       M2_bool collect_syz, 
			       int n_rows_to_keep,
			       M2_arrayint gb_weights,
			       int strategy, 
			       M2_bool use_max_degree,
			       int max_degree)
{
}

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
