// Copyright 2004 Michael E. Stillman.

#ifndef _comp_res_hpp_
#define _comp_res_hpp_

#include "computation.hpp"
class buffer;

class ResolutionComputation : public Computation
// This is the base type for all resolution computations
{
protected:
  ResolutionComputation();
  virtual ~ResolutionComputation();

  virtual bool stop_conditions_ok() = 0;
  // If the stop conditions in stop_ are inappropriate,
  // return false, and use ERROR(...) to provide an error message.

public:
  virtual ResolutionComputation * cast_to_ResolutionComputation() { return this;} 

  static ResolutionComputation *choose_res(const Matrix *m,
					   M2_bool resolve_cokernel,
					   int max_level,
					   M2_bool use_max_slanted_degree,
					   int max_slanted_degree,
					   int algorithm,
					   int strategy
					   );
  // Values for algorithm and strategy are documented in engine.h

  virtual void start_computation() = 0;

  virtual int res_complete_thru_degree() = 0;
  // The computation is complete up through this slanted degree.

  ////////////////////////////////
  // Results of the computation //
  ////////////////////////////////
  virtual const MatrixOrNull *get_matrix(int level) = 0;

  virtual const FreeModuleOrNull *get_free(int level) = 0;

  virtual const M2_arrayint get_betti(int type) const = 0;
  // type is documented under rawResolutionBetti, in engine.h

  //////////////////////////////////////
  // Statistics and spair information //
  //////////////////////////////////////

  virtual void text_out(buffer &o) = 0;
  // This displays statistical information, and depends on the
  // gbTrace value.

};


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
