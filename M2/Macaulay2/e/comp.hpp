// (c) 1994 Michael E. Stillman.

#ifndef _comp_hh_
#define _comp_hh_

#include "hash.hpp"

extern char system_interruptedFlag;
extern int gbTrace;

class computation : public mutable_object
{
public:
  computation() : mutable_object() {}
  virtual ~computation() {}
  virtual int calc(int nsteps) = 0;

  // The following are only defined for det computations
  // at the moment.
  virtual void discard() {}
  virtual void set_next_minor(const int * /*rows*/, const int * /*cols*/) {}
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
