// Copyright 2012  Michael E. Stillman
#ifndef __interrupted__hpp__
#define  __interrupted__hpp__

#include "../system/supervisorinterface.h"
#define system_interrupted() test_Field(THREADLOCAL(interrupts_interruptedFlag,struct atomic_field))

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
