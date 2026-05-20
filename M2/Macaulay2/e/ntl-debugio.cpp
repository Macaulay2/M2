// Copyright 2005, Michael Stillman

#include <iostream>
#include "ntl-interface.hpp"

void dntl_matZZ(const NTL::mat_ZZ *A) { std::cout << *A << std::endl; }
void dntl_ZZ(const NTL::ZZ *f) { std::cout << *f << std::endl; }
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
