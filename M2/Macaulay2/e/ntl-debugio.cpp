// Copyright 2005, Michael Stillman

/**
 * @file ntl-debugio.cpp
 * @brief Debugger-callable `dntl_*` printers for NTL `mat_ZZ` and `ZZ` values.
 *
 * Defines two short helpers --- `dntl_matZZ(const NTL::mat_ZZ*)`
 * and `dntl_ZZ(const NTL::ZZ*)` --- that send an NTL matrix or
 * big integer to `std::cout`. They exist solely for interactive
 * debugging: the `d` prefix matches the convention used in
 * `debug.hpp` so a developer can `call dntl_matZZ(A)` from gdb
 * or lldb and see human-readable output without having to set
 * up an `iostream` formatter manually. The companion file
 * `ntl-internal.cpp` carries the NTL-namespace-isolated
 * conversions (`ntl_ZZ_to_mpz` and friends) that bridge to GMP.
 *
 * @see ntl-interface.hpp
 * @see debug.hpp
 */

#include <iostream>
#include "ntl-interface.hpp"

void dntl_matZZ(const NTL::mat_ZZ *A) { std::cout << *A << std::endl; }
void dntl_ZZ(const NTL::ZZ *f) { std::cout << *f << std::endl; }
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
