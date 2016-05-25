// Includes a file from gtest that pulls in all of the implementation                                                                 
// of gtest. The gtest docs recommend building gtest individually for                                                                 
// each program rather than using an installed gtest and this is as                                                                   
// easy a way of doing it as any. Especially because it guarantees that                                                               
// the compiler flags are the same, which is the whole point of the                                                                   
// recommendation to build gtest for each program.                                                                                    

#include "src/gtest-all.cc"

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/free-resolutions check"
// indent-tabs-mode: nil
// End:
