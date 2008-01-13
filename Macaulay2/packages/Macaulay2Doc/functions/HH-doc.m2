--- status: draft
--- author(s): Popescu 
--- notes: 

document { 
     Key => HH,
     Headline => "general homology and cohomology functor",
     TT "HH", " is a ", TO "ScriptedFunctor", " which serves as an interface
     to the methods for ",  TO "homology", " and ", TO "cohomology", ", in
     the sense that, ", TT "HH_i(M)", " is an abbreviation for ", TT "homology(i,M)", "
     ", TT "HH^i(M)", " is an abbreviation for ", TT "cohomology(i,M)", ",
     and ", TT "HH(M)", " is an abbreviation for ", TT "homology(,M)", ".
     A second argument and optional arguments may be added."
     }

TEST ("
     R=ZZ/101[a..d]
     C=resolution cokernel vars R
     D = C ++ C[1] ++ C[2]
     betti D
     assert( degree HH_1 D === 0 )
     assert( degree HH_0 D === 1 )
     assert( degree HH_-1 D === 1 )
     assert( degree HH_-2 D === 1 )
     ")

