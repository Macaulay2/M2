--- status: draft
--- author(s): Sorin Popescu
--- notes: 

undocumented {(cohomology,ZZ,Sequence)}
-*
-- this is the old version
document {
     Key => {cohomology,[cohomology,Degree]},
     Headline => "general cohomology functor",
     TT "cohomology", " -- a method name available for computing expressions
     of the forms ", TT "HH^i(X)", " and ", TT "HH^i(M,N)", ".",
     PARA{},
     "If it is intended that ", TT "i", " be of class ", TO "ZZ", ", ", TT "M", " be of
     class ", TT "A", ", and ", TT "N", " be of 
     class ", TT "B", ", then the method can be installed with ",
     PRE "     cohomology(ZZ, A, B) := opts -> (i,M,N) -> ...",
     SeeAlso => {"homology", "HH", "ScriptedFunctor"}
     }
*-
document { 
     Key => cohomology,
     Headline => "general cohomology functor",
      TT "cohomology", " -- a method name available for computing expressions
     of the forms ", TT "HH^i(X)", " and ", TT "HH^i(M,N)", ".",
     PARA{},
     "If it is intended that ", TT "i", " be of class ", TO "ZZ", ", ", TT "M", " be of
     class ", TT "A", ", and ", TT "N", " be of
     class ", TT "B", ", then the method can be installed with ",
     PRE "     cohomology(ZZ, A, B) := opts -> (i,M,N) -> ...",
     SeeAlso => {"homology", "HH", "ScriptedFunctor"}
     }

document { 
     Key => (cohomology,ZZ,ChainComplexMap),
     Headline => "cohomology of a chain complex map",
     Usage => "HH^i f",
     Inputs => {"i","f"
	  },
     Outputs => {Matrix=>{"the ", TT "i", 
	       "-th cohomology map induced by the chain complex map ", TT "f"}
	  },
     "The command provides the map on the ", TT "i", "-th cohomology module
     induced by a map ", TT "f", " of chain complexes.",
     SeeAlso => {"cohomology", "HH", "ChainComplex"}
     }
document { 
     Key => (cohomology,ZZ,Module),
     Headline => "local cohomology of a module",
     Usage => "HH^i(M)",
     Inputs => {"i" => " which is non negative", 
	       "M" => " which is graded over its base polynomial ring"
	  },
     Outputs => {Module
	  },
     "The command computes the local cohomology of the graded 
     module ", TT "M", " with respect to the maximal irrelevant ideal 
     (the ideal of variables in the base ring of ", TT "M", ").",
     PARA{},
     "The package ", TO "Dmodules::Dmodules", " has alternative code to
     compute local cohomology (even in the non homogeneous case)",
     PARA{},
     "A very simple example:",
     EXAMPLE {
          "R = QQ[a,b];",
          "HH^2 (R^{-3})",
          "HH^2 (R^{-4})"
           },
     PARA{},
     "Another example, a singular surface in projective fourspace 
     (with one apparent double point):",
     EXAMPLE {
	        "R = ZZ/101[x_0..x_4];",
	        "I = ideal(x_1*x_4-x_2*x_3, x_1^2*x_3+x_1*x_2*x_0-x_2^2*x_0, x_3^3+x_3*x_4*x_0-x_4^2*x_0)",
	        "M = R^1/module(I)",
	        "HH^1(M)",
	        "HH^2(M)"
	  },
     Caveat => {"There is no check made if the given module 
	  is graded over the base polynomial ring"},
     SeeAlso => {"Dmodules::Dmodules",(cohomology,ZZ,SumOfTwists),(cohomology,ZZ,CoherentSheaf)}
     }

document { 
     Key => (cohomology,ZZ,ChainComplex),
     Headline => "cohomology of a chain complex",
     Usage => "HH^i C",
     Inputs => {"i"=> ZZ, "C" => ChainComplex
	  	  },
     Outputs => {Module => {"HH^i C", " -- homology at the i-th spot of the chain complex ", TT "C", "."}
	  },
     "By definition, this is the same as computing HH_(-i) C.",
     PARA{},
     EXAMPLE {
           "R = ZZ/101[x,y]",
           "C = chainComplex(matrix{{x,y}},matrix{{x*y},{-x^2}})",
           "M = HH^1 C",
           "prune M"
            },
     PARA{},
     "Here is another example computing simplicial cohomology
     (for a hollow tetrahedron):",
     EXAMPLE {
	  "needsPackage \"SimplicialComplexes\"",
	  "R = QQ[a..d]",
          "D = simplicialComplex {a*b*c,a*b*d,a*c*d,b*c*d}",
          "C = complex D",  
          "HH_2 C",
	  "prune oo"
	  },
     SeeAlso => {"GradedModule", "HH"}
     }
