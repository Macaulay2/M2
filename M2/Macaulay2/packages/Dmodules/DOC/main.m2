-- Copyright 1999-2009 by Anton Leykin and Harrison Tsai

document {
     Key => "Dmodules",
     Headline => "algorithms for D-modules",
     
     "To begin, read the ", TO {"D-modules tutorial"}, ".",

     
     HEADER3 "How to make Weyl algebras:",
          
     UL{TO {"WeylAlgebra", " -- 
	       The class of Weyl algebras"},
     TO {"makeWeylAlgebra", 
	       " -- Weyl algebra associated to a polynomial ring"}},
     
     HEADER3 "Basic commands:",
     UL{
	  TO {"gbw", " -- Groebner bases with respect to weight vectors"},
	  TO {"inw", " -- initial ideals with respect to weight vectors"},
	  TO {"Fourier", " -- Fourier transform"},
	  TO {"Dtransposition", " -- standard transposition"},
	  TO {"makeCyclic", " -- cyclic presentation"},
	  TO {"stafford", " -- compute 2 generators for an ideal in the Weyl algebra"}
	  },

     HEADER3 "Basic invariants of D-modules:",
     UL{TO {"Ddim", " -- dimension"}, 
	  TO {"holonomicRank"," -- holonomic rank"}, 
	  TO {"charIdeal", " -- characteristic ideal"},
	  TO {"singLocus", " -- singular locus"}},

     HEADER3 "Programming aids:",
     UL{TO {"createDpairs", " -- tags coordinate and derivation variables"},
	  TO {"Dtrace", " -- toggles verbose comments"},
	  TO {"setHomSwitch", " -- toggles use of homogeneous Weyl algebra"}}
     }

-*
-- FIXME: this is excluded because the Macaulay2Doc package owns the WeylAlgebra key
document {
     Key => WeylAlgebra,
     TT "WeylAlgebra", " --
     name for an optional argument for a monoid that
     specifies that a PolynomialRing created from it will
     be a Weyl Algebra.",

     PARA{},
     "The n-th Weyl algebra is the associative ring on 2n variables,
     e.g., K<x_1..x_n, D_1..D_n>, where all the variables commute except
     for (D_i x_i = x_i D_i + 1).  It can be viewed as the ring
     of algebraic differential operators on affine space K^n.",

     PARA{},
     "A simple example:",
     EXAMPLE {
	"W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]",
     	"x*Dx", 
     	"Dx*x"},     
     PARA{},
     "Caveats and known problems:",
     UL{"The variables can be called by any name, but for each
	  pair such as x => Dx, the commutative variable (in this case x)
	  must be listed before the derivation variable (in this case Dx)"}
     }
*-

-----------------------------------------------

scan({
    -- some optional arguments
    	 SetVariables
         },
    s -> if s =!= null then document {
         Key => s,
         Headline => "name for an optional argument",
         "A symbol used as the name of an optional argument, for some function(s)."
         }
    )

document {
     Key => {(Dtrace, ZZ), Dtrace},
     Headline => "set the depth of comments made by D-module routines",
     Usage => "Dtrace n",
     Inputs => {
	  "n" => { "new level" }
	  },
     Outputs => {
	  ZZ => { "old level" }
	  },
     SeeAlso => {"getDtrace"}
     }  
document {
     Key => getDtrace,
     Headline => "(internal) -- get the INFOLEVEL switch",
     SeeAlso => {"Dtrace"}
     }  

document {
     Key => {(setHomSwitch, Boolean), setHomSwitch},
     Headline => "toggles the use of homogeneous Weyl algebra",
     Usage => "setHomSwitch n",
     Inputs => {
	  "n" => Boolean => { "new value" }
	  },
     Outputs => {
	  Boolean => "old value"
	  },
     SeeAlso => {"getHomSwitch"}
     }  

document {
     Key => getHomSwitch,
     Headline => "(internal) -- get the HOMOGENIZATION switch",
     SeeAlso => {"setHomSwitch"}
     }  

document {
     Key => {(createDpairs,PolynomialRing), createDpairs},
     Headline => "pairs up the variables in Weyl algebra ",
     Usage => "createDpairs A",
     Inputs => {
	  "A" => "the Weyl algebra"
	  },
     Consequences => {
      	  {"attaches to ", TT "A", " a pair of keys to help distinguish the
     	  coordinate variables from the derivation variables."}    	  
	  },
     "Since the Weyl algebra has commutation rules, this routine
     attaches to the Weyl algebra two keys to organize the
     variables.  The first key 'dpairVars' contains 3 lists: a list of the coordinate
     variables, a list of the derivative variables, and a list
     of the central variables.  The second key 'dpairInds' also contains 3 lists
     of the corresponding indices to 'dpairVars'.",
     EXAMPLE lines ///
	     W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]
	     createDpairs W 
	     W.dpairVars
	     W.dpairInds
	     ///,
     SeeAlso => {"WeylAlgebra"}
     },

document {
     Key => dpairInds,
     Headline => "a key attached by createDpairs",
     "see ", TO "createDpairs"
     }
document {
     Key => dpairVars,
     Headline => "a key attached by createDpairs",
     "see ", TO "createDpairs"
     }

document {
     Key => {pInfo, (pInfo, ZZ, Thing), (pInfo, ZZ, List)},
     Headline => "prints tracing info",
     "Prints tracing information according to the print level set by ", 
	TT "Dtrace", ".",
     SeeAlso => { "Dtrace" }
     }

----------------------------------------------------------------------------
-- (better docs needed)
----------------------------------------------------------------------------

document {
     Key => {Dprune, (Dprune, Matrix), (Dprune, Module), [Dprune, MinimalGenerators]},
     Headline => "prunes a D-module",
     Usage => "Dprune M",
     Inputs => {
	  "M" => {ofClass Matrix, " or ", ofClass Module},
	  MinimalGenerators => Boolean => {"indicates whether a Gröbner basis should be computed"}
	  },
     Outputs => {
	  {ofClass Matrix, " or ", ofClass Module, " of the same type as ", TT "M"} 
	  },
     "Finds another (possibly smaller) representation of a D-module. 
     If given a matrix, prunes its cokernel; the result is a matrix whose cokernel is isomorphic.",
     EXAMPLE lines ///
     	 W = makeWA(QQ[x,y])
	 M = matrix{{x,dx},{1,1}} 
	 Dprune M
	 Dprune coker M
     	 M = matrix{{x,dx},{x,y}}
	 Dprune M
      	 ///,
     -- Caveat => {},
--     SeeAlso => {"BerinsteinSato::pruneCechComplexCC"} -- FIXME
     }

document {
     Key => {
	  FourierInverse, (FourierInverse,Matrix), (FourierInverse,Ideal), 
	  (FourierInverse,ChainComplex), (FourierInverse,RingElement),
      	  (FourierInverse,Module)
	  },
     Headline => "Inverse Fourier map (D-modules)",
     " see ", TO "Fourier" 
     }

end
------------------------------------------------------------------------------------------------------------
THE END
restart
loadPackage "Dmodules"
uninstallPackage "Dmodules"
installPackage("Dmodules")
installPackage("Dmodules", SeparateExec=>true, RerunExamples=>true)
check Dmodules
