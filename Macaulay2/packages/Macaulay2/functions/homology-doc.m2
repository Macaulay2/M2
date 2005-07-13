--- status: TODO
--- author(s): 
--- notes: 


document {
     -- this is the old version
     Key => homology,
     Headline => "general homology functor",
     TT "homology(f,g)", " -- computes the homology module ", TT "(kernel f)/(image g)", ".",
     BR, NOINDENT,
     TT "homology", " -- a method name available for computing expressions
     of the forms ", TT "HH_i(X) and", " ", TT "HH_i(M,N).",
     PARA,
     "If it is intended that ", TT "i", " be of class ", TO "ZZ", ", 
     ", TT "M", " be of class ", TT "A", ", and ", TT "N", " be of
     class ", TT "B", ", then the method can be installed with ",
     PRE "     homology(ZZ, A, B) := opts -> (i,M,N) -> ...",
     SeeAlso => {"HH", "cohomology", "ScriptedFunctor"}
     }

document { 
     -- this will be the newversion
     Key => homology,
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (homology,ChainComplexMap),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (homology,ZZ,ChainComplexMap),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (homology,Nothing,ChainComplexMap),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (homology,ChainComplex),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (homology,ZZ,ChainComplex),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (homology,ZZ,Sequence),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (homology,Matrix,Matrix),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (homology,Nothing,ChainComplex),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (homology,Nothing,Sequence),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
 -- doc10.m2:731:     Key => (cohomology, ZZ, Module),
 -- doc10.m2:936:     Key => (cohomology, ZZ, SumOfTwists),
 -- doc10.m2:962:     Key => (cohomology, ZZ, CoherentSheaf),
 -- doc12.m2:1032:     Key => cohomology,
 -- doc12.m2:1045:     Key => homology,
 -- doc7.m2:1419:     Key => (homology,Matrix,Matrix),
 -- doc9.m2:771:     Key => (homology,ZZ,ChainComplex),
 -- doc9.m2:1573:     Key => (cohomology,ZZ,ChainComplex),
 -- doc9.m2:1581:     Key => (homology,ZZ,ChainComplexMap),
 -- doc9.m2:1590:     Key => (cohomology,ZZ,ChainComplexMap),
 -- doc9.m2:1599:     Key => (homology,ChainComplex),
