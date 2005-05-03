--- status: Draft
--- author(s): 
--- notes: 

document { 
     Key => prune,
     Headline => "minimize generators and relations",
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
     Key => (prune,ChainComplex),
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
     Key => (prune,CoherentSheaf),
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
     Key => (prune,GradedModuleMap),
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
     Key => (prune,GradedModule),
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
     Key => (prune,ChainComplexMap),
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
     Key => (prune,Matrix),
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
     Key => (prune,Module),
     Usage => "N = prune M",
     Inputs => {
	  "M" => ""
	  },
     Outputs => {
	  "N" => Module => {"isomorphic to ", TT "M",
	       ", minimally presented if ", TT "M", " is graded"}
	  },
     "As an example, we prune the cokernel of a matrix.",
     EXAMPLE {
	  "R = ZZ/32003[a..d];",
	  "M = coker matrix {{a,1,b},{c,3,b+d}}",
	  "N = prune M"
	  },
     "The isomorphism from ", TT "N", " to ", TT "M", " can 
     be obtained with ", TT "g = N.cache.pruningMap", " unless ", TT "M.cache.pruningMap", "
     already exists, in which case ", TT "N", " is the same as ", TT "M", ".  You may obtain 
     the inverse isomorphism with ", TT "g^-1", ".",
     EXAMPLE {
	  "peek N.cache",
	  "g = N.cache.pruningMap",
	  "g^-1"
	  },
     Caveat => {"If the module is not graded, then an attempt is made to 
	  improve the presentation"},
     SeeAlso => {"pruningMap"}
     }
 -- doc7.m2:2439:     Key => prune,
 -- doc7.m2:2443:     Key => (prune, Matrix),
 -- doc7.m2:2454:     Key => (prune, Module),
document {
     Key => (prune, Matrix),
     Usage => "prune f",
     Inputs => {
	  "f" => null
	  },
     Outputs => {
	  { "the map corresponding to ", TT "f", " obtained by pruning its source and target modules" }
	  },
     SeeAlso => {(prune,Module), "presentation", "trim"}
     }
