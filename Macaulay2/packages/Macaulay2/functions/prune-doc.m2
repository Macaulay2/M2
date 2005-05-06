--- status: Draft
--- author(s): M2Fest + Amelia Taylor
--- notes: 

document { 
     Key => prune,
     Headline => "minimize generators and relations",
--     Usage => "",
--     Inputs => {
--	  },
--     Outputs => {
--	  },
--     Consequences => {
--	  },     
--     Caveat => {},
     SeeAlso => {trim, minPres, presentation}
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
     Headline => "minimize generators and relations for source and target",
     Usage => "prune f",
     Inputs => {
	  "f" => ""
	  },
     Outputs => {
	  "g" => Matrix => ""
	  },
     "If the source and target of ", TT "f", " are graded, then minimal 
     presentations of the source and target modules for ", TT "f", " are 
     computed using ", TO (prune, Module), " and ", TT "g", " is the matrix 
     corresponding to ", TT "f", " with source and target the minimally 
     presented source and target. If either the source or target of ", TT "f", 
     " is not graded then an attempt is made to improve their presentations 
     and ", TT "g", "is the matrix with resulting source and target. An 
     example follows.",
     EXAMPLE {
	  "R = ZZ/32003[a..d];",
	  "f = map(coker matrix {{a,1,b},{c,3,b+d}},R^2)",
	  "g = prune f",
	  "source g",
	  "target g"
	  },
     SeeAlso => {(prune, Module)}
     }
document { 
     Key => (prune,Module),
     Headline => "minimize generators and relations",
     Usage => "N = prune M",
     Inputs => {
	  "M" => ""
	  },
     Outputs => {
	  "N" => Module => {"isomorphic to ", TT "M"}
	  },
     Consequences => {
	  {TT "prune", " stores the isomorphism from ", TT "M", " to ", TT "N", 
	       " as ", TT "g = N.cache.pruningMap", " unless ", 
	       TT "M.cache.pruningMap", "already exists, in which case ", 
	       TT "N", " is the same as ", TT "M", " and the inverse 
	       isomorphism is obtained by ", TT "g^-1"}
	       },   	      
     "If the Module ", TT "M", " is graded then the module ", TT "N", " 
     is a minimal presentation of ", TT "M", ".  If not, then an 
     attempt is made to improve the presentation of ", TT "M", ".  An 
     example follows.", 
     EXAMPLE {
	  "R = ZZ/32003[a..d];",
	  "M = coker matrix {{a,1,b},{c,3,b+d}}",
	  "N = prune M",
 	  "peek N.cache",
	  "g = N.cache.pruningMap",
	  "g^-1"
	  },
     SeeAlso => {"pruningMap", (prune, Matrix), (trim, Module)}
     }

