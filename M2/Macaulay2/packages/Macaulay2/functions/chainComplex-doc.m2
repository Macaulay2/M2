--- status: TODO
--- author(s): 
--- notes: 



document {
     Key => chainComplex,
     Headline => "make a chain complex",
     TT "chainComplex", " -- a method for creating chain complexes.",
     }

document {
     Key => (chainComplex, Matrix),
     Headline => "make a small chain complex",
     TT "chainComplex f", " -- create a chain complex ", TT "C", " with
     the map ", TT "f", " serving as the differential ", TT "C.dd_1", "."
     }

document {
     Key => (chainComplex, Sequence),
     Headline => "make a chain complex",
     TT "chainComplex(f,g,h,...)", " -- create a chain complex ", TT "C", " whose
     differentials are the maps ", TT "f", ", ", TT "g", ", ", TT "h", ".",
     PARA,
     "The map ", TT "f", " appears as ", TT "C.dd_1", ", the map ", TT "g", " appears
     as ", TT "C.dd_2", ", and so on.",
     PARA,
     "The following example illustrates how chainComplex adjusts the degrees of
     the modules involved to ensure that sources and targets of the differentials
     correspond exactly.",
     EXAMPLE {
	  "R = ZZ/101[x,y]",
      	  "C = chainComplex(matrix{{x,y}},matrix{{x*y},{-x^2}})",
	  }
     }


document {
     Key => (chainComplex,GradedModule),
     Headline => "make a chain complex from a graded module",
     TT "chainComplex M", " -- convert a graded module to a chain complex by
     installing the zero map as differential.",
     PARA,
     SeeAlso => {"GradedModule", "ChainComplex"}
     }

document { 
     Key => chainComplex,
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
     Key => (chainComplex,List),
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
     Key => (chainComplex,GradedModule),
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
     Key => (chainComplex,Matrix),
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
 -- doc9.m2:1608:     Key => chainComplex,
 -- doc9.m2:1614:     Key => (chainComplex, Matrix),
 -- doc9.m2:1621:     Key => (chainComplex, Sequence),
 -- doc9.m2:1830:     Key => (chainComplex,GradedModule),
