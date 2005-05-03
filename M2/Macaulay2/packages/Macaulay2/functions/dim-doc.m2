--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => dim,
     Headline => "compute the Krull dimension",
     Caveat => {"To compute the dimension of a vector space, 
	one should use ", TO rank, ".",
	PARA,
	"Over the integers, the computation effectively 
	 tensors first with the rational numbers, yielding the wrong 
	 answer in some cases."},
     SeeAlso => {codim}
     }

document { 
     Key => (dim,GaloisField),
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
     Key => (dim,MonomialIdeal),
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
     Key => (dim,Ring),
     Usage => "dim R",
     Inputs => {"R" => ""
	  },
     Outputs => {ZZ => ""
	  },
     "Compute the Krull dimension of the given ring.",
     PARA,
     "A cuspidal plane curve", 
     EXAMPLE {
	  "R = QQ[x,y,z]",
	  "I =ideal(y^2*z-x^3)",
	  "sing = singularLocus(R/I)",
	  "dim sing"
	  },
     "The exterior algebra is artinian:",
     EXAMPLE {
	  "R = ZZ/101[a,b,SkewCommutative => true]",
	  "dim R"
	  },
     "The Weyl algebra in 2 variables:",
     EXAMPLE {
          "R = ZZ/101[x,dx,y,dy,WeylAlgebra => {x=>dx, y=>dy}];",
	  "dim R"
	  },
     "An example over ", TO ZZ, ":",
     EXAMPLE {
	  "R = ZZ[a,b]/(a*b-1)",
	  "dim R",
	  "S = R[x,y]",
	  "dim S"
	  },
     Caveat => {},
     SeeAlso => {codim}
     }
document { 
     Key => (dim,FractionField),
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
     Key => (dim,AffineVariety),
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
     Key => (dim,ProjectiveVariety),
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
     Key => (dim,QuotientRing),
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
     Key => (dim,Module),
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
     Key => (dim,ProjectiveHilbertPolynomial),
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
     Key => (dim,Ideal),
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
 -- doc10.m2:900:     Key => (codim, CoherentSheaf),
 -- doc7.m2:2357:     Key => codim,
 -- doc7.m2:2378:     Key => (codim, Module),
 -- doc7.m2:2397:     Key => dim,
 -- doc8.m2:4:     Key => pdim,
 -- doc8.m2:618:     Key => CodimensionLimit,
 -- doc8.m2:637:     Key => [gb,CodimensionLimit], 
 -- doc8.m2:926:     Key => [syz,CodimensionLimit],
 -- overview2.m2:1725:     Key => "two dimensional formatting",
 -- overviewB.m2:270:     Key => "dimension, codimension, and degree",
