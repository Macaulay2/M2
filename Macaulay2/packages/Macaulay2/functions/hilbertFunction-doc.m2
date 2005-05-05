--- status: DRAFT
--- author(s): L. Gold
--- notes: working on this

document { 
     Key => hilbertFunction,
     Headline => "compute the Hilbert function",
     SeeAlso => {"degreesRing", "reduceHilbert", "poincare", "hilbertSeries",
	  "hilbertPolynomial"}
     }

document { 
     Key => {(hilbertFunction,ZZ,Ring),(hilbertFunction,List,Ring)},
     Headline => "compute the Hilbert function of a ring",
     Usage => "hilbertFunction(d,R)",
     Inputs => {
	  "d" => "for the degree, or a ",
	  "R" => ""
	  },
     Outputs => {
	  Expression => "the Hilbert function" },
     "compute the dimension of the degree d part of the ring R",
     PARA,
     "At the moment, the function is computed simply by calling ", TO "basis", "
     and extracting the number of basis elements.",
     EXAMPLE {
	  },
     SeeAlso => {}
     }

document { 
     Key => {(hilbertFunction,ZZ,Module),(hilbertFunction,List,Module)},
     Headline => "compute the Hilbert function of a module",
     Usage => "hilbertFunction(d,M)",
     Inputs => {
	  },
     Outputs => {
	  },
     EXAMPLE {
	  },
     SeeAlso => {}
     }

document { 
     Key => {(hilbertFunction,ZZ,Ideal),(hilbertFunction,List,Ideal)},
     Headline => "compute the Hilbert function of an ideal",
     Usage => "hilbertFunction(d,I)",
     Inputs => {
	  },
     Outputs => {
	  },
     EXAMPLE {
	  },
     SeeAlso => {}
     }

document { 
     Key => {(hilbertFunction,ZZ,CoherentSheaf),(hilbertFunction,List,CoherentSheaf)},
     Headline => "compute the Hilbert function of a coherent sheaf",
     Usage => "hilbertFunction(d,S)",
     Inputs => {
	  },
     Outputs => {
	  },
     EXAMPLE {
	  },
     SeeAlso => {}
     }

document { 
     Key => {(hilbertFunction,ZZ,ProjectiveVariety),(hilbertFunction,List,ProjectiveVariety)},
     Headline => "compute the Hilbert function of a projective variety",
     Usage => "hilbertFunction(d,V)",
     Inputs => {
	  },
     Outputs => {
	  },
     EXAMPLE {
	  },
     SeeAlso => {}
     }


