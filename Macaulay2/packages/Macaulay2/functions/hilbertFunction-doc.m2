--- status: DRAFT
--- author(s): L. Gold
--- notes: working on: coherent sheaf node, projective variety node
--         and need to figure out how to write the types of the degree "d"

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
	  "d" => "fix this!",
	  "R" => ""
	  },
     Outputs => {
	  Expression => "the Hilbert function" },
     "We compute the dimension of the degree d part of the ring R.",
     PARA,
     "At the moment, the function is computed simply by calling ", TO "basis", "
     and extracting the number of basis elements.",
     EXAMPLE {	
	  "R = QQ[x,y,z];",
	  "hilbertFunction(3, R)",
     	  "basis(3,R)"   	  
  	  }	
     }

document { 
     Key => {(hilbertFunction,ZZ,Module),(hilbertFunction,List,Module)},
     Headline => "compute the Hilbert function of a module",
     Usage => "hilbertFunction(d,M)",
     Inputs => {
	  "d" => "fix this!",
	  "M" => ""
	  },
     Outputs => {
	  Expression => "the Hilbert function" 
	  },
     "We compute the dimension of the degree d part of the module M.",
     PARA,
     "At the moment, the function is computed simply by calling ", TO "basis", "
     and extracting the number of basis elements.",
     EXAMPLE {	
	  "R = QQ[a..d];",
	  "M = coker matrix {{a,c,5},{1,b,d}}",
	  "hilbertFunction(2, M)",
     	  "basis(2,M)"   	  
  	  }	
     }

document { 
     Key => {(hilbertFunction,ZZ,Ideal),(hilbertFunction,List,Ideal)},
     Headline => "compute the Hilbert function of an ideal",
     Usage => "hilbertFunction(d,I)",
     Inputs => {
	  "d" => "fix this!",
	  "I" => ""
	  },
     Outputs => {
	  Expression => "the Hilbert function" 
	  },
     "We compute the dimension of the degree d part of the ideal R/ I.",
     PARA,
     "At the moment, the function is computed simply by calling ", TO "basis", "
     and extracting the number of basis elements.",
     EXAMPLE {	
	  "R = QQ[a..f];",
	  "I = ideal (a*b, c*d, e*f);",
	  "hilbertFunction(2, I)",
	  "S = R/I;",
	  "basis(2,S)"   	  
  	  }	
     }

document { 
     Key => {(hilbertFunction,ZZ,CoherentSheaf),(hilbertFunction,List,CoherentSheaf)},
     Headline => "compute the Hilbert function of a coherent sheaf",
     Usage => "hilbertFunction(d,S)",
     Inputs => {
	  "d" => "fix this!",
	  "S" => ""
	  },
     Outputs => {
	  Expression => "the Hilbert function" 
	  },
     "We compute the dimension of the degree d part of the coherent sheaf S.",
     PARA,
     "At the moment, the function is computed simply by calling ", TO "basis", "
     and extracting the number of basis elements.",
     EXAMPLE {	
	  "V = Proj(ZZ/101[x_0..x_2]);",
	  "S = sheaf(image matrix {{x_0^3+x_1^3+x_2^3}})",
	  "hilbertFunction(3, S)",
     	  "basis(3,S)"   	  
  	  }	
     }

document { 
     Key => {(hilbertFunction,ZZ,ProjectiveVariety),(hilbertFunction,List,ProjectiveVariety)},
     Headline => "compute the Hilbert function of a projective variety",
     Usage => "hilbertFunction(d,V)",
     Inputs => {
	  "d" => "fix this!",
	  "V" => ""
	  },
     Outputs => {
	  Expression => "the Hilbert function" 
	  },
     "We compute the dimension of the degree d part of the projective variety.",
     PARA,
     "At the moment, the function is computed simply by calling ", TO "basis", "
     and extracting the number of basis elements.",
     EXAMPLE {	
	  "V = Proj(ZZ/101[x_0..x_2]);",
	  "hilbertFunction(3, V)",
     	  "basis(3,V)"   	  
  	  }	
     }

