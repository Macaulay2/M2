--- status: DRAFT
--- author(s): L. Gold
--- notes: 

document { 
     Key => hilbertFunction,
     Headline => "compute the Hilbert function",
     SeeAlso => {"degreesRing", "reduceHilbert", "poincare", "poincareN",
	  "hilbertSeries", "hilbertPolynomial"}
     }

document { 
     Key => {(hilbertFunction,List,Ring),(hilbertFunction,ZZ,Ring)},
     Headline => "compute the Hilbert function of a ring",
     Usage => "hilbertFunction(d,R)",
     Inputs => {
	  "d" => List => {" a multidegree, or if singly graded, then just ", TO "ZZ"},
	  "R" => ""
	  },
     Outputs => {
	  ZZ => "the Hilbert function of the ring" },
     EXAMPLE {	
	  "R = QQ[x,y,z, Degrees=>{{1,1},{1,1},{1,1}}];",
	  "hilbertFunction({3,3}, R)",
    	  "basis({3,3},R)"   	  
  	  },	
     "In the case where the ring is singly graded, then instead of having the 
     input be a list of length 1 containing the degree, it is sufficient to
     input an integer.",
          EXAMPLE {	
	  "R = QQ[x,y,z];",
	  "hilbertFunction(3, R)",
    	  "basis(3,R)"   	  
  	  },	
     Caveat => {
	  "At the moment, the function is computed simply by calling ", TO "basis",
     	  " and extracting the number of basis elements."
	  },
     }

document { 
     Key => {(hilbertFunction,List,Module),(hilbertFunction,ZZ,Module)},
     Headline => "compute the Hilbert function of a module",
     Usage => "hilbertFunction(d,M)",
     Inputs => {
	  "d" => List => {" a multidegree, or if singly graded, then just ", TO "ZZ"},
	  "M" => ""
	  },
     Outputs => {
	  ZZ => "the Hilbert function of the module" 
	  },
     EXAMPLE {	
	  "R = QQ[a..d, Degrees=>{{1,1},{1,1},{1,1},{1,1}}];",
	  "M = coker matrix {{a,c,5},{1,b,d}}",
	  "hilbertFunction({2,2}, M)",
     	  "basis({2,2},M)"   	  
  	  },
     "In the case where the ring is singly graded, then instead of having the 
     input be a list of length 1 containing the degree, it is sufficient to
     input an integer.",
     EXAMPLE {	
	  "R = QQ[a..d];",
	  "M = coker matrix {{a,c,5},{1,b,d}}",
	  "hilbertFunction(2, M)",
     	  "basis(2,M)"   	  
  	  },
     Caveat => {
	  "At the moment, the function is computed simply by calling ", TO "basis",
     	  " and extracting the number of basis elements."
	  },
     }

document { 
     Key => {(hilbertFunction,List,Ideal),(hilbertFunction,ZZ,Ideal)},
     Headline => "compute the Hilbert function of an ideal",
     Usage => "hilbertFunction(d,I)",
     Inputs => {
	  "d" => List => {" a multidegree, or if singly graded, then just ", TO "ZZ"},
	  "I" => ""
	  },
     Outputs => {
	  ZZ => "the Hilbert function of the quotient of its ambient ring by the ideal"
	  },
     EXAMPLE {	
	  "R = QQ[a..f, Degrees=>{{1,1},{1,1},{1,1},{1,1},{1,1},{1,1}}];",
	  "I = ideal (a*b, c*d, e*f);",
	  "hilbertFunction({2,2}, I)",
	  "S = R/I;",
	  "basis({2,2},S)"   	  
  	  },	
     "In the case where the ring is singly graded, then instead of having the 
     input be a list of length 1 containing the degree, it is sufficient to
     input an integer.",
     EXAMPLE {	
	  "R = QQ[a..f];",
	  "I = ideal (a*b, c*d, e*f);",
	  "hilbertFunction(2, I)",
	  "S = R/I;",
	  "basis(2,S)"   	  
  	  },	
     Caveat => {
	  "As often is the case, calling this function on an ideal I
	  actually computes it for R/I where R is the ring of I.",
	  PARA,	  
	  "At the moment, the function is computed simply by calling ", TO "basis",
     	  " and extracting the number of basis elements."
	  },
     }

document { 
     Key => {(hilbertFunction,List,CoherentSheaf),(hilbertFunction,ZZ,CoherentSheaf)},
     Headline => "compute the Hilbert function of a coherent sheaf",
     Usage => "hilbertFunction(d,S)",
     Inputs => {
	  "d" => List => {" a multidegree, or if singly graded, then just ", TO "ZZ"},
	  "S" => ""
	  },
     Outputs => {
	  ZZ => "the Hilbert function of the coherent sheaf" 
	  },
     EXAMPLE {	
	  "V = Proj(ZZ/101[x_0..x_2,Degrees=>{{1,1},{1,1},{1,1}}]);",
	  "S = sheaf(image matrix {{x_0^3+x_1^3+x_2^3}})",
	  "hilbertFunction({3,3}, S)",
	  "basis({3,3}, module S)"
	  },	
     "In the case where the ring is singly graded, then instead of having the 
     input be a list of length 1 containing the degree, it is sufficient to
     input an integer.",
     EXAMPLE {	
	  "V = Proj(ZZ/101[x_0..x_2]);",
	  "S = sheaf(image matrix {{x_0^3+x_1^3+x_2^3}})",
	  "hilbertFunction(3, S)",
	  "basis(3, module S)"
	  },	
     Caveat => {
	  "At the moment, the function is computed simply by calling ", TO "basis",
     	  " and extracting the number of basis elements."
	  },
     }

document { 
     Key => {(hilbertFunction,List,ProjectiveVariety),(hilbertFunction,ZZ,ProjectiveVariety)},
     Headline => "compute the Hilbert function of a projective variety",
     Usage => "hilbertFunction(d,V)",
     Inputs => {
	  "d" => List => {" a multidegree, or if singly graded, then just ", TO "ZZ"},
	  "V" => ""
	  },
     Outputs => {
	  ZZ => "the Hilbert function of the projective variety" 
	  },
     EXAMPLE {	
	  "V = Proj(ZZ/101[x_0..x_2,Degrees=>{{1,1},{1,1},{1,1}}]);",
	  "hilbertFunction({3,3}, V)",
     	  "basis({3,3}, ring V)"
  	  },	
     "In the case where the ring is singly graded, then instead of having the 
     input be a list of length 1 containing the degree, it is sufficient to
     input an integer.",
     EXAMPLE {	
	  "V = Proj(ZZ/101[x_0..x_2]);",
	  "hilbertFunction(3, V)",
     	  "basis(3, ring V)"
  	  },	
     Caveat => {
	  "At the moment, the function is computed simply by calling ", TO "basis",
     	  " and extracting the number of basis elements."
	  },
     }

