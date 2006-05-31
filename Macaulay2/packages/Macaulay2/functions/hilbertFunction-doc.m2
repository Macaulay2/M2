--- status: DRAFT
--- author(s): L. Gold
--- notes: still need to write description on main node

undocumented {(hilbertFunction,List,CoherentSheaf),(hilbertFunction,ZZ,CoherentSheaf)}

document { 
     Key => hilbertFunction,
     Headline => "compute the Hilbert function",
     "The Hilbert function is a function gives the dimension of the
     given degree piece of an object. ",
     SeeAlso => {"degreesRing", "reduceHilbert", "poincare", "poincareN",
	  "hilbertSeries", "hilbertPolynomial"}
     }

document { 
     Key => {(hilbertFunction,List,Ring),(hilbertFunction,ZZ,Ring)},
     Headline => "compute the Hilbert function of a ring",
     Usage => "hilbertFunction(d,R)",
     Inputs => {
	  "d" => List => {" which is a multidegree, or if R is singly graded, then one may write just ", TO "ZZ"},
	  "R"
	  },
     Outputs => {
	  ZZ => "the value of the Hilbert function of the ring" },
     EXAMPLE {	
	  "R = QQ[x,y,z, Degrees=>{3:{1,1}}];",
	  "hilbertFunction({3,3}, R)",
    	  "basis({3,3},R)"   	  
  	  },	
     "In the case where the ring is singly graded, then instead of having the 
     input be a list of length 1 containing the degree, it is sufficient to
     write an integer.",
          EXAMPLE {	
	  "R = QQ[x,y,z];",
	  "hilbertFunction(3, R)",
    	  "basis(3,R)"   	  
  	  }
     }

document { 
     Key => {(hilbertFunction,List,Module),(hilbertFunction,ZZ,Module)},
     Headline => "compute the Hilbert function of a module",
     Usage => "hilbertFunction(d,M)",
     Inputs => {
	  "d" => List => {" a multidegree, or if M is singly graded, then one may write just ", TO "ZZ"},
	  "M"
	  },
     Outputs => {
	  ZZ => "the value of the Hilbert function of the module" 
	  },
     EXAMPLE {	
	  "R = QQ[a..d, Degrees=>{4:{1,1}}];",
	  "M = coker matrix {{a,c,d},{c,b,d}}",
	  "hilbertFunction({2,2}, M)",
     	  "B = basis({2,2},M)",
	  "numgens source B"
  	  },
     "The last command ", TO "numgens", " ", TO "source", " counts the
     number of columns in this matrix.",
     PARA{},
     "In the case where the ring is singly graded, then instead of having the 
     input be a list of length 1 containing the degree, it is sufficient to
     write an integer.",
     EXAMPLE {	
	  "R = QQ[a..d];",
	  "M = coker matrix {{a,c,d},{c,b,d}}",
	  "hilbertFunction(2, M)",
    	  "basis(2,M)"   	  
  	  },
     Caveat => {
     	  "This requires a homogeneous module to compute properly, but
	  will output something if run on a module which is not homogeneous."
	  }
     }

document { 
     Key => {(hilbertFunction,List,Ideal),(hilbertFunction,ZZ,Ideal)},
     Headline => "compute the Hilbert function of the quotient of the ambient ring by an ideal",
     Usage => "hilbertFunction(d,I)",
     Inputs => {
	  "d" => List => {" a multidegree, or if I is singly graded, then one may write just ", TO "ZZ"},
	  "I"
	  },
     Outputs => {
	  ZZ => "the value of the Hilbert function of the quotient of the ambient ring by the ideal"
	  },
     EXAMPLE {	
	  "R = QQ[a..f, Degrees=>{6:{1,1}}];",
	  "I = ideal (a*b, c*d, e*f);",
	  "hilbertFunction({2,2}, I)",
	  "S = R/I;",
	  "basis({2,2},S)"   	  
  	  },	
     "In the case where the ring is singly graded, then instead of having the 
     input be a list of length 1 containing the degree, it is sufficient to
     write an integer.",
     EXAMPLE {	
	  "R = QQ[a..f];",
	  "I = ideal (a*b, c*d, e*f);",
	  "hilbertFunction(2, I)",
	  "S = R/I;",
	  "basis(2,S)"   	  
  	  },	
     Caveat => {
	  "As is often the case, calling this function on an ideal ", 
	  TT "I", " actually computes it for ", TT "R/I", " where ",
	  TT "R" , " is the ring of ", TT "I", ".",
	  }     
     }

-- NOTE: listed as undocumented above
--document { 
--     Key => {(hilbertFunction,List,CoherentSheaf),(hilbertFunction,ZZ,CoherentSheaf)},
--     Headline => "compute the Hilbert function of a coherent sheaf",
--     Usage => "hilbertFunction(d,S)",
--     Inputs => {
--	  "d" => List => {" a multidegree, or if S is singly graded, then one may write just ", TO "ZZ"},
--	  "S"
--	  },
--     Outputs => {
-- 	  ZZ => "the value of the Hilbert function of the coherent sheaf" 
-- 	  },
--      EXAMPLE {	
-- 	  "V = Proj(ZZ/101[x_0..x_2,Degrees=>{3:{1,1}}]);",
-- 	  "S = sheaf(image matrix {{x_0^3+x_1^3+x_2^3}})",
-- 	  "hilbertFunction({3,3}, S)",
-- 	  "basis({3,3}, module S)"
-- 	  },	
--      "In the case where the ring is singly graded, then instead of having the 
--      input be a list of length 1 containing the degree, it is sufficient to
--      write an integer.",
--      EXAMPLE {	
-- 	  "V = Proj(ZZ/101[x_0..x_2]);",
-- 	  "S = sheaf(image matrix {{x_0^3+x_1^3+x_2^3}})",
-- 	  "hilbertFunction(3, S)",
-- 	  "basis(3, module S)"
-- 	  }
--      }

document { 
     Key =>{(hilbertFunction,List,ProjectiveVariety),(hilbertFunction,ZZ,ProjectiveVariety)},
     Headline => "compute the Hilbert function of a projective variety",
     Usage => "hilbertFunction(d,V)",
     Inputs => {
	  "d" => List => {" a multidegree, or if V is singly graded,
	       then one may write just ", TO "ZZ"},
	  "V"
	  },
     Outputs => {
	  ZZ => "the value of the Hilbert function of the projective variety" 
	  },
     EXAMPLE {	
     	  "R =ZZ/101[x_0..x_2,Degrees=>{3:{1,1}}];",
	  "V = Proj R;",
	  "hilbertFunction({3,3}, V)",
     	  "basis({3,3}, ring V)"
  	  },	
     "In the case where the ring is singly graded, then instead of having the 
     input be a list of length 1 containing the degree, it is sufficient to
     write an integer.",
     EXAMPLE {	
	  "R =ZZ/101[x_0..x_2];",
	  "V = Proj R;",
	  "hilbertFunction(3, V)",
     	  "basis(3, ring V)"
  	  }	
     }

