-- -*- coding: utf-8 -*-
--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {forceGB, (forceGB,Matrix)},
     Headline => "declare that the columns of a matrix are a Gröbner basis",
     Usage => "forceGB f",
     Inputs => { 
	  "f" => Matrix 
	  },
     Outputs => { GroebnerBasis },
     "Declares that the columns of the matrix ", TT "f", "
     constitute a Gröbner basis, autoreduces it, minimizes it, sorts it, and returns a Gröbner basis object
     declaring itself complete, without computing any S-pairs.",
     PARA{},
     "Sometimes one knows that a set of polynomials (or columns of such)
     form a Gröbner basis, but ", EM "Macaulay2", " doesn't.  This is the
     way to inform the system of this fact.",
     EXAMPLE {
	  "gbTrace = 3;",
	  "R = ZZ[x,y,z];",
	  "f = matrix{{x^2-3, y^3-1, z^4-2}};",
	  "g = forceGB f"},
     "This Gröbner basis object is stored with the matrix and can be
     obtained as usual:",
     EXAMPLE {
	  "g === gb(f, StopBeforeComputation=>true)"
	  },
     "Requesting a Gröbner basis for ", TT "f", " requires no computation.",
     EXAMPLE {
	  "gens gb f"
	  },
     PARA {
	  "If an autoreduced Gröbner basis is desired, replace ", TT "f", " by ", TT "gens forceGB f", " first."
	  },
     Caveat => {"If the columns do not form a Gröbner basis, nonsensical answers may result"},
     SeeAlso => {"Gröbner bases"},
     Subnodes => {
	 TO [forceGB, ChangeMatrix],
	 TO [forceGB, MinimalMatrix],
	 TO [forceGB, SyzygyMatrix],
         },
     }
document { 
     Key => [forceGB, ChangeMatrix],
     Headline => "specify the change of basis matrix from a Gröbner basis to the generators of a module",
     Usage => "forceGB(...,ChangeMatrix=>m)",
     Inputs => { "m" => Matrix },
     Consequences => {
	  "Set the change of basis matrix from the Gröbner basis
	  to the original generators"
	  },     
     "The matrix ", TT "m", " should have size a by b, where a is the
     number of columns of the original matrix, and b is the number
     of columns of f.",
     EXAMPLE {
	  "gbTrace = 3",
	  "R = ZZ[x,y,z];",
	  "f = matrix{{x^2-3, y^3-1, z^4-2}};",
	  "g = forceGB(f, ChangeMatrix=>id_(source f));",
	  "x^2*y^3 // g"
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [forceGB, MinimalMatrix],
     Headline => "specify the minimal generator matrix"
     }
document { 
     Key => [forceGB, SyzygyMatrix],
     Headline => "specify the syzygy matrix",
     Usage => "forceGB(f,SyzygyMatrix=>z,...)",
     Inputs => { "z" => Matrix },
     Consequences => {
	  {"A request for the syzygy matrix of ", 
	  TT "f", " will return ", TT "z"}
	  },
     "In the following example, the only computation being performed
     when asked to compute the ", TO kernel, " or ", TO syz, " of ", 
     TT "f", " is the minimal generator matrix of ", TT "z", ".",
     EXAMPLE {
	  "gbTrace = 3",
	  "R = ZZ[x,y,z];",
	  "f = matrix{{x^2-3, y^3-1, z^4-2}};",
	  "z = koszul(2,f)",
	  "g = forceGB(f, SyzygyMatrix=>z);",
	  "syz g -- no extra computation",
	  "syz f",
	  "kernel f",
	  },
     "If you know that the columns of z already form a set of minimal
     generators, then one may use ", TO forceGB, " once again.",
     Caveat => {"If the columns of ", TT "z", " do not generate the 
	  syzygy module of ", TT "f", ",
	  nonsensical answers may result"},
     SeeAlso => {"Gröbner bases"}
     }
