document { 
     Key => {support,(support,RingElement)},
     Headline => "list of variables occuring in a polynomial",
     Usage => "support f",
     Inputs => {
	  "f" => RingElement => "in a polynomial ring"
	  },
     Outputs => {
	  List => {"of ", TO2 (RingElement, "ring elements"), ", the variables occuring in the polynomial"}
	  },
     EXAMPLE {
	  "R = QQ[a..g]",
	  "f = a^3+b^2*c+3*f^10*d-1+e-e",
	  "support f",
	  },
     "If the ring is a polynomial ring over another polynomial ring, then
     the support contains all of the variables, even the ones in the coefficient ring.
     The  ring of each of these is the ring of f.",
     EXAMPLE {
	  "A = ZZ[a,b]; B = A[r,s,t]; C = B[x,y,z,w];",
	  "f = (a+r+z+1)^2+y",
	  "S = support f",
          "ring S_2 === ring f"
	  },
     "Here is one way to select only the top level variables.",
     EXAMPLE {
	  "select(S, x -> index x < numgens C)"
	  },
     "To obtain a list of the integer indices of the variables one can use either ",
     TO (indices,RingElement), " or apply ", TO index, " to each variable.",
     EXAMPLE {
	  "indices f",
	  "apply(support f, index)"
	  },
     SeeAlso => {index, (indices,RingElement), (symbol_,Ring,ZZ)}
     }

document { 
     Key => {(support,Ideal)},
     Headline => "list of variables occuring in the generators of an ideal",
     Usage => "support I",
     Inputs => { "I" => "an ideal in a polynomial ring" },
     Outputs => {
	  List => {"of ", TO2 (RingElement, "ring elements"), ", the variables occuring in the generators of the ideal"}
	  },
     EXAMPLE lines ///
	  R = QQ[a..g]
	  I = ideal(b,c,e+f)
	  support I
	  ///
     }
