--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {index,(index,RingElement)},
     Headline => "numeric index of a ring variable",
     Usage => "index v",
     Inputs => {
	  "v" => RingElement
	  },
     Outputs => {
	  ZZ => {"the integer index, starting at 0, of v, if it is a variable.
	  If v is not a variable, then ", TO null, " is returned"}
	  },
     "Variables are indexed in the following way: the first variable has index 0,
     the second index 1, and so on, until n-1, where n is the number of generators of R,
     the ring of v.  Then, if the coefficient ring is a polynomial ring, those variables
     are numbered starting at n, and so on.",
    EXAMPLE {
	 "R = ZZ/101[a..d,t]",
     	 "index a",
     	 "index t",
	 },
     EXAMPLE {
	  "A = ZZ[a..d]; B = A[r,s,t]; C = B[x,y,z]",
	  "index x",
	  "index z",
	  "index r"
	  },
     "Notice that r is an element of B, and so indices are taken from that ring.
     If we consider r as an element of C, we get a different answer.  Variables
     of coefficient rings of coefficient rings have an index too.",
     EXAMPLE {
	  "index(r*1_C)",
	  "index(b*1_C)"
	  },
     PARA{},
    "The symbol ", TT "index", " is also as a key used in 
    ", TO {"GeneralOrderedMonoid", "s"}, " to store a table that is used to 
    map generator names to the position of the generator in the list of generators.",
     SeeAlso => {indices, support, (symbol_,Ring,ZZ)}
     }

