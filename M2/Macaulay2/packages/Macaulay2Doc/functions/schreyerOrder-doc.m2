--- status: DRAFT
--- author(s): MES
--- notes: 

document { 
     Key => schreyerOrder,
     Headline => "create or obtain free modules with Schreyer monomial orders",
     SeeAlso => "monomial orders for free modules"
     }

document {
     Key => (schreyerOrder,Matrix),
     Headline => "create a matrix with the same entries whose source free module has a Schreyer monomial order",
     Usage => "schreyerOrder m",
     Inputs => {
	  "m" => "G <-- F between free modules"
	  },
     Outputs => {
	  {
	       "the same matrix as ", TT "m", ", except that its source free module
	       is endowed with a Schreyer, or induced, monomial order"
	       }
	  },
     TEX "Given a matrix $m : F --> G$, the Schreyer order on the monomials
     of F is given by: If $a e_i$ and $b e_j$ are monomials of $F$, i.e. $a$ and $b$ are 
     monomials in the ring, and
     $e_i$ and $e_j$ are unit column vectors of $F$, then $a e_i > b e_j$ if and only if
     either $leadterm(m)(a e_i) > leadterm(m)(b e_j)$ or they are scalar multiples of
     the same monomial in $G$, and $i > j$.",
     PARA{},
     "If the base ring is a quotient ring, we think of ", TT "leadterm(m)", " as a matrix
     over the ambient polynomial ring for the purpose of this definition.",
     PARA{},
     "In the example below, the source of ", TT "f", " is endowed with a Schreyer order.",
     EXAMPLE lines ///
	  R = ZZ/101[a..d];
	  m = matrix{{a,b,c,d}};
	  f = schreyerOrder m
	  g = syz f
	  leadTerm g
	  hf = map(source f, 1, {{d},{c},{b},{a}})
	  hm = map(source m, 1, {{d},{c},{b},{a}})	  
	  leadTerm hf
  	  leadTerm hm
	  ///,
     "Use ", TO (schreyerOrder,Module), " to see if a free module is endowed with a
     Schreyer order.",
     EXAMPLE lines ///
	  schreyerOrder source m
	  schreyerOrder source f
	  ///,
     SeeAlso => {
	  "Schreyer orders",
	  "monomial orders for free modules",
	  leadTerm,
	  (schreyerOrder,Module)
	  }
     }
document { 
     Key => (schreyerOrder,Module),
     Headline => "obtain Schreyer order information",
     Usage => "schreyerOrder F",
     Inputs => {
	  "F" => "a free module"
	  },
     Outputs => {
	  Matrix => {"the zero matrix, if F is not equipped with a Schreyer order, otherwise
	    a matrix with source F, such that the monomial order on F is the one induced
	    by this matrix."}
	  },
     "For examples, see ", TO "Schreyer orders", ", and ", TO (schreyerOrder,Matrix), ".",
     SeeAlso => {"monomial orders for free modules"}
     }

