--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {GF,[GF, Variable],[GF, PrimitiveElement],[GF, SizeLimit],(GF,ZZ,ZZ),(GF,ZZ),(GF,Ring),[GF,Strategy]},
     Headline => "make a finite field",
     SYNOPSIS (
	  BaseFunction => GF,
	  Usage => "GF(p,n)\nGF(q)",
	  Inputs => {
	       "p" => "a prime number", "n",
	       Variable => Symbol => "the name to use for the generator of the field",
	       SizeLimit => ZZ => {
		    "the limit on the size of a Galois field whose elements will be represented
		    internally as powers of the primitive element"
		    }
	       },
	  Outputs => {
	       GaloisField => {"a finite field with ", TT "q = p^n", " elements"},
	       },
	  "The generator of this ring is a primitive element: it generates
	  the multiplicative group of non-zero elements.",
	  PARA{"If the single argument form GF(q) is given, q should be a prime power q = p^n"},
	  EXAMPLE lines ///
	  A = GF(3,2,Variable=>b);
	  ambient A
	  b^8
	  b^4
	  ///,
	  EXAMPLE lines ///
	  K = GF 8
	  x = K_0
	  x^3+x
	  ///
	  ),
     SeeAlso => {toField},
     SYNOPSIS (
	  BaseFunction => GF,
	  Usage => "GF R",
	  Inputs => {
	       "R" => Ring => {
		    "A quotient of a polynomial ring over ", TT "ZZ/p", " in one variable, modulo
	       	    an irreducible polynomial"
		    },
	       PrimitiveElement => {
		    "either an element of ", TT "R", ", or the symbol ", TO FindOne,
		    ".  An element is primitive if it generates the multiplicative group
		    of non-zero elements of R"
		    },
	       SizeLimit => ZZ => {
		    "the limit on the size of a Galois field whose elements will be represented
		    internally as powers of the primitive element"
		    }
	       },
	  Outputs => {
	       GaloisField => {"a finite field isomorphic to ", TT "R"}
	       },
	  EXAMPLE lines ///
	  A = ZZ/5[a]/(a^3-a-2)
	  B = GF A
	  C = ZZ/5[b]/(b^3+1+3*b^2+b)
	  D = GF C
	  map(B,D,{a^2})
	  ///
	  )
     }

