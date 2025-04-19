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
	       Variable => Symbol => {
		   "the name to use for the generator of the field.  If null, ",
		   "then ", VAR "a", " is used."},
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
	  a^3+a
	  ///
	  ),
     SeeAlso => {toField, "finite fields"},
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
	  ),
     }

document {
     Key => isPrimitive,
     Headline => "whether an element is a primitive element of a finite field",
     TT "isPrimitive(f)", " -- Given an element ", TT "f", " in a quotient of a polynomial ring ",
     TT "R", " over a finite field ", TT "K", "which is itself a finite field,
      with the ring being finite dimensional over the field,
     determine if ", TT "f", " generates the multiplicative group of this field.",
     EXAMPLE { "R = ZZ/5[t]/(t^2+t+1);", "isPrimitive t", "isPrimitive (t-1)" }
     }

document {
     Key => order,
     Headline => "a key used internally",
     TT "order", " -- used as a key inside finite fields under which is
     stored the number of elements in the field.  Intended for internal use only",
     PARA{},
     SeeAlso => "GaloisField"
     }
