thisPackage := newPackage ( "Classic",
     Authors => {
	  { Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/" }
	  },
     Date => "June, 2006",
     Version => "1.0",
     Headline => "a classic Macaulay parser",
     DebuggingMode => true
     )

needsPackage "Parsing"

symbolP = (transform (x -> ( if not isGlobalSymbol x then error("symbol ",x," undefined"); getGlobalSymbol x ))) letterParser
listP = comma -> parser -> (transform splice) (parser @ * (transform last) (fixedStringParser comma @ parser))
variableP = (transform value) symbolP
intP = natNumberP | variableP
subscriptP = (transform ((lb,x,rb) -> x)) andP( fixedStringParser "[", (transform unsequence) (listP ",") intP, fixedStringParser "]" )
ringVariableP = (transform ((x,n) -> if n === nil then value x else x_n)) (symbolP @ optP subscriptP)
numberP = ZZParser | QQParser
powerP = (transform ((x,n) -> if n === nil then x else x^n)) ((futureParser parenExprP | ringVariableP) @ optP natNumberP)
monomialP = (transform (times @@ deepSplice)) (optionalSignParser @ (numberP @ *powerP | +powerP ))
polyP = (transform (plus @@ deepSplice)) (+monomialP) | terminalParser 0
parenExprP = (transform ((lp,x,rp) -> x)) andP(fixedStringParser "(", futureParser parenExprP | polyP, fixedStringParser ")")
listPolyP = (transform toList) (listP ",") polyP
arrayPolyP = (transform toList) (listP ";") listPolyP
export poly ; poly = method()
poly String :=  polyP ++ nonspaceAnalyzer
ideal String := ideal @@ (listPolyP ++ nonspaceAnalyzer)
matrix String := opts -> s -> matrix((arrayPolyP ++ nonspaceAnalyzer) s, opts)

beginDocumentation()

document { 
     Key => {poly,(poly,String)},
     Headline => "make a polynomial using classic Macaulay-like syntax",
     Usage => "poly s",
     Inputs => {
	  "s" => String
	  },
     Outputs => {
	  RingElement
	  },
     "The classic Macaulay polynomial format is useful for fast entry
     of polynomials, ideals, and matrices.  Only ring variables which are
     single letters, or single letters indexed by a sequence of numbers can
     be handled with this parser.",
     PARA{},
     "The rules for creating polynomials using the classic parser include:",
     UL {
	  "Spaces and newline characters are completely ignored.",

	  "A variable is either (1) a single letter, (2) a subscripted variable, or
	  (3) a polynomial enclosed in parentheses.",

	  "Subscripted variables, e.g. y_(1,1) are written using brackets, as in
	  y[1,1].  Instead of explicit numbers for subscripted variables, Macaulay2
	  user variables which have an integer value may be used.",

	  "Coefficients are either integers or rational numbers, starting with
	  a + or - (which may be omitted for the first monomial).  Over finite
	  fields, the division is performed in that field.",

	  "A monomial is written without symbols for multiplication or exponentiation,
	  where the (optional) coefficient comes first.",

	  "A polynomial is a collection of monomials one after the other.",
	  },
     "Except for indices for subscripted variables, integers must be explicitly given.
     All of the variables used must already belong to a specific ring.  If in doubt, 
     first do ", TO (use,Ring), ".",
     EXAMPLE {
	  ///R = ZZ/32003[a..d,x_1..x_4,y_(1,1)..y_(2,2)];///,
	  ///poly"a2b-3ab-1"///,
	  ///poly"a2y[1,1]3-3ab-1"///,
	  ///poly"(a+b)(a+2c)"///,
	  ///poly"(a+b+c)3-1"///,
	  ///poly"3/4a2b-3ab-1"///,
	  ///poly"a5+5a4b+10a3b2+10a2b3+5ab4+b5
	        -10a4c-40a3bc-60a2b2c-40ab3c-10b4c
		+40a3c2+120a2bc2+120ab2c2+40b3c2
		-80a2c3-160abc3-80b2c3+80ac4+80bc4-32c5"///,
	  ///poly"(a+(c+y[1,1])2)2-1"///
	  },
     Caveat => {"Ring variables which are not single characters, or are not
	  indexed by a sequence of integers, cannot be input using this function."
	  },
     SeeAlso => {(ideal,String),(matrix,String)}
     }

document { 
     Key => (ideal,String),
     Headline => "make an ideal using classic Macaulay-like syntax",
     Usage => "ideal s",
     Inputs => {
	  "s" => "in the form: \"f1,f2,...,fr\""
	  },
     Outputs => {
	  Ideal
	  },
     "Creates an ideal using an abbreviated format.
     Each polynomial has the form described in ", TO (poly,String), 
     ".  The polynomials are separated by commas.
     Spaces and newline characters are ignored. ",
     EXAMPLE {
	  "R = ZZ/32003[a..d,x_1..x_4];",
	  ///I = ideal"a+b2-1,(a+b)(c+d),x[1]-x[2]3"///
	  },
     Caveat => {"Ring variables which are not single characters, or are not
	  indexed by a sequence of integers, cannot be input using this function."
	  },
     SeeAlso => {(poly,String), (matrix,String)}
     }

document { 
     Key => (matrix,String),
     Headline => "make a matrix using classic Macaulay-like syntax",
     Usage => "matrix s",
     Inputs => {
	  "s" => "in the form: \"f1,f2,...,fr;g1,...,gr;...;h1,...,hr\""
	  },
     Outputs => {
	  Matrix
	  },
     "Creates a matrix using an abbreviated format.
     Each polynomial has the form described in ", TO (poly,String), 
     ".  The rows of the matrix are separated by semicolons, and
     within each row, the polynomials are separated by commas.  Any
     entry which is missing is assumed to be 0.
     Spaces and newline
     characters are ignored. ",
     EXAMPLE {
	  "R = ZZ/32003[a..d,x_1..x_4];",
	  ///N = matrix"a,b,c,d;x[1],x[2],x[3],x[4]"///,
	  ///M = matrix"ad-2c3,(a-b)2+1,ab;,abc-1,"///,
	  },
     Caveat => {"Ring variables which are not single characters, or are not
	  indexed by a sequence of integers, cannot be input using this function."
	  },
     SeeAlso => {(poly,String), (ideal,String)}
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
