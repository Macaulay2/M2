-- -*- coding: utf-8 -*-
newPackage ( "Classic",
     Authors => {
	  { Name => "Daniel R. Grayson", Email => "dan@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~dan/" }
	  },
     Date => "June, 2006",
     Version => "1.0",
     Headline => "a classic Macaulay parser",
     PackageImports => {"Parsing"}
     )

-- the following comment is needed:
-- start --
export "poly"
symbolP = (x -> (
	  if not isGlobalSymbol x then error("symbol ",x," undefined");
 	  getGlobalSymbol x)) % letterParser
seqP = (comma, parser) -> prepend % parser @ * (last % comma @ parser)
variableP = value % symbolP
intP = NNParser | variableP
subscriptP = ((lb,x,rb) -> x) % andP( "[", unsequence % seqP_"," intP, "]" )
ringVariableP = ((x,n) -> value if n === nil then x else x_n) % 
                symbolP @ optP subscriptP
numberP = ZZParser | QQParser
powerP = ((x,n) -> if n === nil then x else x^n) % 
         (futureParser parenExprP | ringVariableP) @ optP NNParser
monomialP = times @@ deepSplice % 
            optionalSignParser @ (numberP @ *powerP | +powerP )
polyP = plus @@ deepSplice % +monomialP | terminalParser 0
parenExprP = ((l,x,r) -> x) % andP("(", futureParser parenExprP | polyP, ")")
listPolyP = toList % seqP_"," polyP
arrayPolyP = toList % seqP_";" listPolyP
poly = method()
polyParser = polyP : nonspaceAnalyzer
poly String :=  RingElement => x -> polyParser x
idealParser = ideal % listPolyP : nonspaceAnalyzer
ideal String := Ideal => x -> idealParser x
monomialIdealParser = monomialIdeal % listPolyP : nonspaceAnalyzer
monomialIdeal String := MonomialIdeal => x -> monomialIdealParser x
matrix String := Matrix => 
                 opts -> matrix_opts % arrayPolyP : nonspaceAnalyzer
-- end --
-- the preceding comment is needed:

beginDocumentation()

sourcecode = (() -> (
	  l := lines get currentFileName;
	  i := position(l, match_"^-- start --");
	  j := position(l, match_"^-- end --");
	  stack take(l, {i+1,j-1})
	  )) ()

document {
     Key => Classic,
     Headline => "a parser for classic Macaulay syntax",
     PARA "This package provides a parser for polynomials in the classic Macaulay format.  Some users prefer it, for ease and speed of typing
	  polynomials, ideals, and matrices.",
     PARA "Only ring variables that are single letters, or single letters indexed by a sequence of numbers can be handled with this parser.",
     PARA "The rules for creating polynomials using the classic parser include:",
     UL {
	  LI "Spaces, tabs, and newline characters are completely ignored.",
	  LI "A variable is either (1) a single letter, (2) a subscripted variable, or (3) a polynomial enclosed in parentheses.",
	  LI { "Subscripted variables, e.g., ", TT "y_(1,1)", ", are written using brackets, as in ", TT "y[1,1]", ".  
	       Instead of explicit numbers for subscripted variables, Macaulay2 user variables that have an integer value may be used."},
	  LI { "Coefficients are either integers or rational numbers, starting with a + or - (which may be omitted for the first monomial).  Over finite
	       fields, the division is performed in that field."},
	  LI { "A monomial is written without symbols for multiplication or exponentiation,
	       where the (optional) coefficient comes first."},
	  LI "A polynomial is a collection of monomials one after the other.",
 	  LI "Parenthesized subexpressions are allowed.",
	  LI "Except for indices for subscripted variables, integers must be explicitly given.",
     	  LI {"All of the variables used must already belong to a specific ring.  If in doubt, 
     	       first type ", TT "use R", " to ensure that all the symbols of ", TT "R", " are in use."}
	  },
     PARA { "The source code for this parser is relatively short, since it is based on the package ", TO "Parsing", ".  Here it is." },
     TABLE { "class" => "examples",  TR TD PRE sourcecode },
     Subnodes => {
	  TO (poly,String),
	  TO (ideal,String),
	  TO (matrix,String)
	  }
     }

document { 
     Key => {(poly,String),poly},
     Headline => "make a polynomial using classic Macaulay syntax",
     Usage => "poly s",
     Inputs => { "s" },
     Outputs => { RingElement => { "created from ", TT "s", " using classic Macaulay syntax, as described in ", TO "Classic" } },
     EXAMPLE lines ///
	  R = ZZ/32003[a..d,x_1..x_4,y_(1,1)..y_(2,2)];
	  poly "a2b-3ab-1"
	  poly "a2y[1,1]3-3ab-1"
	  poly "(a+b)(a+2c)"
	  poly "(a+b+c)3-1"
	  poly "3/4a2b-3ab-1"
	  poly "a5+5a4b+10a3b2+10a2b3+5ab4+b5-10a4c-40a3bc-60a2b2c-40ab3c-10b4c"
	  poly "(a+(c+y[1,1])2)2-1"
     ///,
     SeeAlso => {(ideal,String),(matrix,String)}
     }

document { 
     Key => (ideal,String),
     Headline => "make an ideal using classic Macaulay syntax",
     Usage => "ideal s",
     Inputs => {
	  "s" => "in the form: \"f1,f2,...,fr\""
	  },
     Outputs => {
	  Ideal
	  },
     PARA {
	  "Creates an ideal using an abbreviated format. Each polynomial has the form described in ", TO Classic, ".  The polynomials are separated by commas.
     	  Spaces and newline characters are ignored."
	  },
     EXAMPLE lines ///
	  R = ZZ/32003[a..d,x_1..x_4];
	  I = ideal "a+b2-1,(a+b)(c+d),x[1]-x[2]3"
     ///,
     SeeAlso => {(poly,String), (matrix,String)}
     }

document { 
     Key => (monomialIdeal,String),
     Headline => "make a monomial ideal using classic Macaulay syntax",
     Usage => "monomialIdeal s",
     Inputs => {
	  "s" => "in the form: \"f1,f2,...,fr\""
	  },
     Outputs => { MonomialIdeal },
     PARA {
	  "Creates a monomial ideal using an abbreviated format.  Each polynomial has the form described in ", TO Classic, ".  The polynomials are separated by commas.
     	  Spaces and newline characters are ignored."
	  },
     EXAMPLE lines ///
	  R = ZZ[a..e];
	  I = monomialIdeal "abc,bcd,cde11"
     ///,
     SeeAlso => {(poly,String), (matrix,String)}
     }

document { 
     Key => (matrix,String),
     Headline => "make a matrix using classic Macaulay syntax",
     Usage => "matrix s",
     Inputs => { "s" => { "in the form: ", TT "\"f1,f2,...,fr;g1,...,gr;...;h1,...,hr\"" } },
     Outputs => { Matrix },
     "Creates a matrix using an abbreviated format. Each polynomial has the form described in ", TO Classic, ".  The rows of the matrix are separated by semicolons, and
     within each row, the polynomials are separated by commas.  Any entry that is missing is assumed to be 0. Spaces and newline characters are ignored. ",
     EXAMPLE lines ///
	  R = ZZ/32003[a..d,x_1..x_4];
	  N = matrix "a,b,c,d;x[1],x[2],x[3],x[4]"
	  M = matrix "ad-2c3,(a-b)2+1,ab;,abc-1,"
     ///,
     SeeAlso => {(poly,String), (ideal,String)}
     }

TEST ///
     R = QQ[x,y,z]
     assert (poly "x2y3-11z+1" == x^2 * y^3 - 11*z + 1)
     assert (ideal "x2,y3-1,z+3" == ideal (x^2, y^3-1, z+3))
     assert (monomialIdeal "x2,yz2" == monomialIdeal (x^2, y*z^2))
     assert (matrix "x,11y,z;x2,y-1,z5" == matrix {{x,11*y,z},{x^2,y-1,z^5}})
///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=Classic pre-install"
-- End:
