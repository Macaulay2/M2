--- status: TODO
--- author(s): MES
--- notes: 

document { 
     Key => {someTerms,(someTerms,RingElement,ZZ,ZZ)},
     Headline => "select some terms of a polynomial",
     Usage => "someTerms(f,i,n)",
     Inputs => {
	  "f" => RingElement => "in a polynomial ring",
	  "i" => ZZ => "the starting index",
	  "n" => ZZ => "the number of terms to select"
	  },
     Outputs => {
	  RingElement => {"the ", TT "n", " terms of the polynomial ", TT "f", " starting with the 
	  ", TT "i", "-th one"}
	  },
     "The index i is 0-based, and a negative number refers to location from the end of the polynomial.",
     EXAMPLE {
	  "R = ZZ[a..d];",
	  "f = (a+2*b-3)^2",
	  "someTerms(f,0,1) -- the lead term",
	  "someTerms(f,-1,1) -- the last term",
	  "someTerms(f,-2,2) -- the last 2 terms"
	  },
     "Here is an example where the coefficient ring is also a polynomial ring.",
     EXAMPLE {
	  "S = R[x,y,z];",
	  "f = (x*(a-b)+y^2+a-1)^2",
	  "someTerms(f,-1,1) -- the last term",
	  "someTerms(f,-2,2) -- the last 2 terms"
	  },
     SeeAlso => {monomials, coefficients, terms, leadTerm}
     }
