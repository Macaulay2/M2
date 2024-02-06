--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {monomials,(monomials,RingElement),(monomials,Matrix)},
     Headline => "matrix of monomials in a ring element or matrix",
     Usage => "monomials f",
     Inputs => {
	  "f" => {"either a ", TO "Matrix", " or a ", TO "RingElement"}
	  },
     Outputs => {
	  Matrix => {"a one row matrix in the same ring with all of the monomials that
	       appear in ", TT "f"}
	  },
     "Each monomial only appears once, and the monomials are sorted in what order?",
     EXAMPLE {
	  "R = ZZ[a..d,x,y];",
	  "m = matrix{{a*x^2+b*x*y+c*y^2, a*x^3+b*x^2*y+c*x*y^2+d*y^3+a*x^2}}",
	  "monomials m"
	  },
     "If the monomials in only some of the variables are desired, use the optional
     argument ", TO [monomials,Variables], ".",
     EXAMPLE {
	  "monomials(m, Variables=>{x,y})"
	  },
     SeeAlso => {coefficients}
     }
document { 
     Key => [monomials, Variables],
     Headline => "specify variables",
     Usage => "monomials(m,Variables=>x)",
     Inputs => {
	  "x" => {"either a list or sequence of ring variables, 
	       a list or sequence of integers denoting indices of
	       ring variables, or a single ring element or integer index.
	       The default value is: all of the variables"}
	  },
     Consequences => {
	  {"Each variable not in the set of variables x is considered a coefficient"}
	  },     
     SeeAlso => {coefficients}
     }
