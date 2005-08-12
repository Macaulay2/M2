--- status: TODO
--- author(s): MES
--- notes: 
document {
     Key => coefficients,
     Headline => "the coefficients",
     TT "coefficients({i,j,...},p)", " -- yields the coefficients and
     monomials of the polynomial or matrix p with respect to variables 
     numbered i, j, ... .",
     BR,NOINDENT,
     TT "coefficients(p)", " -- yields the coefficients and monomials of
     the polynomial or matrix p with respect to all of the variables."
     }

document { 
     Key => coefficients,
     Headline => "monomials and their coefficients in a ring element or matrix",
     Usage => "(monoms,coeffs) = coefficients f",
     Inputs => {
	  "f" => {"either a ", TO "Matrix", " or a ", TO "RingElement"}
	  },
     Outputs => {
	  "monoms" => Matrix => {"a one row matrix of the 
	                         monomials appearing in ", TT "f"},
	  "coeffs" => Matrix => {"a one row matrix of the 
	                         monomials appearing in ", TT "f"},
	   
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (coefficients,Matrix),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => (coefficients,RingElement),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [coefficients,Monomials]
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [coefficients,Variables],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
 -- doc1.m2:521:     Key => coefficients,
