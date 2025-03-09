-- -*- coding: utf-8 -*-
--		Copyright 1993-1999 by Daniel R. Grayson

document {
     Key => {getChangeMatrix,(getChangeMatrix, GroebnerBasis)},
     Headline => "get the change of basis matrix",
     TT "getChangeMatrix G", " -- for a Gröbner basis G, return the change of
     basis matrix from the Gröbner basis to another generating set, 
     usually a minimal, or original, generating set.",
     PARA{},
     "The option ", TO "ChangeMatrix", " can be used with ", TO "gb", " 
     to enable the computation of the change of basis matrix."
     }

document {
     Key => {(modulo, Matrix, Matrix),modulo,(modulo, Matrix, Nothing),(modulo, Nothing, Matrix)},
     Headline => "find the pre-image (pullback) of image of a map (low level version)",
     Usage => "modulo(f,g)",
     Inputs => { "f", "g" },
     Outputs => {
	  Matrix => { " whose image is the pre-image (pullback) of the image of ", TT "g", " under ", TT "f" }
	  },
     PARA {
     	  "The maps ", TT "f", " and ", TT "g", " must have the same target, and their sources and targets must be free.
     	  If ", TT "f", " is ", TO "null", ", then it is taken to be the identity.  If ", TT "g", " is ", TO "null", ", it is taken to be zero."
	  },
     PARA {"This function is mainly for internal use."},
     EXAMPLE lines ///
     R = QQ[x,y,z]
     f = matrix {{x,y}}
     g = matrix {{y,z}}
     modulo(f,g)
     kernel( inducedMap(coker g, target g) * f )
     ///
     }

document {
     Key => {(complement, Matrix),complement},
     Headline => "find the minimal generators for cokernel of a matrix (low level form)",
     TT "complement f", " -- for a matrix ", TT "f", ", return a map ", TT "g", " with the same
     target whose columns are minimal generators for the cokernel of ", TT "f", ".",
     PARA{},
     "The map ", TT "f", " must be homogeneous."
     }

-----------------------------------------------------------------------------

document {
     Key => {homogenize,(homogenize, Ideal, RingElement),(homogenize, Matrix, RingElement),
	  (homogenize, Matrix, RingElement, List),(homogenize, Module, RingElement),
	  (homogenize, Module, RingElement, List),(homogenize, RingElement, RingElement),(homogenize, RingElement, RingElement, List),
	  (homogenize, Vector, RingElement),(homogenize, Vector, RingElement, List)},
     Headline => "homogenize with respect to a variable",
     TT "homogenize(m,v)", " -- homogenize the ring element, vector,
     matrix, or module ", TT "m", " using the variable ", TT "v", " in the ring of ", TT "m", ".",
     BR{},     
     TT "homogenize(m,v,w)", " -- homogenize ", TT "m", " using the variable ", TT "v", ",
     so that the result is homogeneous with respect to the given list ", TT "w", " of
     integers provided as weights for the variables.",
     PARA{},
     EXAMPLE {
	  "R = ZZ/101[x,y,z,Degrees => {1,2,3}]",
      	  "f = 1 + y + z^2",
      	  "homogenize(f,x)",
      	  "homogenize(f,x,{1,0,-1})",
	  },
     PARA{},
     "The weights that may be used are limited (roughly) to the range -2^30 .. 2^30.",
     PARA{},
     Caveat => {
	  "If the homogenization overflows the monomial, this is not
     	  reported as an error."
	  }
     }


document {
     Key => Ascending,
     Headline => "specify ascending order",
     TT "Ascending", " -- a symbol used as a value for optional
     arguments ", TO "DegreeOrder", " and ", TO "MonomialOrder", "."
     }

document {
     Key => Descending,
     Headline => "specify descending order",
     TT "Descending", " -- a symbol used as a value for optional
     arguments ", TO "DegreeOrder", " and ", TO "MonomialOrder", "."
     }

document {
     Key => DegreeOrder,
     Headline => "sort primarily by degree",
     TT "DegreeOrder", " -- an optional argument for use with certain
     functions, used to specify sort order."
     }

document {
     Key => {selectInSubring,(selectInSubring, ZZ, Matrix)},
     Headline => "select columns in a subring",
     Usage => "selectInSubring(i,m)",
     Inputs => {
	  "i" => ZZ,
	  "m" => Matrix
	  },
     Outputs => {
	  Matrix => {"with the same target and ring as ", TT "m", ", consisting of those columns
	  of ", TT "m", " which lie in the subring where the first 
	  ", TT "i", " blocks of the monomial order are zero"}
	  },
     "For example, consider the following block (or product) order.",
     EXAMPLE lines ///
     	  R = QQ[x,y,a..d,t,MonomialOrder=>{2,4,1}];
	  m = matrix{{x*a-d^2, a^3-1, x-a^100, a*b*d+t*c^3, t^3-t^2-t+1}}
	  selectInSubring(1,m)
	  selectInSubring(2,m)
     ///,
     PARA{},
     "The lexicographic order is considered as one block, as in the following example.",
     EXAMPLE lines ///
     	  S = QQ[a..d,MonomialOrder=>Lex];
	  m = matrix{{a^2-b, b^2-c, c^2-d, d^2-1}}
	  selectInSubring(1,m)
     ///,
     PARA{},
     "If you wish to be able to pick out the elements not involving a, or a and b, etc,
     then create a block monomial order.",
     EXAMPLE lines ///
     	  S = QQ[a..d,MonomialOrder=>{4:1}];
	  m = matrix{{a^2-b, b^2-c, c^2-d, d^2-1}}
	  selectInSubring(1,m)
	  selectInSubring(2,m)
	  selectInSubring(3,m)	  
     ///,
     Caveat => {
	  "This routine doesn't do what one would expect for graded orders
     	  such as ", TT "GLex", ".  There, the first part of the monomial 
	  order is the degree, which is usually not zero."
          },
     SeeAlso => {"monomial orderings", leadTerm, "Elimination::eliminate"}
     }

document {
     Key => {divideByVariable,(divideByVariable, Matrix, RingElement),(divideByVariable, Matrix, RingElement, ZZ)},
     Headline => "divide all columns by a (power of a) variable",
     TT "divideByVariable(m,v)", " -- divide each column of the matrix 'm' by 
     as high a power of the variable 'v' as possible.",
     BR{},
     TT "divideByVariable(m,v,d)", " -- divide each column of the matrix 'm' by 
     as high a power of the variable 'v' as possible, but divide by no more than v^d.",
     PARA{},
     EXAMPLE {
	  "R = ZZ/101[a..d]",
      	  "m = matrix{{a*b, a^2*c}, {a*b^2, a^4*d}}",
      	  "divideByVariable(m,a)",
      	  "divideByVariable(m,a,1)",
	  },
     Caveat => "You can only divide by a variable, not a monomial,
     and you have little control on what power will be divided.  This routine is mostly
     used by the saturation commands as a fast internal way of dividing.",
     PARA{},
     "We may eliminate this routine."
     }

document {
     Key => {newCoordinateSystem,(newCoordinateSystem, PolynomialRing, Matrix)},
     Headline => "change variables",
     TT "newCoordinateSystem(S,m)", " -- takes a one-rowed matrix ", TT "m", " of
     independent linear forms over a ring ", TT "R", " and returns a pair
     ", TT "(f,g)", ", where ", TT "f", " is a ring map given by some linear change 
     of coordinates from ", TT "R", " to ", TT "S", " which sends the last variables 
     of ", TT"R", " to the forms in ", TT "m", ", and ", TT "g", " is the inverse 
     of ", TT "f", ".",
     PARA{},
     "The ring ", TT "S", " should have the same number of variables as 
     ", TT "S", ".",
     EXAMPLE {
	  "R = ZZ/101[a..d]",
      	  "S = ZZ/101[p..s]",
      	  "(f,g) = newCoordinateSystem(S,matrix{{a+2*b,3*c-d}});",
	  "f",
	  "g"
	  },
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
