-- -*- coding: utf-8 -*-
--		Copyright 1993-1999 by Daniel R. Grayson

document {
     Key => {(symbol /, Module, Module),
	  (symbol /, Module, Ideal),
	  (symbol /, Module, List),
	  (symbol /, Module, Sequence),
	  (symbol /, Module, Vector),
	  (symbol /, Module, RingElement)},
     Headline => "quotient module",
     Usage => "M/N",
     Inputs => {
	  "M",
	  "N" => Nothing => {
	       ofClass Module, ", ",
	       ofClass Ideal, ", ",
	       ofClass List, ", ",
	       ofClass Sequence, ", ",
	       ofClass RingElement, ", or ",
	       ofClass Vector}
	  },
     Outputs => {
	  Module => "The quotient module M/N of M"
	  },
     "If N is an ideal, ring element, or list or sequence of 
     ring elements (in the ring of M), 
     then the quotient is
     by the submodule N*M of M.",
     PARA{},
     "If N is a submodule of M, or a list or sequence of submodules, or a vector, then the quotient
     is by these elements or submodules.",
     EXAMPLE lines ///
     	  R = ZZ/173[a..d]
	  M = ker matrix{{a^3-a*c*d,a*b*c-b^3,a*b*d-b*c^2}}
	  M/a == M/(a*M)
	  M/M_0
	  M/(R*M_0 + b*M)
	  M/(M_0,a*M_1+M_2)
     	  presentation oo
	  ///,
      SeeAlso => {"subquotient modules", presentation}
     }

document {
     Key => (symbol /, Ideal, Ideal),
     Headline => "quotient module",
     Usage => "I/J",
     Inputs => { "I", "J" => {"in the same ring as ", TT "I"}},
     Outputs => {
	  Module => {"The quotient module ", TT "(I+J)/J"}
	  },
     EXAMPLE lines ///
     	  R = QQ[a,b,c]
	  I = ideal vars R
	  M = I / I^2
	  ///,
     "There is a difference between typing I/J and (I+J)/J
     in Macaulay2, although conceptually they are the same module.
     The former has as its generating set the generators of I,
     while the latter has as its (redundant) generators 
     the generators of I and J.  Generally, the former method is preferable.",
     EXAMPLE lines ///     
	  gens M
	  N = (I + I^2)/I^2
	  gens N
     ///,
     SeeAlso => {"subquotient modules", generators}
     }


document {
     Key => {(symbol ^,Module,Array),
       (symbol ^,ChainComplex,Array)},
     Headline => "projection onto summand",
     Usage => "M^[i,j,...,k]",
     Inputs => {"M" => {"or ", ofClass ChainComplex},
	  Nothing => {TT "[i,j,...,k]", ", an array of indices"}},
     Outputs => {
     	  Nothing => {ofClass Matrix, ", or ", ofClass ChainComplexMap}
	  },
     PARA{},
     "The module ", TT "M", " should be a direct sum, and the result is the map
     obtained by projection onto the sum of the components numbered or named
     ", TT "i, j, ..., k", ".  Free modules are regarded as direct sums of modules.",
     PARA{},
     EXAMPLE lines ///
	  M = ZZ^2 ++ ZZ^3
      	  M^[0]
      	  M^[1]
      	  M^[1,0]
	  ///,
     PARA{},
     "If the components have been given names (see ", TO directSum, "), use those instead.",
     EXAMPLE lines ///
	  R = QQ[a..d];
	  M = (a => image vars R) ++ (b => coker vars R)
	  M^[a]
	  isWellDefined oo
	  M^[b]
	  isWellDefined oo
	  isWellDefined(M^{2})
	  ///,
     PARA{},
     "This works the same way for chain complexes.",
     EXAMPLE lines ///
	  C = res coker vars R
	  D = (a=>C) ++ (b=>C)
	  D^[a]
	  ///,
     SeeAlso => {directSum, (symbol ^,Matrix,Array), (symbol _,Module,Array),(symbol ^,Module,List)}
     }

document { 
     Key => {
	  (symbol ^, Matrix, Array),
	  (symbol ^, ChainComplexMap, Array),
	  (symbol ^, GradedModuleMap, Array)
	  },
     Headline => "component of map corresponding to summand of target",
     Usage => "F^[i,j,...,k]",
     Inputs => {"F" => {"or ", ofClass{ChainComplexMap,GradedModuleMap}},
	  Nothing => {TT "[i,j,...,k]", ", an array of indices"}},
     Outputs => {
     	  Nothing => ofClass{Matrix, ChainComplexMap, GradedModuleMap}
	  },
     "The target of the module or chain complex ", TT "F", " should be a 
     direct sum, and the result is the component of this map 
     corresponding to the sum of the components numbered or named
     ", TT "i, j, ..., k", ".  Free modules are regarded as direct sums of modules.
     In otherwords, this routine returns the map given by certain blocks of columns.",
     EXAMPLE lines ///
          R = ZZ[a..d];
          F = (vars R) ++ ((vars R) ++ matrix{{a-1,b-3},{c,d}})
	  F^[1]
	  F_[1]^[1]
          ///,
     PARA{"If the components have been given names (see ", TO directSum, "), use those instead."},
     EXAMPLE lines ///
          G = (a=>R^2) ++ (b=>R^1)
	  N = map(G,R^2, (i,j) -> (i+37*j)_R)
	  N^[a]
	  N^[b]
     	  N = directSum(x1 => matrix{{a,b-1}}, x2 => matrix{{a-3,b-17,c-35}}, x3 => vars R)
	  N^[x1,x3]
	  ///,
     PARA {"This works the same way for maps between chain complexes."},
     SeeAlso => {(symbol^,Matrix,Array),(symbol_,Module,Array),directSum}
     }

document {
     Key => (symbol ^, Module, List),
     Headline => "projection onto summand",
     TT "M^{i,j,k,...}", " -- provides the projection map from a free module
     ", TT "M", " to the free module corresponding to the basis vectors whose
     index numbers are listed.",
     PARA{},
     EXAMPLE "(ZZ^5)^{2,3}",
     SeeAlso => {"_", Module, List}
     }

     
-----------------------------------------------------------------------------

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
     Key => {(symbol //, Matrix, Matrix),(symbol \\, Matrix, Matrix),
       (symbol //, RingElement, MonomialIdeal),
       (symbol //, RingElement, GroebnerBasis),
       (symbol //, RingElement, RingElement),
       (symbol //, Matrix, MonomialIdeal),
       (symbol //, Matrix, GroebnerBasis),
       (symbol //, Matrix, RingElement),(symbol \\, Matrix, RingElement),
       (symbol //, RingElement, Matrix),(symbol \\, RingElement, Matrix)
       },
     Headline => "factor a map through another",
     Usage => "f//g\ng\\\\f",
     Inputs => {
	  "f" => {"between modules F --> H, or ",
	     ofClass RingElement},
	  "g" => {"between modules G --> H, ",
	       ofClass RingElement, ", ", 
	       ofClass MonomialIdeal, ", or ",
	       ofClass GroebnerBasis}
	  },
     Outputs => {
	  Matrix => "a matrix h : F --> G"
	  },
     "If ", TT "f", " is a matrix, and ", TT "g", " is a matrix or Gröbner basis, then ", TT "quotient(f,g)", " is an alternate 
     notation for ", TT "f//g", ".",
     PARA{},
     "If either ", TT "f", " or ", TT "g", " is a ring element, then it is taken to be a scalar matrix acting on ", TT "H", ".  If both are ring elements,
     then the result is also a ring element.  If ", TT "g", " is a
     ", TO "MonomialIdeal", ", then it is taken to be the matrix of generators of ", TT "g", ".  Finally, if ", TT "g", " is a ", TO "GroebnerBasis", "
     object, then the Gröbner basis as so far computed is used.  In these latter two cases, no Gröbner bases 
     will be computed.",
     PARA{},
     "The resulting matrix ", TT "h", " is such that ", TT "f - g*h", " is the reduction of ", TT "f", " modulo a Gröbner basis 
     for the image of ", TT "g", ".",
     PARA{},
     "If the remainder ", TT "f - g*h", " is zero,
     then the quotient ", TT "f//g", " satisfies the equation ", TT "f === g * (f//g)", "
     and the quotient ", TT "g\\\\f", " satisfies the equation ", TT "f === g * (g\\\\f)", ".",
     PARA{},
     "One common use is the following.  If an ideal contains 1, then we may write 1 in terms
     of the generators of the ideal.  First we make an ideal.",
     EXAMPLE lines ///
     A = ZZ/101[x,y,z]
     F = x^4 - y*z*(1-x)^2 - z - y^3
     I = ideal(F,diff(x,F),diff(y,F),diff(z,F))
     ///,
     "Transposing the (row) matrix of generators of the ideal puts the generators on separate lines and shows the degrees.",
     EXAMPLE lines ///
     transpose gens I
     ///,
     "Next we test whether 1 is in the ideal.",
     EXAMPLE lines ///
     1 % I
     ///,
     "We see that 1 is in the ideal.  Now we represent 1 in terms of the generators of ", TT "I", ".",
     EXAMPLE lines ///
     h = 1 // gens I
     gens I * h
     ///,
     SeeAlso => {(symbol %, Matrix, Matrix), generators, diff, substitute, quotient, remainder, quotientRemainder }
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

document {
     Key => PrimitiveElement,
     Headline => "specify a primitive element",
     TT "PrimitiveElement => g", " -- an option used with ", TO "GF", ".",
     PARA{},
     "The value can be a ring element providing a primitive element, or the
     symbol ", TO "FindOne", " (the default) which specifies that
     ", TO "GF", " should search for a primitive element."
     }

document {
     Key => FindOne,
     Headline => "find a primitive element",
     TT "FindOne", " -- a value for the option ", TO "PrimitiveElement", "
     to ", TO "GF", " which specifies that ", TO "GF", " should search 
     for a primitive element."
     }

document {
     Key => Variable,
     Headline => "specify a name for a variable",
     -- it is also used with integralClosure, but we should automate that
     TT "Variable => x", " -- an option used with ", TO "GF", ", to specify
     a symbol to be used as a name for the generator of the Galois field."
     }

document {
     Key => GaloisField,
     Headline => "the class of all Galois fields" }

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
     Headline => "a key used internally ",
     TT "order", " -- used as a key inside finite fields under which is
     stored the number of elements in the field.  Intended for internal use only",
     PARA{},
     SeeAlso => "GaloisField"
     }


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
