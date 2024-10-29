-- -*- coding: utf-8 -*-
-- -*- coding: utf-8 -*-
--		Copyright 1993-1999 by Daniel R. Grayson
document {
     Key => contract,
     Headline => "contract one matrix by another",
     SeeAlso => {"diff and contract",contract'}
     }

document {
     Key => {(contract,Matrix,Matrix),(contract,RingElement,RingElement), (contract,Vector,RingElement),
	  (contract,RingElement,Vector), (contract,Vector,Vector), (contract,Matrix,RingElement),
       	  (contract,RingElement,Matrix), (contract,Vector,Matrix), (contract,Matrix,Vector),
	  (contract,Number,RingElement), (contract,RingElement,Number), (contract,Number,Number), 
	  (contract,Number,Vector), (contract,Vector,Number), (contract,Number,Matrix),
     	  (contract,Matrix,Number)},
     Headline => "contract a matrix by a matrix",
     Usage => "h = contract(m,n)",
     Inputs => {
	  "m" => {"a map ", TT "m : F <--- P", " between free modules of ranks f and p."},
	  "n" => {"a map ", TT "n : G <--- Q", " between free modules of ranks g and q."}
	  },
     Outputs => {
	  "h" => {"the contraction of ", TT "n", " by ", TT "m", ", a matrix with the shape ", TT "h : dual F ** G <--- dual P ** Q", ", 
	       whose entry in the slot ", TT {"h", SUB "g*i+j,q*k+l"}, " is the result of contracting
	        ", TT { "n", SUB "j,l" }, ", by ", TT {"m", SUB "i,k"}
		}
	   },
     "The arguments can also be ring elements or vectors.",
     EXAMPLE lines ///
     	  R = ZZ[x,y,z]
	  f = vars R ** vars R
	  contract(transpose vars R, f)
	  contract(x, f)
	  contract(y, f)
	  contract(z, f)
     ///,
     PARA{},
     "This function is identical to ", TO (diff,Matrix,Matrix), ", except that 
     the multiplication by integers that occurs during differentiation is
     omitted.",
     PARA{},
     SeeAlso => {contract', "diff and contract"}
     }

document { Key => {(contract', Matrix, Matrix),contract'},
     Headline => "contract a matrix by a matrix, the dual notion",
     Usage => "h = contract'(m,n)",
     Inputs => {
	  "m" => {"a map ", TT "m : F <--- P", " between free modules of ranks f and p."},
	  "n" => {"a map ", TT "n : G <--- Q", " between free modules of ranks g and q."}
	  },
     Outputs => {
	  "h" => {"a matrix with the shape ", TT "h : F ** dual G <--- P ** dual Q", ", whose entry in the slot ", TT {"h", SUB "g*i+j,q*k+l"}, "
	       is the result of contracting ", TT { "m", SUB "i,k" }, ", by ", TT {"n", SUB "j,l", "."}}},
     PARA{},
     "This function is identical to ", TO (diff',Matrix,Matrix), ", except that 
     the multiplication by integers that occurs during differentiation is
     omitted.",
     PARA{},
     SeeAlso => {contract,"diff and contract"}
     }

document { Key => {(diff', Matrix, Matrix),diff'},
     Headline => "differentiate a matrix by a matrix, the dual notion",
     Usage => "h = diff'(m,n)",
     Inputs => {
	  "m" => {"a map ", TT "m : F <--- P", " between free modules of ranks f and p."},
	  "n" => {"a map ", TT "n : G <--- Q", " between free modules of ranks g and q."}
	  },
     Outputs => {
	  "h" => {"a matrix with the shape ", TT "h : F ** dual G <--- P ** dual Q", ", whose entry in the slot ", TT {"h", SUB "g*i+j,q*k+l"}, "
	       is the result of differentiating ", TT {"n", SUB "j,l"}, ", by ", TT { "m", SUB "i,k" }}},
     SeeAlso => {diff,"diff and contract"}
     }

document {
     Key => (leadTerm, RingElement),
     Headline => "get the greatest term",
     Usage => "leadTerm f",
     Inputs => {"f" => "in a polynomial ring"},
     Outputs => { RingElement => {"the lead term of ", TT "f", ""}},
     "Each polynomial ring comes equipped with a ", TO2("monomial orderings", "monomial ordering"),
     " and this routine
     returns the lead (greatest) monomial and its coefficient.  
     Recall that the default monomial order is
     the graded reverse lexicographic order.", -- Mike wanted this: TO "graded reverse lexicographic order"
     EXAMPLE {
	  "R = QQ[a..d];",
	  "leadTerm (3*b*c^2-d^3-1)",
	  "S = QQ[a..d, MonomialOrder => Lex]",
	  "leadTerm (3*b*c^2-d^3-1)"
	  },
     "Coefficients are included in the result:",
     EXAMPLE {
	  "R = ZZ[a..d][x,y,z];",
	  "leadTerm((a+b)*y^2 + (b+c)*x*z)"
	  },
     SeeAlso => {"leadCoefficient", "leadMonomial", "leadComponent"}
     }
document {
     Key => {(leadTerm, Matrix),(leadTerm, GroebnerBasis),(leadTerm, Vector)},
     Headline => "get the greatest term of each column",
     Usage => "leadTerm f",
     Inputs => {"f" => "in a polynomial ring"},
     Outputs => { Matrix => {"the lead term matrix of ", TT "f", ""}},
     "In Macaulay2, each free module over a polynomial ring comes equipped with a ", 
     TO2("monomial orderings", "monomial order"),
     " and this routine
     returns the matrix whose ", TT "i", "-th column is the lead term of the ", 
     TT "i", " th column of ", TT "f", ".",
     EXAMPLE lines ///
	  R = QQ[a..d];
	  f = matrix{{0,a^2-b*c},{c,d}}
	  leadTerm f
	  ///,
     "Coefficients are included in the result:",
     EXAMPLE {
	  "R = ZZ[a..d][x,y,z];",
	  "f = matrix{{0,(a+b)*x^2},{c*x, (b+c)*y}}",
	  "leadTerm f"
	  },
     "The argument ", TT "f", " can also be ", ofClass GroebnerBasis, ", in which case the lead term matrix of the generating matrix of ", TT "f", " is returned.",
     SeeAlso => {"leadCoefficient", "leadMonomial", "leadComponent"}
     }
document {
     Key => (leadTerm, Ideal),
     Headline => "get the ideal of greatest terms",
     Usage => "leadTerm I",
     Inputs => {"I"},
     Outputs => {{"The ideal of all possible lead terms of ", TT "I"}},
     "Compute a ", TO2("Gröbner bases", "Gröbner basis"), 
     " and return the ideal generated by the lead terms of the Gröbner 
     basis elements.",
     EXAMPLE {
	  "R = QQ[a..d];",
	  "I = ideal(a*b-c*d, a*c-b*d)",
	  "leadTerm I"
	  },
     EXAMPLE {
	  "R = ZZ[a..d][x,y,z];",
	  "I = ideal(a*x-b*y, x^3, y^3, z^3)",
	  "leadTerm I"
	  }
     }

document {
     Key => (leadTerm, ZZ, RingElement),
     Headline => "get the lead polynomials using part of the monomial order",
     Usage => "leadTerm(n,f)",
     Inputs => {"n", "f" => "in a polynomial ring" },
     Outputs => { RingElement => {"the lead term of ", TT "f", " using the first ", TT "n", " parts of the monomial order"}},
     "Returns the sum of the terms of ", TT "f", " which are greatest using the      first ", TT "n", " parts of
     the monomial order in the ring of ", TT "f", ".",
     -- Mike wanted this: "  See ", TO "parts of monomial orders", " for an explanation.",
     PARA{},
     "In the following example, the lead terms using the first part refers to all the
     monomials that have the lead monomial in the indeterminates ", TT "a", " and ", TT "b", ".  This has a
     effect similar to selecting leadTerm in the ring ", TT "QQ[c,d][a,b]", ".",
     EXAMPLE lines ///
	  R = QQ[a..d, MonomialOrder => ProductOrder{2,2}];
	  leadTerm(1, (c+d)*a^3 - c^100*a - 1)
	  ///,
     SeeAlso => {selectInSubring}
     }

document {
     Key => (leadTerm, ZZ, Matrix),
     Headline => "get the matrix of lead polynomials of each column",
     Usage => "leadTerm(n,f)",
     Inputs => {"n", "f" => "in a polynomial ring"},
     Outputs => { Matrix => {"the lead term matrix of ", TT "f", " using the first ", 
	       TT "n", " parts of the monomial order"}},
     "Returns the matrix whose ", TT "i", "-th column is the lead term of the ", 
     TT "i", "-th column of ", TT "f", ", using the first ", TT "n", " parts 
     of the monomial order.  ",
     -- Mike wanted this: "See ", TO "parts of monomial orders", " for an explanation.",
     EXAMPLE {
	  "R = QQ[x,y,z,a..d,MonomialOrder=>ProductOrder{3,4}];",
	  "f = matrix{{0,x^2*(a+b)}, {a*x+2*b*y, y^2*(c+d)}}",
	  "leadTerm(1,f)"
	  },
     SeeAlso => { "selectInSubring" }
     }
document {
     Key => (leadTerm, ZZ, Ideal),
     Headline => "get the ideal of lead polynomials",
     Usage => "leadTerm(n,I)",
     Inputs => {"n", "I"},
     Outputs => {{"The ideal of all possible lead polynomials of ", TT "I", 
	       " using the first ", TT "n", " parts of the monomial order"}},
     "Compute a ", TO2("Gröbner bases", "Gröbner basis"), 
     " and return the ideal generated by the lead terms of the Gröbner 
     basis elements using the first n.  ",
     -- Mike wanted this: "See ", TO "parts of monomial orders", " for an explanation.",
     EXAMPLE {
	  "R = QQ[a..d,MonomialOrder=>ProductOrder{1,3}];",
	  "I = ideal(a*b-c*d, a*c-b*d)",
	  "leadTerm(1,I)"
	  }
     }
document {
     Key => leadTerm,
     Headline => "get the greatest term",
     "Every polynomial ring in Macaulay2 comes equipped with a monomial ordering.
     For ring elements and matrices, this function returns the greatest term in this order.",
     PARA{},
     "For an ideal, a Gröbner basis is first computed, and the ideal of lead terms is
     returned.",
     PARA{},
     "If an initial integer ", TT "n", " is specified, then the returned value contains the sum
     of all of the terms with the greatest value on the first ", TT "n", " ",
     "parts of the monomial order." -- Mike wanted this: TO2 ("parts of a monomial order", "parts of the monomial order"), "."
     }
document {
     Key => {(borel, Matrix),borel,(borel, MonomialIdeal)},
     Headline => "make a Borel fixed submodule",
     TT "borel m", " -- make a Borel fixed submodule",
     PARA{},
     "Yields the matrix with the same target as the matrix ", TT "m", ", whose columns
     generate the smallest Borel fixed submodule containing the lead monomials
     of the columns of ", TT "m", ".  If ", TT "m", " is a monomial ideal, then the minimal Borel
     fixed monomial ideal containing it is returned.",
     PARA{},
     "For example, if R = ZZ/101[a..f], then",
     EXAMPLE {
	  "R = ZZ/101[a..e]",
	  "borel matrix {{a*d*e, b^2}}"
	  }
     }

document {
     Key => {id,(id,Ring),(id,Module),(id,ChainComplex)},
     Headline => "identity map",
     Usage => "id_F",
     Inputs => {
	  "F" => {ofClass Ring, ", ", ofClass Module, ", or ", ofClass ChainComplex}
	  },
     Outputs => {
	  {ofClass RingMap, ", ", ofClass Matrix, ", or ", ofClass ChainComplexMap, " the identity map on ", TT "F"}
	  },
     EXAMPLE lines ///
     	  R = QQ[a..d];
	  id_R
	  id_(R^3)
	  C = res coker vars R
	  id_C
	  ///
     }

document {
     Key => SubringLimit,
     Headline => "stop after finding enough elements of a subring",
     TT "SubringLimit", " -- an option for  ", TO "kernel", " and ", TO "gb", "
     which can stop the computation after a certain number of basis elements in
     a subring have been found.",
     SeeAlso => "Gröbner bases"
     }
document {
     Key => [kernel,SubringLimit],
     TT "SubringLimit => n", " -- an option for ", TO "kernel", " which
     causes the computation of the kernel of a ring map to stop after ", TT "n", "
     elements have been discovered."
     }
document {
     Key => (dual, Matrix),
     Headline => "dual of a map",
     TT "dual f", " -- the dual (transpose) of a homomorphism."
     }
document {
     Key => {singularLocus, (singularLocus, Ideal), (singularLocus, Ring)},
     Headline => "singular locus",
     TT "singularLocus R", " -- produce the singular locus of a ring, which is assumed to be integral.",
     PARA{},
     "This function can also be applied to an ideal, in which case the singular locus of
     the quotient ring is returned, or to a variety.",
     EXAMPLE lines ///
     	  singularLocus(QQ[x,y] / (x^2 - y^3))
	  singularLocus Spec( QQ[x,y,z] / (x^2 - y^3) )
	  singularLocus Proj( QQ[x,y,z] / (x^2*z - y^3) )
     ///,
     PARA { 
	  "For rings over ", TO "ZZ", " the locus where the ring is not smooth over ", TO "ZZ", " is
     	  computed."
	  },
     EXAMPLE lines ///
     	  singularLocus(ZZ[x,y]/(x^2-x-y^3+y^2))
	  gens gb ideal oo
	  ///
     }

document {
     Key => {isSurjective,(isSurjective, Matrix)},
     Headline => "whether a map is surjective",
     SeeAlso => "isInjective"
     }
document {
     Key => {isQuotientOf},
     Headline => "whether one thing is a quotient of another"
     }
document {
     Key => {(isQuotientOf, Ring, Ring),(isQuotientOf, Ring, QuotientRing)},
     Headline => "whether one ring is a quotient of another"
     }
document {
     Key => {(isQuotientOf, Type, Ring),(isQuotientOf, Type, QuotientRing)},
     Headline => "whether one ring is a quotient of a ring of a given type"
     }

document {
     Key => (symbol /, RingElement, RingElement),
     Headline => "fraction",
     Usage => "f/g",
     Inputs => { "f", "g" },
     Outputs => { RingElement => "the fraction f/g" },
     "If either f or g is in a base ring of the other, then that one is promoted 
     so that both are elements in the same ring R.",
     PARA{},
     "The fraction will be an element of the fraction field, frac R, of R.  
     If R is already a field, then this means that the fraction will be an element 
     of R.",
     EXAMPLE lines ///
      	   4/2
	   ///,
     EXAMPLE lines ///
	   R = GF(9,Variable=>a);
	   (a/a^3) * a^2 == 1
	   ///,
     EXAMPLE lines ///
     	   S = ZZ[a,b]
	   (a^6-b^6)/(a^9-b^9)
	   ///,
     "If the ring contains zero divisors, the fraction field is not defined.
     Macaulay2 will not inform you of this right away.  However, if computation
     finds a zero-divisor, an error message is generated.",
     EXAMPLE lines ///
     	  A = ZZ/101[a,b]/(a*b)
	  (a+b)/(a-b)
	  ///,
     "At this point, if one types ", TT "a/b", ", then Macaulay2 would give an error
     saying that a zero divisor was found in the denominator.",
     SeeAlso => {symbol //}
     }

document {
     Key => {(symbol /, Ring, Ideal),
	  (symbol /, Ring, Module),
	  (symbol /, Ring, RingElement),
	  (symbol /, Ring, List),
	  (symbol /, Ring, Sequence),
	  (symbol /, Ring, ZZ)
	  },
     Headline => "make a quotient ring",
     Usage => "S = R/I",
     Inputs => {
	  "R",
	  "I" => Nothing => {
	       { ofClass Ideal, " or element of ", TT "R", 
		    "or ", ofClass List, " or ", ofClass Sequence, " of elements of ", TT "R"}}
	  },
     Outputs => {
	  "S" => {"the quotient ring ", TT "R/I"}
	  },
     "If ", TT "I", " is a ring element of ", TT "R", " or ", TT "ZZ", 
     ", or a list or sequence of such elements, then ", 
     TT "I", " is understood to be the 
     ideal generated by these elements.  If ", 
     TT "I", " is a module, then it must be a submodule 
     of a free module of rank 1.",
     EXAMPLE lines ///
     	  ZZ[x]/367236427846278621
	  ///,
     EXAMPLE lines ///
     	  A = QQ[u,v];
	  I = ideal random(A^1, A^{-2,-2,-2})
	  B = A/I;
	  use A;
	  C = A/(u^2-v^2,u*v);
	  ///,
     EXAMPLE lines ///	  
	  D = GF(9,Variable=>a)[x,y]/(y^2 - x*(x-1)*(x-a))
	  ambient D
     	  ///,
     "The names of the variables are assigned values in the new quotient ring
     (by automatically running ", TT "use R", ") when the new ring is assigned
     to a global variable.",
     PARA{},
     "Warning: quotient rings are bulky objects, because they contain 
     a Gröbner basis for their ideals, so only quotients of ", TO "ZZ", " 
     are remembered forever.  Typically the ring created by ", TT "R/I", " 
     will be a brand new ring, and its elements will be incompatible with 
     the elements of previously created quotient rings for the same ideal.",
     EXAMPLE {
	  "ZZ/2 === ZZ/(4,6)",
      	  "R = ZZ/101[t]",
      	  "R/t === R/t",
	  }
     }

doc ///
   Key
     symmetricPower
     (symmetricPower,ZZ,Matrix)
     (symmetricPower,ZZ,Module)
   Headline
     symmetric power
   Usage
     symmetricPower(i,f)
   Inputs
     i:ZZ
     M:Matrix
       or @ofClass Module@
   Outputs
     :Matrix
       or @ofClass Module@, the $i$-th symmetric power of the matrix or module $f$
   Description
    Text
      There is currently one restriction: if $f$ is a matrix, then it must have only one row,
      and be a map of free modules, as in this example.
    Example
      R = ZZ/101[a..d]
      symmetricPower(2,vars R)
    Text
      If G --> F --> M --> 0 is a presentation for the module M = coker(f:G-->F), 
      then symmetricPower(i,f) is the cokernel of the map 
      symmetricPower(i-1,F) ** G --> symmetricPower(i,F).  
    Example
      R = ZZ/101[a,b]
      symmetricPower(2,image vars R)
   SeeAlso
     exteriorPower
     basis
///

document {
     Key => (exteriorPower,ZZ,Matrix),
     Headline => "exterior power of a matrix",
     Usage => "exteriorPower(i,f)\nexteriorPower_i f",
     Inputs => { "i", "f" },
     Outputs => {
	  { "the ", TT "i", "-th exterior power of ", TT "f", "."}
	  },
     EXAMPLE {
	  "R = ZZ/2[x,y];",
	  "f = random(R^3,R^{3:-1})",
	  "exteriorPower_2 f"
	  },
     "The matrix may be a more general homomorphism of modules.  For example,",
     EXAMPLE {
	  "g = map(coker matrix {{x^2},{x*y},{y^2}}, R^3, id_(R^3))",
	  "g2 = exteriorPower(2,g)",
	  "target g2"
	  },
	SeeAlso => {(exteriorPower,ZZ,Module)}
     }
document {
     Key => (exteriorPower,ZZ,Module),
     Headline => "exterior power of a module",
     Usage => "exteriorPower(i,M)\nexteriorPower_i M",
     Inputs => { "i", "M" },
     Outputs => {
		{"the ", TT "i", "-th exterior power of ", TT "M", "."}
	  },
     EXAMPLE {
	  "M = ZZ^5",
	  "exteriorPower(3,M)"
	  },
	"When ", TT "i", " is ", TT "1", ", then the result is equal to ", TT "M",
     ".  When ", TT "M", " is not a free module, then the generators used for the result
     will be wedges of the generators of ", TT "M", ".  In other words, the modules
     ", TT "cover exteriorPower(i,M)", " and ", TT "exteriorPower(i,cover M)", " 
     will be equal.",
     SeeAlso => {(exteriorPower,ZZ,Matrix)}
     }
document {
     Key => exteriorPower,
     Headline => "exterior power",
     SeeAlso => {"minors", "det", "wedgeProduct"}
     }
document {
     Key => {(trace, Matrix),trace},
     Headline => "trace of a matrix",
     TT "trace f", " -- returns the trace of the matrix f.",
     PARA{},
     EXAMPLE {
	  "R = ZZ/101[a..d]",
      	  "p = matrix {{a,b},{c,d}}",
      	  "trace p"
	  },
     }
document {
     Key => {fittingIdeal,(fittingIdeal, ZZ, Module)},
     Headline => "Fitting ideal of a module",
     TT "fittingIdeal(i,M)", " -- the i-th Fitting ideal of the module M",
     PARA{},
     EXAMPLE {
	  "R = ZZ/101[x];",
      	  "k = coker vars R",
      	  "M = R^3 ++ k^5;",
      	  "fittingIdeal(3,M)",
      	  "fittingIdeal(8,M)"
	  },
     }

document {
     Key => (symbol +, Module, Module),
     Headline => "sum of submodules",
     TT "M + N", " -- the sum of two submodules.",
     PARA{},
     "The two modules should be submodules of the same module."
     }
document {
     Key => Order,
     Headline => "specify the order of a Hilbert series required",
     TT "Order", " -- an optional argument used with ", TO "hilbertSeries", "
     to specify the order of the series requested."
     }
document {
     Key => Projective,
     Headline => "whether to produce a projective Hilbert polynomial",
     TT "Projective", " -- an optional argument used with ", TO "hilbertPolynomial", 
     " to specify the way the Hilbert Polynomial is expressed."
     }
document {
     Key => ProjectiveHilbertPolynomial,
     Headline => "the class of all Hilbert polynomials",
     "For convenience, these polynomials are expressed in terms of the Hilbert 
     polynomials of projective space.",
     PARA{},
     "The functions ", TO "degree", " and ", TO "dim", " are designed so they
     correspond the degree and dimension of the algebraic variety that may have
     been used to produce the Hilbert polynomial.",
     EXAMPLE {
	  "Z = Proj(QQ[x_0..x_12]/(x_0^3+x_12^3))",
	  "hilbertPolynomial Z"
	  }
     }
document {
     Key => (symbol SPACE, ProjectiveHilbertPolynomial, ZZ),
     Headline => "value of polynomial",
     TT "P i", " -- the value of a projective Hilbert polynomial ", TT "P", " at 
     an integer ", TT "i", ".",
     PARA{},
     EXAMPLE {
	  "P = projectiveHilbertPolynomial 2",
      	  "apply(0 .. 12, i -> P i)",
	  },
     SeeAlso => ProjectiveHilbertPolynomial
     }
document {
     Key => {projectiveHilbertPolynomial,(projectiveHilbertPolynomial, ZZ),(projectiveHilbertPolynomial, ZZ, ZZ)},
     Headline => "Hilbert polynomial of projective space",
     TT "projectiveHilbertPolynomial n", " -- produces the projective
     Hilbert polynomial corresponding to projective space of dimension n.",
     BR{},
     TT "projectiveHilbertPolynomial(n,d)", " -- produces the projective
     Hilbert polynomial corresponding to the graded ring of projective space
     of dimension n, but with its generator in degree -d.",
     PARA{},
     SeeAlso => "ProjectiveHilbertPolynomial"
     }
document {
     Key => dual,
     Headline => "dual module or map",
     }
document {
     Key => (dual, Module),
     Headline => "dual module",
     TT "dual M", " -- the dual of a module."
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
