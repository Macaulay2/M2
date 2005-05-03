--		Copyright 1993-1999 by Daniel R. Grayson
document {
     Key => (ambient,Matrix),
     Headline => "WRITE THIS ROUTINE",
     Usage => "ambient f",
     Inputs => {
	  "f" => null
	  },
     Outputs => {
	  {"WRITE THIS"}
	  },
     -- description here, with examples
     SeeAlso => {},
     Caveat => {}
     }
document {
     Key => (symbol _, Matrix, List),
     Headline => "select some columns from a matrix",
     Usage => "f_cols",
     Inputs => {
	  "f" => {"a matrix between free modules"},
	  "cols" => "a list of integers denoting the choice of columns",
	  },
     Outputs => {
	  {"the submatrix of ", TT "f", " corresponding to the columns ", TT "cols", "."}
	  },
     "This is the same as calling ", 
     TO2 ((submatrix,Matrix,VisibleList), TT "submatrix(f,cols)"), ".",
     EXAMPLE {
	  "R = QQ[a..f];",
	  "p = matrix {{a,b,c},{d,e,f}}",
      	  "p_{1}",
      	  "p_{1,2,0}"
	  },
     SeeAlso => {submatrix, (symbol ^, Matrix, List)}
     }
document {
     Key => (symbol ^, Matrix, List),
     Headline => "select some rows from a matrix",
     Usage => "f_rows",
     Inputs => {
	  "f" => {"a matrix between free modules"},
	  "rows" => "a list of integers denoting the choice of rows",
	  },
     Outputs => {
	  {"the submatrix of ", TT "f", " corresponding to the rows in the list ", TT "rows", "."}
	  },
     "This is the same as calling ", 
     TO2 ((submatrix,Matrix,VisibleList,VisibleList), TT "submatrix(f,rows,)"), ".",
     EXAMPLE {
	  "R = QQ[a..f];",
	  "p = matrix {{a,b,c},{d,e,f}}",
      	  "p^{1}",
      	  "p^{1,0}"
	  },
     SeeAlso => {submatrix, (symbol _, Matrix, List)}
     }
TEST ///
     R = ZZ[x_1..x_12,y]
     f = genericMatrix(R,3,4)
     assert(source (f_{1,2}) == R^{-1,-1})
     assert(target (f_{1,2}) == target f)
     M1 = (target f)/(y * target f)
     M2 = (source f)/(y * source f)
     g = map(target f,M2,f)
     h = map(M1,M2,f)
     k = submatrix(g, {1})
     assert(target k === target g)
     l = submatrix(h, {1})
     assert(target l === target h)
     assert(source l === R^{-1})
     m = submatrix(h, {1,2},{2,3})
     assert(target m === R^2)
     assert(source m === R^{2:-1})
     n = submatrix(h, {1,2}, )
     assert(target n === R^2)
     assert(source n === source h)
///
document {
     Key => (submatrix,Matrix,VisibleList,VisibleList),
     Headline => "select part of a matrix",
     Usage => "submatrix(f, rows, cols) or submatrix(f,,cols) or submatrix(f,rows,)",
     Inputs => { "f" => "a map between free modules",
	  "rows" => "a list of integers denoting the choice of rows.  
	            If omitted, use all rows",
	  "cols" => "a list of integers denoting the choice of columns.
	            If omitted, use all columns"},
     Outputs => {Matrix => {"the submatrix of ", TT "f", " corresponding to the lists ", TT "rows", 
	  " and ", TT "cols", ""}},
     "Yields an r by c matrix, where r is the length of the list of integers
     ", TT "rows", ", and c is the length of the list of integers ", TT "cols", ".  
     The ", TT "(i,j)", "-th entry of the result is ", TT "f_(rows_i, cols_j)", ".",
     PARA,
     "The lists of integers may contain ", TO "ranges and repetitions", ", as in ", 
     TT "{3, 5..7, 3:0}",
     PARA,
     "If ", TT "rows", " or ", TT "cols", " is omitted, all the corresponding indices are used.",
     PARA,
     "Both ", TT "rows", " and/or ", TT "cols", " may contain duplicate values, 
     in which case the result will contain
     duplicate rows and/or columns.",
     EXAMPLE {
	  "R = ZZ/101[a .. o];",
      	  "f = genericMatrix(R, a, 3, 5)",
      	  "submatrix(f, {1,2,0}, {0..2, 4})",
	  },
     PARA,
     EXAMPLE {"submatrix(f, {1,2}, )"},
     TT "submatrix(f,,cols)", " can be written as ", TT "submatrix(f,cols)", ", or ",
     TT "f_cols", ".",
     EXAMPLE {
	  "submatrix(f,,{0,1})",
	  "f_{0,1}"
	  },
     SeeAlso => {
	  (symbol_,Matrix,List),
	  (symbol^,Matrix,List),
	  (submatrix,Matrix,VisibleList),
	  (symbol_,Matrix,Array),
	  (symbol^,Matrix,Array),
	  },
     }
document {
     Key => (submatrix,Matrix,VisibleList),
     Headline => "select certain columns of a matrix",
     Usage => "submatrix(f, cols)",
     Inputs => { "f" => "a map between free modules",
	  "cols" => "a list of integers denoting the choice of columns"},
     Outputs => {Matrix => {"the submatrix of ", TT "f", " corresponding to the columns ",
	        TT "cols", ""}},
     "Yields an r by c matrix, where r is the number of rows of ", TT "f", ", and 
     c is the length of the list of integers ", TT "cols", ".  
     The ", TT "(i,j)", "-th entry of the result is ", TT "f_(i, cols_j)", ".",
     PARA,
     "The list ", TT "cols", " may contain ", TO "ranges and repetitions", ", as in ", 
     TT "{3, 5..7, 3:0}",
     PARA,
     "If the list of column indices is a permutation of 0 .. n-1, where n is
     the number of columns, then the result is the corresponding permutation
     of the columns of f.",
     EXAMPLE {
	  "R = ZZ/101[a .. o];",
      	  "f = genericMatrix(R, a, 3, 5)",
      	  "submatrix(f, {1..3,0})",
	  },
     SeeAlso => {
	  (symbol_,Matrix,VisibleList),
	  (symbol^,Matrix,VisibleList),
	  (symbol_,Matrix,Array),
	  (symbol^,Matrix,Array)
	  },
     }
document {
     Key => submatrix,
     Headline => "select part of a matrix"
     }
document {
     Key => (diff,RingElement,RingElement),
     Headline => "differentiation",
     Usage => "diff(x,f)",
     Inputs => {
	  "x" => "a polynomial",
	  "f" => {"a polynomial in the same ring as ", TT "x",}
	  },
     Outputs => {
	  {"the result of differentiating ", TT "f", " by the ",
	       TO2("differential operator corresponding to a polynomial", 
	       "differential operator"), " corresponding to ", TT "x", "."}
	  },
     "If ", TT "x", " is an indeterminate this is simply the usual differentiation.",
     EXAMPLE {
	  "R = QQ[x,y,z];",
	  "diff(x,x^7 + 4*x^3*y - 3*y)",
	  "diff(x^2+y^2+z^2, y^2*z^2 - x^3 - 1)",
	  },
     "Here is a shortcut that can save some typing.",
     EXAMPLE "diff_x x^6",
     SeeAlso => {
	  contract,
	  jacobian,
	  "differential operator corresponding to a polynomial",
	  "diff and contract"
	  },
     }
document {
     Key => (diff,RingElement,Matrix),
     Headline => "differentiate each entry of a matrix",
     Usage => "diff(x,f)",
     Inputs => {
	  "x" => "a polynomial",
	  "f" => {"a matrix between free modules over the same ring as ", TT "x",}
	  },
     Outputs => {
	  Matrix => {"having the same shape as f, whose (i,j) entry is the 
	       result of differentiating ", TT "f_(i,j)", " by the ",
	       TO2("differential operator corresponding to a polynomial",
	       "differential operator"), " corresponding to ", TT "x", "."}
	  },
     "The shape of the resulting matrix is the same as the shape of f, 
     but the degrees of the source module are different
     in an attempt to ensure that the result is homogeneous.",
     EXAMPLE {
	  "R = QQ[x,y,z];",
	  "f = matrix{{x^2-y*z, x*y*z + z^4}, {x-1, 2*y^2+z^2-1}}",
	  "diff(x,f)",
	  "diff(x^2-y*z,f)"
	  },
     SeeAlso => {
	  contract,
	  jacobian,
	  "differential operator corresponding to a polynomial",
	  "diff and contract"
	  },
     }
document {
     Key => (diff,Matrix,RingElement),
     Headline => "differentiation",
     Usage => "diff(f,g)",
     Inputs => {
	  "f" => "a matrix",
	  "g" => {"a polynomial with the same ring as ", TT "f",}
	  },
     Outputs => {
	  {"the result of differentiating ", TT "g", " by the ",
	       TO2("differential operator corresponding to a polynomial", 
	       "differential operator"), " corresponding to each entry of ", TT "f", "."}
	  },
     "The shape of the resulting matrix is the same as the shape of f, 
     but the degrees of the source module are different
     in an attempt to ensure that the result is homogeneous.",
     EXAMPLE {
	  "R = QQ[x,y,z,q];",
	  "f = vars R",
	  "diff(f, (x+y-z)^2)",
	  "f2 = genericMatrix(R,2,2)",
	  "diff(f2, (x+y-z)^2)"
	  },
     SeeAlso => {
	  contract,
	  jacobian,
	  "differential operator corresponding to a polynomial",
	  "diff and contract"
	  },
     }
document {
     Key => (diff,Matrix,Matrix),
     Headline => "differentiate a matrix by a matrix",
     Usage => "diff(m,n)",
     Inputs => {
	  "m" => {"a map ", TT "m : F <--- P", " between free modules of ranks f and p."},
	  "n" => {"a map ", TT "n : G <--- Q", " between free modules of ranks g and q."}
	  },
     Outputs => {
	  Matrix => {"with the shape ", TT "h : dual F ** G <--- dual P ** Q", ",
	       whose entry in the slot ", TT {"h", SUB "g*i+j,q*k+l"}, "
	       is the result of differentiating ", TT { "n", SUB "j,l" }, "
	       by the ",
	       TO2("differential operator corresponding to a polynomial",
	       "differential operator"),
	       " corresponding to  ", TT {"m", SUB "i,k", "."}
	       }
	  },
     EXAMPLE {
	  "R = QQ[a,b,c][x,y,z]",
	  "m = transpose vars R",
	  "n = matrix{{x^2-a*y^3, x^3-z^2*y, x*y-b, x*z-c}}",
	  "diff(m,n)"
	  },
     SeeAlso => {
	  diff',
	  contract,
	  "differential operator corresponding to a polynomial",
	  "diff and contract"
	  }
     }
document {
     Key => (diff,ProjectiveHilbertPolynomial,ZZ),
     TT "diff(P,i)", " -- compute the i-th difference polynomial"
     }
document {
     Key => (diff, ProjectiveHilbertPolynomial),
     TT "diff P", " -- compute the difference polynomial."
     }
document {
     Key => diff,
     Headline => "differentiate or take difference",
     Usage => "diff(f,g) or diff(P) or diff(P,i)",
     "This function has two different uses.  The most common use is for differentiation:
     differentiate the second input by the first.",
     PARA,
     "The second use, less common but sometimes useful, is to compute the difference
     polynomial of a Hilbert polynomial.",
     SeeAlso => {
	  "differential operator corresponding to a polynomial",
	  "diff and contract",
	  diff',
	  contract,
	  jacobian,
	  hilbertPolynomial
	  },
     Subnodes => {
	  "differentiation",
	  TO (diff,RingElement,RingElement),
	  TO (diff,RingElement,Matrix),
	  TO (diff,Matrix,RingElement),
	  TO (diff,Matrix,Matrix),
	  "difference operator for Hilbert polynomials",
	  TO (diff,ProjectiveHilbertPolynomial),
	  TO (diff,ProjectiveHilbertPolynomial,ZZ)
	  }
     }
document {
     Key => contract,
     Headline => "contract one matrix by another",
     SeeAlso => "diff and contract"
     }
document {
     Key => (contract,Matrix,Matrix),
     Headline => "contract a matrix by a matrix",
     Usage => "h = contract(m,n)",
     Inputs => {
	  "m" => {"a map ", TT "m : F <--- P", " between free modules of ranks f and p."},
	  "n" => {"a map ", TT "n : G <--- Q", " between free modules of ranks g and q."}
	  },
     Outputs => {
	  "h" => {"a matrix with the shape ", TT "h : dual F ** G <--- dual P ** Q", ",
	       whose entry in the slot ", TT {"h", SUB "g*i+j,q*k+l"}, "
	       is the result of contracting ", TT { "n", SUB "j,l" }, ",
		    by ", TT {"m", SUB "i,k", "."}}},
     PARA,
     "This function is identical to ", TO (diff,Matrix,Matrix), ", except that 
     the multiplication by integers that occurs during differentiation is
     omitted.",
     PARA,
     SeeAlso => "diff and contract"
     }
TEST "
R = ZZ/101[a..d]
I = monomialCurveIdeal(R,{1,3,4})
A = R/I
jacobian A
singA = minors(codim ideal presentation A, jacobian A)
generators gb singA
"
document {
     Key => jacobian,
     Headline => "the Jacobian matrix of partial derivatives",
     SeeAlso => {
	 "diff",
	 "contract"
	  }
     }
document {
     Key => (jacobian,Matrix),
     Headline => "the matrix of partial derivatives of polynomials in a matrix",
     Usage => "jacobian f",
     Inputs => {"f" => " with one row"},
     Outputs => {Matrix => {"the Jacobian matrix of partial derivatives of 
	           the polynomial entries of ", TT "f"}},
     "If ", TT "f", " is a 1 by ", TT "m", " matrix over a polynomial ring ",
     TT "R", " with ", TT "n"," indeterminates,
     then the resulting matrix of partial derivatives has dimensions ",TT "n"," by ",TT "m",", 
     and the ", TT "(i,j)", " entry is the partial derivative of the ", TT "j", "-th entry of
     ", TT "f", " by the ", TT "i", "-th indeterminate of the ring.",
     PARA,
     "If the ring of ", TT "f", " is a quotient polynomial ring ", TT "S/J", ",
     	  then only the derivatives of the given entries of ", TT "f", " are
     	  computed and NOT the derivatives of elements of ", TT "J", ".",
     	  EXAMPLE {
	       "R = QQ[x,y,z];",
      	       "f = matrix{{y^2-x*(x-1)*(x-13)}}",
      	       "jacobian f",
	       },
	  "If the ring of ", TT "f", " is a polynomial ring over a polynomial ring,
	  then indeterminates in the coefficient ring are treated as constants.",
     	  EXAMPLE {
	       "R = ZZ[a,b,c][x,y,z]",
	       "jacobian matrix{{a*x+b*y^2+c*z^3, a*x*y+b*x*z}}"
	       }
     }
document {
     Key => (jacobian,Ideal),
     Headline => "the Jacobian matrix of the generators of an ideal",
     Usage => "jacobian I",
     Inputs => {"I" => " in a polynomial ring"},
     Outputs => {Matrix => {"the Jacobian matrix of partial derivatives of 
	           the generators of ", TT "I"}},
     "This is identical to ", TT "jacobian generators I", ".  See ", TO (jacobian,Matrix), 
     " for more information.",
     	  EXAMPLE {
	       "R = QQ[x,y,z];",
      	       "I = ideal(y^2-x*(x-1)*(x-13))",
      	       "jacobian I",
	       },
	  "If the ring of ", TT "I", " is a polynomial ring over a polynomial ring,
	  then indeterminates in the coefficient ring are treated as constants.",
     	  EXAMPLE {
	       "R = ZZ[a,b,c][x,y,z]",
	       "jacobian ideal(a*y*z+b*x*z+c*x*y)"
	       }
     }
document {
     Key => (jacobian,Ring),
     Headline => "the Jacobian matrix of the polynomials defining a quotient ring",
     Usage => "jacobian R",
     Inputs => {"R" => " a quotient of a polynomial ring"},
     Outputs => {Matrix => {"the Jacobian matrix of partial derivatives of 
	           the presentation matrix of ", TT "R"}},
     "This is identical to ", TT "jacobian presentation R", ", except
     that the resulting matrix is over the ring ", TT "R", ".  See ", TO (jacobian,Matrix), 
     " for more information.",
     	  EXAMPLE {
	       "R = QQ[x,y,z]/(y^2-x^3-x^7);",
      	       "jacobian R",
	       },
	  "If the ring ", TT "R", " is a (quotient of a) polynomial ring over a polynomial ring,
	  then the top set of indeterminates is used, on the top set of quotients:",
     	  EXAMPLE {
	       "A = ZZ[a,b,c]/(a^2+b^2+c^2);",
	       "R = A[x,y,z]/(a*x+b*y+c*z-1)",
	       "jacobian R"
	       }
     }
document {
     Key => (leadTerm, RingElement),
     Headline => "get the greatest term",
     Usage => "leadTerm f",
     Inputs => {"f" => "in a polynomial ring"},
     Outputs => { RingElement => {"the lead term of ", TT "f", ""}},
     "Each polynomial ring comes equipped with a ", TO2("monomial orders", "monomial ordering"),
     " and this routine
     returns the lead (greatest) monomial and its coefficient.  
     Recall that the default monomial order is
     the ", TO "graded reverse lexicographic order", ".",
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
     Key => (leadTerm, Matrix),
     Headline => "get the greatest term of each column",
     Usage => "leadTerm f",
     Inputs => {"f" => "in a polynomial ring"},
     Outputs => { Matrix => {"the lead term matrix of ", TT "f", ""}},
     "In Macaulay2, each free module over a polynomial ring comes equipped with a ", 
     TO2("monomial orders", "monomial order"),
     " and this routine
     returns the matrix whose ", TT "i", "-th column is the lead term of the ", 
     TT "i", " th column of ", TT "f", ".",
     EXAMPLE {
	  "R = QQ[a..d];",
	  "f = matrix{{0,a^2-b*c},{c,d}}",
	  "leadTerm f",
	  },
     "Coefficients are included in the result:",
     EXAMPLE {
	  "R = ZZ[a..d][x,y,z];",
	  "f = matrix{{0,(a+b)*x^2},{c*x, (b+c)*y}}",
	  "leadTerm f"
	  },
     SeeAlso => {"leadCoefficient", "leadMonomial", "leadComponent"}
     }
document {
     Key => (leadTerm, Ideal),
     Headline => "get the ideal of greatest terms",
     Usage => "leadTerm I",
     Inputs => {"I" => null},
     Outputs => {{"The ideal of all possible lead terms of ", TT "I"}},
     "Compute a ", TO2("Groebner bases", "Groebner basis"), 
     " and return the ideal generated by the lead terms of the Groebner 
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
	  },
     SeeAlso => {"Groebner bases"}
     }
///
-- Put this in once the routine exists!! (MES 3/27/04)
document {
     Key => (leadTerm, ZZ, RingElement),
     Headline => "get the lead polynomials using part of the monomial order",
     Usage => "leadTerm(n,f)",
     Inputs => {"n" => "",
	  "f" => "in a polynomial ring"},
     Outputs => { RingElement => {"the lead term of ", TT "f", " using the first ", TT "n", " parts 
	       of the monomial order"}},
     "Returns the sum of the terms of ", TT "f", " which are greatest using the 
     first ", TT "n", " parts of
     the monomial order in the ring of ", TT "f", ". ",
     "See ", TO "parts of monomial orders", " for an explanation.",
     PARA,
     "In the following example, the lead terms using the first part refers to all the
     monomials which have the lead monomial in the indeterminates a and b.  This has a
     similar effect as selecting leadTerm in the ring QQ[c,d][a,b].",
     EXAMPLE {
	  "R = QQ[a..d, MonomialOrder=>ProductOrder{2,2}];",
	  "leadTerm(1, (c+d)*a^3 - c^100*a - 1)"
	  },
     SeeAlso => {selectInSubring}
     }
///
document {
     Key => (leadTerm, ZZ, Matrix),
     Headline => "get the matrix of lead polynomials of each column",
     Usage => "leadTerm(n,f)",
     Inputs => {"n" => null,
	  "f" => "in a polynomial ring"},
     Outputs => { Matrix => {"the lead term matrix of ", TT "f", " using the first ", 
	       TT "n", " parts of the monomial order"}},
     "Returns the matrix whose ", TT "i", "-th column is the lead term of the ", 
     TT "i", "-th column of ", TT "f", ", using the first ", TT "n", " parts 
     of the monomial order.  ",
     "See ", TO "parts of monomial orders", " for an explanation.",
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
     Inputs => {"n" => null, "I" => null},
     Outputs => {{"The ideal of all possible lead polynomials of ", TT "I", 
	       " using the first ", TT "n", " parts of the monomial order"}},
     "Compute a ", TO2("Groebner bases", "Groebner basis"), 
     " and return the ideal generated by the lead terms of the Groebner 
     basis elements using the first n . See ", TO "parts of monomial orders", " for an explanation.",
     EXAMPLE {
	  "R = QQ[a..d,MonomialOrder=>ProductOrder{1,3}];",
	  "I = ideal(a*b-c*d, a*c-b*d)",
	  "leadTerm(1,I)"
	  },
     SeeAlso => {"Groebner bases"}
     }
document {
     Key => leadTerm,
     Headline => "get the greatest term",
     "Every polynomial ring in Macaulay 2 comes equipped with a monomial ordering.
     For ring elements and matrices, this function returns the greatest term in this order.",
     PARA,
     "For an ideal, a Groebner basis is first computed, and the ideal of lead terms is
     returned.",
     PARA,
     "If an initial integer ", TT "n", " is specified, then the returned value contains the sum
     of all of the terms with the greatest value on the first ", TT "n", " ",
     TO2 ("parts of a monomial order", "parts of the monomial order"), "."
     }
document {
     Key => borel,
     Headline => "make a Borel fixed submodule",
     TT "borel m", " -- make a Borel fixed submodule",
     PARA,
     "Yields the matrix with the same target as the matrix ", TT "m", ", whose columns
     generate the smallest Borel fixed submodule containing the lead monomials
     of the columns of ", TT "m", ".",
     PARA,
     "For example, if R = ZZ/101[a..f], then",
     EXAMPLE {
	  "R = ZZ/101[a..e]",
	  "borel matrix {{a*d*e, b^2}}"
	  }
     }
document {
     Key => (inducedMap, Module, Module),
     Headline => "compute the map induced by the identity",
     Usage => "inducedMap(M,N)",
     Inputs => {
	  "M" => null,
	  "N" => null 
	  },
     Outputs => {
	  {"the homomorphism ", TT "M <-- N", " induced by the identity."}
	  },
     "The modules ", TT "M", " and ", TT "N", " must both be ", TO "subquotients", " of 
     the same ambient free module ", TT "F", ".
     If ", TT "M = M1/M2", " and ", TT "N = N1/N2", ", where ", TT "M1", ", 
     ", TT "M2", ", ", TT "N1", ", ", TT "N2", " are all submodules of ", TT "F", ", then
     return the map induced by ", TT "F --> F", ". If the optional argument ", TT "Verify", 
     " is given, check that the result defines a well defined homomorphism.",
     PARA,
     "In this example, we make the inclusion map between two submodules of ", TT "R^3", 
     ".  M is defined by two elements and N is generated by one element in M",
     EXAMPLE {
	  "R = ZZ/32003[x,y,z];",
          "P = R^3;",
	  "M = image(x*P_{1}+y*P_{2} | z*P_{0})",
	  "N = image(x^4*P_{1} + x^3*y*P_{2} + x*y*z*P_{0})",
	  "h = inducedMap(M,N)",
	  "source h == N",
	  "target h == M",
	  "ambient M == ambient N"
	  },
     SeeAlso => {inducesWellDefinedMap, subquotient}
     }
document {
     Key => (inducedMap, Module, Module, Matrix),
     Headline => "compute the induced map",
     Usage => "inducedMap(M,N)",
     Inputs => {
	  "M" => null,
	  "N" => null,
	  "f" => {"a homomorphism ", TT "P <-- Q"}
	  },
     Outputs => {
	  {"the homomorphism ", TT "M <-- N", " induced by ", TT "f", "."}
	  },
     "The modules ", TT "M", " and ", TT "N", " must both be ", TO "subquotients", " where
     M and P have the same ambient module, and N and Q have the same ambient module.
     If the optional argument ", TT "Verify", 
     " is given, check that the result defines a well defined homomorphism.",
     PARA,
     "In this example, the module K2 is mapped via g into K1, and we construct the
     induced map from K2 to K1.",
     EXAMPLE {
	  "R = ZZ/32003[x,y,z]",
	  "g1 = matrix{{x,y,z}}",
	  "g2 = matrix{{x^2,y^2,z^2}}",
	  "K1 = ker g1",
	  "K2 = ker g2",
	  "f = map(ambient K1, ambient K2, {{x,0,0},{0,y,0},{0,0,z}})",
	  "h = inducedMap(K1,K2,f)"
	  },
     "If we omit the first argument, then it is understood to be the target of f, and
     if we omit the second argument, it is understood to be the source of f.",
     EXAMPLE {
	  "h1 = inducedMap(target f,K2,f)",
	  "h2 = inducedMap(,K2,f)",
	  "h1 == h2"
	  },
     "In this example, we cannot omit the second argument, since in that case the resulting 
     object is not a homomorphism.",
     SeeAlso => {inducesWellDefinedMap, subquotient}
     }
document {
     Key => inducedMap,
     Headline => "compute an induced map",
     SeeAlso => "inducesWellDefinedMap"
     }
document {
     Key => [inducedMap,Degree],
     Headline => "specify the degree of a map",
     TT "Degree => n", " -- an option to ", TO "inducedMap", " that provides the
     degree of the map produced."
     }
document {
     Key => Verify,
     Headline => "verify that a map is well-defined",
     TT "Verify", " -- an option that can be used to request verification
     that a map is well defined.",
     PARA,
     UL {
	  TO [inducedMap,Verify]
	  }
     }
document {
     Key => [inducedMap,Verify],
     TT "Verify => true", " -- an option for ", TO "inducedMap", " which
     requests verification that the induced map produced is well defined."
     }
document {
     Key => inducesWellDefinedMap,
     Headline => "whether a map is well defined",
     TT "inducesWellDefinedMap(M,N,f)", " -- tells whether the matrix ", TT "f", " would
     induce a well defined map from ", TT "N", " to ", TT "M", ".",
     SeeAlso => "inducedMap"
     }
document {
     Key => matrix,
     Headline => "make a matrix",
     "This function can be used to create a matrix or map (homomorphism) between
     modules, but it is complicated because there are many different ways it can
     be used.  The entries of the matrix can be provided as a list of lists of ring
     elements, or as a function which accepts row and column indices.  The ring of
     the matrix can be provided explicitly, or the source and target modules can be 
     provided.  There are other alternatives.",
     PARA,
     SeeAlso => {"map"}
     }
document {
     Key => (matrix,Matrix),
     TT "matrix f", " -- produce the matrix of a map f.",
     PARA,
     "If the source and target of f are free, then the result is
     f itself.  Otherwise, the source and target will be replaced by
     the free modules whose basis elements correspond to the generators
     of the modules.",
     SeeAlso => {"map", "matrix"}
     }
document {
     Key => (matrix,Ring,List),
     Headline => "create a matrix from a doubly nested list of ring elements or matrices",
     Usage => "matrix(R, v)",
     Inputs => {
	  "R" => null,
	  "v" => "a list of vectors; or a doubly nested list of ring elements 
	         and/or matrices",
	  },
     Outputs => {
	  {"A matrix over ", TT "R", ", whose source and target are both 
	       free, formed by the  elements of ", TT "v", "."}
	  },
     "All of the vectors, ring elements, or matrices must be defined over the ring ", 
     TT "R", ", or a base ring of ", TT "R", ".",
     PARA,
     "If a doubly nested list of matrices is given, then ring elements can be used for
     1 by 1 blocks, and 0 represents a zero block.",
     PARA,
     "This is essentially the same as ", TO (matrix,List), " together with
     the specification of the ring.",
     PARA,
     EXAMPLE {
	  "R = QQ[a..d];",
	  "f = matrix{{a,b},{c,0}}",
	  "h = matrix{{f,f},{f,0}}"
	  }
     }
document { -- This node is used as an example in the node: Usage 
     Key => (matrix,List),
     Headline => "create a matrix from a doubly-nested list of ring 
                  elements or matrices",
     Usage => "matrix v",
     Inputs => {
	  "v" => "a list of lists of either ring elements or matrices"
	  },
     Outputs => {
	  "A matrix where the first list of v gives the first row (or set
	  of rows, if the elements are matrices), the second list is the 
          second row, etc."
	  },
     "An attempt is made to coerce the ring elements and matrices to
     a common ring.  If the entries are ring elements, they are used as
     the entries of the matrix, and if the entries are matrices, then
     they are used to provide blocks of entries in the resulting matrix.",
     PARA,
     "An attempt is made to set up the degrees of the generators of the
     free module serving as source so that the map will be homogeneous and of
     degree zero.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "p = matrix {{x,y,z}}",
      	  "degrees source p",
      	  "isHomogeneous p",
	  },
     "Notice that the degrees were set up so that p is homogeneous, because
     the source module is not explicitly specified by the user.  The next
     example involves block matrices.",
     EXAMPLE {
	  "q = vars R",
      	  "matrix {{q,q,q}}",
      	  "matrix {{q},{q},{q}}",
	  },
     "Here we construct a matrix from column vectors.",
     EXAMPLE {
	  "F = R^3",
      	  "matrix {F_2, F_1, x*F_0 + y*F_1 + z*F_2}",
	  },
     SeeAlso => {"map", "matrix"}
     }
document {
     Key => map,
     Headline => "make a map",
     Usage => "map(Y,X,d) or map(Y,X)",
     Inputs => {
	  "Y" => "an object, such as a ring, module, or chain complex",
	  "X" => {"another object of the same type"},
	  "d" => "a specification, such as a function, list, or 
	          matrix, or if omitted, understood to specify the identity map"
	  },
     Outputs => {
	  {"a map to ", TT "Y", " from ", TT "X", " defined by data ", TT "d", "."},
	  },
     "The function ", TT "map", " provides a general mechanism for constructing a map
     (homomorphism) between rings (", OFCLASS RingMap, "), modules (", OFCLASS Matrix,
     "), chain complexes (", OFCLASS ChainComplexMap, "), 
      or between objects in other categories.",
     PARA,
     "See also the function ", TO matrix, ", which focuses on creating new matrices from
     rectangular arrays of ring elements or matrices.",
     Subnodes => {
	  "Creating a map between modules (a matrix)",
	  TO (map,Module,Module,Function),   -- done
	  TO (map,Module,Module,List),  -- done
	  TO (map,Module,Module,RingElement), -- done
	  TO (map,Module,Module,Matrix), -- done
--	  TO (map,Module,Module), -- TODO
     	  "Creating a map between modules, where the source module is computed",
	  TO (map,Module,ZZ,Function), -- TODO
	  TO (map,Module,Nothing,List), -- TODO
	  TO (map,Module,ZZ,List), -- TODO
	  TO (map,Module,Nothing,Matrix), -- done
	  "Creating a map with a different degree",
	  TO (map,Matrix), -- done
	  "Creating a map between rings",
	  TO (map,Ring,Ring),
	  TO (map,Ring,Ring,List),
	  TO (map,Ring,Ring,Matrix),
	  "Creating a map between chain complexes",
	  TO (map,ChainComplex,ChainComplex,ChainComplexMap),
	  TO (map,ChainComplex,ChainComplex,Function)
	  }
     }
document {
     Key => (map,Module,Module,Function),
     Headline => "create a matrix by specifying a function which gives each entry",
     Usage => "map(M,N,f)",
     Inputs => {
	  "M" => null,
	  "N" => null,
	  "f" => null
	  },
     Outputs => {
	  {"a map from the module ", TT "N", " to the module ", TT "M", " 
	       whose matrix entry ", TT "h_(i,j)", " is obtained from the
	       function ", TT "f", " by evaluating ", TT "f(i,j)", "."
	       }
	  },
     "Recall that all indices in Macaulay 2 start at 0, so the upper left-most entry 
     of a matrix ", TT "f", " is ", TT "f_(0,0)", ".",
     PARA,
     "This function is often used when you already know the source and target modules, 
     including their gradings.  If you wish Macaulay 2 to compute the column degrees for
     you (so the resulting matrix will be homogeneous if possible), use ", 
     TO (map,Module,ZZ,Function), ".",
     EXAMPLE {
	  "R = ZZ[a..c];",
	  "f = map(R^3,R^{0,-1,-2,-3},(i,j) -> R_i^j)",
	  },
     "We specified the degrees of the source basis elements explicitly
     to ensure the matrix would be homogeneous.",
     EXAMPLE "isHomogeneous f",
     SUBSECTION "Alternate approaches",
     "We could have let Macaulay2 take care of that for us, by replacing
     the source module by its desired rank.",
     EXAMPLE {
	  "g = map(R^3,4,(i,j) -> R_i^j)",
	  "degrees g",
	  "isHomogeneous g"
	  },
     PARA,
     "Another way would be to let ", TO "matrix", " take care of that for
     us.",
     EXAMPLE {
	  "h = matrix table(3,4,(i,j) -> R_i^j)",
	  "degrees h",
	  "isHomogeneous h"
	  }
     }
document {
     Key => (map,Module,Module,List),
     Headline => "create a matrix by giving a sparse or dense list of entries",
     Usage => "map(M,N,v)",
     Inputs => {
	  "M" => null,
	  "N" => null,
	  "v" => null
	  },
     Outputs => {
	  {"A matrix ", TT "M <-- N", " whose entries are obtained from ", TT "v"}
	  },
     "The list ", TT "v", " is either a doubly nested list of 
     ring elements, or a list of elements ",
     TT "(i,j) => f", ".  The first version provides all of the elements of the 
     output matrix, row by row.  The second form provides only the 
     non-zero elements of the 
     output matrix ", TT "h: h_(i,j) = f", ", for every ", TT "(i,j) => f", 
     " in the list ", TT "v", ".",
     PARA,
     "In each case, the modules ", TT "M", " and ", TT "N", " should have the 
     same base ring
     ", TT "R", ", and the ring elements appearing in ", TT "v", " should be over ", 
     TT "R", ", or over a base
     ring of ", TT "R", ".",
     PARA,
     "In the first form, each list in v gives a row of the matrix. ",
     "The length of the list ", TT "v", " should be the number of generators of ", TT "M", 
     ", and the length of each element of ", TT "v", " (which is itself a 
     list of ring elements) should be the
     number of generators of the source module ", TT "N", ".",
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "p = map(R^2,R^{-2,-2,0},{{x^2,0,3},{0,y^2,5}})",
      	  "isHomogeneous p",
	  },
     "In the second form, if an index (i,j) occurs more than once, 
     only the last is taken.",
     EXAMPLE {
      	  "p = map(R^2,R^3,{(0,0) => x+y, (1,1) => x^2, (0,2) => x-1, (0,0) => x-y})"
	  },
     SeeAlso => {matrix, (map,Module,Nothing,List), "inputting a matrix"}
     }
document {
     Key => (map,Module,Module,Matrix),
     Headline => "create the matrix induced on generators by a given matrix",
     Usage => "map(M,N,p)",
     Inputs => {
	  "M" => null,
	  "N" => null,
	  "p" => null
	  },
     Outputs => {
	  {"A matrix with the same entries as ", TT "p", ", but whose target 
	  is ", TT "M", " and source is ", TT "N"}
	  },
     TT "M", " and ", TT "N", " should be modules over the same ring, and have the same 
     number of generators as ", TT "target p", " and ", TT "source p", ", respectively.",
     EXAMPLE {
	  "R = QQ[x,y,z]",
      	  "p = matrix {{x,y,z}}",
      	  "q = map(R^1,R^3,p)",
      	  "degrees source p",
      	  "degrees source q",
	  },
     SeeAlso => inducedMap,
     Caveat => {
     	  "If ", TT "M", " or ", TT "N", " is not free,
     	  then we don't check that the the result is a well defined homomorphism."
	  }
     }
document {
     Key => (map,Module,Module,RingElement),
     Headline => "construct the map induced by multiplication by a ring element on the
        generators",
     Usage => "map(M,N,r)",
     Inputs => {
	  "M" => null,
	  "N" => {"over the same ring ", TT "R", " as ", TT "M"},
	  "r" => {"in the ring ", TT "R"}
	  },
     Outputs => {
	  {"The map induced by multiplication by r on the generators"}
	  },
     "Either ", TT "M", " and ", TT "N", " should be equal, or they should 
     have the same number of generators.  This gives the same map as
     r * map(M,N,1).  map(M,N,1) is the map induced by the identity on the
     generators of M and N.",
     EXAMPLE {
	  "R = QQ[x]",
      	  "map(R^2,R^3,0)",
      	  "f = map(R^2,R^2,x)",
	  "f == x *map(R^2,R^2,1)"
	  },
     SeeAlso => inducedMap,
     Caveat => {
     	  "If ", TT "M", " or ", TT "N", " is not free,
     	  then we don't check that the the result is a well defined homomorphism."
	  }
     }
document {
     Key => (map,Matrix),
     Headline => "make a matrix with a different degree",
     Usage => "map(f, Degree => d)",
     Inputs => {
	  "f" => Matrix => null,
	  },
     Outputs => {
	  {"a map identical to ", TT "f", ", except that it has degree ", 
	       TT "d", ", and the source
	       module has been tensored by a graded free module of rank 1 of 
	       the appropriate degree."},
	  },
      "The input ", TT "d", " should be ", OFCLASS ZZ, ", or a list of integers",
      PARA,
      "This routine is often used to take a matrix which has a non-zero degree, 
      and make the degree zero.",
      PARA,
      "For example, multiplication of a matrix by a scalar increases the 
      degree, leaving the source and target fixed:",
      EXAMPLE {
	   "R = QQ[a,b];",
	   "f1 = matrix{{a,b}}",
	   "f = a * f1",
	   "degree f",
	   "source f == source f1",
	   },
      "One solution is to change the degree:",
      EXAMPLE {
	   "g = map(f, Degree => 0)",
	   "degree g",
	   "source g == (source f) ** R^{-1}"
	   },
      "An alternate solution would be to use tensor product with the scalar.",
      EXAMPLE {
     	  "g2 = a ** matrix{{a,b}}",
	  "degree g2",
	  "isHomogeneous g2"
	  }
     }
document {
     Key => (map,Module,Nothing,Matrix),
     Headline => "recast a matrix to have a new target, and a free module as source",
     Usage => "map(M,,f)",
     Inputs => {
	  "M" => null,
	  "f" => {"whose target has the same number of generators as ", TT "M"}
	  },
     Outputs => {
	  {"A map with a free source module, and target ", TT "M", ", whose entries are those of f."}
	  },
     EXAMPLE {
	  "R = ZZ/101[x,y]",
      	  "p = matrix{{x,y}}",
      	  "q = map(R^{3},,p)",
      	  "degrees target q",
      	  "degrees source q",
	  },
     SeeAlso => {matrix}
     }
document {
     Key => (map,Module,ZZ,Function),
     Headline => "create a matrix from a free module by specifying a function which gives each entry",
     Usage => "map(M,n,f)",
     Inputs => {
	  "M" => null,
	  "n" => null,
	  "f" => null
	  },
     Outputs => {
	  {"a map from a graded free module of rank ", TT "n", " to the module ", TT "M", " 
	       whose matrix entry ", TT "h_(i,j)", " is obtained from the
	       function ", TT "f", " by evaluating ", TT "f(i,j)", "."
	       }
	  },
     "This is the same as calling map(M,R^n,f), except that the 
     degrees of the basis elements of the source module are chosen
     in an attempt to ensure that the resulting map is homogeneous of
     degree zero.",
     EXAMPLE {
	  },
     SeeAlso => {(map,Module,Module,Function)}
     }
document {
     Key => (map,Module,ZZ,List),
     Headline => "create a matrix by giving a sparse or dense list of entries",
     Usage => "map(M,n,v)",
     Inputs => {
	  "M" => null,
	  "n" => null,
	  "v" => null
	  },
     Outputs => {
	  {"A matrix ", TT "M <-- R^n", " whose entries are obtained from ", TT "v",
	       ", where R  is the ring of M, and the source of the result is
	       a graded free module chosen in an attempt to make the result 
	       homogeneous of degree zero"}
	  },
     "The list ", TT "v", " is either a doubly nested list of 
     ring elements, or a list of elements ",
     TT "(i,j) => f", ".  The first version provides all of the elements of the 
     output matrix, row by row.  The second form provides only the 
     non-zero elements of the 
     output matrix ", TT "h: h_(i,j) = f", ", for every ", TT "(i,j) => f", 
     " in the list ", TT "v", ".",
     PARA,
     "The ring elements appearing in ", TT "v", " should be be in ", 
     TT "R", ", or in a base
     ring of ", TT "R", ".",
     PARA,
     "In the first form, each list in v gives a row of the matrix. ",
     "The length of the list ", TT "v", " should be the number of generators of ", TT "M", 
     ", and the length of each element of ", TT "v", " (which is itself a 
     list of ring elements) should be the
     number of generators of the source module ", TT "N", ".",
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "p = map(R^2,3,{{x^2,0,3},{0,y^2,5}})",
      	  "isHomogeneous p",
	  },
     "In the second form, if an index (i,j) occurs more than once, 
     only the last is taken.",
     EXAMPLE {
      	  "p = map(R^2,3,{(0,0) => x+y, (1,1) => x^2, (0,2) => x-1, (0,0) => x-y})"
	  },
     SeeAlso => {matrix, (map,Module,Nothing,List), "input a matrix"}
     }
document {
     Key => (map,Module,Nothing,List),
     Headline => "create a matrix by giving a doubly nested list of ring elements",
     Usage => "map(M,v)",
     Inputs => {
	  "M" => null,
	  "v" => null
	  },
     Outputs => {
	  {"A matrix ", TT "M <-- R^n", " whose entries are obtained from ", TT "v",
	       ", where R  is the ring of M, and the source of the result is
	       a graded free module chosen in an attempt to make the result 
	       homogeneous of degree zero"}
	  },
     "The list ", TT "v", " must be a doubly nested list of 
     ring elements, which are used to fill the matrix, row by row.",
     PARA,
     "The ring elements appearing in ", TT "v", " should be be in ", 
     TT "R", ", or in a base
     ring of ", TT "R", ".",
     PARA,
     "Each list in v gives a row of the matrix. ",
     "The length of the list ", TT "v", " should be the number of generators of ", TT "M", 
     ", and the length of each element of ", TT "v", " (which is itself a 
     list of ring elements) should be the
     number of generators of the source module ", TT "N", ".",
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "p = map(R^2,,{{x^2,0,3},{0,y^2,5}})",
      	  "isHomogeneous p",
	  },
     "Another way is to use the ", TO (matrix,List), " routine:",
     EXAMPLE {
      	  "p = matrix {{x^2,0,3},{0,y^2,5}}"
	  },
     PARA,
     "The absence of the second argument indicates that the source of the map
     is to be a free module constructed with an attempt made to assign degrees
     to its basis elements so as to make the map homogeneous of degree zero.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y]",
      	  "f = map(R^2,,{{x^2,y^2},{x*y,0}})",
      	  "degrees source f",
      	  "isHomogeneous f",
	  },
     SeeAlso => {matrix, (map,Module,Module,List), "input a matrix"}
     }
     
///
-- remove this?
document {
     Key => (map,Module,Module),
     TT "map(M,N)", " -- constructs the natural map from N to M.",
     PARA,
     "The modules ", TT "M", " and ", TT "N", " should be subquotient modules of the same
     free module",
     SeeAlso => {"map", "isWellDefined"}
     }
///

///
-- remove?
document {
     Key => (map,Module,Module,ZZ),
     TT "map(M,N,k)", " -- construct a map from a module ", TT "N", " to ", TT "M", " 
     which is provided by the integer ", TT "k", ".",
     PARA,
     "If ", TT "k", " is ", TT "0", ", then the zero map is constructed.  If ", TT "k", " is 1,
     then ", TT "M", " and ", TT "N", " should have the same number and degrees of generators 
     in the sense that the modules ", TT "cover M", " and ", TT "cover N", " are equal, and then the map
     which sends the ", TT "i", "-th generator of ", TT "N", " to the ", TT "i", "-th generator 
     of ", TT "M", " is constructed (and it may not be well-defined).
     Otherwise, ", TT "M", " and ", TT "N", " should be equal, or 
     at least have the same number of generators.",
     PARA,
     SeeAlso => {(map,Module,Module,RingElement), "map", "matrix"}
     }
///

///
-- remove?
document {
     Key => (map,Module,RingElement),
     TT "map(M,r)", " -- construct the map from M to itself which is provided
     by scalar multiplication by the ring element r.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x]",
      	  "map(R^2,x)",
	  },
     SeeAlso => {"map", "matrix"}
     }
///



document {
     Key => Degree,
     TT "Degree => d", " -- an optional argument to ", TO "matrix", " that
     specifies that the degree of the map created should be ", TT "d", ".",
     PARA,
     "The degree may be an integer or a list of integers (multidegree).  The
     length of the list should be the same as the length of a degree for the
     ring, see ", TO "degreeLength", ".",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x]",
      	  "p = map(R^1, R^1, {{x^4}})",
      	  "isHomogeneous p",
      	  "q = map(R^1, R^1, {{x^4}}, Degree => 4)",
      	  "isHomogeneous q",
	  },
     SeeAlso => {"map", "matrix", [inducedMap,Degree]}
     }
document {
     Key => id,
     Headline => "identity map",
     TT "id_M", " -- the identity homomorphism from ", TT "M", " to ", TT "M", "."
     }
document {
     Key => (reshape,Module,Module,Matrix),
     Headline => "reshape a matrix",
     Usage => "reshape(F,G,f)",
     Inputs => {
	  "F" => "a free module",
	  "G" => "a free module",
	  "f" => null
	  },
     Outputs => {
	  { " ", TT "F <-- G", " obtained from f by 
     	       taking elements from the first column of ", TT "f", ", 
	       then the second, and
     	       so on, filling them into the result column by column."
	       }
	  },
     "Currently, it is assumed
     that ", TT "f", " and the result both have the same 
     number of entries.  The resulting map is always of degree zero.",
     EXAMPLE {
	  "f = matrix{{1,3,5,7,9,11},{2,4,6,8,10,12}}",
	  "reshape(ZZ^3,ZZ^4,f)"
	  }
     }
TEST "
R=ZZ/101[a..d]
f = matrix {{a}}
assert( isHomogeneous f )
g = reshape(R^1, R^{-1}, f)
assert isHomogeneous g
"
document {
     Key => (adjoint1,Matrix,Module,Module),
     Headline => "an adjoint map",
     Usage => "adjoint1(f,G,H)",
     Inputs => {
	  "f" => {"a homomorphism ", TT "F --> G ** H", " between free modules"},
	  "G" => "a free module",
	  "H" => "a free module"
	  },
     Outputs => {
	  {"the adjoint homomorphism ", TT "F ** (dual G) --> H"}
	  },
     "All modules should be free modules over the same base ring, and the rank of the
     target of ", TT "f", " should be the product of the ranks of ", TT "G", " and ", 
     TT "H", ".  Recall that ", 
     TT "**", " refers to the tensor product of modules, and that ", TT "dual G", " is 
     a free module with the same rank as ", TT "G", ".",
     PARA,
     "No computation is required.  The resulting matrix has the same entries as ", 
     TT "f", ", but in a different layout.",
     PARA,
     "If ", TT "f", " is homogeneous, and ", TT "target f == G ** H", ",including 
     the grading, then the resulting matrix will be homogeneous.",
     PARA,
     EXAMPLE {
	  "R = QQ[x_1 .. x_12];",
	  "f = genericMatrix(R,6,2)",
	  "g = adjoint1(f,R^2,R^3)",
	  "isHomogeneous g"
	  },
     SeeAlso => {adjoint, flip, reshape, (symbol**,Module,Module), dual}
     }
document {
     Key => (adjoint,Matrix,Module,Module),
     Headline => "an adjoint map",
     Usage => "adjoint(f,G,H)",
     Inputs => {
	  "f" => {"a homomorphism ", TT "F ** G --> H", " between free modules"},
	  "F" => "a free module",
	  "G" => "a free module"
	  },
     Outputs => {
	  {"the adjoint homomorphism ", TT "F --> (dual G) ** H"}
	  },
     "All modules should be free modules over the same base ring, and the rank of the
     source of ", TT "f", " should be the product of the ranks of ", TT "F", " and ", 
     TT "G", ".  Recall that ", 
     TT "**", " refers to the tensor product of modules, and that ", TT "dual G", " is 
     a free module with the same rank as ", TT "G", ".",
     PARA,
     "No computation is required.  The resulting matrix has the same entries as ", 
     TT "f", ", but in a different layout.",
     PARA,
     "If ", TT "f", " is homogeneous, and ", TT "source f == F ** G", 
     ", including the grading, then 
     the resulting matrix will be homogeneous.",
     PARA,
     EXAMPLE {
	  "R = QQ[x_1 .. x_12];",
	  "f = genericMatrix(R,2,6)",
	  "g = adjoint(f,R^2,R^{-1,-1,-1})",
	  "isHomogeneous g"
	  },
     SeeAlso => {adjoint1, flip, reshape, (symbol**,Module,Module), dual}
     }
document {
     Key => (flip,Module,Module),
     Headline => "matrix of commutativity of tensor product",
     TT "flip(F,G)", " -- yields the matrix representing the map F ** G --> G ** F."
     }
document {
     Key => (subquotient,Matrix,Matrix),
     Headline => "make a subquotient module",
     Usage => "subquotient(g,r)",
     Inputs => {
	  "g" => "the matrix of generators",
	  "r" => {"the matrix of relations, with the same target as ", TT "g", ""}
	  },
     Outputs => {
	  {"the image of ", TT "g", " in the cokernel of ", TT "r"}
	  },
     PARA {
     	  "The general form in which modules are represented in Macaulay 2 is as subquotients,
	  and subquotient modules are often returned as values of computations."},
     EXAMPLE {
	  "R = ZZ/101[a..d]",
      	  "M = kernel vars R ++ cokernel vars R",
      	  "generators M",
      	  "relations M",
      	  "prune M",
	  },
     SeeAlso => {"generators", "relations", "prune"}
     }
document {
     Key => (symbol **, Matrix, Matrix),
     Headline => "tensor product of matrices",
     TT "f ** g", " -- computes the tensor product of two matrices.",
     PARA,
     SeeAlso => "Matrix"
     }
TEST "
ZZ[t]
assert (matrix {{t}} ** matrix {{t}} == matrix{{t^2}})
"
document {
     Key => symbol "compactMatrixForm",
     Headline => "global flag for compact printing",
	Usage => "compactMatrixForm = x",
	Consequences => {"changes the display of matrices"},
     TT "compactMatrixForm", " is a global flag which specifies whether to display
     matrices in compact form.",
     PARA,
     "The default value is ", TT "true", ".  The compact form is the form used by
     ", ITALIC "Macaulay", ", in which the multiplication and exponentiation operators
     are suppressed from the notation.",
     EXAMPLE {
	  "R = ZZ[x,y];",
	  "f = random(R^{2},R^2)",
	  "compactMatrixForm = false;",
	  "f"
	  }
     }
TEST "
R = ZZ/101[x,y,z]
I = ideal(x,y)
assert( 1 == dim I )
assert( 2 == codim I )
"
document {
     Key => (module, Ideal),
     Headline => "turn an ideal into a module",
     TT "module I", " -- produce the submodule of ", TT "R^1", " corresponding to an
     ideal ", TT "I", " in a ring ", TT "R", "."
     }
document {
     Key => (ideal, Ring),
     Headline => "get the ideal used to form a quotient ring",
     TT "ideal R", " -- for a quotient ring R=A/I, returns I"
     }
document {
     Key => ideal,
     Headline => "make an ideal",
     "ideal v", " -- produces the ideal spanned by a list or sequence of ring
     elements.",
     PARA,
     EXAMPLE {
	  "ZZ[a..i]",
      	  "ideal (c..h)"
	  },
     }
document {
     Key => kernel,
     Headline => "kernel of a map",
     TT "kernel f", " -- produces the kernel of a matrix or ring homomorphism.",
     PARA,
     "If ", TT "f", " is a ring element, it will be interpreted as a one by one
     matrix."
     }
document {
     Key => SubringLimit,
     Headline => "stop after finding enough elements of a subring",
     TT "SubringLimit", " -- an option for  ", TO "kernel", " and ", TO "gb", "
     which can stop the computation after a certain number of basis elements in
     a subring have been found.",
     SeeAlso => "computing Groebner bases"
     }
document {
     Key => [kernel,SubringLimit],
     TT "SubringLimit => n", " -- an option for ", TO "kernel", " which
     causes the computation of the kernel of a ring map to stop after ", TT "n", "
     elements have been discovered."
     }
document {
     Key => (homology,Matrix,Matrix),
     Headline => "kernel modulo image",
     TT "homology(g,f)", " -- computes the homology module ", TT "ker g/im f", ".",
     PARA,
     "Here ", TT "g", " and ", TT "f", " should be composable maps with ", TT "g*f", "
     equal to zero.",
     SeeAlso => "homology"
     }
document {
     Key => (dual, Matrix),
     Headline => "dual of a map",
     TT "dual f", " -- the dual (transpose) of a homomorphism."
     }
document {
     Key => singularLocus,
     Headline => "singular locus",
     TT "singularLocus R", " -- produce the singular locus of a ring,
     which is assumed to be integral and defined by a homogeneous ideal.",
     PARA,
     "Can also be applied to an ideal, in which case the singular locus of
     the quotient ring is returned."
     }
TEST "
     R=ZZ/101[x,y,z]
     assert( dim singularLocus ideal {y^2*z - x*(x - z)*(x + z) } === 0 )
     assert( dim singularLocus ideal {y^2*z - x*(x - z)*(x - z) } === 1 )
     S = ZZ/103[a..d]
     assert( dim singularLocus ideal { a^2 + b^2 + c^2 + d^2, a^2 + b^2 + 3*c^2 + 2*d^2 } === 1 )
     assert( dim singularLocus ideal { a^2 + b^2 + c^2 + d^2, a^2 + 5*b^2 + 3*c^2 + 2*d^2 } === 0 )
     "
document {
     Key => (symbol ^,Matrix,Array),
     Headline => "select some rows of blocks",
     TT "f^[i,j,k]", " -- extract some rows of blocks from a matrix ", TT "f", ".",
     PARA,
     "The target of ", TT "f", " should be a direct sum, and the result is obtained by
     composition with the projection onto the sum of the components numbered
     ", TT "i, j, k", ".  Free modules are regarded as direct sums.",
     PARA,
     EXAMPLE {
	  "f = map(ZZ^2 ++ ZZ^2, ZZ^2, {{1,2},{3,4},{5,6},{7,8}})",
      	  "f^[0]",
      	  "f^[1]",
      	  "f^[1,0]",
	  },
     SeeAlso => {submatrix, (symbol ^,Module,Array), (symbol _,Matrix,Array)}
     }
document {
     Key => (symbol _, Matrix, Array),
     Headline => "select some columns of blocks",
     TT "f_[i,j,k]", " -- extract some columns of blocks from a matrix ", TT "f", ".",
     PARA,
     "The source of ", TT "f", " should be a direct sum, and the result is obtained by
     composition with the inclusion into the sum of the components numbered
     ", TT "i, j, k", ".  Free modules are regarded as direct sums.",
     PARA,
     EXAMPLE {
	  "f = map(ZZ^2 ++ ZZ^2, ZZ^2, {{1,2},{3,4},{5,6},{7,8}})",
      	  "f^[0]",
      	  "f^[1]",
      	  "f^[1,0]",
	  },
     SeeAlso => {submatrix, (symbol _,Module,Array), (symbol ^,Matrix,Array)}
     }
document {
     Key => entries,
     Headline => "list the entries of a matrix",
     TT "entries f", " -- produces the matrix of the homomorphism f as a doubly
     nested list of ring elements.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "p = matrix {{x^2,y^2},{x*y*z, x^3-y^3}}",
      	  "entries p"
	  },
     }
TEST"
R=ZZ/101[a..f]
p = {{a,b},{c,d},{e,f}}
assert( entries matrix p == p )
"
TEST "
R = ZZ/101[a .. r]
assert ( genericMatrix(R,a,3,6) == genericMatrix(R,a,3,6) )
ff = genericMatrix(R,a,3,6)
fff = genericMatrix(R,a,3,6)
assert( # expression ff == 3 )
assert( ff == matrix {{a,d,g,j,m,p},{b,e,h,k,n,q},{c,f,i,l,o,r}} )
assert( -ff == matrix {
	  {-a,-d,-g,-j,-m,-p},
	  {-b,-e,-h,-k,-n,-q},
	  {-c,-f,-i,-l,-o,-r}} )
assert( 2*ff == matrix {
	  {2*a,2*d,2*g,2*j,2*m,2*p},
	  {2*b,2*e,2*h,2*k,2*n,2*q},
	  {2*c,2*f,2*i,2*l,2*o,2*r}} )
assert( ff != 0 )
assert( ff - ff == 0 )
assert( transpose ff - matrix{{a,b,c},{d,e,f},{g,h,i},{j,k,l},{m,n,o},{p,q,r}} == 0 )
--assert( transpose ff == matrix{{a,b,c},{d,e,f},{g,h,i},{j,k,l},{m,n,o},{p,q,r}} ) -- mike will fix.  DRG: these are not equal: they have different degrees...
--assert( ff_0 == vector {a,b,c} )
--assert( ff_1 == vector {d,e,f} )
--assert( ff_2 == vector {g,h,i} )
M = cokernel ff
assert ( ff === presentation M )		  -- original map saved
assert ( cokernel ff === M )		  -- cokernel memoized
-- gbTrace = 3
-- << \"gb ff ...\" << flush
G = gb ff
pM = poincare M
MM = cokernel fff
MM.cache.poincare = pM
-- << \"gb fff (with poincare provided) ...\" << flush
GG = gb fff
assert( numgens source generators G == numgens source generators GG )
T := (ring pM)_0
assert ( pM == 3-6*T+15*T^4-18*T^5+6*T^6 )
assert ( gb ff === G )
assert ( numgens source generators G == 41 )
assert ( numgens source mingens G == 6 )
time C = resolution M
assert( C === resolution M )
-- betti C
time D = resolution cokernel leadTerm generators G
-- betti D
"
document {
     Key => isInjective,
     Headline => "whether a map is injective",
     SeeAlso => "isSurjective"
     }
document {
     Key => isSurjective,
     Headline => "whether a map is surjective",
     SeeAlso => "isInjective"
     }
TEST "
R = ZZ/101[a]
assert isInjective R^2_{0}
assert not isInjective R^2_{0,0}
assert isSurjective R^2_{0,0,1}
assert not isSurjective R^2_{1}
"
document {
     Key => content,
     Headline => "the content of a polynomial",
     TT "content f", " -- returns the content of a matrix or polynomial.",
     PARA,
     "The content is the ideal of the base ring generated by the 
     coefficients."
     }
document {
     Key => QuotientRing,
     Headline => "the class of all quotient rings"
     }
document {
     Key => isQuotientOf,
     Headline => "whether one thing is a quotient of another"
     }
document {
     Key => isQuotientRing,
     Headline => "whether something is a quotient ring"
     }
TEST "
assert( ZZ/2 === ZZ/(4,6) )
R = ZZ/101[t]
"
document {
     Key => (symbol /, Ring, RingElement),
     Headline => "quotient ring",
     Usage => "S = R/f",
     Inputs => {
	  "R" => null,
	  "f" => { "an element of ", TT "R" }
	  },
     Outputs => {
	  "S" => { "the quotient ring ", TT "R/Rf"}
	  }
     }
document {
     Key => (symbol /, Ring, Sequence),
     Headline => "quotient ring",
     Usage => "S = R/(f,g,h,...)",
     Inputs => {
	  "R" => null,
	  "(f,g,h,...)" => { "a sequence of elements of ", TT "R" }
	  },
     Outputs => {
	  "S" => { "the quotient ring ", TT "R/(Rf+Rg+Rh+...)"}
	  }
     }
document {
     Key => (symbol /, Ring, Ideal),
     Headline => "quotient ring",
     Usage => "S = R/I",
     Inputs => {
	  "R" => null,
	  "I" => { "an ideal of ", TT "R"}
	  },
     Outputs => {
	  "S" => {"the quotient ring ", TT "R/I"}
	  },
     "The names of the variables are assigned values in the new quotient ring
     by automatically running ", TT "use R", ", unless ", TT "R", " has a name,
     or one of the rings ", TT "R", " is a quotient ring of has a name.
     See: ", TO "use", ".",
     PARA,
     "Warning: quotient rings are bulky objects, because they contain 
     a Groebner basis for their ideals, so only quotients of ", TO "ZZ", " 
     are remembered forever.  Typically the ring created by ", TT "R/I", " 
     will be a brand new ring, and its elements will be incompatible with 
     the elements of previously created quotient rings for the same ideal.",
     PARA,
     EXAMPLE {
	  "ZZ/2 === ZZ/(4,6)",
      	  "R = ZZ/101[t]",
      	  "R/t === R/t",
	  }
     }

document {
     Key => koszul, Headline => "a differential in a Koszul complex" }
document {
     Key => (koszul,ZZ,Matrix),
	Headline => "a differential in a koszul complex",
     Usage => "g = koszul(i,f)",
     Inputs => {
          "i" => "",
          "f" => {"a ", TT "1", " by ", TT "n", " matrix"},
          },
     Outputs => {
          "g" => { "the ", TT "i", "-th differential in the Koszul complex of the matrix ", TT "f"}
          },
     EXAMPLE {
          "R = QQ[x_1..x_4];",
          "f = matrix{{x_1..x_4}}"
          },
     "To see the second differential in the Koszul complex of the matrix ", TT "f", " look at:",
     EXAMPLE "koszul(2,f)",
     }
document {
     Key => symmetricPower, Headline => "symmetric power" }
document {
     Key => (symmetricPower,ZZ,Matrix),
     Usage => "symmetricPower(i,f)",
     Inputs => {
	  "i" => "",
	  "f" => "",
	  },
     Outputs => {
	  { "the ", TT "i", "-th symmetric power of ", TT "f"}
	  },
     PARA { "Note: in the current implementation, ", TT "f", " should have just one row." }
     }
document {
     Key => (exteriorPower,ZZ,Matrix),
	Headline => "exterior power of a matrix",
     Usage => {TT "exteriorPower(i,f)", " or ", TT "exteriorPower_i f"},
     Inputs => {
	  "i" => null,
	  "f" => null,
	  },
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
     Usage => {TT "exteriorPower(i,M)", " or ", TT "exteriorPower_i M"},
     Inputs => {
	  "i" => null,
	  "M" => null
	  },
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
     Key => (symbol _,Function,Thing),
     Headline => "attach the first argument to a function of two or more arguments",
     Usage => "g = f_x",
     Inputs => {
	  "f" => Function => "a function of two or more arguments",
	  "x" => Thing => ""
	  },
     Outputs => {
	  "g" => Function => {
	       "a new function with the property that ", TT "g(y)", "
	       returns the value of  ", TT "f(x,y)", ", that
	       ", TT "g(y,z)", " returns the value of ", TT "f(x,y,z)", ", and
	       so on."
	       }
	  },
     PARA {
     	  "This abbreviation allows us to save a bit of typing, and in some
     	  cases, agrees with standard mathematical notation."},
     EXAMPLE {
	  "R = ZZ[a .. i];",
	  "f = genericMatrix(R,a,3,3)",
	  "exteriorPower(2,f)",
	  "exteriorPower_2 f",
	  "p = prepend_7",
	  "p {8,9,10}"
	  }
     }
document {
     Key => MinorsComputation,
     Headline => "the class of all minors computations",
     TT "MinorsComputation", " -- a type of self initializing list used
     internally by ", TO "minors", "."
     }
document {
     Key => wedgeProduct,
     Headline => "the exterior multiplication map",
     TT "wedgeProduct(p,q,M)", " -- returns the matrix which represents the
     multiplication map from ", TT "exteriorPower(p,M) ** exteriorPower(q,M)", "
     to ", TT "exteriorPower(p+q,M)", ".",
     PARA,
     "Here ", TT "M", " is free module."
     }
document {
     Key => exteriorPower,
     Headline => "exterior power",
     SeeAlso => {"minors", "det", "wedgeProduct"}
     }
TEST ///
    R = ZZ[x,y,z]
    modules = {
	 image matrix {{x^2,x,y}},
	 coker matrix {{x^2,y^2,0},{0,y,z}},
	 R^{-1,-2,-3},
	 image matrix {{x,y}} ++ coker matrix {{y,z}}
	 }
    scan(modules, M -> assert( cover exteriorPower(2,M) == exteriorPower(2,cover M) ))
///
document {
     Key => minors,
     Headline => "ideal generated by minors",
     SeeAlso => {"exteriorPower", "det"}
     }
document {
     Key => (minors,ZZ,Matrix),
     Headline => "ideal generated by minors",
     Usage => "minors(p,m)",
     Inputs => {
	  "p" => "",
	  "m" => {"a map between free modules"}
	  },
     Outputs => {
	  {"the ideal generated by the ", TT "p", " by ", TT "p", " minors of the matrix ", TT "m", "."}
	  },
     "Minors are generated in the same order as that used by ", TO (subsets,ZZ,ZZ), ".  The
     order is {{0,1},{0,1}}, {{0,2},{0,1}}, {{1,2},{0,1}}, and so on.",
     EXAMPLE {
	  "R = ZZ[vars(0..11)];",
	  "M = genericMatrix(R,a,4,3)",
	  "transpose generators minors(2,M)",
	  "subsets(4,2)"
	  },
     SUBSECTION "Programming hint",
     "If the ", TO2{ [minors,First], "First"}, " option is not given, the minors are stashed
     in the matrix and are obtainable with ", TT "m.cache#MinorsComputation{p}", ".  The class of
     this stashed object is ", TO "MinorsComputation", "."
     }
document {
     Key => symbol Bareiss,
     "A symbol used by ", TO "det", ", ", TO "minors", ", and ", TO "exteriorPower", "."
     }
document {
     Key => symbol Cofactor,
     "A symbol used by ", TO "det", ", ", TO "minors", ", and ", TO "exteriorPower", "."
     }
scan({det,minors,exteriorPower},fn -> document { 
	  Key => [fn,Strategy],
	  Headline => "choose between Bareiss and Cofactor algorithms",
	  Usage => toString fn | "(..., Strategy => x)",
	  Inputs => {
	       "x" => Symbol => {"either ", TO "Bareiss", " or ", TO "Cofactor", "" }
	       },
	  Consequences => {
	       { "If x is ", TO "Bareiss", ", then the Bareiss fraction-free determinant algorithm is used; if
		    x is ", TO "Cofactor", ", then cofactor expansion is used to compute determinants." }
	       },
	  "The base ring determines the default strategy.  If the base ring is not a quotient
	  polynomial ring, the ", TO "Bareiss", " algorithm is used.  If the base ring is
	  a quotient ring, and the ring has not been declared to be a field with ", TO "toField", ", then
	  the ", TO "Cofactor", " algorithm is used.",
	  Caveat => {"The Bareiss algorithm returns
	       a ring element which may differ from the actual determinant by a zero divisor in the ring.  Thus, 
	       an ", BOLD "incorrect", " answer may be computed if the ring contains zero divisors."}
	  })
document {
     Key => [minors,First],
     Headline => "set the first minor to compute",
     Usage => "minors(..., First => {rows, columns})",
     Inputs => {
	  "rows" => "a list of integers",
	  "columns" => "a list of integers"
	  },
     Consequences => {
	  {"the minors are computed starting with the one determined by the row indices and column indices provided" }
	  },
     "Minors are generated in the same order as that used by ", TO (subsets,ZZ,ZZ), ". See ", TO (minors,ZZ,Matrix), ".",
     EXAMPLE {
	  "R = ZZ[a..f];",
	  "M = matrix{{a,b,c},{d,e,f}}",
	  "minors(2,M,First=>{{0,1},{0,2}})",
	  "minors(2,M,First=>{{0,1},{0,2}},Limit=>1)",
	  },
     }
document {
     Key => [minors,Limit],
     Headline => "the maximum number of minors to compute",
     Usage => "minors(...,Limit => n)",
     Inputs => {
	  "n" => ZZ => ""
	  },
     Consequences => {
	  {"the computation stops after ", TT "n", " minors are obtained"}
	  }
     }
TEST "
-- For more determinant tests, see Macaulay2/test/testdet.m2
R = ZZ/103[a,b,c,d]
h = matrix {{a,b},{c,d}}
assert( det h == a * d - b * c )
assert( minors(1,h) == image matrix {{a,b,c,d}} )
assert( minors(2,h) == image matrix {{a * d - b * c}} )
"
document {
     Key => pfaffians,
     Headline => "ideal generated by Pfaffians",
     Usage => "pfaffians(n,f)",
     Inputs => {"n" => ZZ => "the desired size of the pfaffians",
	  "f" => Matrix => "which is skew-symmetric, and whose ring is an integral domain"
	  },
     Outputs => {Ideal=>{"the ideal generated by the pfaffians of the ", TT "n", " by ", 
	  TT "n", " principal submatrices of f"}},
     "Recall that the determinant of a skew symmetric matrix ", TT "f", " (that is, ", 
     TT "transpose(f) + f == 0)",
     " is always a perfect square, and that its square root is called its pfaffian.",
     PARA,
     "If the matrix ", TT "f", " is ", TT "m", " by ", TT "m", ", then there are ", TT "m", 
     " choose ", TT "n", " possible pfaffians of ", TT "f", ", but we only
     collect the non-zero ones in the result.",
     EXAMPLE {
	  "R = QQ[u..z];",
      	  "f = genericSkewMatrix(R,u,4)",
      	  "pfaffians(2,f)",
      	  "pfaffians(4,f)"
	  },
     SeeAlso => minors,
     Caveat => {
	  "The algorithm used is a modified Gaussian reduction/Bareiss algorithm, 
	  which uses division and therefore we must assume that the ring of ", TT "f", "
	  is an integral domain.",
	  PARA,
          "The skew symmetry of ", TT "f", " is not checked, but the algorithm 
	  proceeds as if it were, with somewhat unpredictable results."
	  }
     }
TEST ///
R=ZZ/101[a..f]
m=genericSkewMatrix(R,a,4)
assert( pfaffians(-2,m) == ideal(0_R) )
assert( pfaffians(0,m) == ideal(1_R) )
assert( pfaffians(1,m) == ideal(0_R) )
assert( pfaffians(2,m) == ideal(a,b,c,d,e,f) )
assert( pfaffians(3,m) == ideal(0_R) )
assert( pfaffians(4,m) == ideal(c*d-b*e+a*f) )
///
document {
     Key => trace,
     Headline => "trace of a matrix",
     TT "trace f", " -- returns the trace of the matrix f.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[a..d]",
      	  "p = matrix {{a,b},{c,d}}",
      	  "trace p"
	  },
     }
document {
     Key => det,
     Headline => "determinant of a matrix",
     SeeAlso => {"minors", "exteriorPower"}
     }
document {
     Key => (det,Matrix),
     Headline => "determinant of a matrix",
     Usage => "det f",
     Inputs => {
	  "f" => { "a square matrix"}
	  },
     Outputs => {
	  { "the determinant of ", TT "f"}
	  },
     EXAMPLE {
	  "R = QQ[a..d]",
      	  "p = matrix {{a,b},{c,d}}",
      	  "det p"
	  }
     }
     
document {
     Key => Limit,
     Headline => "specify how many to compute",
     TT "Limit => n", " -- an optional argument for ",
     TO "minors", " specifying that the computation should stop 
     after ", TT "n", " more elements are computed."
     }
document {
     Key => fittingIdeal,
     Headline => "Fitting ideal of a module",
     TT "fittingIdeal(i,M)", " -- the i-th Fitting ideal of the module M",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x];",
      	  "k = coker vars R",
      	  "M = R^3 ++ k^5;",
      	  "fittingIdeal(3,M)",
      	  "fittingIdeal(8,M)"
	  },
     }
TEST "
R = ZZ/101[x];
k = coker vars R;
M = R^3 ++ k^5;
assert( fittingIdeal(0,M) == ideal 0_R )
assert( fittingIdeal(1,M) == ideal 0_R )
assert( fittingIdeal(2,M) == ideal 0_R )
assert( fittingIdeal(3,M) == ideal x^5 )
assert( fittingIdeal(4,M) == ideal x^4 )
assert( fittingIdeal(5,M) == ideal x^3 )
assert( fittingIdeal(6,M) == ideal x^2 )
assert( fittingIdeal(7,M) == ideal x )
assert( fittingIdeal(8,M) == ideal 1_R )
assert( fittingIdeal(9,M) == ideal 1_R )
"

document {
     Key => (genericMatrix,Ring,RingElement,ZZ,ZZ),
     Headline => "make a generic matrix of indeterminates",
     Usage => "genericMatrix(R,x,m,n)",
     Inputs => {
	  "R" => null,
	  "x" => {"an indeterminate in the ring ", TT "R"},
	  "m" => null,
	  "n" => null
	  },
     Outputs => {
	  {"An ", TT "m", " by ", TT "n", " matrix of indeterminates drawn from the ring ", 
	       TT "R", ", starting with ", TT "x", "."}
	  },
     EXAMPLE {
	  "R = ZZ[a..f];",
      	  "genericMatrix(R,a,2,3)"
	  },
     SeeAlso => {genericSkewMatrix, genericSymmetricMatrix}
     }

document {
     Key => (genericMatrix,Ring,ZZ,ZZ),
     Headline => "make a generic matrix of indeterminates",
     Usage => "genericMatrix(R,m,n)",
     Inputs => {
	  "R" => null,
	  "m" => null,
	  "n" => null
	  },
     Outputs => {
	  {"An ", TT "m", " by ", TT "n", " matrix of indeterminates drawn from the ring ", 
	       TT "R", ", starting with the first indeterminate."}
	  },
     EXAMPLE {
	  "R = ZZ[a..f];",
      	  "genericMatrix(R,2,3)"
	  },
     SeeAlso => {genericSkewMatrix, genericSymmetricMatrix}
     }

document {
     Key => (genericSkewMatrix,Ring,RingElement,ZZ),
     Headline => "make a generic skew symmetric matrix of indeterminates",
     Usage => "genericSkewMatrix(R,x,n)",
     Inputs => {
	  "R" => null,
	  "x" => {"an indeterminate in the ring ", TT "R"},
	  "n" => null
	  },
     Outputs => {
	  {"An ", TT "n", " by ", TT "n", " skew symmetric 
            matrix whose entries above the diagonal are the indeterminates of ", TT "R",
	       ", starting with ", TT "x", "."}
	  },
     EXAMPLE {
	  "R = ZZ[a..f];",
      	  "genericSkewMatrix(R,a,4)"
	  },
     SeeAlso => {genericMatrix, genericSymmetricMatrix}
     }

document {
     Key => (genericSkewMatrix,Ring,ZZ),
     Headline => "make a generic skew symmetric matrix of indeterminates",
     Usage => "genericSkewMatrix(R,n)",
     Inputs => {
	  "R" => null,
	  "n" => null
	  },
     Outputs => {
	  {"An ", TT "n", " by ", TT "n", " skew symmetric
            matrix whose entries above the diagonal are the indeterminates of ", TT "R", ", 
	    starting with the first indeterminate."}
	  },
     EXAMPLE {
	  "R = ZZ[a..f];",
      	  "genericSkewMatrix(R,4)"
	  },
     SeeAlso => {genericMatrix, genericSymmetricMatrix}
     }

document {
     Key => (genericSymmetricMatrix,Ring,RingElement,ZZ),
     Headline => "make a generic symmetric matrix of indeterminates",
     Usage => "genericSymmetricMatrix(R,x,n)",
     Inputs => {
	  "R" => null,
	  "x" => {"an indeterminate in the ring ", TT "R"},
	  "n" => null
	  },
     Outputs => {
	  {"An ", TT "n", " by ", TT "n", " symmetric 
	       matrix whose entries on and above the diagonal
	       are indeterminates drawn from the ring ", 
	       TT "R", ", starting with ", TT "x", "."}
	  },
     EXAMPLE {
	  "R = ZZ[a..j];",
      	  "genericSymmetricMatrix(R,a,4)"
	  },
     SeeAlso => {genericMatrix, genericSkewMatrix}
     }

document {
     Key => (genericSymmetricMatrix,Ring,ZZ),
     Headline => "make a generic symmetric matrix of indeterminates",
     Usage => "genericSymmetricMatrix(R,n)",
     Inputs => {
	  "R" => null,
	  "n" => null
	  },
     Outputs => {
	  {"An ", TT "n", " by ", TT "n", " symmetric matrix whose entries
	       on and above the diagonal
	       are the indeterminates drawn from the ring ", 
	       TT "R", ", starting with the first indeterminate."}
	  },
     EXAMPLE {
	  "R = ZZ[a..j];",
      	  "genericSymmetricMatrix(R,4)"
	  },
     SeeAlso => {genericMatrix, genericSkewMatrix}
     }

document {
     Key => genericMatrix,
     SeeAlso => {genericSymmetricMatrix, genericSkewMatrix}
     }
document {
     Key => genericSkewMatrix,
     Headline => "make a generic skew symmetric matrix"
     }
document {
     Key => genericSymmetricMatrix,
     Headline => "make a generic symmetric matrix"
     }
document {
     Key => (symbol +, Module, Module),
     Headline => "sum of submodules",
     TT "M + N", " -- the sum of two submodules.",
     PARA,
     "The two modules should be submodules of the same module."
     }
document {
     Key => (symbol **, Module, Module),
     Headline => "tensor product of modules",
     TT "M ** N", " -- produce the tensor product of two modules.",
     PARA,
     "Since M and N may be provided as submodules or subquotient modules, it
     may be necessary to replace them by quotient modules in the course of the
     computation, but the generators provided in the resulting tensor product 
     will correspond to the tensor products of the generators, i.e., the modules
     ", TT "cover M ** cover N", " and ", TT "cover(M ** N)", " are equal.
     This makes it easier to make ", TT "M ** N", " into a functor."
     -- i.e., we don't use 'prune'!
     }
TEST ///
    R = ZZ[x,y,z]
    modules = {
	 image matrix {{x^2,x,y}},
	 coker matrix {{x^2,y^2,0},{0,y,z}},
	 R^{-1,-2,-3},
	 image matrix {{x,y}} ++ coker matrix {{y,z}}
	 }
    table(modules, modules, (P,Q) -> assert(cover P ** cover Q == cover (P ** Q)));
///
document {
     Key => (symbol **, Matrix, Module),
     Headline => "tensor product",
     TT "f ** N", " -- tensor product of a matrix ", TT "f", " and a module ", TT "N", ".",
     PARA,
     "This is the same as tensoring ", TT "f", " with the identity map of ", TT "N", ".",
     PARA,
     "When ", TT "N", " is a free module of rank 1 the net effect of the
     operation is to shift the degrees of ", TT "f", ".",
     EXAMPLE {
	  "R = ZZ/101[t]",
      	  "f = matrix {{t}}",
      	  "degrees source f",
      	  "degrees source (f ** R^{-3})",
	  },
     SeeAlso => {"Matrix", "Module"}
     }
document {
     Key => (symbol **, Module, Ring),
     Headline => "tensor product",
     TT "M ** R", " -- form the tensor product of a module ", TT "M", " with a ring ", TT "R", ".",
     PARA,
     "The ring of ", TT "M", " should be a base ring of ", TT "R", ".",
     EXAMPLE {
	  "R = ZZ/101[x,y];",
      	  "M = coker vars R",
      	  "M ** R[t]"
	  },
     }
document {
     Key => (symbol **, Matrix, Ring),
     Headline => "tensor product",
     TT "f ** R", " -- form the tensor product of a module map ", TT "f", " with 
     a ring ", TT "R", ".",
     PARA,
     "The ring of ", TT "f", " should be a base ring of ", TT "R", ".  The degree 
     of the map is preserved.",
     EXAMPLE {
	  "R = ZZ[a..c];",
	  "S = R/(a+b+c);",
      	  "f = vars R",
	  "f ** S",
	  },
     }
document {
     Key => poincareComputation,
     Headline => "store the Poincare polynomial computation",
     TT "poincareComputation", " -- a key used in a module or monomial
     ideal to store a computation of Poincare polynomial.",
     PARA,
     SeeAlso => {"poincare"}
     }
document {
     Key => hilbertFunction,
     Headline => "Hilbert function of a module",
     TT "hilbertFunction(d,M)", " -- compute the dimension of the degree d
     part of the module, ring, or ideal M",
     PARA,
     "At the moment, the function is computed simply by calling ", TO "basis", "
     and extracting the number of basis elements.",
     SeeAlso => {"hilbertSeries", "hilbertPolynomial"}
     }
TEST ///
R = ZZ/101[x,y]
M = R^1/x
T = degreesRing R
t = T_0
assert( hilbertSeries (M, Order => 5) == t^4+t^3+t^2+t+1 )
assert( hilbertSeries (M, Order => 4) == t^3+t^2+t+1 )
assert( hilbertSeries (M, Order => 7) == t^6+t^5+t^4+t^3+t^2+t+1 )
///
document {
     Key => Order,
     Headline => "specify the order of a Hilbert series required",
     TT "Order", " -- an optional argument used with ", TO "hilbertSeries", "
     to specify the order of the series requested."
     }
document {
     Key => hilbertSeries,
     Headline => "compute Hilbert series",
     TT "hilbertSeries M", " -- compute the Hilbert series of the ring or
     module ", TT "M", ".",
     PARA,
     "The Hilbert series is the formal power series in the variables of the
     degrees ring whose coefficients are the dimensions of the corresponding
     graded component.  The series is provided as an ", TO "Expression", "
     representing a rational function with that series.",
     PARA,
     "If an optional integer argument labeled ", TO "Order", " is used, then
     the power series is expanded to that order.",
     EXAMPLE {
	  "R = ZZ/101[x, Degrees => {2}];",
      	  "hilbertSeries(R/x^2)",
      	  "numerator oo",
      	  "value oo",
      	  "poincare (R/x^2)",
      	  "hilbertSeries(R/x^2, Order => 12)",
	  },
     EXAMPLE {
	  "R=ZZ/101[x, Degrees => {{1,1}}];",
      	  "hilbertSeries (R/x^2)",
	  },
     SeeAlso => {"degreesRing", "Order"}
     }
document {
     Key => ProjectiveHilbertPolynomial,
     Headline => "the class of all Hilbert polynomials",
     "For convenience, these polynomials are expressed in terms of the Hilbert 
     polynomials of projective space.",
     PARA,
     "The functions ", TO "degree", " and ", TO "dim", " are designed so they
     correspond the degree and dimension of the algebraic variety that may have
     been used to produce the Hilbert polynomial.",
     EXAMPLE {
	  "Z = Proj(QQ[x_0..x_12]/(x_0^3+x_12^3))",
	  "hilbertPolynomial Z"
	  }
     }
document {
     Key => (symbol " ", ProjectiveHilbertPolynomial, ZZ),
     Headline => "value of polynomial",
     TT "P i", " -- the value of a projective Hilbert polynomial ", TT "P", " at 
     an integer ", TT "i", ".",
     PARA,
     EXAMPLE {
	  "P = projectiveHilbertPolynomial 2",
      	  "apply(0 .. 12, i -> P i)",
	  },
     SeeAlso => ProjectiveHilbertPolynomial
     }
document {
     Key => projectiveHilbertPolynomial,
     Headline => "Hilbert polynomial of projective space",
     TT "projectiveHilbertPolynomial n", " -- produces the projective
     Hilbert polynomial corresponding to projective space of dimension n.",
     BR,NOINDENT,
     TT "projectiveHilbertPolynomial(n,d)", " -- produces the projective
     Hilbert polynomial corresponding to the graded ring of projective space
     of dimension n, but with its generator in degree -d.",
     PARA,
     SeeAlso => "ProjectiveHilbertPolynomial"
     }
TEST "
scan(3, n -> scan(-3 .. 3, d -> (
	       h := projectiveHilbertPolynomial(n,d);
	       scan(3, i -> assert( h i === binomial(n+d+i,n) )))))
"
TEST "
scan(3, n -> (
     R = ZZ/101[x_0 .. x_n];
     scan(-2 .. 2, d -> (
	  M = R^{-d};
	  h = hilbertPolynomial M;
	  scan(d .. d + 4, e -> assert(numgens source basis(e,M) == h e))))))
"
TEST "
scan(3, n -> (
     R = ZZ/101[x_0 .. x_n];
     scan(-2 .. 2, d -> (
	  M = R^{-d};
	  h = hilbertPolynomial (M, Projective => false);
	  i = (ring h)_0;
	  scan(d .. d + 4, e -> (
		    r = numgens source basis(e,M);
		    s = substitute(h, { i => e/1 });
	       	    assert( r == s)))))))
"
document {
     Key => Projective,
     Headline => "whether to produce a projective Hilbert polynomial",
     TT "Projective => true", " -- an option to ", TO "hilbertPolynomial", " which
     specifies that the Hilbert polynomial produced should be expressed in terms
     of the Hilbert polynomials of projective spaces.  This is the default.",
     BR, NOINDENT,
     TT "Projective => false", " -- an option to ", TO "hilbertPolynomial", " which
     specifies that the Hilbert polynomial produced should be expressed as a 
     polynomial in the degree.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[a..d]",
      	  "S = image map(R, R, {a^4, a^3*b, a*b^3, b^4})",
      	  "presentation S",
      	  "h = hilbertPolynomial S",
	  },
     PARA,
     "The rational quartic curve in P^3 is therefore 'like' 4 copies of P^1, with
     three points missing.  One can see this by noticing that there is a deformation
     of the rational quartic to the union of 4 lines, or 'sticks', which intersect
     in three successive points.",
     PARA,
     "These Hilbert polynomials can serve as Hilbert functions, too.",
     EXAMPLE {
	  "h 3",
      	  "basis(3,S)",
      	  "rank source basis(3,S)",
	  },
     PARA,
     "Note that the Hilbert polynomial of P^i is z |--> binomial(z + i, i).",
     PARA,
     SeeAlso => "ProjectiveHilbertPolynomial"
     }
document {
     Key => hilbertPolynomial,
     Headline => "compute Hilbert polynomial",
     TT "hilbertPolynomial M", " -- the Hilbert polynomial of the module ", TT "M", " as
     a polynomial in ", TT "T", "."
     }
document {
     Key => codim,
     Headline => "calculate the codimension",
     TT "codim M", " -- calculate the codimension of the support of a module ", TT "M", ".",
     BR,NOINDENT,
     TT "codim I", " -- calculate the codimension of the quotient ring ", TT "R/I", ".",
     PARA,
     "If ", TT "M", " is an ", TT "R", "-module, then the number return by this 
     routine is ", TT "dim R - dim M", ".  This does not agree with the usual
     definition of codimension unless ", TT "Spec R", " is irreducible.",
     EXAMPLE {
	  "R = QQ[x,y]/(ideal(x,y) * ideal(x-1))",
      	  "codim (R^1/(x,y))"
	  },
     PARA,
     "Warning: over the integers, the computation effectively tensors first with the
     rational numbers, yielding the wrong answer in some cases.",
     PARA,
     "Warning: we don't really compute the codimension when the ring has components of
     different dimension!"
     }
document {
     Key => (codim, Module),
     Usage => "c = codim M",
     Inputs => {
	  "M" => {"a module over a ring ", TT "R"}
	  },
     Outputs => {
	  "c" => {"the codimension of the module, ", TT "dim R - dim M"}
	  },
     "The returned value is the usual codimension if ", TT "R", " is an integral domain, or all
     components have the same dimension.",
     EXAMPLE {
	  "R = ZZ/101[a..d];",
	  "M = coker matrix{{a,b},{c,d}}",
	  "codim M"
	  },
     Caveat => ("If ", TT "R", " is ", TO "ZZ", ", then the computation effectively tensors first with the
	  rational numbers, yielding the wrong answers in some cases.  The ring ", TT "R", " must be a quotient of a commutative polynomial ring.")
     }
document {
     Key => dim,
     Headline => "calculate the dimension of a ring, module",
     "Warning: if you want the dimension of a vector space, you should use ", TO "rank", ".",
     PARA,
     "Warning: over the integers, the computation effectively tensors first with the
     rational numbers, yielding the wrong answer in some cases."
     }
document {
     Key => presentation,
     Headline => "presentation of a module or ring",
     TT "presentation M", " -- produce a presentation of the module ", TT "M", ".",
     BR,NOINDENT,
     TT "presentation R", " -- produce a presentation of the quotient ring ", TT "R", ".",
     BR,NOINDENT,
     TT "presentation(R,S)", " -- produce a presentation of the quotient ring ", TT "S", " over ", TT "R", ".",
     PARA,
     "A presentation of ", TT "M", " is a map ", TT "p", " so that ", TT "coker p", " is 
     isomorphic to ", TT "M", ".  The presentation obtained is expressed in 
     terms of the given generators, i.e., the modules ", TT "cover M", " and 
     ", TT "target p", " are identical.
     The isomorphism can be obtained as ", TT "map(M,coker p,1)", ".",
     PARA,
     "Since a module M may be described as a submodule or a subquotient 
     module of a free module, some computation may be required to produce 
     a presentation.  See also ", TO "prune", " which does a bit more work to try to
     eliminate redundant generators.",
     PARA,
     "For a quotient ring R, the result is a matrix over the ultimate
     ambient polynomial ring, whose image is the ideal defining ", TT "R", ".",
     SeeAlso => {"cover"}
     }
TEST ///
    R = ZZ[x,y,z]
    modules = {
	 image matrix {{x^2,x,y}},
	 coker matrix {{x^2,y^2,0},{0,y,z}},
	 R^{-1,-2,-3},
	 image matrix {{x,y}} ++ coker matrix {{y,z}}
	 }
    scan(modules, M -> assert( cover M == target presentation M ) )
///
document {
     Key => pruningMap,
     Headline => "store the isomorphism obtained by pruning",
     TT "pruningMap", " -- the key under which is stored the isomorphism to
     a module ", TT "M", " from the module ", TT "prune M", ".",
     PARA,
     "This map exists only after ", TT "N = prune M", " has been executed
     at least once, and then the map can be obtained with ", TT "N.cache.pruningMap", ".",
     SeeAlso => "prune"
     }
document {
     Key => dual,
     Headline => "dual module or map",
     TT "dual M", " -- the dual.",
     PARA,
     "For details, see one of the following.",
     UL {
	  TO (dual,ChainComplex),
	  TO (dual,Matrix),
	  TO (dual,Module)
	  }
     }
TEST "
r = ZZ/101[a,b]
assert ( 2 * degree (a * b^2) === {6} )
M = cokernel matrix (r,{{1}})
assert ( isFreeModule prune M )
"
TEST "
GF(8,Variable => x)
assert ( det matrix{{x,1},{x^2,x^3}} == x^4 - x^2 )
"
TEST "
R = ZZ/101[a..f]
M = cokernel matrix (R, {{1},{-1}})
N = prune M
p = N.cache.pruningMap
assert( source p == N )
assert( target p == M )
assert( prune kernel p == 0 )
assert( prune cokernel p == 0 )
assert isIsomorphism p
assert isIsomorphism p^-1
assert ( p * p^-1 == id_M )
assert ( p^-1 * p == id_N )
"
document {
     Key => (dual, Module),
     Headline => "dual module",
     TT "dual M", " -- the dual of a module."
     }
document {
     Key => homomorphism,
     Headline => "get the homomorphism from element of Hom",
     Usage => "homomorphism f",
     Inputs => {
	  "f" => {"of the form Hom(M,N) <-- R^1, where Hom(M,N) has been
	  previously computed, and R is the ring of f, M and N"},
	  },
     Outputs => {
	  {"M <-- N, corresponding to the element f"}
	  },
     "When ", TT "H := Hom(M,N)", " is computed, enough information is stored in ", 
     TT "H.cache.Hom", " to compute this correspondence.",
     EXAMPLE {
	  "R = QQ[x,y,z]/(y^2-x^3)",
	  "H = Hom(ideal(x,y), R^1)",
	  "g = homomorphism H_{1}"
	  },
     "The homomorphism g takes x to y and y to x2.  The source and target are
     what they should be.",
     EXAMPLE {
	  "source g",
	  "target g"
	  },
     SeeAlso => Hom
     }
TEST ///
S = ZZ/101[a..d]
I = monomialCurveIdeal(S, {1,3,4})
R = S/I
use R
J = module ideal(a,d)
K = module ideal(b^2,c^2)
JK = Hom(J,K)
F = JK_{0}
F1 = homomorphism F
source F1
target F1
ker F1
prune coker F1
///
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
