--		Copyright 1993-1999 by Daniel R. Grayson


document { (symbol _, Matrix, List),
     Headline => "get some columns from a matrix",
     TT "f_{i,j,k,...}", " -- produce the submatrix of a matrix f consisting of 
     columns numbered i, j, k, ... .",
     PARA,
     "Repetitions of the indices are allowed.",
     PARA,
     "If the list of column indices is a permutation of 0 .. n-1, where n is
     the number of columns, then the result is the corresponding permutation
     of the columns of f.",
     PARA,
     EXAMPLE "R = ZZ/101[a..f];",
     EXAMPLE {
	  "p = matrix {{a,b,c},{d,e,f}}",
      	  "p_{1}",
      	  "p_{1,1,2}",
      	  "p_{2,1,0}",
	  },
     SEEALSO "_"
     }

document { (symbol ^,Matrix,List),
     Headline => "select some rows of a matrix",
     TT "f^{i,j,k,...}", " -- produce the submatrix of a matrix f consisting of 
     rows numbered i, j, k, ... .",
     PARA,
     "Repetitions of the indices are allowed.",
     PARA,
     "If the list of row indices is a permutation of 0 .. n-1, where n is
     the number of rows, then the result is the corresponding permutation
     of the rows of f.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[a..f]",
      	  "p = matrix {{a,b,c},{d,e,f}}",
      	  "p^{1}",
      	  "p^{1,0}",
	  },
     SEEALSO "^"
     }

     
document { submatrix,
     Headline => "select part of a matrix",
     TT "submatrix(m, rows, cols)", " -- yields a submatrix of the matrix ", TT "m", ".",
     BR,NOINDENT,
     TT "submatrix(m, cols)", " -- yields a submatrix of the matrix ", TT "m", ".",
     PARA,
     "Yields an r by c matrix, where r is the length of the list of integers
     ", TT "rows", ", and c is the length of the list of integers ", TT "cols", ".  
     The (i,j)-th entry of the result is m_(rows_i, cols_j).  If necessary, any
     sequences in the lists are spliced into the list.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[a .. o]",
      	  "m = genericMatrix(R, a, 3, 5)",
      	  "submatrix(m, {1,2,0}, {0..2, 4})",
	  },
     PARA,
     "If ", TT "rows", " or ", TT "cols", " is omitted, all the indices are used.",
     EXAMPLE "submatrix(m, {1,2}, )",
     PARA,
     "It is an error if any element of ", TT "rows", " or ", TT "cols", " is out 
     of range."
     }

document { (diff,Matrix),
     Headline => "differentiate a matrix",
     TT "diff m", " -- differentiate the matrix ", TT "m", " with respect to the variables
     of its ring."
     }

document { (diff,RingElement),
     Headline => "differentiate a ring element",
     TT "diff f", " -- differentiate the ring element ", TT "f", " with respect to 
     the variables of its ring."
     }

document { (diff,ProjectiveHilbertPolynomial,ZZ),
     TT "diff(P,i)", " -- compute the i-th difference polynomial"
     }

document { (diff,Matrix,Matrix),
     Headline => "differentiate a matrix by a matrix",
     Usage => {
	  TT "diff(m,n)", " -- differentiate ", TT "n", " with respect to ", TT "m"
	  },
     Synopsis => {
	  "h = diff(m,n)",
	  "m" => {"a map ", TT "m : F <--- P", " between free modules of ranks f and p."},
	  "n" => {"a map ", TT "n : G <--- Q", " between free modules of ranks g and q."},
	  "h" => {"a matrix with the shape ", TT "h : dual F ** G <--- dual P ** Q", ",
	       whose entry in the slot ", TT {"h", SUB "g*i+j,q*k+l"}, "
	       is the result of differentiating ", TT { "n", SUB "j,l" }, "
	       by the differential operator corresponding to ", TT {"m", SUB "i,k", "."}
	       },
	  },
     PARA,
     "If ", TT "m", " or ", TT "n", " is a ring element, then it is interpreted
     as a one-by-one matrix.  If ", TT "m", " is a vector, it is interpreted as
     a matrix with one column, and if ", TT "n", " is a vector, it is interpreted
     as a matrix with one row.  If both ", TT "m", " and ", TT "n", " are ring
     elements, then the result will be a ring element rather than a one-by-one
     matrix.  If ", TT "m", " is a vector and ", TT "n", " is a ring element,
     then the result will be a vector rather than a matrix with one column.",
     PARA,
     "The most common usage is when ", TT "m", " has one column and ", TT "n", "
     has one row.  In this case the result ", TT "h", " is a matrix whose
     ", TT "(i,j)", "-th entry is the result of differentiating ", TT {"n", SUB "j"}, "
     by the differential operator corresponding to ", TT {"m", SUB "i"}, ".",
     SEEALSO "diff and contract"
     }

document { (diff, ProjectiveHilbertPolynomial),
     TT "diff P", " -- compute the difference polynomial."
     }

document { diff,
     Headline => "differentiate"
     }

document { contract,
     Headline => "contract one matrix by another",
     }

document { (contract,Matrix,Matrix),
     Headline => "contract a matrix by a matrix",
     Usage => {
	  TT "contract(m,n)", " -- contract ", TT "n", " with respect to ", TT "m"
	  },
     Synopsis => {
	  "h = contract(m,n)",
	  "m" => {"a map ", TT "m : F <--- P", " between free modules of ranks f and p."},
	  "n" => {"a map ", TT "n : G <--- Q", " between free modules of ranks g and q."},
	  "h" => {"a matrix with the shape ", TT "h : dual F ** G <--- dual P ** Q", ",
	       whose entry in the slot ", TT {"h", SUB "g*i+j,q*k+l"}, "
	       is the result of contracting ", TT { "n", SUB "j,l" }, "
	       by ", TT {"m", SUB "i,k", "."}
	       },
	  },
     PARA,
     "This function is identical to ", TO (diff,Matrix,Matrix), ", except that 
     the multiplication by integers that occurs during differentiation is
     omitted.",
     PARA,
     SEEALSO "diff and contract"
     }

TEST "
R = ZZ/101[a..d]
I = monomialCurveIdeal(R,{1,3,4})
A = R/I
jacobian A
singA = minors(codim ideal presentation A, jacobian A)
generators gb singA
"
document { jacobian,
     Headline => "the Jacobian matrix of partial derivatives",
     TT "jacobian R", " -- calculates the Jacobian matrix of the ring R",
     BR,NOINDENT,
     TT "jacobian f", " -- calculates the Jacobian matrix of the matrix f,
     which will normally be a matrix with one row.",
     BR,NOINDENT,
     TT "jacobian I", " -- compute the matrix of derivatives of the 
     generators of I w.r.t. all of the variables",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[a..d];",
      	  "I = monomialCurveIdeal(R,{1,3,4})",
      	  "A = R/I",
      	  "jacobian A",
	  },
     "For a one row matrix, the derivatives w.r.t. all the variables
     is given",
     EXAMPLE {
	  "R = ZZ/101[a..c]",
      	  "p = symmetricPower(2,vars R)",
      	  "jacobian p",
	  },
     CAVEAT {
	  "If a matrix or ideal over a quotient polynomial ring ", TT "S/J", "
     	  is given, then only the derivatives of the given elements are
     	  computed and NOT the derivatives of elements of ", TT "J", "."
	  }
     }


document { leadTerm,
     Headline => "get the leading term",
     TT "leadTerm f", " -- return the leading term of the polynomial or 
     vector f.",
     BR, NOINDENT,
     TT "leadTerm m", " -- return the matrix of initial forms of 
     the columns of the matrix m.",
     BR, NOINDENT,
     TT "leadTerm(i,m)", " -- return the matrix of polynomials formed 
     by retaining those monomials of each entry which agree on the first i 
     weight vectors.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[a..d]",
      	  "leadTerm (3 + 8*a^2*b + 7*b*c^2)",
      	  "leadTerm matrix {{a,b},{c,d}}",
      	  "leadTerm matrix {{c,d},{a,b}}",
	  },
     SEEALSO {"leadCoefficient", "leadMonomial", "leadComponent"}
     }

document { borel,
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

document { inducedMap,
     Headline => "compute an induced map",
     TT "inducedMap(M,N,f)", " -- produce the map from ", TT "N", " to ", TT "M", " 
     induced by ", TT "f", ".",
     PARA,
     "Here ", TT "M", " should be a subquotient module of the target of ", TT "f", ", and
     ", TT "N", " should be a subquotient module of the source of ", TT "f", ".",
     PARA,
     SEEALSO "inducesWellDefinedMap"
     }

document { (inducedMap => Degree),
     Headline => "specify the degree of a map",
     TT "Degree => n", " -- an option to ", TO "inducedMap", " that provides the
     degree of the map produced."
     }

document { Verify,
     Headline => "verify that a map is well-defined",
     TT "Verify", " -- an option that can be used to request verification
     that a map is well defined.",
     PARA,
     MENU {
	  TO (inducedMap => Verify)
	  }
     }

document { (inducedMap => Verify),
     TT "Verify => true", " -- an option for ", TO "inducedMap", " which
     requests verification that the induced map produced is well defined."
     }


document { inducesWellDefinedMap,
     Headline => "whether a map is well defined",
     TT "inducesWellDefinedMap(M,N,f)", " -- tells whether the matrix ", TT "f", " would
     induce a well defined map from ", TT "N", " to ", TT "M", ".",
     SEEALSO "inducedMap"
     }

document { (map,Module,Module,Function),
     Synopsis => {
	  "h = map(M,N,f)",
	  "M" => {},
	  "N" => {},
	  "f" => {},
	  "h" => {"a map from the module ", TT "N", " to the module ", TT "M", " 
	       whose matrix entry ", TT "h_(i,j)", " is obtained from the
	       function ", TT "f", " by evaluating ", TT "f(i,j)", "."
	       }
	  },
     EXAMPLE {
	  "R = ZZ[a..c];",
	  "f = map(R^3,R^{0,-1,-2},(i,j) -> R_i^j)",
	  },
     "We specified the degrees of the source basis elements explicitly
     to ensure the matrix would be homogeneous.",
     EXAMPLE "isHomogeneous f",
     "We could have let Macaulay2 take care of that for us, by replacing
     the source module by its desired rank.",
     EXAMPLE {
	  "g = map(R^3,3,(i,j) -> R_i^j)",
	  "degrees g",
	  "isHomogeneous g"
	  },
     PARA,
     "Another way would be to let ", TO "matrix", " take care of that for
     us.",
     EXAMPLE {
	  "h = matrix table(3,3,(i,j) -> R_i^j)",
	  "degrees h",
	  "isHomogeneous h"
	  }
     }

document { matrix,
     Headline => "make a matrix",
     "This function can be used to create a matrix or map (homomorphism) between
     modules, but it is complicated because there are many different ways it can
     be used.  The entries of the matrix can be provided as a list of lists of ring
     elements, or as a function which accepts row and column indices.  The ring of
     the matrix can be provided explicitly, or the source and target modules can be 
     provided.  There are other alternatives.",
     PARA,
     SEEALSO {"map"}
     }



document { (map,Matrix),
     TT "map(f, Degree => d)", " -- make a map of degree d from a map f
     of modules by tensoring the source module with a free module of
     rank 1 and appropriate degree."
     }

document { (matrix,Matrix),
     TT "matrix f", " -- produce the matrix of a map f.",
     PARA,
     "If the source and target of f are free, then the result is
     f itself.  Otherwise, the source and target will be replaced by
     the free modules whose basis elements correspond to the generators
     of the modules.",
     SEEALSO {"map", "matrix"}
     }

document { (matrix,Ring,List),
     TT "matrix(R,v)", " -- create a matrix over R from a doubly-nested list of
     ring elements or matrices.",
     PARA,
     "This is essentially the same as ", TO (matrix,List), " together with
     the specification of the ring.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[a..f]",
      	  "matrix(R, {{a,b,0},{d,0,f}})",
	  },
     SEEALSO {"map", "matrix"}
     }

document { (map,Module,Module),
     TT "map(M,N)", " -- constructs the natural map from N to M.",
     PARA,
     "The modules ", TT "M", " and ", TT "N", " should be subquotient modules of the same
     free module",
     SEEALSO {"map", "isWellDefined"}
     }

document { (map,Module,Matrix),
     TT "map(M,p)", " -- recasts a matrix p to a map whose target is M by
     tensoring p with a graded free module of rank 1.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y]",
      	  "p = matrix{{x,y}}",
      	  "q = map(R^{3},p)",
      	  "degrees target q",
      	  "degrees source q",
	  },
     SEEALSO {"map", "matrix"}
     }

document { (map,Module,Module,List),
     TT "map(M,N,v)", " -- produces a map (matrix) from the module N
     to the module M whose entries are obtained from the doubly-nested list
     v of ring elements.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "p = map(R^2,R^{-2,-2},{{x^2,0},{0,y^2}})",
      	  "isHomogeneous p",
	  },
     SEEALSO {"map", "matrix"}
     }
document { (map,Module,Module,Matrix),
     TT "map(M,N,p)", " -- recasts the matrix p as a map (matrix) from
     the module N to the module M.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
      	  "p = matrix {{x,y,z}}",
      	  "q = map(R^1,R^3,p)",
      	  "degrees source p",
      	  "degrees source q",
	  },
     SEEALSO {"map", "matrix"}
     }
document { (map,Module,Module,RingElement),
     TT "map(M,N,r)", " -- construct a map from a module ", TT "N", " to ", TT "M", " which is provided
     by the ring element ", TT "r", ".",
     PARA,
     "If ", TT "r", " is nonzero, then ", TT "M", " and ", TT "N", " should be equal, 
     or at least have the same number of generators.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x]",
      	  "map(R^2,R^3,0)",
      	  "map(R^2,R^2,x)",
      	  "q = map(R^2,R^2,x,Degree=>1)",
      	  "isHomogeneous q",
	  },
     PARA,
     SEEALSO {(map,Module,Module,ZZ), "map", "matrix"}
     }
document { (map,Module,Module,ZZ),
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
     EXAMPLE {
	  "R = QQ[x,y];",
	  "M = image vars R",
	  "N = coker presentation M",
	  "f = map(M,N,1)",
	  "isWellDefined f",
	  "isIsomorphism f",
	  "g = map(M,cover M,1)",
	  "isWellDefined g",
	  "isIsomorphism g",
	  "h = map(cover M,M,1)",
	  "isWellDefined h",
	  },
     PARA,
     SEEALSO {(map,Module,Module,RingElement), "map", "matrix"}
     }
document { (map,Module),
     TT "map M", " -- construct the identity map from M to itself.",
     PARA,
     "This can also be accomplished with ", TT "id_M", " or ", TT "map(M,1)", ".",
     SEEALSO {"map", "id"}
     }
document { (map,Module,RingElement),
     TT "map(M,r)", " -- construct the map from M to itself which is provided
     by scalar multiplication by the ring element r.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x]",
      	  "map(R^2,x)",
	  },
     SEEALSO {"map", "matrix"}
     }
document { Degree,
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
     SEEALSO {"map", "matrix", (inducedMap => Degree)}
     }

document { (map,Module,ZZ,Function),
     TT "map(M,n,f)", " -- construct a map from a free graded module of
     rank n to M whose entries are obtained from the function f by 
     evaluating f(i,j).",
     PARA,
     "The degrees of the basis elements of the source module are chosen
     in an attempt to ensure that the resulting map is homogeneous of
     degree zero."
     }

document { (map,Module,ZZ,List),
     TT "map(M,n,v)", " -- construct a map from a free graded module of
     rank n to M whose entries are in the doubly nested list ", TT "v", ".",
     PARA,
     "The degrees of the basis elements of the source module are chosen
     in an attempt to ensure that the resulting map is homogeneous of
     degree zero."
     }

document { (map,Module,Nothing,List),
     TT "map(M,,v)", " -- construct a map from a free graded module to M
     whose entries are obtained from the doubly-nested list v of
     ring elements.",
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
     SEEALSO {"map", "matrix"}
     }
document { (matrix,List),
     TT "matrix v", " -- create a matrix from a doubly-nested list of
     ring elements or matrices, or from a list of (column) vectors.",
     PARA,
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
     SEEALSO {"map", "matrix"}
     }

document { id,
     Headline => "identity map",
     TT "id_M", " -- the identity homomorphism from ", TT "M", " to ", TT "M", "."
     }

document { reshape,
     Headline => "reshape a matrix",
     TT "reshape(F,G,m)", " -- reshapes the matrix ", TT "m", " to give
     a map from ", TT "G", " to ", TT "F", ".",
     PARA,
     "It yields the matrix obtained from ", TT "m", " of shape F <--- G, by
     taking elements from the first row of ", TT "m", ", then the second, and
     so on, filling them into the result row by row.  Currently, it is assumed
     that ", TT "m", " and the result both have the same number of entries.
     The resulting map is always of degree zero."
     }

TEST "
R=ZZ/101[a..d]
f = matrix {{a}}
assert( isHomogeneous f )

g = reshape(R^1, R^{-1}, f)
assert isHomogeneous g
"

document { adjoint1,
     Headline => "the adjoint map",
     TT "adjoint1 (f,G,H)", " -- if f is a homomorphism of free modules of the
     form F -> G ** H, then produce the adjoint homomorphism of the
     form F ** (dual G) -> H.",
     SEEALSO "adjoint"
     }

document { adjoint,
     Headline => "the adjoint map",
     TT "adjoint (f,F,G)", " -- if f is a homomorphism of free modules of the
     form F ** G -> H, then produce the adjoint homomorphism of the
     form F -> (dual G) ** H.",
     SEEALSO "adjoint1"
     }

document { (flip,Module,Module),
     Headline => "matrix of commutativity of tensor product",
     TT "flip(F,G)", " -- yields the matrix representing the map F ** G --> G ** F."
     }

document { subquotient,
     Headline => "make a subquotient module",
     Usage => {
	  TT "subquotient(g,r)", " -- the image of ", TT "g", " in the cokernel of ", TT "r", "."
	  },
     TT "subquotient(g,r)", " -- given matrices ", TT "g", " and ", TT "r", " with the same target, 
     produces a new module representing the image of ", TT "g", " in the cokernel
     of ", TT "r", ".",
     PARA,
     "The columns of ", TT "g", " are called the generators, and the columns of
     ", TT "r", " are the relations.",
     PARA,
     "Functions:",
     MENU {
	  TO "generators",
	  TO "relations",
	  TO "prune"
	  },
     "This is the general form in which modules are represented, and
     subquotient modules are often returned as values of computations.",
     EXAMPLE {
	  "R = ZZ/101[a..d]",
      	  "M = kernel vars R ++ cokernel vars R",
      	  "generators M",
      	  "relations M",
      	  "prune M",
	  },
     SEEALSO {"generators", "relations"}
     }

document { (symbol **, Matrix, Matrix),
     Headline => "tensor product of matrices",
     TT "f ** g", " -- computes the tensor product of two matrices.",
     PARA,
     SEEALSO "Matrix"
     }

TEST "
ZZ[t]
assert (matrix {{t}} ** matrix {{t}} == matrix{{t^2}})
"

document { symbol "compactMatrixForm",
     Headline => "global flag for compact printing",
     TT "compactMatrixForm", " -- a global flag which specifies whether to display
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

document { (symbol +, Ideal, Ideal), 
     Headline => "sum of ideals",
     TT "I + J", " -- the sum of two ideals."
     }

TEST "
R = ZZ/101[x,y,z]
I = ideal(x,y)
assert( 1 == dim I )
assert( 2 == codim I )
"

document { Ideal,
     Headline => "the class of all ideals",
     SEEALSO "ideals",
     "Common ways to make an ideal:",
     MENU {
	  TO "ideal",
	  TO "annihilator",
	  TO "content",
	  TO "fittingIdeal",
	  SHIELD TO (kernel, RingMap),
	  },
     "Common ways to get information about an ideal:",
     MENU {
	  TO (codim, Ideal),
	  TO (dim, Ideal),
	  TO (generators, Ideal),
	  TO (isSubset, Ideal, Ideal),
	  },
     "Common operations on ideals:",
     MENU {
	  TO (symbol +,Ideal,Ideal),
	  TO (symbol *,Ideal,Ideal),
	  TO (symbol ==,Ideal,Ideal),
	  TO (symbol ==,Ideal,ZZ),
	  TO (symbol :,Ideal,Ideal),
	  TO (symbol ^,Ideal,ZZ),
	  TO (decompose, Ideal),
	  TO (gb, Ideal),
	  TO (radical, Ideal),
	  TO "saturate",
	  TO "top",
	  TO (trim, Ideal),
	  },
     "Common ways to use an ideal:",
     MENU {
	  SHIELD TO (symbol /, Ring, Ideal),
	  },
     PARA,
     "An ideal ", TT "I", " is an immutable object, so if you want to 
     cache information about it, put it in the hash table ", TT "I.cache", ".",
     PARA
     }

document { (symbol *,Ideal,Ideal),
     Headline => "product of ideals",
     TT "I * J", " -- the product of two ideals."
     }

document { (symbol ^,Ideal,ZZ),
     Headline => "power of an ideal",
     TT "I^n", " -- the n-th power of an ideal I."
     }

document { (module, Ideal),
     Headline => "turn an ideal into a module",
     TT "module I", " -- produce the submodule of ", TT "R^1", " corresponding to an
     ideal ", TT "I", " in a ring ", TT "R", "."
     }

document { (ideal, Ring),
     Headline => "get the ideal used to form a quotient ring",
     TT "ideal R", " -- for a quotient ring R=A/I, returns I"
     }

document { ideal,
     Headline => "make an ideal",
     "ideal v", " -- produces the ideal spanned by a list or sequence of ring
     elements.",
     PARA,
     EXAMPLE {
	  "ZZ[a..i]",
      	  "ideal (c..h)"
	  },
     }

document { symbol "ker",
     Headline => "kernel of a map",
     "See ", TO "kernel", "."
     }

document { kernel,
     Headline => "kernel of a map",
     TT "kernel f", " -- produces the kernel of a matrix or ring homomorphism.",
     PARA,
     "If ", TT "f", " is a ring element, it will be interpreted as a one by one
     matrix.",
     PARA,
     "For an abbreviation, use ", TO "ker", "."
     }

document { SubringLimit,
     Headline => "stop after finding enough elements of a subring",
     TT "SubringLimit", " -- an option for  ", TO "kernel", " and ", TO "gb", "
     which can stop the computation after a certain number of basis elements in
     a subring have been found.",
     SEEALSO "computing Groebner bases"
     }


document { kernel => SubringLimit,
     TT "SubringLimit => n", " -- an option for ", TO "kernel", " which
     causes the computation of the kernel of a ring map to stop after ", TT "n", "
     elements have been discovered."
     }

document { (homology,Matrix,Matrix),
     Headline => "kernel modulo image",
     TT "homology(g,f)", " -- computes the homology module ", TT "ker g/im f", ".",
     PARA,
     "Here ", TT "g", " and ", TT "f", " should be composable maps with ", TT "g*f", "
     equal to zero.",
     SEEALSO "homology"
     }

document { (dual, Matrix),
     Headline => "dual of a map",
     TT "dual f", " -- the dual (transpose) of a homomorphism."
     }

document { singularLocus,
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

document { (symbol ^,Matrix,Array),
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
     SEEALSO {submatrix, (symbol ^,Module,Array), (symbol _,Matrix,Array)}
     }

document { (symbol _, Matrix, Array),
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
     SEEALSO {submatrix, (symbol _,Module,Array), (symbol ^,Matrix,Array)}
     }

document { entries,
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
-- gbTrace 3
-- << \"gb ff ...\" << flush
G = gb ff
pM = poincare M
MM = cokernel fff
MM.poincare = pM
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

document { isInjective,
     Headline => "whether a map is injective",
     SEEALSO "isSurjective"
     }

document { isSurjective,
     Headline => "whether a map is surjective",
     SEEALSO "isInjective"
     }

TEST "
R = ZZ/101[a]
assert isInjective R^2_{0}
assert not isInjective R^2_{0,0}
assert isSurjective R^2_{0,0,1}
assert not isSurjective R^2_{1}
"


document { content,
     Headline => "the content of a polynomial",
     TT "content f", " -- returns the content of a matrix or polynomial.",
     PARA,
     "The content is the ideal of the base ring generated by the 
     coefficients."
     }

document { QuotientRing,
     Headline => "the class of all quotient rings"
     }

document { isQuotientOf,
     Headline => "whether one thing is a quotient of another"
     }

document { isQuotientRing,
     Headline => "whether something is a quotient ring"
     }

TEST "
assert( ZZ/2 === ZZ/(4,6) )
R = ZZ/101[t]
"

document { (symbol /, Ring, RingElement),
     Headline => "quotient ring",
     Synopsis => {
	  "S = R/f",
	  "R" => null,
	  "f" => { "an element of ", TT "R", "" },
	  "S" => { "the quotient ring ", TT "R/Rf", "."}
	  }
     }

document { (symbol /, Ring, Sequence),
     Headline => "quotient ring",
     Synopsis => {
	  "S = R/(f,g,h,...)",
	  "R" => null,
	  "(f,g,h,...)" => { "a sequence of elements of ", TT "R", "" },
	  "S" => { "the quotient ring ", TT "R/(Rf+Rg+Rh+...)", "."}
	  }
     }

document { (symbol /, Ring, Ideal),
     Headline => "quotient ring",
     Synopsis => {
	  "S = R/I",
	  "R" => null,
	  "I" => { "an ideal of ", TT "R", "" },
	  "S" => "the quotient ring"
	  },
     "The names of the variables are assigned values in the new quotient ring
     by automatically running ", TT "use R", ", unless ", TT "R", " has a name,
     or one of the rings ", TT "R", " is a quotient ring of has a name.
     See: ", TO "use", ".",
     PARA,
     "Warning: quotient rings are bulky objects, because they contain 
     a Groebner basis for their ideals, so only quotients of ", TT "ZZ", " 
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

document { koszul,
     Headline => "a differential in a Koszul complex"
     }

document { (koszul,ZZ,Matrix),
     Usage => { TT "koszul(i,f)", " -- provides the ", TT "i", "-th symmetric 
	  power of the matrix ", TT "f", "."},
     Synopsis => {
	  "g = koszul(i,f)",
	  "i" => null,
	  "f" => null,
	  "g" => { "the ", TT "i", "-th differential in the Koszul complex of the
	       map ", TT "f", "."}
	  },
     PARA,
     "Here ", TT "f", " should be a ", TT "1", " by ", TT "n", " matrix."
     }

document { symmetricPower,
     Headline => "symmetric power"
     }

document { (symmetricPower,ZZ,Matrix),
     Usage => { TT "symmetricPower(i,f)", " -- provides the ", TT "i", "-th symmetric 
	  power of the matrix ", TT "f", "."},
     Synopsis => {
	  "g = symmetricPower(i,f)",
	  "i" => null,
	  "f" => null,
	  "g" => { "the ", TT "i", "-th symmetric power of the matrix ", TT "f", "."}
	  },
     PARA,
     "Note: in the current implementation, ", TT "f", " should have just one row."
     }

document { (exteriorPower,ZZ,Matrix),
     Usage => { TT "exteriorPower(p,f)", " -- provides the ", TT "p", "-th exterior 
	  power of the matrix ", TT "f", "."},
     Synopsis => {
	  "g = exteriorPower(p,f)",
	  "p" => null,
	  "f" => null,
	  "g" => { "the ", TT "p", "-th exterior power of the matrix ", TT "f", "."}
	  },
     "Note: we may write ", TT "exteriorPower_p f", " instead of
     ", TT "exteriorPower(p,f)", "; see ", TO (symbol _,Function,Thing), ".",
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
	  }
     }

document { (symbol _,Function,Thing),
     Headline => "attach the first argument to a function of two or more arguments",
     Synopsis => {
	  "g = f_x",
	  "f" => "a function of two or more arguments",
	  "x" => "anything",
	  "g" => {
	       "a new function with the property that ", TT "g(y)", "
	       returns the value of  ", TT "f(x,y)", ", that
	       ", TT "g(y,z)", " returns the value of ", TT "f(x,y,z)", ", and
	       so on."
	       }
	  },
     "This abbreviation allows us to save a bit of typing, and in some
     cases, agrees with standard mathematical notation.",
     EXAMPLE {
	  "R = ZZ[a .. i];",
	  "f = genericMatrix(R,a,3,3)",
	  "exteriorPower(2,f)",
	  "exteriorPower_2 f",
	  "p = prepend_7",
	  "p {8,9,10}"
	  },
     SEEALSO { "splice" }
     }

document { MinorsComputation,
     Headline => "the class of all minors computations",
     TT "MinorsComputation", " -- a type of self initializing list used
     internally by ", TO "minors", "."
     }

document { PfaffiansComputation,
     Headline => "the class of all Pfaffians computations",
     TT "PfaffiansComputation", " -- a type of self initializing list used
     internally by ", TO "pfaffians", "."
     }

document { wedgeProduct,
     Headline => "the exterior multiplication map",
     TT "wedgeProduct(p,q,M)", " -- returns the matrix which represents the
     multiplication map from ", TT "exteriorPower(p,M) ** exteriorPower(q,M)", "
     to ", TT "exteriorPower(p+q,M)", ".",
     PARA,
     "Here ", TT "M", " is free module."
     }

document { exteriorPower,
     Headline => "exterior power",
     TT "exteriorPower(i,M)", " -- the i-th exterior power of a module ", TT "M", ".",
     BR,NOINDENT,
     TT "exteriorPower(i,f)", " -- the i-th exterior power of a matrix ", TT "f", ".",
     PARA,
     "The rows and columns are indexed in the same order as that used by
     ", TO "subsets", " when listing the subsets of a set.",
     PARA,
     "When ", TT "i", " is ", TT "1", ", then the result is equal to ", TT "M", ".",
     PARA,
     "When M is not a free module, then the generators used for the result
     will be wedges of the generators of M.  In other words, the modules
     ", TT "cover exteriorPower(i,M)", " and ", TT "exteriorPower(i,cover M)", " 
     will be equal.",
     PARA,
     SEEALSO {"determinants", "minors", "det", "wedgeProduct"}
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


document { minors,
     Headline => "ideal generated by minors",
     TT "minors(p,m)", " -- produces the ideal generated by
     the determinants of the p-by-p submatrices of the matrix ", TT "m", ".",
     PARA,
     SEEALSO {"determinants", "det", "exteriorPower"}
     }

document { (minors,ZZ,Matrix),
     Headline => "ideal generated by minors",
     Usage => {
	  TT "minors(p,m)", " -- ideal of ", TT "p", " by ", TT "p", " minors of the matrix ", TT "m"
	  },
     Synopsis => {
	  "J = minors(p,m)",
	  "p" => {"the size of minors to compute"},
	  "m" => {"a matrix between free modules"},
	  "J" => {"the ideal generated by the ", TT "p", " by ", TT "p", " minors of the matrix ", 
	       TT "m", "."},
	  },
     PARA,
     "Minors are generated in the same order as that used by ", TO (subsets,ZZ,ZZ), ".",
     EXAMPLE {
	  "R = ZZ[vars(0..11)];",
	  "M = genericMatrix(R,a,4,3)",
	  "transpose generators minors(2,M)",
	  "subsets(4,2)"
	  },
     "The order is {{0,1},{0,1}}, {{0,2},{0,1}}, {{1,2},{0,1}}, and so on.",
     PARA,
     "If the ", TO (minors=>First), " option is not given, the minors are stashed 
     in the matrix under the key ", TT "m.cache#MinorsComputation{j}", ".  The class of
     this stashed object is the internal class ", TO "MinorsComputation", "."
     }


document { symbol Bareiss,
     "A keyword used by ", TO "det", ", ", TO "minors", ", and ", TO "exteriorPower", "."
     }
document { symbol Cofactor,
     "A keyword used by ", TO "det", ", ", TO "minors", ", and ", TO "exteriorPower", "."
     }

apply({minors => Strategy, det => Strategy, exteriorPower => Strategy}, node -> 
  document { node,
     Headline => "choose between Bareiss and Cofactor algorithms",
     TT "Strategy => Bareiss", " -- use Bareiss' fraction free determinant algorithm",
     BR,
     TT "Strategy => Cofactor", " -- use cofactor expansion to compute determinants",
     PARA,
     "The base ring determines the default strategy.  If the base ring is not a quotient
     polynomial ring, the ", TO "Bareiss", " algorithm is used.  If the polynomial ring is
     a quotient, and the ring has not been declared to be a field, (see ", TO "toField", "), then
     the ", TO "Cofactor", " algorithm is used.",
     CAVEAT "The Bareiss algorithm only returns
     a ring element which differs from the actual determinant by a zero divisor in the ring.  Thus, 
     an INCORRECT answer may be computed if the ring contains zero divisors.",
     PARA,
     "For an example, see ", TO "determinants", "."
     }
  )

document { minors => First,
     Headline => "set the first minor to compute",
     TT "First => {rows, columns}", " -- set the first minor to compute",
     PARA,
     "Minors are generated in the same order as that used by ", TO (subsets,ZZ,ZZ), ". See ",
     TO (minors,ZZ,Matrix), ".",
     EXAMPLE {
	  "R = ZZ[a..f];",
	  "M = matrix{{a,b,c},{d,e,f}}",
	  "minors(2,M,First=>{{0,1},{0,2}})",
	  "minors(2,M,First=>{{0,1},{0,2}},Limit=>1)",
	  },
     SEEALSO {"determinants", minors, (minors=>Limit)}
     }

document { minors => Limit,
     TT "Limit => n", " -- the number of minors to compute",
     "For an example, see ", TO "determinants", ".",
     SEEALSO (minors=>First)
     }

TEST "
-- For more determinant tests, see Macaulay2/test/testdet.m2
R = ZZ/103[a,b,c,d]
h = matrix {{a,b},{c,d}}
assert( det h == a * d - b * c )
assert( minors(1,h) == image matrix {{a,b,c,d}} )
assert( minors(2,h) == image matrix {{a * d - b * c}} )
"

document { pfaffians,
     Headline => "ideal generated by Pfaffians",
     TT "pfaffians(n,f)", " -- given a skew symmetric matrix f, produce the 
     ideal generated by its ", TT "n", " by ", TT "n", " Pfaffians.",
     PARA,
     EXAMPLE {
	  "R=ZZ/101[a..f]",
      	  "m=genericSkewMatrix(R,a,4)",
      	  "pfaffians(2,m)",
      	  "pfaffians(4,m)",
	  },
     PARA,
     SEEALSO "PfaffiansComputation"
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

document { trace,
     Headline => "trace of a matrix",
     TT "trace f", " -- returns the trace of the matrix f.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[a..d]",
      	  "p = matrix {{a,b},{c,d}}",
      	  "trace p"
	  },
     }

document { det,
     Headline => "determinant of a matrix",
     SEEALSO {"determinants", "minors", "exteriorPower"}
     }

document { (det,Matrix),
     Headline => "determinant of a matrix",
     Usage => {TT "det f", " -- compute the determinant of the matrix ", TT "f", "."},
     Synopsis => {
	  "d = det f",
	  "f" => { "a square matrix"},
	  "d" => { "the determinant of ", TT "f"}
	  },
     PARA,
     EXAMPLE {
	  "R = QQ[a..d]",
      	  "p = matrix {{a,b},{c,d}}",
      	  "det p"
	  },
     }

     
document { Limit,
     Headline => "specify how many to compute",
     TT "Limit => n", " -- an optional argument for ", TO "pfaffians", "
     or for ", TO "minors", " specifying that the computation should stop 
     after ", TT "n", " more elements are computed."
     }

document { fittingIdeal,
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
document { genericMatrix,
     Headline => "make a generic matrix of variables",
     TT "genericMatrix(R,x,m,n)", " -- produce an m by n matrix of variables drawn
     from the ring R, starting with variable x.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[a..d]",
      	  "genericMatrix(R,a,2,2)"
	  },
     }

document { genericSkewMatrix,
     Headline => "make a generic skew symmetric matrix",
     TT "genericSkewMatrix(R,x,n)", " -- make a skew symmetric n by n 
     matrix whose entries above the diagonal are the variables of R, starting 
     with the variable x."
     }

document { genericSymmetricMatrix,
     Headline => "make a generic symmetric matrix",
     TT "genericSymmetricMatrix(R,x,n)", " -- make a symmetric n by n matrix 
     whose entries on and above the diagonal are the variables of R, starting 
     with the variable x."
     }

document { (symbol +, Module, Module),
     Headline => "sum of submodules",
     TT "M + N", " -- the sum of two submodules.",
     PARA,
     "The two modules should be submodules of the same module."
     }

document { (symbol **, Module, Module),
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

document { (symbol **, Matrix, Module),
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
     SEEALSO {"Matrix", "Module"}
     }

document { (symbol **, Module, Ring),
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

document { (symbol **, Matrix, Ring),
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

document { poincareComputation,
     Headline => "store the Poincare polynomial computation",
     TT "poincareComputation", " -- a key used in a module or monomial
     ideal to store a computation of Poincare polynomial.",
     PARA,
     SEEALSO {"poincare"}
     }

document { hilbertFunction,
     Headline => "Hilbert function of a module",
     TT "hilbertFunction(d,M)", " -- compute the dimension of the degree d
     part of the module, ring, or ideal M",
     PARA,
     "At the moment, the function is computed simply by calling ", TO "basis", "
     and extracting the number of basis elements.",
     SEEALSO {"hilbertSeries", "hilbertPolynomial"}
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

document { Order,
     Headline => "specify the order of a Hilbert series required",
     TT "Order", " -- an optional argument used with ", TO "hilbertSeries", "
     to specify the order of the series requested."
     }

document { hilbertSeries,
     Headline => "compute Hilbert series",
     TT "hilbertSeries M", " -- compute the Hilbert series of the ring or
     module ", TT "M", ".",
     PARA,
     "The Hilbert series is the formal power series in the variables of the
     degrees ring whose coefficients are the dimensions of the corresponding
     graded component.  The series is provided as an ", TO "Expression", "
     representing a rational function with that series.",
     PARA,
     "If an optional integer argument labelled ", TO "Order", " is used, then
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
     SEEALSO {"degreesRing", "Order"}
     }

document { ProjectiveHilbertPolynomial,
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

document { (symbol " ", ProjectiveHilbertPolynomial, ZZ),
     Headline => "value of polynomial",
     TT "P i", " -- the value of a projective Hilbert polynomial ", TT "P", " at 
     an integer ", TT "i", ".",
     PARA,
     EXAMPLE {
	  "P = projectiveHilbertPolynomial 2",
      	  "apply(0 .. 12, i -> P i)",
	  },
     SEEALSO ProjectiveHilbertPolynomial
     }

document { projectiveHilbertPolynomial,
     Headline => "Hilbert polynomial of projective space",
     TT "projectiveHilbertPolynomial n", " -- produces the projective
     Hilbert polynomial corresponding to projective space of dimension n.",
     BR,NOINDENT,
     TT "projectiveHilbertPolynomial(n,d)", " -- produces the projective
     Hilbert polynomial corresponding to the graded ring of projective space
     of dimension n, but with its generator in degree -d.",
     PARA,
     SEEALSO "ProjectiveHilbertPolynomial"
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

document { Projective,
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
     SEEALSO "ProjectiveHilbertPolynomial"
     }

document { hilbertPolynomial,
     Headline => "compute Hilbert polynomial",
     TT "hilbertPolynomial M", " -- the Hilbert polynomial of the module ", TT "M", " as
     a polynomial in ", TT "T", "."
     }

document { codim,
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

document { (codim, Module),
     Synopsis => {
	  "c = codim M",
	  "M" => {"A module over a ring ", TT "R", "."},
	  "c" => {"The number ", TT "dim R - dim M", "."}
	  },
     PARA,
     "The returned value is the usual codimension if R is an integral domain, or all
     components have the same dimension.",
     EXAMPLE {
	  "R = ZZ/101[a..d];",
	  "M = coker matrix{{a,b},{c,d}}",
	  "codim M"
	  },
     CAVEAT ("If R is ZZ, then the computation effectively tensors first with the
	  rational numbers, yielding the wrong answers in some cases.",
	  "R must be a quotient of a commutative polynomial ring.")
     }
document { dim,
     Headline => "calculate the dimension",
     TT "dim M", " -- calculate the dimension of the support of a module M.",
     BR,NOINDENT,
     TT "dim R", " -- calculate the dimension of a ring R.",
     BR,NOINDENT,
     TT "dim I", " -- calculate the dimension of the quotient ring R/I.",
     BR,NOINDENT,
     TT "dim r", " -- calculate the dimension of the virtual representation
     corresponding to an element of a Schur ring.",
     PARA,
     "Warning: if you want the dimension of a vector space, you should use
     ", TO "rank", ".",
     PARA,
     "Warning: over the integers, the computation effectively tensors first with the
     rational numbers, yielding the wrong answer in some cases.",
     PARA,
     SEEALSO {"Schur"}
     }

document { presentation,
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
     SEEALSO {"cover"}
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


document { prune,
     Headline => "minimize generators and relations"
     }

document { (prune, Matrix),
     Synopsis => {
	  "h = prune f",
	  "f" => null,
	  "h" => { "the map corresponding to f obtained by pruning its source
	       and target"
	       }
	  },
     SEEALSO {"presentation", "trim"}
     }

document { (prune, Module),
     Synopsis => {
	  "N = prune M",
	  "M" => null,
	  "N" => { "a module isomorphic to ", TT "M", " with a minimal number of
	       generators and relations."
	       }
	  },
     PARA,
     "The isomorphism from ", TT "N", " to ", TT "M", " can 
     be obtained with as ", TT "g = N.pruningMap", " unless ", TT "M.pruningMap", "
     already exists, in which case ", TT "N", " is the same as ", TT "M", ".  You may obtain 
     the inverse isomorphism with ", TT "g^-1", ".",
     SEEALSO { "pruningMap" }
     }

document { pruningMap,
     Headline => "store the isomorphism obtained by pruning",
     TT "pruningMap", " -- the key under which is stored the isomorphism to
     a module ", TT "M", " from the module ", TT "prune M", ".",
     PARA,
     "This map exists only after ", TT "N = prune M", " has been executed
     at least once, and then the map can be obtained with ", TT "N.pruningMap", ".",
     SEEALSO "prune"
     }

document { dual,
     Headline => "dual module or map",
     TT "dual M", " -- the dual.",
     PARA,
     "For details, see one of the following.",
     MENU {
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
p = N.pruningMap

assert( source p == N )
assert( target p == M )
assert( prune kernel p == 0 )
assert( prune cokernel p == 0 )
assert isIsomorphism p
assert isIsomorphism p^-1
assert ( p * p^-1 == id_M )
assert ( p^-1 * p == id_N )
"

document { (dual, Module),
     Headline => "dual module",
     TT "dual M", " -- the dual of a module."
     }

document { homomorphism,
     Headline => "get the homomorphism from element of Hom",
     TT "homomorphism f", " -- finds the matrix ", TT "M <-- N", " corresponding to the 
     element ", TT "f", ".",
     PARA,
     "This element should be a matrix ", TT "f : Hom(M,N) <--- R^1", ", where ", TT "Hom(M,N)", " 
     has been previously computed, and ", TT "R", " is the ring of ", TT "M", " and ", TT "N", ".",
     PARA,
     "When ", TT "A := Hom(M,N)", " is computed, enough information is stored in ", TT "A.Hom", "
     to compute this correspondence.",
     PARA,
     SEEALSO "Hom"
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
