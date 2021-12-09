-- Overview: matrices

document {
     Key => "matrices",
     HEADER2 "An overview",     
     "In Macaulay2, each matrix is defined over a ring, (see ", TO "rings", "). 
     Matrices are perhaps the most common data type in Macaulay2.",
     Subnodes => {
	  "making matrices", 
	  TO "inputting a matrix",
   	  TO "projections, inclusions, and permutations",
	  TO "random and generic matrices",
	  "operations involving matrices",
	  TO "extracting information about a matrix",
	  TO "basic arithmetic of matrices",
	  TO "concatenating matrices",
 	  -- Mike wanted this: TO "submatrices",
	  TO "differentiation",
	  "determinants and related computations",
	  TO "rank of a matrix",
	  TO "determinants and minors",
	  TO "Pfaffians",
	  TO "exterior power of a matrix",
	  "display of matrices and saving matrices to a file",
	  TO "format and display of matrices in Macaulay2",
	  TO "importing and exporting matrices",
	  },
     PARA{},
     -- Mike wanted this: "For an overview of matrices as homomorphisms between modules, see ", TO "homomorphisms of modules", ".  ",
     "For additional common operations and a comprehensive list of all routines
     in Macaulay2 which return or use matrices, see ", TO "Matrix", "."
     }

document {
     Key => "inputting a matrix",
     
     SUBSECTION "by its entries",
       "Using the function ", TO "matrix", " is the most basic method 
       for inputting a matrix.  The entries are typed in by rows.",
       EXAMPLE {
	    "R = ZZ/5[s..z];",
	    "M = matrix {{ x^2+y, z^3}, {y^3-z,3*z-6*x-5*y}}"
	    },

     SUBSECTION "by a function",  
       "The function ", TO map, " can be used to construct 
       matrices. ", 
       EXAMPLE {
	    "G = map(R^3,3,(i,j)->R_i^j)",
	    "f = 3*s^2*v-t*u*v+s*t^2",
	    "H = map(R^4,R^4,(i,j)->diff(R_j*R_i,f))"
	    },

     SUBSECTION "identity matrix",
       "The function ", TO id, " is used to form the identity matrix 
       as a map from a module to itself.  ",
       EXAMPLE {
	    "id_(R^3)",
	    "id_(source M)"
	    },
       "The first example gives a 3x3 identity matrix with entries 
       in the ring R.  The second gives a 2x2 identity matrix whose
       source and target are the (graded) source of the matrix ", TT "M", "."
     }

document {
     Key => "projections, inclusions, and permutations",
     -- F_List, F^List
     }

document {
     Key => "random and generic matrices",
     
     SUBSECTION "random matrices",
       "To construct a random m by n matrix with entries in a ring 
       R use the function ", TO "random", " by 
       typing ", TT "random(R^m,R^n)", ".",
       EXAMPLE {
	    "R = GF(3^2,Variable => a);",
	    "random(R^3,R^4)"
	    },
       "Over a polynomial ring, this will select elements in the base 
       ring or field. To obtain a matrix of (say) linear polynomials, use",
       EXAMPLE {
	    "T = R[x,y];",
	    "random(T^3,T^{4:-1})"
	    },

     SUBSECTION "matrices of variables",
       "To build an m by n matrix of variables drawn from the ring R, 
       use ", TO "genericMatrix", ".  The syntax 
       is ", TT "genericMatrix(R,x,m,n)", " where R is the ring, x is 
       the variable where we start and m and n specify the size of 
       the matrix.",
       EXAMPLE {
	    "S = R[p..z];",
	    "genericMatrix(S,t,3,2)",
	    },
       "Note that to use the function genericMatrix the number of 
       variables in the ring R must be at least as large as ", 
       TT "m*n", ".",

     SUBSECTION "genericSymmetricMatrix",
       "To construct an n by n symmetric matrix whose entries on 
       and above the diagonal are the variables of R use ", 
       TO "genericSymmetricMatrix", ".  
       The syntax is ", TT "genericSymmetricMatrix(R,x,n)", " where R is 
       the ring, x is the variable you want to start with and n is the 
       size of the matrix.",
       EXAMPLE { 
	    "genericSymmetricMatrix(S,s,3)"
	    },

     SUBSECTION "genericSkewMatrix",
       "To construct an n by n skew symmetric matrix whose 
       entries above the diagonal are the variables of R 
       use ", TO "genericSkewMatrix", ".  The syntax 
       is ", TT "genericSkewMatrix(R,x,n)", " where R is the 
       ring, x is the variable you want to start with and n is the 
       size of the matrix.",
       EXAMPLE { 
	    "genericSkewMatrix(S,u,3)"
	    }     
     }

document {
     Key => "extracting information about a matrix",
     "Consider the ring ", TT "R", " and the matrix ", TT "f", ".",
     EXAMPLE {
	  "R = QQ[x,y,z];",
	  "f = matrix{{2,x,y,x^2},{z,32,2,x}}"
	  },

     SUBSECTION "target",
       "From the above output, one sees that Macaulay2 considers ", 
       TT "f", " as a linear transformation. Use the ", 
       TO "target", " command to obtain the target of the 
       linear transformation ", TT "f", ".",
       EXAMPLE {
	    "target f"
	    },     

     SUBSECTION "source",
       "Likewise, to obtain the source of our linear transformation,
       use the ", TO "source", " command.",
       EXAMPLE {
	    "source f"
	    },

     SUBSECTION "number of rows or columns",
       "Use ", TO "numgens", " to obtain the rank of a free module. Combining 
       it with the commands ", TO "target", " or ", TO "source", 
       " gives us a way to determine the number of rows or columns 
       of a matrix ", TT "f", ".",
       EXAMPLE {
	    "numgens target f",
	    "numgens source f"
	    },

     SUBSECTION "extracting an element from a matrix",
       "To extract the ", TT "(i,j)", "-th element of a matrix, type ", 
       TT "f_(i,j)", ".", 
       EXAMPLE {
	    "f_(1,3)"
	    },
       "Note that the first number selects the row, starting at ", 
       TT "0", " and the second number selects the column, also 
       starting at ", TT "0", ".",

     SUBSECTION "entries of a matrix",
       "To obtain the entries of a matrix in the form of a list of 
       lists, use the ", TO "entries", " command.",
       EXAMPLE {
	    "entries f"
	  },	 
       "Note that each inner list is a list of elements from a row of ", 
       TT "f", ".",

     SUBSECTION "ring",
       "The ", TO "ring", " command can be used to return the ring of 
       the matrix, that is, the ring containing entries of the matrix.",
       EXAMPLE {
	    "ring f"
	    },
       "Use the ", TO "describe", " command to recover how the ring of ", 
       TT "f", " was constructed.",
       EXAMPLE {
	    "describe ring f"
	    }
     }

document {
     Key => "basic arithmetic of matrices",

     SUBSECTION "+",
       "To add two matrices, use the ", TO "+", " operator.",
       EXAMPLE {
	    "ff = matrix{{1,2,3},{4,5,6}}",
	    "gg = matrix{{4,5,6},{1,2,3}}",
	    "ff+gg"
	    },
       "The matrices in question must have the same number of rows 
       and columns and also must have the same ring.",

     SUBSECTION "-",
       "To subtract two matrices, use the ", TO "-", " operator.",
       EXAMPLE {
	    "ff-gg"
	    },
       "The matrices in question must have the same number of rows 
       and columns and also must have the same ring.",		

     SUBSECTION "*",
       "To multiply two matrices use the ", TO "*", " operator.",
       EXAMPLE {
	    "R = ZZ/17[a..l];",
	    "ff = matrix {{a,b,c},{d,e,f}}",
	    "gg = matrix {{g,h},{i,j},{k,l}}",
	    "ff * gg"
	    },

     SUBSECTION "^",
       "To raise a square matrix to a power, use the ", TO "^", " operator.",
       EXAMPLE {
	    "ff = matrix{{1,2,3},{4,5,6},{7,8,9}}",
	    "ff^4"
	    },

     SUBSECTION "inverse of a matrix",
       "If a matrix ", TT "f", " is invertible, then ", TT "f^-1", 
       " will work.",
       -- EXAMPLE {				
       -- 	    -- f^-1 tends to work except no error if not working
       -- 	    },

     SUBSECTION "==", 
       "To check whether two matrices are equal, one can use ", TO "==", ".",
       EXAMPLE {
	    "ff == gg",
	    "ff == ff"
	    },
       "However, given two matrices ", TT "ff", " and ", TT "gg", ",
       it can be the case that ", TT "ff - gg == 0", " returns ", TO "true",
       " but ", TT "ff == gg", " returns ", TO "false", ".",
       EXAMPLE {
	    "M = R^{1,2,3}",
	    "N = R^3",
	    "ff = id_M",
	    "gg = id_N",
	    "ff - gg == 0",
	    "ff == gg"			
	    },
       "Since the degrees attached to the matrices were different, ", 
       TO "==", " returned the value ", TO "false", ".", 

     SUBSECTION "!=",
       "To check whether two matrices are not equal, one can use ", 
       TO "!=", ":",
       EXAMPLE {
	    "ff != gg"
	    },
       "From the definition above of ", TT "ff", " and ", TT "gg", 
       " we see that ", TO "!=", " will return a value of ", TO "true", 
       " if the degrees attached the the matrices are different, 
       even if the entries are the same.", 

     SUBSECTION "**",
       "Since tensor product (also known as Kronecker product and 
       outer product) is a functor of two variables, we may compute 
       the tensor product of two matrices. 
       Recalling that a matrix is a map between modules, we may write:",
       PRE///
       ff : K ---> L
       gg : M ---> N
       ff ** gg : K ** M  ---> L ** N
       ///,
       EXAMPLE {
	    "ff = matrix {{a,b,c},{d,e,f}}",
	    "gg = matrix {{g,h},{i,j},{k,l}}",
	    "ff ** gg"
	    },
       SeeAlso => {"extracting information about a matrix"}
     }

document {
     Key => "concatenating matrices",
     "Before Macaulay2 can concatenate matrices, the matrices in question ", 
     EM "must", " have entries in the same ring.",

     SUBSECTION "concatenate horizontally",
       "Use ", TO "|", " to concatenate two matrices horizontally.",
       EXAMPLE {
	    "R = ZZ/32003[a..f];",
	    "M = genericMatrix(R,a,3,2)",
	    "N = matrix{{d^2,a*d},{b*c,b*d},{a,c}}",
	    "M|N"
	    },

     SUBSECTION "concatenate vertically",
       "Use ", TO "||", " to concatenate two matrices vertically.",
       EXAMPLE {
	    "P = matrix{{d^2,a*d,e*f},{b*c,b*d,b*e},{a,c,d}}",
	    "transpose(M)||P"
	    },

     SUBSECTION "making block matrices",
       "The matrix function can take matrices as input as long as the sizes 
       match up.",
       EXAMPLE { 
	    "matrix{{id_(R^3),M,P},{random(R^1,R^3),random(R^1,R^3),random(R^1,R^2)}}"
	    },
       "Also, the number input entries in each row must be equal.  It might 
       seem like we could form the same matrix with the 
       input ", TT "matrix{{id_(R^3),M,P},{random(R^1,R^8)}}", " 
       since ", TT "random(R^1,R^8)", " will construct a 1 by 8 matrix that 
       has the same number of columns as 
       matrix ", TT "matrix{{id_(R^3),M,P}}", " but as input into the 
       matrix function that row entry must have 3 entries.",

     SUBSECTION "direct sum of matrices as maps between modules", 
       "++"
     }

-*
-- Mike wanted this: 
document {
     Key => "submatrices",
     }
*-

document {
     Key => "differentiation",
     SUBSECTION "diff",
     SUBSECTION "diff'",
     SUBSECTION "contract",
     SUBSECTION "contract'",
     SUBSECTION "jacobian",
     }

document {
     Key => "rank of a matrix",
     
     SUBSECTION "rank",

     SUBSECTION "rank of a matrix, determined probabilistically"
     }

document {
     Key => "determinants and minors",
     "The command ", TO2(determinant, "det"), " can be used to compute the determinant of
     a square matrix.",
     EXAMPLE {
	  "R = ZZ[a..d];",
	  "f = matrix{{a,b},{c,d}}",
	  "det f"
	  },      
     "The command ", TO "minors", " can be used to construct the ideal 
     generated by the ", TT "n", " by ", TT "n", " minors of a matrix. 
     Recall that the ", TT "n", " by ", TT "n", " minors of a matrix are 
     the determinants of the ", TT "n", " by ", TT "n", " submatrices 
     of a matrix.",
     EXAMPLE {
	  "R = QQ[x,y,z];",
	  "f = matrix{{x,y,z},{y,z,x^2}}",
	  "I = minors(2,f)"
	  }
     }

document {
     Key => "Pfaffians",
     
     SUBSECTION "pfaffians"
     }

document {
     Key => "exterior power of a matrix",
     "Since the ", TT "i", "-th exterior power is a functor, it applies 
     to matrices as well as to modules.",
     EXAMPLE {
	  "R = ZZ[vars(0..19)]",
	  "ff = genericMatrix(R,4,5)",
	  "exteriorPower (2,ff)"
	  },
     "Note that each entry of in the above matrix is a ", TT "2", " by ", 
     TT "2", 
     " minor (the determinant of a ", TT "2", " by ", TT "2", " submatrix) 
     of the matrix ", TT "ff", ".",
     SeeAlso => "exterior power of a module"
     }

document { -- something should be said about the degrees
		 -- and also about line wrapping
     Key => "format and display of matrices in Macaulay2",
     "By default, Macaulay2 displays matrices in a compact form.",
     EXAMPLE {
	  "QQ[x,y];",
	  "f = matrix{{x^2, x*y},{x*y, y^2}}",
	  "dual f",
	  "source f",
	  "target dual f"
	  },
     "Integers inside braces to the left of the matrix give the degrees of the basis elements of the
     target of the matrix; they are omitted if the degrees are all zero.",
     PARA{},
     "Note how the exponents have been placed to the
     right of the variables rather than formatted in superscript.  While this
     format is generally considered to be desirable, this can
     be turned off by setting the variable ", TO "compactMatrixForm", " to ", 
     TT "false", ".",
     EXAMPLE {
	  "compactMatrixForm = false",
	  "matrix{{x^2 + 3, x^4 + 1},{x^13 - 5, x^7 - 1}}"		
	  },
     "To have Macaulay2 display matrices in compact form again, type:",
     EXAMPLE {
	  "compactMatrixForm = true",
	  "matrix{{x^2 + 3, x^4 + 1},{x^13 - 5, x^7 - 1}}"
	  },
     }

document {
     Key => "importing and exporting matrices",
     
     SUBSECTION "toString",

     SUBSECTION "toExternalString"
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
