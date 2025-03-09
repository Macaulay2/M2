--- status: DRAFT
--- author(s): MES + from earlier
--- notes: 

-- in Classic: (matrix, String)

document { 
     Key => matrix,
     Headline => "make a matrix",
     "The simplest use of this function is to define a matrix by
     giving a doubly nested list of ring elements.  One may also
     explicitly provide the ring, or give a matrix as a doubly-nested list
     of matrices.",
     EXAMPLE {
	  "matrix{{1,2,3},{4,5,6}}",
	  "R = QQ[x,y,z];",
	  "matrix{{x,0,2},{1,2,y}}"
	  },
     "The ", TO map, " function provides other methods to define a matrix.",
     SeeAlso => {
	 map,
	 "Classic::matrix(String)",
         },
     Subnodes => {
	 TO (matrix, List),
	 TO (matrix, Ring, List),
	 TO (matrix, Matrix),
	 TO (matrix, MutableMatrix),
	 TO (matrix, RingElement),
	 TO (matrix, Vector),
	 TO (matrix, RingMap),
         },
     }

document {
     Key => {(matrix,Ring,List),(matrix,RingFamily,List)},
     Headline => "create a matrix from a doubly nested list of ring elements or matrices",
     Usage => "matrix(R, v)",
     Inputs => { "R", "v" => "a list of vectors; or a doubly nested list of ring elements and/or matrices",
	  },
     Outputs => {
	  {"A matrix over ", TT "R", ", whose source and target are both 
	       free, formed by the  elements of ", TT "v", "."}
	  },
     "All of the vectors, ring elements, or matrices must be defined over the ring ", 
     TT "R", ", or a base ring of ", TT "R", ".",
     PARA{},
     "If a doubly nested list of matrices is given, then ring elements can be used for
     1 by 1 blocks, and 0 represents a zero block.",
     PARA{},
     "This is essentially the same as ", TO (matrix,List), " together with
     the specification of the ring.",
     PARA{},
     EXAMPLE {
	  "R = QQ[a..d];",
	  "f = matrix{{a,b},{c,0}}",
	  "h = matrix{{f,f},{f,0}}"
	  }
     }
document { -- This node is used as an example in the node: Usage 
     Key => {(matrix,List),[matrix,Degree]},
     Headline => "create a matrix from a doubly-nested list of ring elements or matrices",
     Usage => "matrix v",
     Inputs => {
	  "v" => "a list of lists of either ring elements or matrices",
	  Degree => {"an integer or a list of integers, to serve as the degree of the matrix"}
	  },
     Outputs => { {
	       "A matrix where the first list of ", TT "v", " gives the first 
	       row (or set of rows, if the elements are matrices), the second list is the second row, etc."
	       } },
     "An attempt is made to coerce the ring elements and matrices to
     a common ring.  If the entries are ring elements, they are used as
     the entries of the matrix, and if the entries are matrices, then
     they are used to provide blocks of entries in the resulting matrix.",
     PARA{},
     "An attempt is made to set up the degrees of the generators of the
     free module serving as source so that the map will be homogeneous and of
     degree zero.",
     PARA{},
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
     SeeAlso => {map}
     }

doc ///
Node
  Key
    (matrix, Vector)
  Usage
    matrix v
  Inputs
    v:
  Outputs
    :Vector
      the matrix with a single column containing the vector {\tt v}
  Description
    Example
      v = vector {1,2,3}
      matrix v
///

document { 
     Key => (matrix,Matrix),
     Headline => "the matrix between generators",
     Usage => "matrix f",
     Inputs => {
	  "f" => "a map of modules"
	  },
     Outputs => {
	  Matrix => 
	       "If the source and target of f are free, then the result is
     	       f itself.  Otherwise, the source and target will be replaced by
     	       the free modules whose basis elements correspond to the generators
     	       of the modules.",
	  },
     TEX "Each homomorphism of modules $f : M \\rightarrow N$
     in Macaulay2 is induced from a matrix $f0 : \\mathtt{cover} M \\rightarrow \\mathtt{cover} N$.
     This function returns this matrix.",
     EXAMPLE {
	  "R = QQ[a..d];",
	  "I = ideal(a^2,b^2,c*d)",
	  "f = basis(3,I)",
	  "source f",
	  "target f"
	  },
     "The map f is induced by the following 3 by 12 matrix from R^12
     to the 3 generators of ", TT "I", ".",
     EXAMPLE {
	  "matrix f"
	  },
     "To obtain the map that is the composite of this with the inclusion of I onto R,
     use ", TO (super,Matrix), ".",
     EXAMPLE {
	  "super f"
	  },
     SeeAlso => {cover,super,basis}
     }

document {
     Key => (matrix, RingMap),
     Headline => "the matrix associated to a ring map",
     Usage => "matrix f",
     Inputs => {"f"},
     Outputs => {{"the matrix associated to the ring map ", TT "f", ", computed by applying ", TT "f", " to the matrix of variables of its source ring" }},
     EXAMPLE lines ///
     	  R = QQ[x,y]
	  S = QQ[s,t,u]
	  f = map(R,S,{x^2,x*y,y^2})
	  matrix f
     ///	  
     }

document { 
     Key => (matrix,MutableMatrix),
     Headline => "make a matrix from a mutable one",
     Usage => "matrix m",
     Inputs => {
	  "m"
	  },
     Outputs => {
	  Matrix => "the same matrix as m, but not mutable."
	  },
     "There are many more operations available for matrices than
     for immutable matrices, but mutable matrices are sometimes easier
     to construct.",
     EXAMPLE {
	  "m = mutableMatrix(ZZ,3,5)",
	  "m_(1,2) = 3",
	  "m_(2,4) = 54",
	  "m_(0,0) = -12",
	  "m",
	  "matrix m"
	  },
     SeeAlso => {mutableMatrix, mutableIdentity, MutableMatrix}
     }

document {
     Key => {(matrix, RingElement),(matrix, Number)},
     Headline => "make a matrix from a ring element",
     Usage => "matrix r",
     Inputs => { "r" },
     Outputs => { Matrix => {"the one by one matrix with ", TT "r", " as its single entry"} },
     EXAMPLE lines ///
     matrix 48
     ///
     }
