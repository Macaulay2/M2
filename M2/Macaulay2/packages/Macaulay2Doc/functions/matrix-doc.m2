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
     "The ", TO map, " function provides other methods to define a matrix."
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

TEST ///
R = ZZ/32003[a..d,x_1..x_4,y_(1,1)..y_(2,2)];
F = time poly"a5+5a4b+10a3b2+10a2b3+5ab4+b5+5a4x[1]+20a3bx[1]
  +30a2b2x[1]+20ab3x[1]+5b4x[1]+10a3x[1]2+30a2bx[1]2
  +30ab2x[1]2+10b3x[1]2+10a2x[1]3+20abx[1]3+10b2x[1]3
  +5ax[1]4+5bx[1]4+x[1]5+5a4y[1,1]+20a3by[1,1]
  +30a2b2y[1,1]+20ab3y[1,1]+5b4y[1,1]+20a3x[1]y[1,1]
  +60a2bx[1]y[1,1]+60ab2x[1]y[1,1]+20b3x[1]y[1,1]
  +30a2x[1]2y[1,1]+60abx[1]2y[1,1]+30b2x[1]2y[1,1]
  +20ax[1]3y[1,1]+20bx[1]3y[1,1]+5x[1]4y[1,1]
  +10a3y[1,1]2+30a2by[1,1]2+30ab2y[1,1]2+10b3y[1,1]2
  +30a2x[1]y[1,1]2+60abx[1]y[1,1]2+30b2x[1]y[1,1]2
  +30ax[1]2y[1,1]2+30bx[1]2y[1,1]2+10x[1]3y[1,1]2
  +10a2y[1,1]3+20aby[1,1]3+10b2y[1,1]3+20ax[1]y[1,1]3
  +20bx[1]y[1,1]3+10x[1]2y[1,1]3+5ay[1,1]4+5by[1,1]4
  +5x[1]y[1,1]4+y[1,1]5+5a4y[2,2]+20a3by[2,2]+30a2b2y[2,2]
  +20ab3y[2,2]+5b4y[2,2]+20a3x[1]y[2,2]+60a2bx[1]y[2,2]
  +60ab2x[1]y[2,2]+20b3x[1]y[2,2]+30a2x[1]2y[2,2]
  +60abx[1]2y[2,2]+30b2x[1]2y[2,2]+20ax[1]3y[2,2]
  +20bx[1]3y[2,2]+5x[1]4y[2,2]+20a3y[1,1]y[2,2]
  +60a2by[1,1]y[2,2]+60ab2y[1,1]y[2,2]+20b3y[1,1]y[2,2]
  +60a2x[1]y[1,1]y[2,2]+120abx[1]y[1,1]y[2,2]
  +60b2x[1]y[1,1]y[2,2]+60ax[1]2y[1,1]y[2,2]
  +60bx[1]2y[1,1]y[2,2]+20x[1]3y[1,1]y[2,2]+30a2y[1,1]2y[2,2]
  +60aby[1,1]2y[2,2]+30b2y[1,1]2y[2,2]+60ax[1]y[1,1]2y[2,2]
  +60bx[1]y[1,1]2y[2,2]+30x[1]2y[1,1]2y[2,2]+20ay[1,1]3y[2,2]
  +20by[1,1]3y[2,2]+20x[1]y[1,1]3y[2,2]+5y[1,1]4y[2,2]
  +10a3y[2,2]2+30a2by[2,2]2+30ab2y[2,2]2+10b3y[2,2]2
  +30a2x[1]y[2,2]2+60abx[1]y[2,2]2+30b2x[1]y[2,2]2
  +30ax[1]2y[2,2]2+30bx[1]2y[2,2]2+10x[1]3y[2,2]2
  +30a2y[1,1]y[2,2]2+60aby[1,1]y[2,2]2+30b2y[1,1]y[2,2]2
  +60ax[1]y[1,1]y[2,2]2+60bx[1]y[1,1]y[2,2]2+30x[1]2y[1,1]y[2,2]2
  +30ay[1,1]2y[2,2]2+30by[1,1]2y[2,2]2+30x[1]y[1,1]2y[2,2]2
  +10y[1,1]3y[2,2]2+10a2y[2,2]3+20aby[2,2]3+10b2y[2,2]3
  +20ax[1]y[2,2]3+20bx[1]y[2,2]3+10x[1]2y[2,2]3+20ay[1,1]y[2,2]3
  +20by[1,1]y[2,2]3+20x[1]y[1,1]y[2,2]3+10y[1,1]2y[2,2]3+5ay[2,2]4
  +5by[2,2]4+5x[1]y[2,2]4+5y[1,1]y[2,2]4+y[2,2]5"
assert( F == (a+b+x_1+y_(1,1)+y_(2,2))^5 )
///

///
R = ZZ/32003[a..d,x_1..x_4,y_(1,1)..y_(2,2)];
assert( time (a+3*b+5*d-1)^7 ==
time poly "a7+21a6b+189a5b2+945a4b3+2835a3b4+5103a2b5+5103ab6+2187b7+35a6d+630a5bd+
     4725a4b2d+18900a3b3d+42525a2b4d+51030ab5d+25515b6d+525a5d2+7875a4bd2+
     47250a3b2d2+141750a2b3d2+212625ab4d2+127575b5d2+4375a4d3+52500a3bd3+
     236250a2b2d3+472500ab3d3+354375b4d3+21875a3d4+196875a2bd4+590625ab2d4+
     590625b3d4+65625a2d5+393750abd5+590625b2d5+109375ad6+328125bd6+78125d7-7a6-
     126a5b-945a4b2-3780a3b3-8505a2b4-10206ab5-5103b6-210a5d-3150a4bd-18900a3b2d
     -56700a2b3d-85050ab4d-51030b5d-2625a4d2-31500a3bd2-141750a2b2d2-283500ab3d2
     -212625b4d2-17500a3d3-157500a2bd3-472500ab2d3-472500b3d3-65625a2d4-
     393750abd4-590625b2d4-131250ad5-393750bd5-109375d6+21a5+315a4b+1890a3b2+
     5670a2b3+8505ab4+5103b5+525a4d+6300a3bd+28350a2b2d+56700ab3d+42525b4d+
     5250a3d2+47250a2bd2+141750ab2d2+141750b3d2+26250a2d3+157500abd3+236250b2d3+
     65625ad4+196875bd4+65625d5-35a4-420a3b-1890a2b2-3780ab3-2835b4-700a3d-
     6300a2bd-18900ab2d-18900b3d-5250a2d2-31500abd2-47250b2d2-17500ad3-52500bd3-
     21875d4+35a3+315a2b+945ab2+945b3+525a2d+3150abd+4725b2d+2625ad2+7875bd2+
     4375d3-21a2-126ab-189b2-210ad-630bd-525d2+7a+21b+35d-1" )
///
