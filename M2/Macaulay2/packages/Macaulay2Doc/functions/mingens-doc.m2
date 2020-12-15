-- -*- coding: utf-8 -*-
--- status: TODO
--- author(s): 
--- notes: 

///
document {
     Key => "minimal generating sets and minimal presentations",
     "We describe the differences between the routines ",
     TO (mingens, Module), ", ", TO(trim,Module), ", and ", TO(minimalPresentation,Module),
     ".",
     PARA{},
     "Every module in Macaulay2 is a ",  TO "subquotient module", ".  A module
     M is a submodule of a quotient of an ambient free module F.  If M is graded,
     or the ring R of M is local, then the notion of minimal generators and minimal
     presentation make sense.",
     PARA{},
     TO (mingens,Module), " returns a matrix whose target is F, and whose columns 
     minimally generate M.",
     PARA{},
     TO (trim,Module), " returns a different presentation of the same module, still
     having ambient free module F, but whose generator and relation matrices have
     been made minimal.",
     TO (minimalPresentation,Module), " returns a minimal presentation matrix of the 
     module M: M is isomorphic to the cokernel of this matrix, but the new matrix
     potentially has a different ambient free module. In this situation, the
     isomorphism is stored in M.cache.PruningMap",
     PARA{},
     EXAMPLE {
	  kk = frac(QQ[a])
	  A = kk[x,y,z]
	  F = y^2*z-x*(x-z)*(x-a*z)
	  B = A/F
	  I = ideal(x,y)
	  M = I/I^2
	  mingens M
	  minimalPresentation M
	  M
	  m = syz gens ideal(x,y)
	  coker lift(m,A) ** presentation B
	  I = (kernel vars R
	  mingens I
	  image mingens I == I
	  },
     }
///

document { 
     Key => mingens,
     Headline => "minimal generator matrix",
     "Every ideal and module in Macaulay2 comes equipped with
     a generating set, which is sometimes a larger 
     generating set than is required.  This routine returns a matrix
     whose columns generate the given ideal or module.",
     SeeAlso => {trim, minimalPresentation}
     }

document {
     Key => {Complement, [mingens, Strategy], [trim, Strategy]},
     Headline => "a Strategy option value",
     "The standard (default) strategy for ", TO "mingens", " or ", TO "trim", " is ", TO "Complement", ", at least in the case where the module (or
     ideal) is homogeneous, and the ring is (quotient ring of) a polynomial ring over a field.  The other strategy, ", TO "Inhomogeneous", ", is experimental."
     }

document { 
     Key => {(mingens,Module),(mingens,Ideal)},
     Usage => "mingens I",
     Inputs => {
	  "I" => "or an ideal"
	  },
     Outputs => {
	  Matrix => {"a matrix, whose columns generate I."}
	  },
     "If I is homogeneous, then a matrix, whose columns
     minimally generate I, is returned.",
     EXAMPLE {
	  "R = QQ[a..f]",
	  "I = ideal(a,b,c) * ideal(a,b,c)",
	  "mingens I"
	  },
     "If I is not homogeneous, then an attempt is made to find
     a more efficient generating matrix, one which is better than a 
     Gröbner basis.  There is no guarantee that the generating set is small,
     or that no subset also generates.  The only thing known is that the entries
     do generate the ideal.",
     EXAMPLE {
	  "J = ideal(a-1, b-2, c-3)",
	  "I = J*J",
	  "mingens I"
	  },
     "Every module I in Macaulay2 is a submodule of a quotient of some ambient
     free module F.  This routine returns a minimal, or improved generating set 
     for the same module I.  If you want to minimize the generators and the relations of a 
     subquotient module, use ", TO trim, ".  If you want a minimal presentation, then use ", TO minimalPresentation, ".",
     EXAMPLE {
	  "M = matrix{{a^2*b*c-d*e*f,a^3*c-d^2*f,a*d*f-b*c*e-1}}",
	  "I = kernel M",
	  "J = image mingens I",
	  "I == J",
	  "trim I"
	  },
     PARA{},
     "If the base ring is a polynomial ring (or quotient of one), then a Gröbner
     basis computation is started, and continued until all generators have been 
     considered.",
     SeeAlso => {trim, minimalPresentation,kernel,image}
     }
document { 
     Key => (mingens,GroebnerBasis),
     Headline => "(partially constructed) minimal generator matrix",
     Usage => "mingens G",
     Inputs => {
	  "G"
	  },
     Outputs => {
	  Matrix => "whose columns form a (partially computed) minimal generating set"
	  },
     "Every GroebnerBasis computation in Macaulay2 computes a generator matrix, in the process
     of constructing the Gröbner basis.  If the
     original ideal or module is homogeneous, then the columns of this matrix form a
     minimal set of generators.  In the inhomogeneous case, the columns generate, and an attempt 
     is made to keep the size of the generating set small.",
     PARA{},
     "If the Gröbner basis is only partially constructed,
     the returned result will be a partial answer.  In the graded case this set can be 
     extended to a minimal set of generators for the ideal or module.",
     EXAMPLE {
	  "R = QQ[a..f]",
	  "M = genericSymmetricMatrix(R,a,3)",
	  "I = minors(2,M)",
	  "G = gb(I, PairLimit=>5)",
	  "mingens G",
	  "mingens I"
	  },
     SeeAlso => {GroebnerBasis, gb,  genericSymmetricMatrix, minors}
     }

TEST "
R = ZZ/101[a..d,MonomialOrder => Position => Up]
f = matrix{{a,b},{c,d}}
h = matrix {{1,0,0},{0,c,d}}
M = subquotient(h,f)
assert( mingens M == matrix (R, {{1},{0}}))
"

TEST "
R = ZZ/101[a..d,MonomialOrder => Position => Up]
f = matrix{{a,b},{c,d}}
h = matrix {{1,0,0},{0,c,d}}
M = subquotient(h,f)
assert( generators trim M == matrix (R, {{1},{0}}))
"
