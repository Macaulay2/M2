-- -*- coding: utf-8 -*-
document {
     Key => Module,
     Headline => "the class of all modules",
     PARA{},
     "See ", TO "modules", " for an overview of modules in Macaulay2.  See
     ", TO "modules in Macaulay2", " for a tutorial overview of modules.",
     PARA{},
     "Modules in Macaulay2 are implemented as ", TO "subquotient modules", ".  
     Submodules and quotients of free modules are perhaps the most common and important
     modules, and subquotients form the smallest class
     of modules that naturally includes these cases.",
     PARA{},
     "Common ways to make a module:",
     UL {
	  TO (symbol ^, Ring, ZZ),
	  TO (symbol ^, Ring, List),
	  TO (cokernel, Matrix),
	  TO (image, Matrix),
	  TO (kernel, Matrix),
	  },
     "Common ways to get information about modules:",
     UL {
	  TT "ring Module",
	  TO (numgens, Module),
	  TO (degrees, Module),
	  TO (generators, Module),
	  TO (relations, Module),
	  TO "isFreeModule",
	  TO (isHomogeneous, Module),
	  },
     "Numerical information about a module:",
     UL {
	  TO (codim, Module),
	  TO (dim, Module),
	  TO (rank, Module)
	  },
     "Submodules, quotients, and subquotient modules:",
     UL {
	  TO (ambient, Module),
	  TO (cover, Module),
	  TO (super, Module),
	  TO (symbol /, Module, Module),
     	  TO (subquotient, Matrix, Matrix),
	  TO (isSubset, Module, Module),
	  },
     "Common operations on modules:",
     UL {
	  TO (symbol +, Module, Module),
	  TO (symbol ==, Module, Module),
	  TO (symbol ++, Module, Module),
	  TO (symbol ^, Module, List),
	  TO (symbol **, Module, Module),
	  TO (symbol ^**, Module, ZZ),
	  TO (symbol _, Module, List),
	  },
     "Minimalization:",
     UL {
	  TO (mingens,Module),
	  TO (trim,Module),
	  TO (minimalPresentation,Module)
	  },
     "Graded modules:",
     UL {
	  TO basis,
	  TO "Truncations::truncate(ZZ,Module)",
	  TO (degree, Module),
	  TO (genera, Module),
	  TO (hilbertSeries, Module),
	  TO (hilbertFunction, ZZ, Module),
	  TO (poincare, Module),
	  TO (regularity, Module),
	  },
     "Annihilators, quotients and Gröbner bases:",
     UL {
	  TO (gb, Module),
	  TO "Saturation::Ideal : Ideal",
	  TO "Saturation::annihilator(Module)",
	  TO "Saturation::saturate(Module,Ideal)",
	  },
     "Common homological computations:",
     UL {
	  TO (res, Module),
	  TO (pdim, Module),
	  TO "Hom",
	  TO (homomorphism,Matrix),
	  TO (Ext,ZZ,Module,Module),
	  TO (Tor,ZZ,Module,Module),
	  TO (cohomology,ZZ,Module),
	  TO (homology, Matrix, Matrix),
	  TO (fittingIdeal, ZZ, Module),
	  },
     "Multilinear algebra:",
     UL {
	  TO (exteriorPower,ZZ,Module),
	  }}

document {
     Key => "modules",
     "For more operations in homological algebra, see ", 
     TO "chain complexes", ".  For additional common operations and a 
     comprehensive list of all routines
     in Macaulay2 which return or use modules, see ", TO Module, ".",
     Subnodes => {
	  "construction of modules",
	  TO "free modules",
	  TO "matrices to and from modules",
	  TO "submodules and quotients",
	  TO "subquotient modules",

	  "homomorphisms (maps) between modules",
	  TO "module homomorphisms",
	  -- Mike wanted this: TO "canonical maps between modules",
	  TO "right modules or left modules?",

	  -- Mike wanted this: "operations on modules",
	  -- Mike wanted this: TO "direct sums of modules",
	  -- Mike wanted this: TO "tensor products of modules",
	  -- Mike wanted this: TO "Hom modules and homomorphisms",
	  -- Mike wanted this: TO "annihilators and submodule quotients",
	  TO "Saturation :: module quotients, saturation, and annihilator",

	  "graded modules",
	  TO "Hilbert functions and free resolutions",
	  -- Mike wanted this: TO "degrees of elements and free modules",
	  -- Mike wanted this: TO "degree and multiplicity of a module",
	  TO "basis",
	  -- Mike wanted this: TO "Hilbert functions and polynomials",
	  -- Mike wanted this: TO "homogenization",
	  -- Mike wanted this: TO "truncation and homogeneous components of a graded module",
	  
	  "multilinear algebra",
	  TO "exterior power of a module",
	  -- Mike wanted this: TO "Fitting ideals",
	  -- Mike wanted this: TO "adjoints of maps",
	  
	  -- Mike wanted this: "homological algebra",
	  -- Mike wanted this: TO "Ext and Tor",
	  -- Mike wanted this: TO "local cohomology",
	  -- Mike wanted this: TO "is a module Cohen-Macaulay?"
	  }
     }

document { Key => "right modules or left modules?",
     "Macaulay2 can handle non-commutative rings, and for such rings there is a difference between left modules and right modules.
     In Macaulay2, all the modules are left modules, but matrices act on the left, too.  The usual convention would be to have the matrices
     act on the right, so the homomorphism rule (", TT "f(av)=af(v)", ") becomes a consequence of associativity of matrix-vector-scalar
     multiplication (", TT "(av)f=a(vf)", ").  Macaulay2 makes things come out okay in the end -- a left ", TT "R", "-module can be regarded
     naturally as a right ", TT "R'", "-module, where ", TT "R'", " is the opposite ring of ", TT "R", ", obtained from the ring ", TT "R", "
     by reversing the multiplication.  Thus matrices over ", TT "R'", " can act on ", TT "R", "-modules from the left.  Matrices over ", TT "R", "
     in Macaulay2 are ", EM "really", " matrices over ", TT "R'", ".",
     PARA{},
     "We illustrate this state of affairs with an example over a (noncommutative) Weyl algebra.  First observe the noncommutativity.",
     EXAMPLE lines ///
     	  R = QQ[x,dx,WeylAlgebra=>{x=>dx}]
	  x*dx
	  dx*x
     ///,
     "Now verify the module is a left module by checking associativity.",
     EXAMPLE lines ///
	  M = R^2
	  v = M_0
     	  dx*v
	  x*(dx*v)
	  (x*dx)*v
     	  x*(dx*v) == (x*dx)*v
     ///,
     "Now make a matrix and check that left multiplication by it is a homomorphism from ", TT "M", " to ", TT "M", ".",
     EXAMPLE lines ///
     	  f = dx * id_M
	  f*(x*v)
	  x*(f*v)
	  f*(x*v) == x*(f*v)
     ///,
     "Now we make another matrix and check that matrix multiplication treats the entries of the matrices as residing in the opposite ring, ", TT "R'", ".",
     EXAMPLE lines ///
     	  g = x * id_M
	  f*g
     	  f*g == (x*dx) * id_M
     	  (dx * id_M)*(x * id_M) == (x*dx) * id_M
     ///,
     "Here we check that multiplication of a scalar times a matrix is compatible with multiplication of a scalar times a vector.",
     EXAMPLE lines ///
     	  x * ( (dx * id_M) * v )
	  (x *  (dx * id_M) ) * v
	  (x *  (dx * id_M) ) * v == x * ( (dx * id_M) * v )	  
     ///,
     "One desirable associativity rule does ", EM "not", " hold, the one for ", EM "RingElement * Matrix * Matrix", ", as we
     see in this example.",
     EXAMPLE lines ///
          x * ( id_M * ( dx * id_M ) )
          (x * id_M) * ( dx * id_M )
          x * ( id_M * ( dx * id_M ) ) == (x * id_M) * ( dx * id_M )
     ///,
     "The reason for this discrepancy is that, as explained above, matrix multiplication is done over ", TT "R'", ", not over ", TT "R", ".",
     PARA{},
     "Currently, tensor product of a module ", TT "M", " by a ring ", TT "R", " works on either side and does the same thing.  In
     other words, you can write ", TT "R**M", " or ", TT "M**R", ".  That may change in the future."
     }

-------------------
-- module nodes ---
-------------------

document {
     Key => "free modules",
     "We use ", TO (symbol ^,Ring,ZZ), " to make a new free module.",
     PARA{},
     EXAMPLE {
	  "R = ZZ/101[x,y,z];",
	  "M = R^4"
	  },
     "Such modules are often made implicitly when constructing matrices.",
     EXAMPLE {
	  "m = matrix{{x,y,z},{y,z,0}}",
	  "target m == R^2"
	  },
     PARA{},
     "When a ring is graded, so are its free modules.  By default,
     the degrees of the basis elements are taken to be 0.",
     EXAMPLE {
	  "degrees M"
	  },
     "We can use ", TO (symbol ^, Ring, List), " to specify other degrees,
     or more precisely, their additive inverses.",
     EXAMPLE {
	  "F = R^{1,4:2,3,3:4}",
      	  "degrees F",
	  },
     "Notice the use of ", TO ":", " above to indicate repetition.",
     PARA{},
     "If the variables of the ring have multi-degrees represented by
     lists (vectors) of integers,",
     -- Mike wanted this: " as described in ", TO "multi-graded polynomial rings", ",",
     " then the degrees of a
     free module must also be multi-degrees.",
     EXAMPLE {
	  "S = ZZ[a,b,c, Degrees=>{{1,2},{2,0},{3,3}}]",
	  "N = S ^ {{-1,-1},{-4,4},{0,0}}",
	  "degree N_0",
	  "degree (a*b*N_1)",
	  },
     SeeAlso => {
	  -- Mike wanted this: "multigraded polynomial rings",
	  "graded modules"
	  },
     }

document {
     Key => "matrices to and from modules",
     Headline => "including kernel, cokernel and image",
     
     SUBSECTION "matrices to modules (kernel, image, cokernel)",
     "Given a matrix, we may compute the ", TO kernel, ", ", 
     TO image, ", and ", TO cokernel, ".",
     EXAMPLE {
	  "R = QQ[a..f];",
	  "F = matrix{{a,b,d,e},{b,c,e,f}}",
	  },
     EXAMPLE {
	  "M = ker F",
	  "coker F",
	  "image F",
	  },
     "Some routines in Macaulay2 have abbreviations, for example ", TT "ker", 
     " may be used for ", TT "kernel", ", and 
     ", TT "coker", " may be used for ", TT "cokernel", ".  
     The ", TT "image", " function has no abbreviated form.",
     
     SUBSECTION "modules to matrices",
     "Each module has, at least implicitly, two matrices associated to it:
     ", TO generators, " (abbreviated form: ", TT "gens", "), and ", 
     TO relations, ".  If a module is
     a submodule of a free module, then the relations matrix is zero.  If a
     module is a quotient of a free module, then the generator matrix 
     is the identity matrix.  If a module is a ", TO "subquotient", ", 
     then both may be more general.",
     EXAMPLE {
	  "generators M",
	  "relations M",
	  },
     PARA{},
     "Every finitely generated module has a presentation matrix.
     In Macaulay2,  
     if the module is not a quotient of a free module, then a syzygy
     computation is performed to find a presentation matrix.",
     EXAMPLE {
	  "presentation M",
	  },
     SeeAlso => {(cokernel,Matrix), 
	  (image,Matrix), 
	  (kernel,Matrix),
	  (generators,Module),
	  (relations,Module),
	  (presentation,Module)
	  },
}

document {
     Key => "submodules and quotients",
     SUBSECTION "submodules",
     "We can create submodules by using standard mathematical notation, keeping in mind
     that the generators of a module ", TT "M", " are denoted by ", TT "M_0, M_1", ", etc.",
     EXAMPLE {
	  "R = QQ[x,y,z];",
	  "M = R^3",
	  "I = ideal(x^2,y^2-x*z)",
	  },
     "Here are some examples of submodules of ", TT "M", ".",
     EXAMPLE {
     	  "I*M",
	  "R*M_0",
	  "I*M_1",
	  "J = I*M_1 + R*y^5*M_1 + R*M_2"
	  },
     "To determine if one submodule is contained in the other, use
     ", TO (isSubset,Module,Module), ".",
     EXAMPLE {
     	  "isSubset(I*M,M)",
	  "isSubset((x^3-x)*M,x*M)"
	  },
     "Another way to construct submodules is to take the kernel or image of a matrix.",
     EXAMPLE {
	  "F = matrix{{x,y,z}}",
	  "image F",
	  "kernel F"
	  },
     "The module ", TT "M", " does not need to be a free module.  We will see examples below.",
     SUBSECTION "quotients",
     "If N is a submodule of M, construct the quotient using ", TO (symbol/,Module,Module), ".",
     EXAMPLE {
	  "F = R^3",
	  "F/(x*F+y*F+R*F_2)"
	  },
     "When constructing M/N, it is not necessary that M be a free module, or a quotient of a free
     module.  In this case, we obtain a subquotient module, which we describe below."
     }

document {
     Key => "subquotient modules",
     Headline => "the way Macaulay2 represents modules",
     "Not all modules arise naturally as submodules or quotients of free modules.  As an example,
     consider the module ", TEX "$M = I/I^2$", " in the example below.",
     EXAMPLE {
	  "R = QQ[x,y,z];",
	  "I = ideal(x*y,x*z,y*z)",
	  "M = I/I^2"
	  },
     TEX "Macaulay2 represents each module (at least conceptually) as a subquotient module, that is, a submodule of 
     a quotient of an ambient free module.  A subquotient module is determined by two
     matrices $f : R^m \\rightarrow{} R^n$ and $g : R^p \\rightarrow{} R^n$.
     The {\\em subquotient module} with generators $f$ and relations $g$ is by definition the module
     $M = ((image f) + (image g))/(image g)$.",
     PARA{},
     TEX "If $f$ is the identity map, $M = coker g$, and if $g = 0$, then $M = image f$.  
     The class of subquotient modules is the smallest class containing free modules, which is closed
     under taking submodules and quotients.",
     PARA{},
     "One may create a subquotient module directly from matrices f and g having the same target free module.",
     EXAMPLE {
	  "f = matrix{{x,y}}",
	  "g = matrix{{x^2,x*y,y^2,z^4}}",
	  "M = subquotient(f,g)"
	  },
     "The same module can be constructed in the following manner.",
     EXAMPLE {
	  "N = (image f)/(image g)",
 	  "N1 = (image f + image g)/(image g)",
	  "M === N"
	  },
     "Notice that Macaulay2 allows one to write (image f)/(image g), even though 
     mathematically this really means: (image f + image g)/(image g).  There is an important
     difference however.  Modules in Macaulay2 always come with an ordered set of generators,
     and N1 has 4 more generators (all zero in the module!) than N.  The 
     modules M and N though are identical.",
     PARA{},
     "The two matrices f and g mentioned above are recovered using the
     routines ", TO (generators,Module), " and ", TO (relations,Module), ".",
     EXAMPLE {
	  "generators M",
	  "relations M"
	  },
     PARA{},
     "Submodules and quotients of free modules work as one would imagine.",
     EXAMPLE {
	  "N2 = R*M_0 + I*M",
	  "M/N2",
	  "prune(M/N2)"
	  },
     PARA{},
     "Given a subquotient module M, there are several useful modules associated to M.",
     "The free module of which M is a subquotient is obtained using ", TO (ambient,Module), ".",
     EXAMPLE {
	  "ambient M"
	  },
     "This is the same as the common target of the matrices of generators and
     relations.",
     EXAMPLE {
	  "ambient M === target relations M",
	  "ambient M === target generators M"
	  },
     "M is a submodule of the module R^n/(image g).  The routine ", TO (super,Module),
     " returns this quotient module.",
     EXAMPLE {
	  "super M"
	  },
     "This may be obtained directly as the cokernel of the matrix of relations.",
     EXAMPLE {
	  "super M === cokernel relations M"
	  },
     "Often the given representation of a module is not very efficient.
     Use ", TO (trim,Module), " to keep the module as a subquotient of the 
     same ambient free module,
     but change the generators and relations to be minimal, or in the nonlocal or
     non-graded case, at least more efficient.",
     EXAMPLE {
	  "M + M",
	  "trim (M+M)"
	  },
     "Use ", TO (minimalPresentation,Module), " to also allow the ambient free
     module to be improved.  This currently returns a quotient of a free
     module, but in the future it might not.",
     EXAMPLE {
	  "minimalPresentation M"
	  },
     TT "prune", " is a synonym for ", TT "minimalPresentation", ".",
     EXAMPLE {
	  "prune M"
	  },
     "For maps between modules, including between subquotient modules, see ", 
     TO "homomorphisms (maps) between modules", ".",
     SeeAlso => {
	  (ambient,Module),
	  (super,Module),
	  (generators,Module),
	  (relations,Module),
	  subquotient,
	  (trim,Module),
	  (minimalPresentation,Module)
	  }
     }

document {
     Key => "module homomorphisms",
     "A homomorphism ", TT "f : M --> N", " is represented as a matrix
     from the generators of M to the generators of N.",
     EXAMPLE {
	  "R = QQ[x,y]/(y^2-x^3);",
	  "M = module ideal(x,y)"
	  },
     "One homomorphism ", TT "F : M --> R", " is ", 
     TT "x |--> y, y |--> x^2", " (this is
     multiplication by the fraction ", TT "y/x", ").  
     We write this in the following way.",
     EXAMPLE {
	  "F = map(R^1,M,matrix{{y,x^2}})"
	  },
     "Notice that as is usual in Macaulay2, the target comes before the source.",
     PARA{},
     "Macaulay2 doesn't display the source and target, unless they are both free
     modules.  Use ", TO target, " and ", TO source, " to get them.  The ",
     TO matrix, " routine recovers the matrix of free modules between the
     generators of the source and target.",
     EXAMPLE {
	  "source F",
	  "target F == R^1",
	  "matrix F"
	  },
     "Macaulay2 also does not check that the homomorphism is well defined
     (i.e. the relations of the source map into the relations of the target).
     Use ", TO isWellDefined, " to check.  This generally requires a Gröbner
     basis computation (which is performed automatically, if it is required
	  and has not already been done).",
     EXAMPLE {
	  "isWellDefined F",
	  "isIsomorphism F"
	  },
     "The image of ", TT "F", " lies in the submodule ", TT "M", 
     " of ", TT "R^1", ".  Suppose we wish to 
     define this new map ", TT "G : M --> M", ".  How does one do this?",
     PARA{},
     "To obtain the map ", TT "M --> M", ", we use ", 
     TO (symbol//,Matrix,Matrix), ".
     In order to do this, we need the inclusion map of ", TT "M", 
     " into ", TT "R^1", ".",
     -*
     -- Mike wanted this: 
     "  We explain these canonical maps
     more thoroughly in ", TO "canonical maps between modules", ", 
     but for now we just write down the inclusion map.",
     *-
     EXAMPLE {
	  "inc = inducedMap(R^1, M)"
	  },
     "Now we use // to lift ", TT "F : M --> R^1", " along ",
     TT "inc : M --> R^1", ", to obtain
     a map ", TT "G : M --> M", ", such that ", TT "inc * G == F", ".",
     EXAMPLE {
	  "G = F // inc",
	  "target G == M and source G == M",
	  "inc * G == F"
	  },
     "Let's make sure that this map ", TT "G", " is well defined.",
     EXAMPLE {
	  "isWellDefined G",
	  "isIsomorphism G",
	  "prune coker G",
	  "kernel G == 0"
	  },
     SeeAlso => {
	  -- Mike wanted this: "canonical maps between modules",
	  (isWellDefined,Matrix),
	  (isIsomorphism,Matrix),
	  (isInjective,Matrix),
	  (isSurjective,Matrix),
	  (kernel,Matrix),
	  (cokernel,Matrix),
	  (symbol//,Matrix,Matrix)
	  }
     }

-*
-- Mike wanted this: 
document {
     Key => "canonical maps between modules",
     Headline => "empty"
     }
*-

document {
     -- old??
     Key => "constructing maps between modules",
	"Let's start with a free module.",
	EXAMPLE {
		"R = ZZ/5[x,y,z];",
		"F = R^3"
		},
	"A list of indices can be used to produce homomorphisms corresponding to the corresponding basis vectors.",
	EXAMPLE {
		"F_{0,1,2}",
		"F_{0,1}",
		"F_{1,2}"
		},
	"Matrices are viewed as linear transformations.",
	EXAMPLE {
		"f = matrix{{x,y,z}}"
		},
--     "The standard way to define a map from an R-module M to an 
--     R-module N is to give a matrix whose columns are the image vectors
--     of the generators of M.",
--     EXAMPLE {
--	  "R = QQ[x,y,z];",
--	  "m = cokernel vars R",
--	  "--F = map(m/m^2, R^1/m, {{x*y*z}})"
--	  }
     }

-*
-- Mike wanted this: 
document {
     Key => "direct sums of modules",
     Headline => "empty",
     "
     "
     }
*-

-*
-- Mike wanted this: 
document {
     Key => "tensor products of modules",
     Headline => "empty",
     }
*-

-*
-- Mike wanted this: 
document {
     Key => "Hom modules and homomorphisms",
     Headline => "empty",
///
     R = QQ[a..e]
     C = res coker vars R
     M = coker C.dd_3 ++ R^{-4}
     N = coker C.dd_4
     H = Hom(N,M);
     B = basis(0,H);
     F  = B * random(source B, R^1) 
     f = homomorphism F
     source f
target f     
     syz transpose matrix f
     ///
}
*-


document {
     Key => "Hilbert functions and free resolutions",
     Headline => "including degree and betti numbers",
     "In this section, we give examples of common operations
     involving modules.  Throughout this section, we suppose that the base
     ring ", TT "R", " is graded, with each variable having degree one, and that  ",
     TT "M", " is a graded ", TT "R", "-module.  If the ring is not graded, or is multi-graded,
     or if ", TT "M", " is not graded, some of these functions still work, but
     care must be taken in interpreting the output.  Here, we just consider the
     standard grading case.",
     SUBSECTION "checking homogeneity",
     "Let's start by making a module over a ring with 18 variables",
     EXAMPLE {
	  "R = ZZ/32003[vars(0..17)];",
	  "M = coker genericMatrix(R,a,3,6)"
	  },
     "Use ", TO "isHomogeneous", " to check whether a given module is
     graded.",
     EXAMPLE "isHomogeneous M",
     SUBSECTION "codimension, degree, and sectional arithmetic genera",
     "Use ", TO (codim,Module), ", ", TO (degree,Module), ", and ", TO (genera,Module), " for some basic 
     numeric information about a module.",
     EXAMPLE {
	  "codim M",
	  "degree M",
	  "genera M"
	  },
     "The last number in the list of genera is the degree minus one.  The second to last
     number is the genus of the generic linear section curve, ..., and the first
     number is the arithmetic genus.",
     SUBSECTION "the Hilbert series",
     "The Hilbert series (", TO (hilbertSeries, Module), ") of ", TT "M", " is by definition the formal power series ",
     TT "H(t) = sum(d in ZZ) dim(M_d) t^d", ".  This is a rational function with 
     denominator ", TT "(1-t)^n", ", where ", TT "n", " is the number of variables
     in the polynomial ring.  The numerator of this rational function is called
     the poincare polynomial, and is obtained by the ", TO (poincare,Module), " function.",
     EXAMPLE {
	  "poincare M",
	  "hf = hilbertSeries M"
	  },
     PARA{},
     "It is often useful to divide the poincare polynomial by ", TT "(1-t)", " as many
     times as possible.  This can be done by using ", TO reduceHilbert, ":",
     EXAMPLE {
	  "reduceHilbert hf"
	  },
     EXAMPLE {
	  "poincare' = (M) -> (
	H := poincare M;
	t := (ring H)_0;  -- The variable t above
	while H % (1-t) == 0 do H = H // (1-t);
	H);",
          "poincare' M",
	  },
     SUBSECTION "free resolutions",
     "The minimal free resolution ", TT "C", " is computed using ", TO (resolution,Module), ".  
     The specific matrices are obtained by indexing ", TT "C.dd", ".",
     EXAMPLE {
	  "C = resolution M",
	  "C.dd_3"
	  },
     "For more information about chain complexes and resolutions, see ", TO "chain complexes",
     " and ", TO "computing resolutions", ".",
     SUBSECTION "betti numbers",
     "Use ", TO2{(betti,GradedModule),"betti"}, " to display the graded betti numbers of ", TT "M", ".",
     EXAMPLE "betti C",
     "This table should be interpreted as follows: the number in the ", 
     TT "i", "-th row and ", TT "j", "-th column (indices starting at 0),
     is the number of ", TT "j", "-th syzygies in degree ", TT "i+j", ".
     In the above example, there are 15 second syzygies of degree 4, and the entries
     of the maps ", TT "CC.d_1, CC.d_3, CC.d_4", " are all linear.",
     Subnodes => {
	 TO isHomogeneous,
	 TO (codim, Module),
	 TO (degree, Module),
	 TO (genera, Module),
	 TO (hilbertSeries, Module),
	 TO (poincare, Module),
	 TO reduceHilbert,
	 TO betti,
	 }
     }

-*
-- Mike wanted this: 
document {
     Key => "degrees of elements and free modules",
     Headline => "empty",
     }
*-

-*
-- Mike wanted this: 
document {
     Key => "degree and multiplicity of a module",
     Headline => "empty",
     }
*-

-*
-- Mike wanted this: 
document {
     Key => "Hilbert functions and polynomials",
     Headline => "empty",
     }
*-

-*
-- Mike wanted this: 
document {
     Key => "homogenization",
     Headline => "empty",
     }
*-

-*
-- Mike wanted this: 
document {
     Key => "truncation and homogeneous components of a graded module",
     Headline => "empty",
     }
*-

document {
     Key => "exterior power of a module",
	"The ", TT "k","-th exterior power of a module ", TT "M"," is the ", TT "k", "-fold tensor product of ",
	TT "M", " together with the equivalence relation:",
	PRE ///
	m_1 ** m_2 ** .. ** m_k = 0     if m_i = m_j for i != j
	///,
	"If ", TT "M", " is a free ", TT "R", "-module of rank ", TT "n", ", then the ", TT "k", "-th exterior power of ", TT "M",
	" is a free ", TT "R", "-module of rank ", TT "binomial(n,k)", ". Macaulay2 computes the ", TT "k", "-th
	exterior power of a module ", TT "M", " with the command exteriorPower.",
	EXAMPLE {
		"R = ZZ/2[x,y]",
		"exteriorPower(3,R^6)",
		"binomial(6,3)"
		},
	"Macaulay2 can compute exterior powers of modules that are not free as well.",
	EXAMPLE {
		"exteriorPower(2,R^1)",
		"I = module ideal (x,y)",
		"exteriorPower(2,I)"
		},
	SeeAlso => "exterior power of a matrix"
     }

-*
-- Mike wanted this: 
document {
     Key => "Fitting ideals",
     Headline => "empty",
     }
*-

-*
-- Mike wanted this: 
document {
     Key => "adjoints of maps",
     Headline => "empty",
     }
*-

-*
-- Mike wanted this: 
document {
     Key => "Ext and Tor",
     Headline => "empty",
     }
*-

-*
-- Mike wanted this: 
document {
     Key => "local cohomology",
     Headline => "empty",
     }
*-

-*
-- Mike wanted this: 
document {
     Key => "is a module Cohen-Macaulay?",
     Headline => "empty",
     }
*-

-- one link (in this file) to this node
document {
     Key => "homomorphisms (maps) between modules",
     Headline => "including elements of modules",
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
	  }
     }


-- no links to this node
document {
     Key => "extracting elements",
     "If M is an R-module, the best way to think of an element v of M
     in Macaulay2 is as a map of the ring into M, mapping 1 to v."
     }

-- no links to this node
document {
     Key => "equality and containment of modules",
     "==, isSubset"
     }

-- no links to this node
document {
     Key => "minimal presentations and generators",
     "prune, trim"
     }

-- no links to this node
document {
     Key => "information about a map of modules",
     "usual information: source, target, ring.",
     }

-- no links to this node
-* -- Mike wanted this: 
document {
     Key => "kernel, cokernel and image of a map of modules",
     }
*-

-* -- Mike wanted this: 

-- no links to this node
document {
     Key => "extracting parts of a subquotient module",
     "Include: "
     }

-- no links to this node
document {
     Key => "quotients of modules",
     }

-- no links to this node
document {
     Key => "free resolutions",
     }

-- no links to this node
document {
     Key => "Hom module",
     }
*-
