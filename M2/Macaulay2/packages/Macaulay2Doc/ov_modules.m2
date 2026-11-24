-- -*- coding: utf-8 -*-
doc ///
  Key
    "modules"
  Description
    Text
      In Macaulay2, modules are implemented as @TO "subquotient modules"@.
      Submodules and quotients of free modules are perhaps the most common and
      important modules, and subquotients form the smallest class of modules
      that naturally includes these cases.

      See @TO "Tutorial: Modules in Macaulay2"@ for an overview tutorial on
      modules.

      For additional common operations and a comprehensive list of all routines
      in Macaulay2 which return or use modules, see @TO Module@.
  Subnodes
    Module
    module
    isModule
    :construction of modules
    "free modules"
    "matrices to and from modules"
    "making modules from matrices"
    "submodules and quotients"
    "subquotient modules"
    :properties of modules
    "manipulating modules"
    "computing syzygies"
    "extracting elements"
    "minimal presentations and generators"
    "equality and containment of modules"
    :homomorphisms (maps) between modules
    -- TODO: combine these
    "module homomorphisms"
    "maps between modules"
    "information about a map of modules"
    "constructing maps between modules"
    -- Mike wanted this: TO "canonical maps between modules",
    "right modules or left modules?"
    -- Mike wanted this: "operations on modules",
    -- Mike wanted this: TO "direct sums of modules",
    -- Mike wanted this: TO "tensor products of modules",
    -- Mike wanted this: TO "Hom modules and homomorphisms",
    -- Mike wanted this: TO "annihilators and submodule quotients",
    "Saturation :: module quotients, saturation, and annihilator"
    :graded modules
    gradedModule
    "Hilbert functions and free resolutions"
    -- Mike wanted this: TO "degrees of elements and free modules",
    -- Mike wanted this: TO "degree and multiplicity of a module",
    "basis"
    -- Mike wanted this: TO "Hilbert functions and polynomials",
    -- Mike wanted this: TO "homogenization",
    -- Mike wanted this: TO "truncation and homogeneous components of a graded module",
    :multilinear algebra
    "exterior power of a module"
    -- Mike wanted this: TO "Fitting ideals",
    -- Mike wanted this: TO "adjoints of maps",
    -- Mike wanted this: "homological algebra",
    -- Mike wanted this: TO "Ext and Tor",
    -- Mike wanted this: TO "local cohomology",
    -- Mike wanted this: TO "is a module Cohen-Macaulay?"
///

doc ///
  Key
    "right modules or left modules?"
  Description
    Text
      Macaulay2 can handle non-commutative rings, and for such rings there is a
      difference between left modules and right modules. In Macaulay2, all the
      modules are left modules, but matrices act on the left, too. The usual
      convention would be to have the matrices act on the right, so the
      homomorphism rule (@TT "f(av)=af(v)"@) becomes a consequence of
      associativity of matrix-vector-scalar multiplication
      (@TT "(av)f=a(vf)"@). Macaulay2 makes things come out okay in the end --
      a left @TT "R"@-module can be regarded naturally as a right
      @TT "R'"@-module, where @TT "R'"@ is the opposite ring of @TT "R"@,
      obtained from the ring @TT "R"@ by reversing the multiplication. Thus
      matrices over @TT "R'"@ can act on @TT "R"@-modules from the left.
      Matrices over @TT "R"@ in Macaulay2 are @EM "really"@ matrices over
      @TT "R'"@.

      We illustrate this state of affairs with an example over a
      (noncommutative) Weyl algebra. First observe the noncommutativity.
    Example
      R = QQ[x,dx,WeylAlgebra=>{x=>dx}]
      x*dx
      dx*x
    Text
      Now verify the module is a left module by checking associativity.
    Example
      M = R^2
      v = M_0
      dx*v
      x*(dx*v)
      (x*dx)*v
      x*(dx*v) == (x*dx)*v
    Text
      Now make a matrix and check that left multiplication by it is a
      homomorphism from @TT "M"@ to @TT "M"@.
    Example
      f = dx * id_M
      f*(x*v)
      x*(f*v)
      f*(x*v) == x*(f*v)
    Text
      Now we make another matrix and check that matrix multiplication treats
      the entries of the matrices as residing in the opposite ring, @TT "R'"@.
    Example
      g = x * id_M
      f*g
      f*g == (x*dx) * id_M
      (dx * id_M)*(x * id_M) == (x*dx) * id_M
    Text
      Here we check that multiplication of a scalar times a matrix is
      compatible with multiplication of a scalar times a vector.
    Example
      x * ( (dx * id_M) * v )
      (x *  (dx * id_M) ) * v
      (x *  (dx * id_M) ) * v == x * ( (dx * id_M) * v )
    Text
      One desirable associativity rule does @EM "not"@ hold, the one for
      @EM "RingElement * Matrix * Matrix"@, as we see in this example.
    Example
      x * ( id_M * ( dx * id_M ) )
      (x * id_M) * ( dx * id_M )
      x * ( id_M * ( dx * id_M ) ) == (x * id_M) * ( dx * id_M )
    Text
      The reason for this discrepancy is that, as explained above, matrix
      multiplication is done over @TT "R'"@, not over @TT "R"@.

      Currently, tensor product of a module @TT "M"@ by a ring @TT "R"@ works
      on either side and does the same thing. In other words, you can write
      @TT "R**M"@ or @TT "M**R"@. That may change in the future.
///

-------------------
-- module nodes ---
-------------------

doc ///
  Key
    "free modules"
  Description
    Text
      We use @TO (symbol ^, Ring, ZZ)@ to make a new free module.
    Example
      R = ZZ/101[x,y,z];
      M = R^4
    Text
      Such modules are often made implicitly when constructing matrices.
    Example
      m = matrix{{x,y,z},{y,z,0}}
      target m == R^2
    Text
      When a ring is graded, so are its free modules. By default, the degrees
      of the basis elements are taken to be 0.
    Example
      degrees M
    Text
      We can use @TO (symbol ^, Ring, List)@ to specify other degrees, or more
      precisely, their additive inverses.
    Example
      F = R^{1,4:2,3,3:4}
      degrees F
    Text
      Notice the use of @TO ":"@ above to indicate repetition.
      -- Mike wanted this: " as described in ", TO "multi-graded polynomial rings", ",",

      If the variables of the ring have multi-degrees represented by lists
      (vectors) of integers, then the degrees of a free module must also be
      multi-degrees.
    Example
      S = ZZ[a,b,c, Degrees=>{{1,2},{2,0},{3,3}}]
      N = S ^ {{-1,-1},{-4,4},{0,0}}
      degree N_0
      degree (a*b*N_1)
  SeeAlso
    -- Mike wanted this: "multigraded polynomial rings",
    -- "graded modules"
  Subnodes
    (symbol ^, Ring, ZZ)
    (symbol ^, Ring, List)
    isFreeModule
///

doc ///
  Key
    "matrices to and from modules"
  Headline
    including kernel, cokernel and image
  Description
    Text
      @SUBSECTION "matrices to modules (kernel, image, cokernel)"@

      Given a matrix, we may compute the @TO kernel@, @TO image@, and
      @TO cokernel@.
    Example
      R = QQ[a..f];
      F = matrix{{a,b,d,e},{b,c,e,f}}
    Example
      M = ker F
      coker F
      image F
    Text
      Some routines in Macaulay2 have abbreviations, for example @TT "ker"@ may
      be used for @TT "kernel"@, and @TT "coker"@ may be used for
      @TT "cokernel"@. The @TT "image"@ function has no abbreviated form.

      @SUBSECTION "modules to matrices"@

      Each module has, at least implicitly, two matrices associated to it:
      @TO generators@ (abbreviated form: @TT "gens"@), and @TO relations@. If a
      module is a submodule of a free module, then the relations matrix is
      zero. If a module is a quotient of a free module, then the generator
      matrix is the identity matrix. If a module is a @TO "subquotient"@, then
      both may be more general.
    Example
      generators M
      relations M
    Text
      Every finitely generated module has a presentation matrix. In Macaulay2,
      if the module is not a quotient of a free module, then a syzygy
      computation is performed to find a presentation matrix.
    Example
      presentation M
  SeeAlso
    (generators,Module)
    (relations,Module)
    (presentation,Module)
  Subnodes
    (kernel, Matrix)
    (cokernel, Matrix)
    (image, Matrix)
    (generators, Module)
    (relations, Module)
///

doc ///
  Key
    "submodules and quotients"
  Description
    Text
      @SUBSECTION "submodules"@

      We can create submodules by using standard mathematical notation, keeping
      in mind that the generators of a module @TT "M"@ are denoted by
      @TT "M_0, M_1"@, etc.
    Example
      R = QQ[x,y,z];
      M = R^3
      I = ideal(x^2,y^2-x*z)
    Text
      Here are some examples of submodules of @TT "M"@.
    Example
      I*M
      R*M_0
      I*M_1
      J = I*M_1 + R*y^5*M_1 + R*M_2
    Text
      To determine if one submodule is contained in the other, use
      @TO (isSubset, Module, Module)@.
    Example
      isSubset(I*M,M)
      isSubset((x^3-x)*M,x*M)
    Text
      Another way to construct submodules is to take the kernel or image of a
      matrix.
    Example
      F = matrix{{x,y,z}}
      image F
      kernel F
    Text
      The module @TT "M"@ does not need to be a free module. We will see
      examples below.

      @SUBSECTION "quotients"@

      If N is a submodule of M, construct the quotient using
      @TO (symbol/, Module, Module)@.
    Example
      F = R^3
      F/(x*F+y*F+R*F_2)
    Text
      When constructing M/N, it is not necessary that M be a free module, or a
      quotient of a free module. In this case, we obtain a subquotient module,
      which we describe below.
  Subnodes
    isSubmodule
    isQuotientModule
///

doc ///
  Key
    "subquotient modules"
  Headline
    the way Macaulay2 represents modules
  Description
    Text
      Not all modules arise naturally as submodules or quotients of free
      modules. As an example, consider the module $M = I/I^2$ in the example
      below.
    Example
      R = QQ[x,y,z];
      I = ideal(x*y,x*z,y*z)
      M = I/I^2
    Text
      Macaulay2 represents each module (at least conceptually) as a subquotient
      module, that is, a submodule of a quotient of an ambient free module. A
      subquotient module is determined by two matrices $f : R^m \rightarrow{}
      R^n$ and $g : R^p \rightarrow{} R^n$. The {\em subquotient module} with
      generators $f$ and relations $g$ is by definition the module $M = ((image
      f) + (image g))/(image g)$.

      If $f$ is the identity map, $M = coker g$, and if $g = 0$, then $M =
      image f$. The class of subquotient modules is the smallest class
      containing free modules, which is closed under taking submodules and
      quotients.

      One may create a subquotient module directly from matrices f and g having
      the same target free module.
    Example
      f = matrix{{x,y}}
      g = matrix{{x^2,x*y,y^2,z^4}}
      M = subquotient(f,g)
    Text
      The same module can be constructed in the following manner.
    Example
      N = (image f)/(image g)
      N1 = (image f + image g)/(image g)
      M === N
    Text
      Notice that Macaulay2 allows one to write (image f)/(image g), even
      though mathematically this really means: (image f + image g)/(image g).
      There is an important difference however. Modules in Macaulay2 always
      come with an ordered set of generators, and N1 has 4 more generators (all
      zero in the module!) than N. The modules M and N though are identical.

      The two matrices f and g mentioned above are recovered using the routines
      @TO (generators, Module)@ and @TO (relations, Module)@.
    Example
      generators M
      relations M
    Text
      Submodules and quotients of free modules work as one would imagine.
    Example
      N2 = R*M_0 + I*M
      M/N2
      prune(M/N2)
    Text
      Given a subquotient module M, there are several useful modules associated
      to M.The free module of which M is a subquotient is obtained using
      @TO (ambient, Module)@.
    Example
      ambient M
    Text
      This is the same as the common target of the matrices of generators and
      relations.
    Example
      ambient M === target relations M
      ambient M === target generators M
    Text
      M is a submodule of the module R^n/(image g). The routine
      @TO (super, Module)@ returns this quotient module.
    Example
      super M
    Text
      This may be obtained directly as the cokernel of the matrix of relations.
    Example
      super M === cokernel relations M
    Text
      Often the given representation of a module is not very efficient. Use
      @TO (trim, Module)@ to keep the module as a subquotient of the same
      ambient free module, but change the generators and relations to be
      minimal, or in the nonlocal or non-graded case, at least more efficient.
    Example
      M + M
      trim (M+M)
    Text
      Use @TO (minimalPresentation, Module)@ to also allow the ambient free
      module to be improved. This currently returns a quotient of a free
      module, but in the future it might not.
    Example
      minimalPresentation M
    Text
      @TT "prune"@ is a synonym for @TT "minimalPresentation"@.
    Example
      prune M
    Text
      For maps between modules, including between subquotient modules, see
      @TO "module homomorphisms"@.
  SeeAlso
    (ambient,Module)
    (super,Module)
    (generators,Module)
    (relations,Module)
    (trim,Module)
    (minimalPresentation,Module)
  Subnodes
    subquotient
///

doc ///
  Key
    "module homomorphisms"
  Description
    Text
      A homomorphism @TT "f : M --> N"@ is represented as a matrix from the
      generators of M to the generators of N.
    Example
      R = QQ[x,y]/(y^2-x^3);
      M = module ideal(x,y)
    Text
      One homomorphism @TT "F : M --> R"@ is @TT "x |--> y, y |--> x^2"@ (this
      is multiplication by the fraction @TT "y/x"@). We write this in the
      following way.
    Example
      F = map(R^1,M,matrix{{y,x^2}})
    Text
      Notice that as is usual in Macaulay2, the target comes before the source.

      Macaulay2 doesn't display the source and target, unless they are both
      free modules or have been assigned to global variables. Use @TO target@
      and @TO source@ to get them. The @TO matrix@ routine recovers the matrix
      of free modules between the generators of the source and target.
    Example
      source F
      target F == R^1
      matrix F
    Text
      Macaulay2 also does not check that the homomorphism is well defined (i.e.
      the relations of the source map into the relations of the target). Use
      @TO isWellDefined@ to check. This generally requires a Gröbner basis
      computation (which is performed automatically, if it is required and has
      not already been done).
    Example
      isWellDefined F
      isIsomorphism F
    Text
      The image of @TT "F"@ lies in the submodule @TT "M"@ of @TT "R^1"@.
      Suppose we wish to define this new map @TT "G : M --> M"@. How does one
      do this?
      --
      -- -- Mike wanted this:
      -- "  We explain these canonical maps
      -- more thoroughly in ", TO "canonical maps between modules", ",
      -- but for now we just write down the inclusion map.",
      --

      To obtain the map @TT "M --> M"@, we use @TO (symbol//, Matrix, Matrix)@.
      In order to do this, we need the inclusion map of @TT "M"@ into
      @TT "R^1"@.
    Example
      inc = inducedMap(R^1, M)
    Text
      Now we use // to lift @TT "F : M --> R^1"@ along @TT "inc : M --> R^1"@,
      to obtain a map @TT "G : M --> M"@, such that @TT "inc * G == F"@.
    Example
      G = F // inc
      target G == M and source G == M
      inc * G == F
    Text
      Let's make sure that this map @TT "G"@ is well defined.
    Example
      isWellDefined G
      isIsomorphism G
      prune coker G
      kernel G == 0
  SeeAlso
    -- Mike wanted this: "canonical maps between modules",
    (isWellDefined,Matrix)
    (isIsomorphism,Matrix)
    (isInjective,Matrix)
    (isSurjective,Matrix)
    (kernel,Matrix)
    (cokernel,Matrix)
    (symbol//,Matrix,Matrix)
///

-*
-- Mike wanted this: 
document {
     Key => "canonical maps between modules",
     Headline => "empty"
     }
*-

doc ///
  Key
    -- old??
    "constructing maps between modules"
  Description
    Text
      Let's start with a free module.
    Example
      R = ZZ/5[x,y,z];
      F = R^3
    Text
      A list of indices can be used to produce homomorphisms corresponding to
      the corresponding basis vectors.
    Example
      F_{0,1,2}
      F_{0,1}
      F_{1,2}
    Text
      Matrices are viewed as linear transformations.
    Example
      f = matrix{{x,y,z}}
      --     "The standard way to define a map from an R-module M to an
      --     R-module N is to give a matrix whose columns are the image vectors
      --     of the generators of M.",
      --     EXAMPLE {
      --	  "R = QQ[x,y,z];",
      --	  "m = cokernel vars R",
      --	  "--F = map(m/m^2, R^1/m, {{x*y*z}})"
      --	  }
///

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


doc ///
  Key
    "Hilbert functions and free resolutions"
  Headline
    including degree and betti numbers
  Description
    Text
      In this section, we give examples of common operations involving modules.
      Throughout this section, we suppose that the base ring @TT "R"@ is
      graded, with each variable having degree one, and that @TT "M"@ is a
      graded @TT "R"@-module. If the ring is not graded, or is multi-graded, or
      if @TT "M"@ is not graded, some of these functions still work, but care
      must be taken in interpreting the output. Here, we just consider the
      standard grading case.

      @SUBSECTION "checking homogeneity"@

      Let's start by making a module over a ring with 18 variables
    Example
      R = ZZ/32003[vars(0..17)];
      M = coker genericMatrix(R,a,3,6)
    Text
      Use @TO "isHomogeneous"@ to check whether a given module is graded.
    Example
      isHomogeneous M
    Text
      @SUBSECTION "codimension, degree, and sectional arithmetic genera"@

      Use @TO (codim, Module)@, @TO (degree, Module)@, and
      @TO "Varieties::genera(Module)"@ for some basic numeric information about
      a module.
    Example
      codim M
      degree M
      genera M
    Text
      The last number in the list of genera is the degree minus one. The second
      to last number is the genus of the generic linear section curve, ..., and
      the first number is the arithmetic genus.

      @SUBSECTION "the Hilbert series"@

      The Hilbert series (@TO (hilbertSeries, Module)@) of @TT "M"@ is by
      definition the formal power series
      @TT "H(t) = sum(d in ZZ) dim(M_d) t^d"@. This is a rational function with
      denominator @TT "(1-t)^n"@, where @TT "n"@ is the number of variables in
      the polynomial ring. The numerator of this rational function is called
      the poincare polynomial, and is obtained by the @TO (poincare, Module)@
      function.
    Example
      poincare M
      hf = hilbertSeries M
    Text
      It is often useful to divide the poincare polynomial by @TT "(1-t)"@ as
      many times as possible. This can be done by using @TO reduceHilbert@:
    Example
      reduceHilbert hf
    Example
      poincare' = (M) -> (
              H := poincare M;
              t := (ring H)_0;  -- The variable t above
              while H % (1-t) == 0 do H = H // (1-t);
              H);
      poincare' M
    Text
      @SUBSECTION "free resolutions"@

      The minimal free resolution @TT "C"@ is computed using
      @TO "OldChainComplexes :: resolution(Module)"@. The specific matrices are
      obtained by indexing @TT "C.dd"@.
    Example
      C = resolution M
      C.dd_3
    Text
      For more information about chain complexes and resolutions, see
      @TO "Complexes :: Complexes"@ and
      @TO "OldChainComplexes :: computing resolutions"@.

      @SUBSECTION "betti numbers"@

      Use @TO betti@ to display the graded betti numbers of @TT "M"@.
    Example
      betti C
    Text
      This table should be interpreted as follows: the number in the
      @TT "i"@-th row and @TT "j"@-th column (indices starting at 0), is the
      number of @TT "j"@-th syzygies in degree @TT "i+j"@. In the above
      example, there are 15 second syzygies of degree 4, and the entries of the
      maps @TT "CC.d_1, CC.d_3, CC.d_4"@ are all linear.
  Subnodes
    isHomogeneous
    (codim, Module)
    (degree, Module)
    "Varieties::genera(Module)"
    (hilbertSeries, Module)
    poincare
    poincareN
    reduceHilbert
    betti
///

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

doc ///
  Key
    "exterior power of a module"
  Description
    Text
      The @TT "k"@-th exterior power of a module @TT "M"@ is the @TT "k"@-fold
      tensor product of @TT "M"@ together with the equivalence relation:$m_1
      \otimes m_2 \otimes \cdots \otimes m_k = 0$ if $m_i = m_j$ for $i != j$If
      @TT "M"@ is a free @TT "R"@-module of rank @TT "n"@, then the @TT "k"@-th
      exterior power of @TT "M"@ is a free @TT "R"@-module of rank
      @TT "binomial(n,k)"@. Macaulay2 computes the @TT "k"@-th exterior power
      of a module @TT "M"@ with the command exteriorPower.
    Example
      R = ZZ/2[x,y]
      exteriorPower(3,R^6)
      binomial(6,3)
    Text
      Macaulay2 can compute exterior powers of modules that are not free as
      well.
    Example
      exteriorPower(2,R^1)
      I = module ideal (x,y)
      exteriorPower(2,I)
  SeeAlso
    "exterior power of a matrix"
///

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

-- no links to this node
doc ///
  Key
    "extracting elements"
  Description
    Text
      If M is an R-module, the best way to think of an element v of M in
      Macaulay2 is as a map of the ring into M, mapping 1 to v.
///

-- no links to this node
doc ///
  Key
    "equality and containment of modules"
    (symbol ==, Module, Module)
  Description
    Text
      ==, isSubset
///

-- no links to this node
doc ///
  Key
    "minimal presentations and generators"
  Description
    Text
      prune, trim
///

-- no links to this node
doc ///
  Key
    "information about a map of modules"
  Description
    Text
      usual information: source, target, ring.
///

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
doc ///
  Key
    "making modules from matrices"
  Description
    Text
      Let's make some matrices.
    Example
      R = ZZ/101[a..c];
      f = vars R
    Text
      We can easily compute a @TO "kernel"@, @TO "image"@ or @TT "cokernel"@.
    Example
      ker f
      coker f
      image f
    Text
      Every module is represented internally as a submodule of a quotient
      module. Such modules often appear in computations, for example, when
      taking the direct sum of a quotient module and a submodule.
    Example
      image f ++ coker f
    Text
      We may use @TO "subquotient"@ to make such modules directly, although
      it's usually more convenient to use other operations. The first argument
      is the matrix whose columns are the generators, and the second argument
      is the matrix whose columns are the relations.
    Example
      M = subquotient(f, matrix {{a}})
      prune M
///

doc ///
  Key
    "manipulating modules"
  Description
    Text
      Suppose we have a module that is represented as an image of a matrix, and
      we want to represent it as a cokernel of a matrix. This task may be
      accomplished with @TO "prune"@.
    Example
      R = QQ[x,y];
      I = ideal vars R
      M = image vars R
      N = prune M
    Text
      The isomorphism between them may be found under the key
      @TT "pruningMap"@.
    Example
      f = N.cache.pruningMap
      isIsomorphism f
      f^-1
    Text
      The matrix form of @TT "f"@ looks nondescript, but the map knows its
      source and target
    Example
      source f
      target f
    Text
      It's a 2 by 2 matrix because @TT "M"@ and @TT "N"@ are both represented
      as modules with two generators.

      Functions for finding related modules:

      @UL {TO "ambient", TO "cover", TO "super"}@
    Example
      super M
      cover N
    Text
      Some simple operations on modules:

      @UL {
          TO (symbol ^, Module, ZZ),
          TO (symbol ++, Module, Module),
          TO (symbol **, Module, Module)
          }@
    Example
      M ++ N
      M ** N
    Text
      Ideals and modules behave differently when making powers:
    Example
      M^3
      I^3
///

doc ///
  Key
    "maps between modules"
    -- map
  Description
    Text
      Maps between free modules are usually specified as matrices, as described
      in the section on @TO "matrices"@. In this section we cover a few other
      techniques.

      Let's set up a ring, a matrix, and a free module.
    Example
      R = ZZ/101[x,y,z];
      f = vars R
      M = R^4
    Text
      We can use @TO (symbol ^, Module, List)@ and
      @TO (symbol _, Module, List)@ to produce projection maps to quotient
      modules and injection maps from submodules corresponding to specified
      basis vectors.
    Example
      M^{0,1}
      M_{2,3}
    Text
      Natural maps between modules can be obtained with @TO "inducedMap"@; the
      first argument is the desired target, and the second is the source.
    Example
      inducedMap(source f, ker f)
      inducedMap(coker f, target f)
///

doc ///
  Key
    "computing syzygies"
  Description
    Text
      A syzygy among the columns of a matrix is, by definition, an element of
      the kernel of the corresponding map between free modules, and the easiest
      way to compute the syzygies applying the function @TO "kernel"@.
    Example
      R = QQ[x..z];
      f = vars R
      K = kernel f
    Text
      The answer is provided as a submodule of the source of @TT "f"@. The
      function @TO "super"@ can be used to produce the module that @TT "K"@ is
      a submodule of; indeed, this works for any module.
    Example
      L = super K
      L == source f
    Text
      The matrix whose columns are the generators of @TT "K"@, lifted to the
      ambient free module of @TT "L"@ if necessary, can be obtained with the
      function @TO "generators"@, an abbreviation for which is @TT "gens"@.
    Example
      g = generators K
    Text
      We can check at least that the columns of @TT "g"@ are syzygies of the
      columns of @TT "f"@ by checking that @TT "f*g"@ is zero.
    Example
      f*g
      f*g == 0
    Text
      Use the function @TO "syz"@ if you need detailed control over the extent
      of the computation.
///
