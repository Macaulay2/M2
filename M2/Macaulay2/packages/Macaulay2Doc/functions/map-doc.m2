--- status: TODO
--- author(s): 
--- notes: 

undocumented {(map, RingFamily, Thing, Thing),(map, Thing, RingFamily, Thing),
    (map, RingFamily, Thing), (map, Thing, RingFamily)}

doc ///
  Key
    map
    "morphisms"
  Headline
    make a map
  Usage
    map(Y,X,d) or map(Y,X)
  Inputs
    Y:
      an object, such as a ring, module, or chain complex
    X:
      another object of the same type
    d:
      a specification, such as a function, list, or matrix, or if omitted,
      understood to specify the identity map
  Outputs
    :
      a map to @TT "Y"@ from @TT "X"@ defined by data @TT "d"@.
  Description
    Text
      The function @TT "map"@ provides a general mechanism for constructing a
      map (homomorphism) between rings (@ofClass RingMap@), modules
      (@ofClass Matrix@), or between objects in other categories.

      See also the function @TO matrix@, which focuses on creating new matrices
      from rectangular arrays of ring elements or matrices.
  Subnodes
    :Getting information about a map
    source
    target
    :Creating a map (matrix) between modules
    (map,Module,Module,Function)
    (map,Module,Module,List)
    (map,Module,Module,RingElement)
    (map,Module,Module,Matrix)
    :Creating a map between modules, where the source module is computed
    (map,Module,ZZ,Function)
    (map,Module,Nothing,List)
    (map,Module,ZZ,List)
    (map,Module,Nothing,Matrix)
    :Creating a map with a different degree
    (map,Matrix)
    :Creating a map between modules over different rings
    (map,Module,Module,RingMap,Matrix)
    :Creating a map between rings
    "ConwayPolynomials :: map(GaloisField,GaloisField)"
    (map,Ring,Ring)
    (map,Ring,Ring,List)
    (map,Ring,Ring,Matrix)
    -- "Creating a map between chain complexes",
    -- TO "Complexes :: map(Complex,Complex,ComplexMap)",
    -- TO "Complexes :: map(Complex,Complex,Function)",
    -- TO "Complexes :: map(Complex,Complex,HashTable)",
    -- TO "Complexes :: map(Complex,Complex,List)",
    -- TO "Complexes :: map(Complex,Complex,ZZ)",
    :The Hom and End modules
    Hom
    End
    -- TODO: also add mention inducedMap, etc.
///
doc ///
  Key
    (map,Module,Module,Function)
  Headline
    create a matrix by specifying a function that gives each entry
  Usage
    map(M,N,f)
  Inputs
    M:
    N:
    f:
  Outputs
    :
      a map from the module @TT "N"@ to the module @TT "M"@ whose matrix entry
      @TT "h_(i,j)"@ is obtained from the function @TT "f"@ by evaluating
      @TT "f(i,j)"@.
  Description
    Text
      Recall that all indices in Macaulay2 start at 0, so the upper left-most
      entry of a matrix @TT "f"@ is @TT "f_(0,0)"@.

      This function is often used when you already know the source and target
      modules, including their gradings. If you wish Macaulay2 to compute the
      column degrees for you (so the resulting matrix will be homogeneous if
      possible), use @TO (map, Module, ZZ, Function)@.
    Example
      R = ZZ[a..c];
      f = map(R^3,R^{0,-1,-2,-3},(i,j) -> R_i^j)
    Text
      We specified the degrees of the source basis elements explicitly to
      ensure the matrix would be homogeneous.
    Example
      isHomogeneous f
    Text
      @SUBSECTION "Alternate approaches"@

      We could have let Macaulay2 take care of that for us, by replacing the
      source module by its desired rank.
    Example
      g = map(R^3,4,(i,j) -> R_i^j)
      degrees g
      isHomogeneous g
    Text
      Another way would be to let @TO "matrix"@ take care of that for us.
    Example
      h = matrix table(3,4,(i,j) -> R_i^j)
      degrees h
      isHomogeneous h
///
doc ///
  Key
    (map,Module,Module,List)
  Headline
    create a matrix by giving a sparse or dense list of entries
  Usage
    map(M,N,v)
  Inputs
    M:
    N:
    v:
  Outputs
    :
      A matrix @TT "M <-- N"@ whose entries are obtained from @TT "v"@
  Description
    Text
      The list @TT "v"@ is either a doubly nested list of ring elements, or a
      list of elements @TT "(i,j) => f"@. The first version provides all of the
      elements of the output matrix, row by row. The second form provides only
      the non-zero elements of the output matrix @TT "h: h_(i,j) = f"@, for
      every @TT "(i,j) => f"@ in the list @TT "v"@.

      In each case, the modules @TT "M"@ and @TT "N"@ should have the same base
      ring @TT "R"@, and the ring elements appearing in @TT "v"@ should be over
      @TT "R"@, or over a base ring of @TT "R"@.

      In the first form, each list in v gives a row of the matrix. The length
      of the list @TT "v"@ should be the number of generators of @TT "M"@, and
      the length of each element of @TT "v"@ (which is itself a list of ring
      elements) should be the number of generators of the source module
      @TT "N"@.
    Example
      R = ZZ/101[x,y,z];
      p = map(R^2,R^{-2,-2,0},{{x^2,0,3},{0,y^2,5}})
      isHomogeneous p
    Text
      In the second form, if an index (i,j) occurs more than once, only the
      last is taken.
    Example
      p = map(R^2,R^3,{(0,0) => x+y, (1,1) => x^2, (0,2) => x-1, (0,0) => x-y})
  SeeAlso
    matrix
    (map,Module,Nothing,List)
    "inputting a matrix"
///
doc ///
  Key
    (map,Module,Module,Matrix)
  Headline
    create the matrix induced on generators by a given matrix
  Usage
    map(M,N,p)
  Inputs
    M:
    N:
    p:
  Outputs
    :
      A matrix with the same entries as @TT "p"@, but whose target is @TT "M"@
      and source is @TT "N"@
  Description
    Text
      @TT "M"@ and @TT "N"@ should be modules over the same ring, and have the
      same number of generators as @TT "target p"@ and @TT "source p"@,
      respectively.
    Example
      R = QQ[x,y,z];
      p = matrix {{x,y,z}}
      q = map(R^1,R^3,p)
      degrees source p
      degrees source q
  Caveat
    If @TT "M"@ or @TT "N"@ is not free, then we do not check that the result
    is a well defined homomorphism.
  SeeAlso
    inducedMap
///

doc ///
  Key
    (map,Module,Module,RingElement)
    (map,Module,Module,Number)
    (map,Module,ZZ,ZZ)
    (map,Module,Module,ZZ)
  Headline
    construct the map induced by multiplication by a ring element on the generators
  Usage
    map(M,N,r)
  Inputs
    M:
    N:
      over the same ring @TT "R"@ as @TT "M"@. An integer here stands for the
      free module of that rank.
    r:
      in the ring @TT "R"@
  Outputs
    :
      The map induced by multiplication by @TT "r"@ on the generators
  Description
    Text
      If @TT "r"@ is not zero, then either @TT "M"@ and @TT "N"@ should be
      equal, or they should have the same number of generators. This gives the
      same map as r * map(M,N,1). map(M,N,1) is the map induced by the identity
      on the generators of M and N.
    Example
      R = QQ[x];
      map(R^2,R^3,0)
      f = map(R^2,R^2,x)
      f == x *map(R^2,R^2,1)
  Caveat
    If @TT "M"@ or @TT "N"@ is not free, then we do not check that the result
    is a well defined homomorphism.
  SeeAlso
    inducedMap
///
doc ///
  Key
    (map,Matrix)
  Headline
    make a matrix with a different degree
  Usage
    map(f, Degree => d)
  Inputs
    f:Matrix
  Outputs
    :
      a map identical to @TT "f"@, except that it has degree @TT "d"@, and the
      source module has been tensored by a graded free module of rank 1 of the
      appropriate degree.
  Description
    Text
      The input @TT "d"@ should be @ofClass ZZ@, or a list of integers.

      This routine is often used to take a matrix that has a non-zero degree,
      and make the degree zero.

      For example, multiplication of a matrix by a scalar increases the degree,
      leaving the source and target fixed:
    Example
      R = QQ[a,b];
      f1 = matrix{{a,b}}
      f = a * f1
      degree f
      source f == source f1
    Text
      One solution is to change the degree:
    Example
      g = map(f, Degree => 0)
      degree g
      source g == (source f) ** R^{-1}
    Text
      An alternate solution would be to use tensor product with the scalar.
    Example
      g2 = a ** matrix{{a,b}}
      degree g2
      isHomogeneous g2
  Subnodes
    [map, Degree]
///
doc ///
  Key
    (map,Module,Nothing,Matrix)
  Headline
    recast a matrix to have a new target, and a free module as source
  Usage
    map(M,,f)
  Inputs
    M:
    f:
      whose target has the same number of generators as @TT "M"@
  Outputs
    :
      A map with a free source module, and target @TT "M"@, whose entries are
      those of f.
  Description
    Example
      R = ZZ/101[x,y];
      p = matrix{{x,y}}
      q = map(R^{3},,p)
      degrees target q
      degrees source q
  SeeAlso
    matrix
///
doc ///
  Key
    (map,Module,ZZ,Function)
  Headline
    create a matrix from a free module by specifying a function that gives each entry
  Usage
    map(M,n,f)
  Inputs
    M:
    n:
    f:
  Outputs
    :
      a map from a graded free module of rank @TT "n"@ to the module @TT "M"@
      whose matrix entry @TT "h_(i,j)"@ is obtained from the function @TT "f"@
      by evaluating @TT "f(i,j)"@.
  Description
    Text
      This is the same as calling map(M,R^n,f), except that the degrees of the
      basis elements of the source module are chosen in an attempt to ensure
      that the resulting map is homogeneous of degree zero.
    Example
      R = GF(9,Variable=>a)[x,y,z];
      f = map(R^1, 3, (i,j) -> (a^j * x - y)^(j+1))
      source f
      isHomogeneous f
  SeeAlso
    (map,Module,Module,Function)
    (source,Matrix)
    (isHomogeneous,Matrix)
///
doc ///
  Key
    (map,Module,ZZ,List)
  Headline
    create a matrix by giving a sparse or dense list of entries
  Usage
    map(M,n,v)
  Inputs
    M:
    n:
    v:
  Outputs
    :
      A matrix @TT "M <-- R^n"@ whose entries are obtained from @TT "v"@, where
      R is the ring of M, and the source of the result is a graded free module
      chosen in an attempt to make the result homogeneous of degree zero
  Description
    Text
      The list @TT "v"@ is either a doubly nested list of ring elements, or a
      list of elements @TT "(i,j) => f"@. The first version provides all of the
      elements of the output matrix, row by row. The second form provides only
      the non-zero elements of the output matrix @TT "h: h_(i,j) = f"@, for
      every @TT "(i,j) => f"@ in the list @TT "v"@.

      The ring elements appearing in @TT "v"@ should be in @TT "R"@, or in a
      base ring of @TT "R"@.

      In the first form, each list in v gives a row of the matrix. The length
      of the list @TT "v"@ should be the number of generators of @TT "M"@, and
      the length of each element of @TT "v"@ (which is itself a list of ring
      elements) should be the number of generators of the source module
      @TT "N"@.
    Example
      R = ZZ/101[x,y,z];
      p = map(R^2,3,{{x^2,0,3},{0,y^2,5}})
      isHomogeneous p
    Text
      In the second form, if an index (i,j) occurs more than once, only the
      last is taken.
    Example
      p = map(R^2,3,{(0,0) => x+y, (1,1) => x^2, (0,2) => x-1, (0,0) => x-y})
  SeeAlso
    matrix
    -- Mike wanted this: , "input a matrix"
    (map,Module,Nothing,List)
///
doc ///
  Key
    (map,Module,Nothing,List)
  Headline
    create a matrix by giving a doubly nested list of ring elements
  Usage
    map(M,v)
  Inputs
    M:
    v:
  Outputs
    :
      A matrix @TT "M <-- R^n"@ whose entries are obtained from @TT "v"@, where
      R is the ring of M, and the source of the result is a graded free module
      chosen in an attempt to make the result homogeneous of degree zero
  Description
    Text
      The list @TT "v"@ must be a doubly nested list of ring elements, which
      are used to fill the matrix, row by row.

      The ring elements appearing in @TT "v"@ should be in @TT "R"@, or in a
      base ring of @TT "R"@.

      Each list in v gives a row of the matrix. The length of the list @TT "v"@
      should be the number of generators of @TT "M"@, and the length of each
      element of @TT "v"@ (which is itself a list of ring elements) should be
      the number of generators of the source module @TT "N"@.
    Example
      R = ZZ/101[x,y,z]
      p = map(R^2,,{{x^2,0,3},{0,y^2,5}})
      isHomogeneous p
    Text
      Another way is to use the @TO (matrix, List)@ routine:
    Example
      p = matrix {{x^2,0,3},{0,y^2,5}}
    Text
      The absence of the second argument indicates that the source of the map
      is to be a free module constructed with an attempt made to assign degrees
      to its basis elements so as to make the map homogeneous of degree zero.
    Example
      R = ZZ/101[x,y]
      f = map(R^2,,{{x^2,y^2},{x*y,0}})
      degrees source f
      isHomogeneous f
  SeeAlso
    matrix
    -- Mike wanted this: , "input a matrix"
    (map,Module,Module,List)
///
doc ///
  Key
    (map,Ring,Ring,Matrix)
    [map,DegreeLift]
    [map,DegreeMap]
    (map,Ring,Ring,RingMap)
  Headline
    make a ring map
  Usage
    map(R,S,m)
  Inputs
    R:
      the target ring
    S:
      the source ring
    m:
      a @TT "1"@ by @TT "n"@ matrix over @TT "R"@, where @TT "n"@ is the number
      of variables in the polynomial ring @TT "S"@, or a matrix over the common
      coefficient ring of the two rings. If a ring map is used here, (just) its
      matrix will be used instead.
    DegreeMap => Function
      the degree map: a (linear) function from the multidegrees of @TT "S"@ to
      the multidegrees of @TT "R"@, to be used in determining homogeneity and
      in determining degrees in tensor products. If the two rings have the same
      degree length, then the default degree map is the identity function.
    DegreeLift => Function
      the degree lift function: a (partial) inverse of the degree map, giving
      an error when lifting is not possible. If the degree map is the identity,
      then by default the identity map will be provided. If the degree length
      of @TT "R"@ is 0, then by default a suitable degree lift function will be
      provided.
  Outputs
    :
      the ring homomorphism from @TT "S"@ to @TT "R"@ which, in case m is a
      matrix over R, sends the i-th variable of @TT "S"@ to the i-th entry in
      @TT "m"@, or, in case @TT "m"@ is a matrix over the common coefficient
      ring, is the linear change of coordinates corresponding to @TT "m"@
  Description
    Example
      R = ZZ[x,y];
      S = ZZ[a,b,c];
      f = map(R,S,matrix {{x^2,x*y,y^2}})
      f(a+b+c^2)
      g = map(R,S,matrix {{1,2,3},{4,5,6}})
      g(a+b)
    Text
      If the coefficient ring of @TT "S"@ is itself a polynomial ring, then one
      may optionally include values to which its variables should be sent: they
      should appear last in the matrix @TT "m"@.
    Example
      S = ZZ[a][b,c];
      h = map(S,S,matrix {{b,c,2*a}})
      h(a^7 + b^3 + c)
      k = map(S,S,matrix {{c,b}})
      k(a^7 + b^3 + c)
    Text
      Specifying a degree map is a useful way to preserve homogeneity, which
      can speed computation.
    Example
      R = QQ[x,y,z];
      S = QQ[t,u];
      f = map(S,R,{t^2,t*u,u^2},DegreeMap => i -> 2*i)
      isHomogeneous f
      M = R^{1,2}
      f M
      f ** M
  SeeAlso
    "substitution and maps between rings"
///
doc ///
  Key
    (map,Ring,Ring,List)
  Headline
    make a ring map
  Usage
    map(R,S,m)
  Inputs
    R:
      the target ring
    S:
      the source ring
    m:
      of @TT "n"@ elements of @TT "R"@, where @TT "n"@ is the number of
      variables in the polynomial ring @TT "S"@, or a list of pairs
      @TT "x => r"@, where @TT "x"@ is a generator of @TT "S"@ and @TT "r"@ is
      an element of @TT "R"@, specifying that @TT "x"@ is to be sent to
      @TT "r"@.
  Outputs
    :
      the ring homomorphism from @TT "S"@ to @TT "R"@ which sends the
      @TT "i"@-th variable of @TT "S"@ to the @TT "i"@-th entry in @TT "m"@, or
      in the second case, performs the indicated substitutions.
  Description
    Example
      R = ZZ[x,y];
      S = ZZ[a,b,c];
      f = map(R,S,{x^2,x*y,y^2})
      f(a+b+c^2)
      g = map(R,S,{a=>x^2,b=>x*y,c=>y^2})
      g(a+b+c^2)
  SeeAlso
    "substitution and maps between rings"
    (map,Ring,Ring,Matrix)
///

doc ///
  Key
    (map,Module)
  Headline
    identity map
  Usage
    map M
  Inputs
    M:
  Outputs
    :Matrix
      the identity map on @TT "M"@
  Description
    Example
      f = map ZZ^3
      g = id_(ZZ^3)
      f === g
  Caveat
    may be removed
  SeeAlso
    (id,Module)
///
doc ///
  Key
    (map,Ring,Ring)
  Headline
    make a ring map, using the names of the variables
  Usage
    map(R,S)
  Inputs
    R:
    S:
  Outputs
    :RingMap
      a map S --> R which maps any variable of S to a variable with the same
      name in R, if any, and zero otherwise
  Description
    Text
      For example, consider the following rings.
    Example
      A = QQ[a..e];
      B = A[x,y,Join=>false];
      C = QQ[a..e,x,y];
    Text
      The natural inclusion and projection maps between @TT "A"@ and @TT "B"@
      are
    Example
      map(B,A)
      map(A,B)
    Text
      The isomorphisms between B and C:
    Example
      F = map(B,C)
      G = map(C,B)
      F*G
      oo === id_B
      G*F
      oo === id_C
    Text
      The ring maps that are created are not always mathematically
      well-defined. For example, the map F below is the natural quotient map,
      but the map @TT "G"@ is not mathematically well-defined, although we can
      use it in Macaulay2 to lift elements of @TT "E"@ to @TT "D"@.
    Example
      D = QQ[x,y,z];
      E = D/(x^2-z-1,y);
      F = map(E,D)
      G = map(D,E)
      x^3
      G x^3
  Caveat
    The map is not always a mathematically well-defined ring map
  SeeAlso
    "substitution and maps between rings"
///
doc ///
  Key
    (map,Ring,Matrix)
  Headline
    make a ring map
  Usage
    map(R,m)
  Inputs
    R:
    m:
      a one row matrix
  Outputs
    :RingMap
      A ring map @TT "f : R --> (ring m)"@, where the entries of @TT "m"@ are
      the images of the variables of @TT "R"@.
  Description
    Text
      This is equivalent to @TT "map(ring m, R, m)"@.
    Example
      R = QQ[a..d];
      S = QQ[s,t];
      F = map(R,matrix{{s^4,s^3*t,s*t^3,t^4}})
      kernel F
  SeeAlso
    "substitution and maps between rings"
    (map,Ring,Ring,Matrix)
///
doc ///
  Key
    [map, Degree]
  Headline
    specify the degree of a map
  Usage
    map(..., Degree=>d)
  Description
    Text
      Specifies that the degree of the map created should be @TT "d"@. The
      degree may be an integer or a list of integers (multidegree). The length
      of the list should be the same as the length of a degree for the ring,
      see @TO "degreeLength"@.
    Example
      R = ZZ/101[x]
      p = map(R^1, R^1, {{x^4}})
      isHomogeneous p
      q = map(R^1, R^1, {{x^4}}, Degree => 4)
      isHomogeneous q
  SeeAlso
    map
    matrix
    [inducedMap, Degree]
    [matrix, Degree]
///
