newPackage(
        "Truncations",
        Version => "0.7", 
        Date => "31 Dec 2018",
        Authors => {
            {
                Name => "David Eisenbud", 
                Email => "de@msri.org", 
                HomePage => "http://www.msri.org/~de"},
            {
                Name => "Mike Stillman", 
                Email => "mike@math.cornell.edu", 
                HomePage=>"http://www.math.cornell.edu/~mike"}
            },
        Headline => "truncation of a module",
	Keywords => {"Commutative Algebra"},
        PackageImports => {"Polyhedra"}
        )

-- we install methods on "truncate", from the Core

protect Exterior

truncationPolyhedron = method(Options=>{Exterior => {}})
  -- Exterior should be a list of variable indices which are skew commutative.
  -- i.e. have max degree 1.
truncationPolyhedron(Matrix, List) := Polyhedron => opts -> (A, b) -> (
    truncationPolyhedron(A, transpose matrix{b}, opts)
    )
truncationPolyhedron(Matrix, Matrix) := Polyhedron => opts -> (A, b) -> (
    -- assumption: A is m x n. b is m x 1.
    -- returns the polyhedron {Ax >= b, x >=0}
    if ring A === ZZ then A = A ** QQ;
    if ring A =!= QQ then error "expected matrix over ZZ or QQ";
    I := id_(source A);
    z := map(source A, QQ^1, 0);
    hdataLHS := -(A || I);
    hdataRHS := -(b || z);
    if #opts.Exterior > 0 then (
        -- also need to add in the conditions that each variable in the list is <= 1.
        ones := matrix toList(#opts.Exterior : {1_QQ});
        hdataLHS = hdataLHS || (I ^ (opts.Exterior));
        hdataRHS = hdataRHS || ones;
        --polyhedronFromHData(-(A || I) || I, -(b || z) || ones)
        );
    polyhedronFromHData(hdataLHS, hdataRHS)
    )

-- basisPolyhedron: this function is not used nor tested here.  It can be used for a
-- perhaps better implementation of 'basis'.
-- BUT: it should have exterior variables added too...
basisPolyhedron = method()
basisPolyhedron(Matrix,List) := (A,b) -> (
    basisPolyhedron(A, transpose matrix{b})
    )
basisPolyhedron(Matrix,Matrix) := (A,b) -> (
    -- assumption: A is m x n. b is m x 1.
    -- returns the polyhedron {Ax = b, x >=0}
    if ring A === ZZ then A = A ** QQ;
    if ring A =!= QQ then error "expected matrix over ZZ or QQ";
    I := id_(source A);
    z := map(source A, QQ^1, 0);
    polyhedronFromHData(-I, -z, A, b)
    )

TEST ///
-*
  restart
*-  
  needsPackage "Truncations"
  E = ZZ/101[a..f, SkewCommutative=>{0,2,4}, Degrees=> {2:{3,1},2:{4,-2},2:{1,3}}]
  elapsedTime truncate({7,1},E)

  A = transpose matrix degrees E  
  debug needsPackage "Truncations"  
  elapsedTime numgens truncationMonomials({7,1},E) == 28
  
  P = truncationPolyhedron(A,{7,1},Exterior => (options E).SkewCommutative)
  P1 = truncationPolyhedron(A,{7,1})  

  needsPackage "Polyhedra"
  elapsedTime halfspaces P
  elapsedTime # hilbertBasis cone P == 1321
  elapsedTime # hilbertBasis cone P1 == 1851
///

-- checkOrMakeDegreeList: takes a degree, and degree rank:ZZ.
-- output: a list of lists of degrees, all of the correct length (degree rank).
--  if it cannot translate the degree(s), an error is issued.
-- in the following list: n represents an integer, and d represents a list of integers.
-- n --> {{n}}  (if degree rank is 1)
-- {n0,...,ns} --> {{n0,...,ns}} (if the length is degree rank).
-- {d0,...,ds} --> {d0,...,ds} (no change, assuming length of each di is the degree rank).
-- an error is provided in any other case.
checkOrMakeDegreeList = method()
checkOrMakeDegreeList(ZZ, ZZ) := (d,degrank) -> (
    if degrank =!= 1 then
        error("expected degree to be of length "|degrank) ;
    {{d}}
    )
checkOrMakeDegreeList(List, ZZ) := (L, degrank) -> (
    if #L === 0 then error "expected non empty list of degrees";
    if all(L, d -> instance(d, ZZ)) then (
        if #L =!= degrank then error("expected a degree of length "|degrank);
        {L}
        )
    else (
        -- all elements of L should be a list of list of integers,
        -- all the same length, and L will be returned.
        if any(L, deg -> not instance(deg, BasicList) 
                         or not all(deg, d -> instance(d,ZZ)) 
                         or #deg =!= degrank)
          then error("expected a list of lists of integers, each of length "|degrank);
        L
        )
    )

-- whether truncation is implemented for this ring type.
truncateImplemented = method()
truncateImplemented Ring := Boolean => R -> (
    (R1, phi1) := flattenRing R;
    A := ambient R1;
    isAffineRing A 
    or
    isPolynomialRing A and isAffineRing coefficientRing A and A.?SkewCommutative
    or
    isPolynomialRing A and ZZ === coefficientRing A
    or
    ZZ === A
    )

TEST ///
-*
  restart
*-
  debug needsPackage "Truncations"
  assert(checkOrMakeDegreeList(3, 1) == {{3}})
  assert(checkOrMakeDegreeList({3}, 1) == {{3}})
  assert try checkOrMakeDegreeList(3, 2) else true 
  assert(checkOrMakeDegreeList({1,2}, 2) === {{1,2}})
  assert try checkOrMakeDegreeList({1,2,3}, 2) else true
  assert(checkOrMakeDegreeList({{1,0},{3,-5}}, 2) === {{1,0},{3,-5}})
  assert try checkOrMakeDegreeList({{1,0},{3,-5},{3,4,5}}, 2) else true
  assert try checkOrMakeDegreeList({{1,0},{3,-5},3}, 2) else true
///

TEST ///
  -- of truncateImplemented
-*
  restart
*-
  debug needsPackage "Truncations"
  R1 = ZZ[a,b,c]
  R2 = R1/(3*a,5*b)
  R3 = R2[s,t]
  R4 = QQ[x,y,z]
  truncateImplemented R1
  truncateImplemented R2
  truncateImplemented R3
  truncateImplemented R4
  
  E1 = ZZ[a,b,c,SkewCommutative=>true]
  E2 = E1/(a*b)
  E3 = ZZ[d,e,f,SkewCommutative=>{0,2}]
  assert((options E3).SkewCommutative == {0,2})
  truncateImplemented E1
  truncateImplemented E2
  truncateImplemented E3
  truncateImplemented (E1[x,y])
  truncateImplemented (E1[x,y,SkewCommutative=>true])
///

truncationMonomials = method()
truncationMonomials(List, Ring) := (degs, R) -> (
    degs = checkOrMakeDegreeList(degs, degreeLength R);
    if #degs > 1 then
        return trim sum for d in degs list truncationMonomials(d, R);
    d := degs#0;
    if not R#?(symbol truncate, d) then R#(symbol truncate, d) = (
      (R1, phi1) := flattenRing R;
      A := transpose matrix degrees R1;
      P := truncationPolyhedron(A,transpose matrix{d}, Exterior => (options R1).SkewCommutative);
      C := cone P;
      H := hilbertBasis C;
      H = for h in H list flatten entries h;
      J := ideal leadTerm ideal R1;
      ambR := ring J;
      --conegens := rsort for h in H list if h#0 === 0 then ambR_(drop(h,1)) else continue;
      --print matrix {conegens};
      mongens := for h in H list if h#0 === 1 then ambR_(drop(h,1)) else continue;
      result := mingens ideal (matrix(ambR, {mongens}) % J);
      if R1 =!= ambR then result = result ** R1;
      if R =!= R1 then result = phi1^-1 result;
      ideal result
      );
    R#(symbol truncate, d)
    )

TEST ///
-* 
restart
*-
  debug needsPackage "Truncations"
  S = ZZ/101[a,b,c, Degrees =>{5,6,7}]
  truncationMonomials({10}, S)
  assert(truncationMonomials({{9},{11}}, S) == truncationMonomials({9},S))

  E = ZZ/101[a, b, c, SkewCommutative=>true]
  truncationMonomials({2}, E)

  E = ZZ/101[a,b,c, SkewCommutative=>{0,1}]
  truncationMonomials({2}, E) -- FAILS: needs a monomial ideal

  use S
  assert(truncationMonomials({12},S) == ideal"a3,a2b,b2,ac,bc,c2")
  R = S/(a*c-2*b^2)  
  assert(truncationMonomials({12},R) == ideal"a3,a2b,ac,bc,c2")
///

truncation0 = (deg, M) -> (
    -- WARNING: only valid for degree length = 1.
    -- deg: a List of integers
    -- M: Module
    -- returns a submodule of M
    trim if M.?generators then (
        b := M.generators * cover basis(deg,deg,cokernel presentation M,Truncate=>true);
        if M.?relations then subquotient(b, M.relations)
        else image b)
    else image basis(deg,deg,M,Truncate=>true)
    )    

truncation1 = (deg, M) -> (
    -- WARNING: only valid for degree length = 1.
    -- deg: a List of integers
    -- M: Module
    -- returns a submodule of M
    R := ring M;
    (R1, phi1) := flattenRing R;
    if R1 === R then 
        truncation0(deg, M)
    else (
        gensM1 := if not M.?generators then null else phi1 M.generators;
        relnsM1 := if not M.?relations then null else phi1 M.relations;
        M1 := if gensM1 === null and relnsM1 === null then phi1 M
              else subquotient(gensM1, relnsM1);
        result1 := truncation0(deg, M1);
        gensM := if not result1.?generators then null else phi1^-1 result1.generators;
        relnsM := if not result1.?relations then null else phi1^-1 result1.relations;
        if gensM === null and relnsM === null then phi1^-1 result1
              else subquotient(gensM, relnsM)
        )
    )

-- truncate the graded ring A in degrees >= d, for d in degs
truncate(List, Ring) := Module => (degs, R) -> (
    if not truncateImplemented R then error "cannot use truncate with this ring type";
    degs = checkOrMakeDegreeList(degs, degreeLength R);
    if degreeLength R === 1 and any(degrees R, d -> d =!= {0}) then
        ideal truncation1(min degs, R^1)
    else 
        ideal gens truncationMonomials(degs,R)
    )

truncate(List, Module) := Module => (degs, M) -> (
    R := ring M;
    if not truncateImplemented R then error "cannot use truncate with this ring type";
    degs = checkOrMakeDegreeList(degs, degreeLength R);
    if degreeLength R === 1 and any(degrees R, d -> d =!= {0}) then
        truncation1(min degs, M)
    else if isFreeModule M then (
        image map(M,,directSum for a in degrees M list 
            gens truncationMonomials(for d in degs list(d-a),R))
        )
    else (
        p := presentation M;
        phi := map(M,,gens truncate(degs, target p));
        trim image phi
        )
    )

truncate(List, Matrix) := Matrix => (degs, phi) -> (
    -- this is the case when source and target of phi are free modules...
    R := ring phi;
    if not truncateImplemented R then error "cannot use truncate with this ring type";
    degs = checkOrMakeDegreeList(degs, degreeLength R);
    F := truncate(degs, source phi);
    G := truncate(degs, target phi);
    f := gens F;
    g := gens G;
    map(G,F,(phi * f) // g)
    )

truncate(List, Ideal) := (degs, I) -> ideal truncate(degs, module I)

truncate(ZZ, Ring) :=
truncate(ZZ, Module) :=
truncate(ZZ, Ideal) :=
truncate(ZZ, Matrix) :=
  (d, R) -> truncate({d}, R)

TEST ///
  -- test of truncations in singly graded poly ring case
-*
  restart
*-
  needsPackage "Truncations"
  
  S = ZZ/101[a..d]
  I = monomialCurveIdeal(S, {1,3,4})
  assert(truncate(2, S) == (ideal vars S)^2)
  assert(truncate(2, S^1) == image gens (ideal vars S)^2)
  elapsedTime truncate(25, S^1);
  -- getting the map from truncate(d,F) --> F
  F = S^{-1} ++ S^{2}
  truncF = truncate(2, F)
  truncF2 = image map(F, truncF, gens truncF)
  truncF === truncF2

  -- test truncation of an ideal
  -- this assumes (tests) that truncation of an ideal is minimally generated.
  truncI = trim((ideal vars S)^2 * I_0 + (ideal vars S) * ideal(I_1, I_2, I_3))
  assert(truncate(4, I) == truncI)
  assert(numgens truncate(4, I) == 18)
  
  -- test of truncation of modules
  -- 1. coker module
  M = Ext^2(comodule I, S)
  assert not M.?generators
  assert(truncate(-3, M) == M)
  assert(truncate(-4, M) == M)
  truncM = truncate(-2, M)
  assert(truncM == ideal(a,b,c,d) * M)
  -- 2. image module
  -- 3. subquotient module
  C = res I  
  E = trim((ker transpose C.dd_3)/(image transpose C.dd_2))
  truncate(-3, E) == E
  truncate(-4, E) == E
  truncE = truncate(-2, E)
  assert(truncE == ideal(a,b,c,d) * E)
  presentation truncM
  presentation truncE
  
  -- check functoriality:
  assert(0 == truncate(3, C.dd_1) * truncate(3, C.dd_2))
  assert(0 == truncate(3, C.dd_2) * truncate(3, C.dd_3))

  -- how to get the map: truncM == truncate(-2,M) --> M ??
  phi = map(M, truncM, gens truncM)
  assert(image phi == truncM)

  F = truncate(-2, target presentation M)
  G = truncate(-2, source presentation M)
  assert(F == target truncate(-2, presentation M))
  assert(G == source truncate(-2, presentation M))
///

TEST ///
-*
  restart
  needsPackage "Truncations" 
*-
  S = ZZ/101[a,b, Degrees =>{{0,1},{1,0}}]
  M = S^{-{5,2}, -{2,3}}
  D = {4,3}
  assert(truncate(D,S) == image matrix{{a^3*b^4}})
  assert(truncate(D,S) == truncate({D},S))

  E = {{4,3},{3,4}}
  assert(truncate(E,S) == image matrix{{a^3*b^4, a^4*b^3}})

  assert(truncate(D, M) == image map(M,, matrix {{a, 0}, {0, b^2}}))
///

TEST ///
-*
  restart
  debug needsPackage "Truncations" 
*-
  S = ZZ/101[a,b,c,d,e,Degrees=>{3,4,5,6,7}]

  assert(
      sort gens truncate({8},S) 
      == 
      sort gens ideal(a*c,b^2,a*d,b*c,a^3,a*e,b*d,c^2,a^2*b,b*e,c*d,c*e,d^2,d*e,e^2)
      )

  truncate({8},S^{-4})
  truncate({8},S^{3})
  truncate({8},S^{-4,-5,-3})
  truncate(8,S^{-4,-5,-3})
  phi = random(S^{-1,-2,-3}, S^{-1,-2,-3,-4,-8})
  psi = truncate({8}, phi)
  assert(isHomogeneous psi)
///


TEST ///
-*
  restart
  needsPackage "Truncations"
*-
  S = ZZ/101[a,b,c,d,e, Degrees=>{3:{1,0},2:{0,1}}]
  assert(sort gens truncate({1,2},S) == matrix {{c*e^2, b*e^2, a*e^2, c*d*e, b*d*e, a*d*e, c*d^2, b*d^2, a*d^2}})
///

TEST ///
-*
  restart
  debug needsPackage "Truncations"
*-
  needsPackage "NormalToricVarieties"
  V = smoothFanoToricVariety(3,5)
  rays V
  max V
  S = ring V
  A = transpose matrix degrees S
  truncate({1,1,1}, S)
  basis({1,1,1},S)
  C = posHull A
  C2 = dualCone C
  rays C2
///

TEST ///
  -- test the following:
  --  gradings:
  --    standard grading
  --    ZZ^1 grading
  --    ZZ^r grading
  --  rings:
  --    polynomial ring
  --    exterior algebra
  --    quotient of a poly ring
  --    quotient of an exterior algebra
  --    Weyl algebra (?? probably not: what does this mean)
  --  coeff rings:
  --    a basic field
  --    a poly ring
  --    a quotient of a polynomial ring
  --  truncations:
  --    truncate(D, S)
  --    truncate(D, S^1)
  --    truncate(D, ideal)
  --    truncate(D, graded free module)
  --    truncate(D, coker module)
  --    truncate(D, image module)
  --    truncate(D, subquotient module)
  --    truncate(D, Matrix)
///

TEST ///
-*
  restart
  needsPackage "Truncations"  
*-
  d = {5,6}
  D = {d,reverse d}

  kk = ZZ/101
  R = kk[a,b,c,Degrees =>{2:{3,4},{7,5}}]
  truncate({5,6},R)
  truncate({6,5},R)

  J1 = truncate(D, R)
  J1 = truncate(D, R^1)
  truncate(D, ideal(a,b,c))

  A = R/(a^2-b^2, c^3)
  truncate(D, A)
  truncate(d, R)
  M = module ideal(a,b,c)
  truncate(d, ideal(a,b,c))
  truncate(D, ideal(a,b,c))
  p = presentation M
  
  truncate(D, presentation M)
  truncate(D, source presentation M)
  truncate(D, target presentation M)
///

beginDocumentation()

doc ///
  Key
    Truncations
  Headline
    truncations of graded ring, ideals and modules
  Description
    Text
      This package provides for the truncation of a graded ring, or a graded
      module or ideal over a graded ring.  Truncation is functorial: it can be applied
      to matrices as well, and the truncation of a composition of maps is the composition
      of the truncations.
    
      If $R$ is a $\ZZ^r$-graded ring, and $M$ is a graded module, and $D$ is a (finite)
      set of degrees in $\ZZ^r$, then the truncation {\tt truncate(D, M)} is
      $$M_{\ge D} = \oplus_{m} M_m,$$
      where the sum is over all $m \in \ZZ^r$, which are
      component-wise greater than at least one element $d \in D$.
      
      This definition makes the truncation into a submodule in the case when all degrees
      are non-negative.  In the case when some degree components are negative, this
      is likely not a submodule.
      
      This package handles the multi-graded case correctly, at least for the case
      of non-negative degree vectors for the variables, and the
      @TO "truncate"@ function is functorial (see @TO (truncate, List, Matrix)@).
  Caveat
    The behavior of @TO "truncate"@ has changed as of Macaulay2
    version 1.13.  This is a (potentially) breaking change.  Before,
    it used a less useful notion of truncation, involving the heft
    vector, and was often not what one wanted in the multi-graded
    case.  Additionally, in the tower ring case, when the coefficient
    ring had variables of nonzero degree, sometimes incorrect answers
    resulted.
  SeeAlso
    (truncate,List,Matrix)
    (truncate,List,Module)
    basis
///


TEST ///
  debug needsPackage "Truncations"
  assert truncateImplemented(ZZ/101[a..d])      
  assert truncateImplemented(ZZ/101[a..d, Degrees => {1,1,-1,-1}])
  assert truncateImplemented(ZZ/101[a..d, Degrees => {2:{3,1},2:{-4,2}}])
      
  assert truncateImplemented(QQ[a..d, SkewCommutative=>true])
  assert truncateImplemented(QQ[a..d, SkewCommutative=>{0,3}])
   
  assert truncateImplemented(ZZ[a..d])      
  assert truncateImplemented(ZZ[a..d, Degrees => {1,1,-1,-1}])
  assert truncateImplemented(ZZ[a..d, Degrees => {2:{3,1},2:{-4,2}}])
      
  assert truncateImplemented(ZZ[a..d, SkewCommutative=>true])
  assert truncateImplemented(ZZ[a..d, SkewCommutative=>{0,3}])

  assert truncateImplemented(ZZ/101[a..d]/(a*d-b*c))
  assert truncateImplemented(ZZ/101[a..d, Degrees => {1,1,-1,-1}]/(a*d-b*c))
  assert truncateImplemented(ZZ/101[a..d, Degrees => {2:{3,1},2:{-4,2}}]/(a*d-b*c))
      
  assert truncateImplemented(QQ[a..d, SkewCommutative=>true]/(a*d-b*c))
  assert truncateImplemented(QQ[a..d, SkewCommutative=>{0,3}]/(a*d-b*c))

  assert truncateImplemented(ZZ[a..d]/(3*a*d-b*c))
  assert truncateImplemented(ZZ[a..d, Degrees => {1,1,-1,-1}]/(a*d-b*c))
  assert truncateImplemented(ZZ[a..d, Degrees => {2:{3,1},2:{-4,2}}]/(a*d-b*c))
      
  assert truncateImplemented(ZZ[a..d, SkewCommutative=>true]/(a*d-b*c))
  assert truncateImplemented(ZZ[a..d, SkewCommutative=>{0,3}]/(a*d-b*c))
///

doc ///
  Key
    (truncate,ZZ,Module)
    (truncate,List,Module)
    (truncate,ZZ,Ideal)
    (truncate,List,Ideal)
    (truncate,ZZ,Ring)
    (truncate,List,Ring)
  Headline
    truncation of the graded ring, ideal or module at a specified degree or set of degrees
  Usage
    truncate(d,M)
  Inputs
    d:ZZ
      or a single multi-degree or a list of multi-degrees
    M:Module
      or a ring or an ideal
  Outputs
    :Module
      or ideal, the submodule of M consisting of all elements of (component-wise) degree $\ge i$
  Description
    Text
      The truncation to degree $d$ in the singly graded case of a module (or ring or ideal) is
      generated by all homogeneous elements of degree at least $d$ in $M$.  The resulting
      truncation is minimally generated (assuming that $M$ is graded).
    Example
      R = ZZ/101[a..c];
      truncate(2, R)
      truncate(2,R^1)
      truncate(2,R^1 ++ R^{-3})
      truncate(2, ideal(a,b,c^3)/ideal(a^2,b^2,c^4))
      truncate(2,ideal(a,b*c,c^7))
      M = coker matrix"a,b,c;c,b,a"
      truncate(2, M)
      M/(truncate(2,M))
      for i from 0 to 5 list hilbertFunction(i,oo)
    Text
      The base may be ZZ, or another polynomial ring.  Over ZZ, the generators may not
      be minimal, but they do generate.
    Example
      A = ZZ[x,y,z];
      truncate(2,ideal(3*x,5*y,15))
      trim oo
      truncate(2,comodule ideal(3*x,5*y,15))
    Text
      If {\tt i} is a multi-degree, then the result is the submodule
      generated by all elements of degree (component-wise) greater
      than or equal to $i$.

      The following example finds the 11 generators needed to
      obtain all graded elements whose degrees are component-wise 
      at least $\{7,24\}$.
    Example
      S = ZZ/101[x,y,z,Degrees=>{{1,3},{1,4},{1,0}}];
      trunc = truncate({7,24}, S^1 ++ S^{{-8,-20}})
      degrees trunc      
    Text
      If  {\tt i} is a list of multi-degrees, then the result is the 
      submodule generated by all elements
      of degree (component-wise) greater than or equal to at least one degree in $i$.  

      The following example finds the generators needed to obtain
      all graded elements whose degrees which are component-wise at
      least $\{3,0\}$ or at least $\{0,1\}$.  The resulting module is
      also minimally generated.
    Example
      S = ZZ/101[x,y,z,Degrees=>{{1,3},{1,4},{1,0}}];
      trunc = truncate({{3,0},{0,1}}, S^1 ++ S^{{-8,-20}})
      degrees trunc
    Text
      The coefficient ring may also be a polynomial ring.  In this
      example, the coefficient variables also have degree one.  The
      given generators will generate the truncation over the
      coefficient ring.
    Example
      B = R[x,y,z, Join=>false]
      degree x
      degree B_3
      truncate(2, B^1)
      truncate(4, ideal(b^2*y,x^3))
    Text
      If the coefficient variables have degree 0:
    Example
      A1 = ZZ/101[a,b,c,Degrees=>{3:{}}]
      degree a
      B1 = A1[x,y]
      degrees B1
      truncate(2,B1^1)
      truncate(2, ideal(a^3*x, b*y^2))
  Caveat
    The behavior of this function has changed as of Macaulay2
    version 1.13.  This is a (potentially) breaking change.  Before,
    it used a less useful notion of truncation, involving the heft
    vector, and was often not what one wanted in the multi-graded
    case.  Additionally, in the tower ring case, when the coefficient
    ring had variables of nonzero degree, sometimes incorrect answers
    resulted.
    
    Also, the function expects a graded module, ring, or ideal, but this is not checked, and
    some answer is returned.
  SeeAlso
    basis
    comodule
///

doc ///
  Key
    (truncate, List, Matrix)
    (truncate, ZZ, Matrix)
  Headline
    truncation of a matrix
  Usage
    truncate(degs, f)
  Inputs
    degs:List
      a list of lists of integers (list of degrees), or a list of integers (a single degree),
      or an integer (a singly graded degree)
    f:Matrix
      a graded map between graded modules (not necessarily free modules)
  Outputs
    :Matrix
  Description
    Text
      This function truncates the source and target of $f$, and returns the induced map
      between them.
    Example
      R = ZZ/101[a..d, Degrees=>{{1,3},{1,0},{1,3},{1,2}}]
      C = res coker vars R
      g1 = truncate({1,1},C.dd_1)
      g2 = truncate({1,1},C.dd_2)
      g3 = truncate({1,1},C.dd_3)
      g4 = truncate({1,1},C.dd_4)
      assert(g1 * g2 == 0)
      assert(g2 * g3 == 0)
      assert(g3 * g4 == 0)
    Text
      This functor is exact.
    Example
      assert(ker g1 == image g2)
      assert(ker g2 == image g3)
      assert(ker g3 == image g4)
  SeeAlso
    (truncate, List, Module)
///

TEST ///
  A = ZZ/101[a..d, Degrees => {1,2,3,4}]
  assert(truncate(2, A^1) == image matrix {{a^2, a*b, a*c, a*d, b, c, d}})
  assert(truncate(4, ideal"a3,b3") == ideal(a^4,a^3*b,a^3*c,a^3*d,b^3))
///

TEST ///
  A = ZZ/101[a..d, Degrees => {4:0}]
  assert(truncate(2, A^1) == image matrix{{0_A}})
///

TEST ///
  R = ZZ/101[x_0,x_1,y_0,y_1,y_2,Degrees=>{2:{1,1,0},3:{1,0,1}}];
  I = ideal random(R^1,R^{6:{-6,-2,-4},4:{-6,-3,-3}});
  J = truncate({6,2,3},I);
  assert(J == I)
///

TEST ///
  -- Singly generated case
  R = QQ[a..d]
  I = ideal(b*c-a*d,b^2-a*c,d^10)
  truncate(2,I)
  assert(truncate(2,I) == I)
  assert(truncate(3,I) == intersect((ideal vars R)^3, I))

  R = QQ[a..d,Degrees=>{3,4,7,9}]
  I = ideal(a^3,b^4,c^6)
  assert(truncate(12,I) == ideal(a^4,a^3*b,a^3*c,a^3*d,b^4,c^6))

  R = ZZ[a,b,c]
  I = ideal(15*a,21*b,19*c)
  trim truncate(2,I) == ideal(19*c^2,b*c,a*c,21*b^2,3*a*b,15*a^2)
///

end--

restart
uninstallPackage "Truncations"
restart
loadPackage "Truncations"
debug needsPackage "Truncations"
restart
installPackage "Truncations"
check "Truncations"

doc ///
  Key
  Headline
  Usage
  Inputs
  Outputs
  Description
    Text
    Example
  Caveat
  SeeAlso
///

