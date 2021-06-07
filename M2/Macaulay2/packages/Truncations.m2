---------------------------------------------------------------------------
-- PURPOSE : Computation of truncations M_{>=d} for modules
--
-- UPDATE HISTORY : created Oct 2018
--
-- TODO :
-- 1. support truncation on Cox ring of a toric variety
---------------------------------------------------------------------------
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

--------------------------------------------------------------------
----- Tests section
--------------------------------------------------------------------

load "./Truncations/tests.m2"

--------------------------------------------------------------------
----- Documentation section
--------------------------------------------------------------------

beginDocumentation()
load "./Truncations/docs.m2"

--------------------------------------------------------------------
----- Development section
--------------------------------------------------------------------

end--

restart
uninstallPackage "Truncations"
restart
loadPackage "Truncations"
debug needsPackage "Truncations"
restart
installPackage "Truncations"
check "Truncations"
