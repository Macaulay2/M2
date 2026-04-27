---------------------------------------------------------------------------
-- PURPOSE : Computation of truncations M_{>=d} for modules
--
-- UPDATE HISTORY : created Oct 2018
--                  updated Jun 2021
--
-- TODO :
-- 1. improve speed and caching
-- 2. flattenRing should keep track of the Nef cone
-- 3. turn into hook strategies
-- 4. remove [basis, Truncate], call rawBasis in truncation0
-- 5. finish basis'
---------------------------------------------------------------------------
newPackage(
    "Truncations",
    Version => "1.0",
    Date => "22 May 2021",
    Headline => "truncation of a module",
    Authors => {
        { Name => "David Eisenbud", Email => "de@msri.org",           HomePage => "https://www.msri.org/~de" },
        { Name => "Mike Stillman",  Email => "mike@math.cornell.edu", HomePage => "https://www.math.cornell.edu/~mike" },
        { Name => "Mahrud Sayrafi", Email => "mahrud@umn.edu",        HomePage => "https://math.umn.edu/~mahrud" }
        },
    Keywords => { "Commutative Algebra" },
    PackageExports => { "Polyhedra" },
    AuxiliaryFiles => true
    )

importFrom_Core {
    "concatCols",
    "freeComponents",
    "raw", "rawHilbertBasis", "rawSelectByDegrees",
    "tryHooks",
    }

-- "truncate" is exported by Core
export {
    "effCone", "effGenerators",
    "nefCone", "nefGenerators",
    }

protect Exterior

--------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------

-- check whether truncation is implemented for this ring type.
truncateImplemented = method()
truncateImplemented Ring := Boolean => R -> (
    ZZ === (A := ambient first flattenRing R)
    or isAffineRing A
    or isPolynomialRing A and isAffineRing coefficientRing A and A.?SkewCommutative
    or isPolynomialRing A and ZZ === coefficientRing A
    )

-- checkOrMakeDegreeList: takes a degree, and degree rank:ZZ
-- output: a list of degrees, all of the correct length (degree rank), otherwise an error
-- in the following n represents an integer, and d represents a list of integers:
--  n          --> {{n}}          if degree rank is 1
-- {n0,...,ns} --> {{n0,...,ns}}  if length is the degree rank
-- {d0,...,ds} --> { d0,...,ds }  if length of each di is the degree rank
checkOrMakeDegreeList = method()
checkOrMakeDegreeList(ZZ,   ZZ) := (n, degrank) -> (
    if degrank === 1 then {{n}} else error("expected a degree of length " | degrank))
checkOrMakeDegreeList(List, ZZ) := (L, degrank) -> (
    if #L === 0 then error "expected nonempty list of degrees";
    if all(L, d -> instance(d, ZZ))
    then if #L === degrank then {L} else error("expected a multidegree of length " | degrank)
    else ( -- If L is a list of lists of integers, all the same length, L will be returned.
        if all(L, deg -> instance(deg, VisibleList) and #deg === degrank and all(deg, d -> instance(d, ZZ)))
        then sort L else error("expected a list of multidegrees, each of length " | degrank))
    )

--------------------------------------------------------------------
-- Divisorial information
--------------------------------------------------------------------

-- Generators of effective cone
effGenerators = method(TypicalValue => Matrix)
effGenerators Ring := R -> tryHooks((effGenerators, Ring), R, R -> map(degreeGroup R, , transpose matrix degrees R))

-- Generators of Nef cone
nefGenerators = method(TypicalValue => Matrix)
nefGenerators Ring := R -> null -- will be overridden in Varieties package

-- Effective cone of X as a polyhedral object
effCone = method(TypicalValue => Cone)
effCone Ring := R -> tryHooks((effCone, Ring), R, R -> convexHull(matrix{0_(degreeGroup R)}, effGenerators R))

-- Nef cone of X as a polyhedral object
nefCone = method(TypicalValue => Cone)
nefCone Ring := R -> null -- will be overridden in Varieties package

--------------------------------------------------------------------
-- Polyhedral algorithms
--------------------------------------------------------------------

-- If columns of A span the effective cone of a simplicial toric variety X
-- (e.g. when columns are the degrees of the variables of the Cox ring S)
-- and b is an element of the Picard group of X (e.g. a multidegree in S)
-- then truncationPolyhedron returns a polyhedron in the lattice of
-- exponent vectors of monomials of S whose degree is in b + nef cone of X
truncationPolyhedron = method(Options => { Exterior => {}, Cone => null })
-- Exterior should be a list of variable indices which are skew commutative; i.e. have max degree 1.
truncationPolyhedron(Matrix, List)   := Polyhedron => opts -> (A, b) -> truncationPolyhedron(A, transpose matrix {b}, opts)
truncationPolyhedron(Matrix, Matrix) := Polyhedron => opts -> (A, b) -> (
    -- assumption: A is m x n. b is m x 1.
    -- returns the polyhedron {Ax >= b, x_i >= 0}
    -- or if cone C is given  {Ax - b in C, x_i >= 0}
    -- TODO: why is it better to be over QQ?
    if ring A === ZZ then A = A ** QQ;
    if ring A =!= QQ then error "expected matrix over ZZ or QQ";
    -- added to ensure x_i >= 0
    I := id_(source A);
    z := map(source A, QQ^1, 0);
    -- data for the polyhedron inequalities
    hdataLHS := -(A || I);
    hdataRHS := -(b || z);
    -- added to ensure x_i <= 1 for skew commuting variables
    if #opts.Exterior > 0 then (
        -- also need to add in the conditions that each variable in the list has degree <= 1.
        hdataLHS = hdataLHS || (I ^ (opts.Exterior));
        hdataRHS = hdataRHS || matrix toList(#opts.Exterior : {1_QQ});
        );
    if opts#Cone === null or rays opts#Cone == id_(target A)
    -- this is correct when the truncation cone equals the positive quadrant
    then polyhedronFromHData(hdataLHS, hdataRHS)
    -- otherwise, compute the preimage of the truncation cone
    else affinePreimage(-hdataLHS, opts#Cone * convexHull(z, I), hdataRHS))

-- Assume the same conditions as above,
-- then basisPolyhedron returns a polyhedron in the lattice of
-- exponent vectors of monomials of S whose degree is exactly b
basisPolyhedron = method(Options => { Exterior => {} })
basisPolyhedron(Matrix, List)   := Polyhedron => opts -> (A, b) -> basisPolyhedron(A, transpose matrix {b}, opts)
basisPolyhedron(Matrix, Matrix) := Polyhedron => opts -> (A, b) -> (
    -- assumption: A is m x n. b is m x 1.
    -- returns the polyhedron {Ax = b, x_i >= 0}
    if ring A === ZZ then A = A ** QQ;
    if ring A =!= QQ then error "expected matrix over ZZ or QQ";
    -- added to ensure x_i >= 0
    I := id_(source A);
    z := map(source A, QQ^1, 0);
    -- added to ensure x_i <= 1 for skew commuting variables
    if #opts.Exterior > 0 then (
        -- also need to add in the conditions that each variable in the list has degree <= 1.
        I = I || (-I ^ (opts.Exterior));
        z = z || matrix toList(#opts.Exterior : {-1_QQ});
        );
    polyhedronFromHData(-I, -z, A, b))

--------------------------------------------------------------------
-- Algorithms for truncations of a polynomial ring
--------------------------------------------------------------------

truncationMonomials = method(Options => { Exterior => {}, Cone => null })
truncationMonomials(List, Module) := opts -> (degs, F) -> (
    -- inputs: a list of multidegrees, a free module F = sum_i R(-a_i)
    -- assume checkOrMakeDegreeList has already been called on degs
    (R1, phi1) := flattenRing (R := ring F);
    ext := if opts.Exterior =!= null then opts.Exterior else (options R1).SkewCommutative;
    nef := if opts#Cone     =!= null then opts#Cone     else  nefCone R1; -- changing to effCone gives an alternative result
    -- the free components of the degree group
    free := freeComponents degreeGroup R;
    -- TODO: call findMins on degs, but with respect to the given cone!
    -- checks to see if twist S(-a) needs to be truncated
    isInCone := if nef === null then a -> any(degs, d -> d_free << a) else (
	-- FIXME
        truncationCone := nef + convexHull(matrix (transpose degs)_free);
	a -> contains(truncationCone, convexHull matrix transpose{a}));
    -- TODO: either figure out a way to use cached results or do this in parallel
    directSum apply(degrees F, a -> if isInCone a_free then gens R^{a} else concatCols(
	     apply(degs, d -> truncationMonomials(d - a, R, Exterior => ext, Cone => nef)))))
truncationMonomials(List, Ring) := opts -> (d, R) -> (
    -- inputs: a single multidegree, a graded ring
    -- valid for total coordinate ring of any simplicial toric variety
    -- or any polynomial ring, quotient ring, or exterior algebra.
    R#(symbol truncate, d, if opts#Cone =!= null then rays opts#Cone) ??= (
        (R1, phi1) := flattenRing R;
        -- generates the effective cone
        A := effGenerators R1;
        F := freeComponents target A;
        b := transpose matrix{d_F};
        P := truncationPolyhedron(A^F, b, opts);
        H := entries map(ZZ, rawHilbertBasis raw transpose rays cone P); -- ~50% of computation
        J := leadTerm ideal R1;
        ambR := ring J;
        -- generates the Nef cone
        --nefgens := matrix(ambR, { for h in H list if h#0 === 0 then ambR_(drop(h, 1)) else continue });
        mongens := matrix(ambR, { for h in H list if h#0 === 1 then ambR_(drop(h, 1)) else continue });
        result := mingens ideal(mongens % J);
        if R1 =!= ambR then result = result ** R1;
        if R =!= R1 then result = phi1^-1 result;
        result))

truncation0 = (deg, M) -> (
    -- WARNING: valid for a polynomial ring with degree length = 1.
    -- uses the engine routines for basis with Truncate => true
    -- deg: a List of integers
    -- M: Module
    -- returns a submodule of M
    if M.?generators then (
        F := cover basis(deg, deg, cokernel presentation M, Truncate => true);
        subquotient(M.generators * F, if M.?relations then M.relations))
    else image basis(deg, deg, M, Truncate => true))

truncation1 = (deg, M) -> (
    -- short-circuit the computation if no truncation is needed
    if all(degrees M, d -> deg <= d) then return M;
    -- WARNING: valid for towers of rings with degree length = 1.
    -- uses the engine routines for basis with Truncate => true
    -- deg: a List of integers
    -- M: Module
    -- returns a submodule of M
    R := ring M;
    (R1, phi1) := flattenRing R;
    if R1 === R then return truncation0(deg, M);
    -- why not just do phi1? or M ** phi1?
    M1 := if isFreeModule M then phi1 M else subquotient(
        if M.?generators then phi1 M.generators,
        if M.?relations  then phi1 M.relations);
    result1 := truncation0(deg, M1);
    gensM := if not result1.?generators then null else phi1^-1 result1.generators;
    relnsM := if not result1.?relations then null else phi1^-1 result1.relations;
    if gensM === null and relnsM === null then phi1^-1 result1
    else subquotient(gensM, relnsM))

--------------------------------------------------------------------
-- truncate
--------------------------------------------------------------------

truncateModuleOpts := {
    Cone              => null,
    MinimalGenerators => true -- whether to trim the output
    }

truncate(ZZ, Ring)   :=
truncate(ZZ, Ideal)  :=
truncate(ZZ, Matrix) :=
truncate(ZZ, Module) := truncateModuleOpts >> opts -> (d, m) -> truncate({d}, m, opts)

truncate(List, Ring)   := Ideal  => truncateModuleOpts >> opts -> (degs, R) -> ideal truncate(degs, module R, opts)
truncate(List, Ideal)  := Ideal  => truncateModuleOpts >> opts -> (degs, I) -> ideal truncate(degs, module I, opts)
truncate(List, Module) := Module => truncateModuleOpts >> opts -> (degs, M) -> (
    if M == 0 then return M;
    if not truncateImplemented(R := ring M) then error "cannot use truncate with this ring type";
    degs = checkOrMakeDegreeList(degs, degreeLength R);
    doTrim := if opts.MinimalGenerators then trim else identity;
    doTrim if degreeLength R === 1 and any(degrees R, d -> d =!= {0})
    then truncation1(min degs, M)
    else if isFreeModule M then return ( -- NOTE: skip trimming
        image map(M, , truncationMonomials(degs, M, Cone => opts#Cone)))
    else if not M.?relations then (
        image map(M, , truncationMonomials(degs, cover M, Cone => opts#Cone)))
    else subquotient(
        gens truncate(degs, image generators M, opts ++ { MinimalGenerators => false }),
        gens truncate(degs, image  relations M, opts ++ { MinimalGenerators => false }))
    )

--------------------------------------------------------------------

inducedTruncationMap = (G, F, f) -> (
    -- Assumes G = truncate(deg, target f) and F = truncate(deg, source f)
    -- this helper routine is useful for truncating complexes or a pair of
    -- composable matrices, when target f is the source of a previously
    -- truncated matrix, so we have truncated it once already.
    f' := f * inducedMap(source f, F)       * inducedMap(F, source gens F, gens F);
    map(G, F, inducedMap(G, source f', f') // inducedMap(G, source gens G, gens G)))

truncate(List, Matrix) := Matrix => truncateModuleOpts >> opts -> (degs, f) -> (
    inducedTruncationMap(truncate(degs, target f, opts), truncate(degs, source f, opts), f))

-- TODO: document these
truncate(ZZ,      ZZ,   Matrix) :=
truncate(Nothing, ZZ,   Matrix) :=
truncate(Nothing, List, Matrix) :=
truncate(List,    List, Matrix) := Matrix => truncateModuleOpts >> opts -> (tardegs, srcdegs, f) -> (
    -- Given a graded map f, truncate source f, possibly higher than target f,
    -- then return the map induced by f and the two inclusions
    -- TODO: benchmark to make sure the inducedMaps are not computing unnecessary gb
    if tardegs === null then return f * inducedMap(source f, truncate(srcdegs, source f, opts));
    -- TODO: assert that srcdegs >= tardegs with respect to the cone of truncation
    inducedTruncationMap(truncate(tardegs, target f, opts),  truncate(srcdegs, source f, opts), f))

--------------------------------------------------------------------

truncate(InfiniteNumber, Thing) := truncateModuleOpts >> o -> (d, M) -> (
    if d === -infinity then M else (ring M)^0)

-- TODO: implement union types in M2 and simplify stuff like this
truncate(Nothing,        InfiniteNumber, Matrix) :=
truncate(InfiniteNumber, InfiniteNumber, Matrix) := lookup(truncate, List, List, Matrix)

-- TODO: implement union types in M2 and simplify stuff like this
truncate(Nothing,        InfiniteNumber, Matrix) :=
truncate(InfiniteNumber, InfiniteNumber, Matrix) := lookup(truncate, List, List, Matrix)

--------------------------------------------------------------------
-- basis using basisPolyhedron (experimental)
--------------------------------------------------------------------
-- c.f https://github.com/Macaulay2/M2/pull/2056
-- add as a strategy to basis
-- add partial multidegree support
-- ensure the output is a module over the degree 0 of R

basisMonomials = method()
basisMonomials(List, Module) := (degs, F) -> (
    -- inputs: a list of multidegrees, a free module
    -- assume checkOrMakeDegreeList has already been called on degs
    -- TODO: either figure out a way to use cached results or do this in parallel
    R := ring F; directSum apply(degrees F, a -> concatCols apply(degs, d -> basisMonomials(d - a, R))))
basisMonomials(List, Ring) := (d, R) -> R#(symbol basis', d) ??= (
    -- inputs: a single multidegree, a graded ring
    -- valid for total coordinate ring of any simplicial toric variety
    -- or any polynomial ring, quotient ring, or exterior algebra.
    -- TODO: we should accept _any_ cached truncation as a hint
    if R#?(symbol truncate, d, null) then (
        -- opportunistically use cached truncation results
        -- TODO: is this always correct? with negative degrees?
        truncgens := R#(symbol truncate, d, null);
	psrc := rawSelectByDegrees(raw source truncgens, d, d);
        submatrix(truncgens, , psrc))
    else (
        (R1, phi1) := flattenRing R;
        -- generates the effective cone
        A := effGenerators R1;
        F := freeComponents target A;
        b := transpose matrix{d_F};
        -- TODO: would be better if basisPolyhedron could account for torsion
        P := basisPolyhedron(A^F, b,
            Exterior => (options R1).SkewCommutative);
        H := entries map(ZZ, rawHilbertBasis raw transpose rays cone P); -- ~40% of computation
        J := leadTerm ideal R1;
        ambR := ring J;
        -- generates the degree zero part of the basis
        --zerogens := matrix(ambR, { for h in H list if h#0 === 0 then ambR_(drop(h, 1)) else continue });
        mongens := matrix(ambR, { for h in H list if h#0 === 1 then ambR_(drop(h, 1)) else continue });
        result := mingens ideal(mongens % J); -- ~40% of computation
        if R1 =!= ambR then result = result ** R1;
        if R =!= R1 then result = phi1^-1 result;
        psrc = rawSelectByDegrees(raw source result, d, d);
        submatrix(result, , psrc)))

-- FIXME: when M has relations, it should be pruned
basis' = method(Options => options basis)
basis'(List, Module) := Matrix => opts -> (degs, M) -> (
    if M == 0 then return M;
    if not truncateImplemented(R := ring M) then error "cannot use basis' with this ring type";
    degs = checkOrMakeDegreeList(degs, degreeLength R);
    if isFreeModule M
    then map(M, , basisMonomials(degs, M))
    else map(M, , basis'(degs, target presentation M, opts)))

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
loadPackage("Truncations", Reload => true)
debug Truncations
restart
installPackage "Truncations"
check "Truncations"
