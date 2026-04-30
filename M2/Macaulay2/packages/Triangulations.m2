newPackage(
        "Triangulations",
        Version => "0.5", 
        Date => "29 April 2026",
        Authors => {{
                Name => "Mike Stillman", 
                Email => "mike@math.cornell.edu", 
                HomePage=>"https://mikestillman.github.io"
                }},
        Headline => "triangulations of polyhedra and point sets",
        Keywords => {"Combinatorics"},
        PackageImports => {"FourierMotzkin"},
        PackageExports => {
            "Topcom", 
            "Polyhedra" -- really only needed for `regularSubdivision`?
            },
        DebuggingMode => false
        )

export {
    -- types defined here
    "Triangulation",
    "Chirotope",

    "triangulation",
    "vectors",
    "regularFineTriangulation",
    "chirotope",
    "naiveChirotope",
    "flips",
    "bistellarFlip",
    "neighbors",
    "generateTriangulations",
    "flipGraph",
    "allTriangulations",
    "isStar",
    "isFine",
    "isRegularTriangulation", -- note the non-use of isRegular.  Is this ok?
    "regularTriangulationWeights",
    
    "flipCandidates",
    "wallCircuits",
    "secondaryCone",
    "secondaryFan",
    "chargeMatrix",
    "ChargeMatrix",
    "Cones",
    "interiorLatticePoint",
    "volumeVector",
    "gkzVector",
    
    "delaunayWeights",
    "delaunaySubdivision",

    "fineStarTriangulation",
    "regularFineStarTriangulation",
    "naiveIsTriangulation",
    
    "ConeIndex",
    "Edges"
    }

-- Engine LP entry point used by isRegularTriangulation / regularTriangulationWeights.
importFrom_"Core" {"raw", "rawConeInteriorPoint"}

augment = method()
augment Matrix := (A) -> (
    -- A is a matrix over ZZ
    -- add in a last row of 1's.
    n := numColumns A;
    ones := matrix {{n : 1}};
    A || ones
    )

-- also defined:
-- isRegularTriangulation T
-- regularTriangulationWeights T
-- naiveIsTriangulation T

-- TODO: design decision:
--   The "rays" or "vertices": stored as matrix or list?
--   Store list as vector configuration (i.e. homogenized?)
--   Accessors: max, rays?
Triangulation = new Type of HashTable
Chirotope = new Type of HashTable

net Triangulation := T -> "triangulation " | net max T
toString Triangulation := T -> toString max T
toExternalString Triangulation := T -> "triangulation(transpose matrix" | toExternalString vectors T | "," | toExternalString max T | ", Homogenize => false)"
--expression Triangulation := X -> (describe Triangulation)#0
--describe Triangulation := X -> describe max X


-- XXX working on this 15 June 2022.
-- Change all uses of fineStarTriangulation to return what?
-- Assumptions: A is a dxn matrix over the integers, or rationals.
--              convexHull A 
-- all columns of A should be boundary points (with the possible exception of the last column).
-- A can either contain the interior point to cone over, or not.
-- If it does contain it, I believe that no element of tri should use that index.

-- Returns: a new triangulation of the point set A | 0.
--  NOTE: not yet!  it currently returns a list of subsets, such that if one appends to each subset 
--    the number "numcols A", this will be a star triangulation.
--  NOTE: I'm not sure why this would need to be regular either...

fineStarTriangulation = method(Options => {ConeIndex => null})
fineStarTriangulation(Matrix, List) := List => opts -> (A, tri) -> (
    coneindex := if opts.ConeIndex === null then 
                     numcols A -- in this case, assume that the cone point is a new index, which would be 'numcols A'
                 else if instance(opts.ConeIndex, ZZ) and opts.ConeIndex >= 0 and opts.ConeIndex <= numcols A + 1 then
                     opts.ConeIndex
                 else
                     error "ConeIndex must be an index from 0 .. number of columns + 1...";
    aA := augment A;
    -- H := first halfspaces convexHull aA;
    H := transpose(-(first fourierMotzkin aA));
    myfacets := for e in entries H list (
        positions(flatten entries(matrix {e} * aA), x -> x == 0)
        );
    sort unique flatten for f in tri list for g in myfacets list (
        a := toList(set g * set f); 
        if #a < numRows A then 
            continue 
        else 
            sort append(a, coneindex)
        )
    -- newtri = for f in newtri list append(f, numColumns A)
    )

-- TODO: is this really a regular triangulation?
-- I think it might be, as long as the cone index is not used in regularFineTriangulation.
regularFineStarTriangulation = method(Options => options fineStarTriangulation)
regularFineStarTriangulation Matrix := List => opts -> (A) -> fineStarTriangulation(A, topcomRegularFineTriangulation A, opts)

-- TODO: I am not sure that this is correct.
naiveIsTriangulation = method()
naiveIsTriangulation(Matrix, List, List) := (A, circuits, tri) -> (
    aA := augment A;
    -- H := first halfspaces convexHull aA;
    H := transpose(-(first fourierMotzkin aA));
    myfacets := for e in entries H list (
        positions(flatten entries(matrix {e} * aA), x -> x == 0)
        );
    -- test 1: each wall should be in a facet of the convex hull, or occur exactly twice.
    -- This is NOT correct?!
    walls := tally flatten for t in tri list subsets(t,#t-1);
    test1 := for k in keys walls list (
        if any(myfacets, f -> isSubset(k,f)) then 
          walls#k == 1
        else
          walls#k == 2
        );
    if any(test1, x -> not x) then return false;
    -- test 2: for each oriented circuit Z = (Z+, Z-)
    test2 := for z in circuits list (
      # select(tri, t -> isSubset(z_0, t)),
      # select(tri, t -> isSubset(z_1, t))
      );
    all(test2, x -> x#0 == 0 or x#1 == 0)
    )
naiveIsTriangulation(Matrix, List) := (A, tri) -> naiveIsTriangulation(A, orientedCircuits A, tri)



-- Allow both rays and points, i.e. homogenized A or not.
-- Homogenize => null (default) auto-detects from max-simplex sizes:
--   simplex size == numRows A     ⇒ vector configuration (no homogenization)
--   simplex size == numRows A + 1 ⇒ point configuration (homogenize)
-- Pass Homogenize => true / false to force; an explicit value inconsistent
-- with the simplex sizes is an error.
triangulation = method(Options => {Homogenize => null})
triangulation(Matrix, List) := Triangulation => opts -> (A, tri) -> (
    if ring A =!= ZZ and ring A =!= QQ then
        error "expected matrix over ZZ or QQ";
    if #tri == 0 then
        error "empty triangulation: expected at least one max simplex";
    if not all(tri, f -> all(f, p -> instance(p, ZZ))) then
        error "expected a list of list of integers";
    d := numRows A;
    sizes := unique(tri / length);
    if #sizes != 1 then
        error "expected all max simplices to have the same length";
    sz := sizes#0;
    inferred := if sz == d + 1 then true
                else if sz == d then false
                else error("max simplex size ", sz, " inconsistent with ", d, "-row matrix");
    homog := opts.Homogenize;
    if homog === null then homog = inferred
    else if homog =!= inferred then
        error("Homogenize => ", toString homog, " contradicts max simplex size ", sz, " for a ", d, "-row matrix");
    A1 := if homog then augment A else A;
    vecs := transpose entries A1;
    sorted := tri//sort/sort; -- this sorts the triangulation
    T := new Triangulation from {
        cache => new CacheTable,
        symbol vectors => vecs,
        symbol max => sorted
        };
    T.cache#"point set" = homog;
    T.cache.matrix = A1;
    T
   )
triangulation(List, List) := Triangulation => opts -> (vecs, tri) -> (
    if #vecs == 0 then error "expected non-empty list of points or vectors";
    if not all(vecs, v -> instance(v, List)) then
        error "expected first argument to be a list of lists";
    triangulation(transpose matrix vecs, tri, opts)
   )

vectors = method()
vectors Triangulation := List => T -> T.vectors
max Triangulation := List => T -> T.max
matrix Triangulation := opts -> T -> T.cache.matrix

isWellDefined Triangulation := Boolean => T -> (
    topcomIsTriangulation(matrix T, max T, Homogenize => false)
    )

Triangulation == Triangulation := Boolean => (S, T) -> S === T

naiveIsTriangulation Triangulation := Boolean => T -> (
    naiveIsTriangulation(matrix T, max T) -- BUG: needs to take Homogenize, I think.
    )

-- The following is currently only for point sets
-- and is probably slow for larger triangulations too.
isTriangulation = method()
isTriangulation(Matrix, List) := (M, tri) -> (
    -- for the moment, we assume that M is a point set.
    d := #(tri#0) - 1;
    P := convexHull M;
    M' := M || matrix{{numcols M: 1}};
    volP := d! * volume P;
    volP2 := sum for t in tri list abs det M'_t;
    if volP != volP2 then (
        << "volume is not correct: " << volP << " != " << volP2 << endl;
        return false;
        );
    simplices := hashTable for t in tri list t => convexHull M_t;
    for x in subsets(keys simplices, 2) do 
        if dim(intersection(simplices#(x#0), simplices#(x#1))) == d then (
            << "simplices " << x << " intersect in full dimensional region" << endl;
            return false;
            );
    true
    )

-- isRegularTriangulation tests whether the secondary cone of (A, tri) has
-- nonempty interior, via the engine LP rawConeInteriorPoint.  This avoids
-- the topcom subprocess on every call (important for RegularOnly bistellar
-- search).  For comparison the topcom path remains available as
-- topcomIsRegularTriangulation T.
isRegularTriangulation = method(Options => {ChargeMatrix => null})
isRegularTriangulation Triangulation := Boolean => opts -> T -> (
    coneFullDim secondaryCone(T, opts)
    )
isRegularTriangulation(Matrix, List) := Boolean => opts -> (A, tri) -> (
    coneFullDim secondaryCone(A, tri, opts)
    )

-- regularTriangulationWeights returns a real-valued weight vector w such
-- that lifting (A_i, w_i) and taking the lower envelope reproduces tri,
-- computed via the engine LP.  For comparison the topcom path remains
-- available as topcomRegularTriangulationWeights T (returns rationals).
-- Returns null if the triangulation is not regular.
regularTriangulationWeights = method(Options => {ChargeMatrix => null})
regularTriangulationWeights Triangulation := List => opts -> T -> (
    A1 := matrix T;
    Q := if opts.ChargeMatrix === null then chargeMatrix A1 else opts.ChargeMatrix;
    weightsFromConeAndQ(secondaryCone(T, ChargeMatrix => Q), Q, numColumns A1)
    )
regularTriangulationWeights(Matrix, List) := List => opts -> (A, tri) -> (
    d := #(tri#0);
    A1 := if numrows A == d-1 then A || matrix{ toList(numcols A : 1) } else A;
    Q := if opts.ChargeMatrix === null then chargeMatrix A1 else opts.ChargeMatrix;
    weightsFromConeAndQ(secondaryCone(A1, tri, ChargeMatrix => Q), Q, numColumns A1)
    )

-- Private: cone full-dimensionality test via engine LP.  M is an integer
-- inequality matrix; cone is { x : M x >= 0 }.  Returns true iff there is
-- a strict interior point (equivalently, the secondary cone modulo lineality
-- has full dimension).
coneFullDim = M -> (
    if numrows M == 0 then return true;
    r := map(RR_53, rawConeInteriorPoint raw(-M));
    (entries r)#0#0 == 1.0
    )

-- Returns a primitive integer interior point x of the cone { x : A x >= 0 },
-- or null if the cone has no interior.  Built atop the engine LP
-- rawConeInteriorPoint: scale + round the real interior point so strict
-- feasibility is preserved (per row, |A_i . eps| <= sum |A_ij| / 2, so a
-- scale > rowBound / (2 tStar) suffices), then divide by gcd to keep
-- entries small.
interiorLatticePoint = method()
interiorLatticePoint Matrix := List => A -> (
    if numrows A == 0 then return toList(numcols A : 0);
    r := map(RR_53, rawConeInteriorPoint raw(-A));
    rE := (entries r)#0;
    if rE#0 != 1.0 then return null;
    tStar := rE#1;
    xFloat := drop(rE, 2);
    rowBound := max apply(entries A, row -> sum(row, abs));
    scale := if tStar > 0 then ceiling(rowBound / (2.0 * tStar)) + 1 else 1_ZZ;
    xInt := apply(xFloat, v -> round(scale * v));
    while any(flatten entries (A * (transpose matrix {xInt})), v -> v <= 0) do (
        scale = 2 * scale;
        xInt = apply(xFloat, v -> round(scale * v));
        );
    g := gcd xInt;
    if g > 1 then apply(xInt, v -> v // g) else xInt
    )

-- Private: lift an integer interior point of M (in Q-coordinates) to an
-- integer weight vector w of length n.  Any w with Q*w = t induces the
-- triangulation; the Moore-Penrose pseudo-inverse Q^T (Q Q^T)^{-1} t
-- gives the canonical rational representative, which we clear to integers
-- and reduce by gcd.
weightsFromConeAndQ = (M, Q, n) -> (
    if numrows M == 0 then return toList(n : 0);
    tInt := interiorLatticePoint M;
    if tInt === null then return null;
    Qq := promote(Q, QQ);
    tcol := transpose matrix {apply(tInt, x -> x * 1_QQ)};
    y := solve(Qq * (transpose Qq), tcol);
    wQ := flatten entries ((transpose Qq) * y);
    denom := lcm apply(wQ, denominator);
    wInt := apply(wQ, x -> lift(denom * x, ZZ));
    g := gcd wInt;
    if g > 1 then apply(wInt, x -> x // g) else wInt
    )

-- Triangulation-level dispatches on the topcom (Matrix, List) methods,
-- so users can compare engine-LP and topcom paths.
topcomIsRegularTriangulation Triangulation := opts -> T -> (
    topcomIsRegularTriangulation(matrix T, max T, Homogenize => false)
    )
topcomRegularTriangulationWeights Triangulation := opts -> T -> (
    topcomRegularTriangulationWeights(matrix T, max T, Homogenize => false)
    )
topcomIsRegularTriangulation Triangulation := opts -> T -> (
    topcomIsRegularTriangulation(matrix T, max T, Homogenize => false)
    )
topcomRegularTriangulationWeights Triangulation := opts -> T -> (
    topcomRegularTriangulationWeights(matrix T, max T, Homogenize => false)
    )

regularFineTriangulation = method(Options => {Homogenize => true})
regularFineTriangulation Matrix := Triangulation => opts -> (A) -> (
    tri := topcomRegularFineTriangulation(A, opts);
    if tri === null then null else
        triangulation(A, tri, opts)
    )

-- TODO/BUG: this ASSUMES (A, tri) is a triangulation.
-- TODO: add in Homogenize as an option?
isFine = method()
isFine(Matrix, List) := Boolean => (A, tri) -> (
    numcols A == tri//flatten//unique//length
    )

-- TODO/BUG: this ASSUMES (A, tri) is a triangulation.
isStar = method()
isStar(Matrix, List) := Boolean => (A, tri) -> (
    -- assumption?  last column of A is the zero element?  (or the one the star is taken with respect to).
    origin := numcols A - 1;
    all(tri, t -> member(origin, t))
    )

isStar Triangulation := Boolean => T -> isStar(matrix T, max T)
isFine Triangulation := Boolean => T -> isFine(matrix T, max T)

allTriangulations = method(Options => options topcomAllTriangulations)
allTriangulations Matrix := List => opts -> A -> (
    tris := topcomAllTriangulations(A, opts);
    for t in tris list triangulation(A, t, Homogenize => opts.Homogenize)
    )

delaunayWeights = method()
delaunayWeights Matrix := (A) -> (
    matrix{for i from 0 to numcols A - 1 list ((transpose A_{i}) * A_{i})_(0,0)}
    )

delaunaySubdivision = method()
delaunaySubdivision Matrix := A -> regularSubdivision(A, delaunayWeights A)

-----------------------------------------------------------
-- Chirotope code.  This could potentially go elsewhere? --
-----------------------------------------------------------

chirotope = method(Options => {Homogenize => true})

chirotope String := Chirotope => opts -> s -> (
    new Chirotope from {
        cache => new CacheTable,
        symbol String => s
        }
    )

toString Chirotope := String => OM -> OM.String

chirotope Matrix := Chirotope => opts -> A -> (
    chirotope chirotopeString(A, Homogenize => opts.Homogenize)
    )

naiveChirotope = method(Options => {Homogenize => true})
naiveChirotope Matrix := Chirotope => opts -> A -> (
    chirotope naiveChirotopeString(A, Homogenize => opts.Homogenize)
    )

Chirotope == Chirotope := Boolean => (C, D) -> toString C === toString D

--------------------------------------
-- Flips and triangulations ----------
--------------------------------------
-- This code is not an interface to topcom.
--   This is my own code, which used to be 
--   part of `StringTorics`, but seems more 
--   appropriate in a `Triangulations` package.
-- It is much slower than topcom, but allows to collect a certain number of triangulations.
-- TODO: make an interface that yields the next triangulation when called? (matching python usage).
--------------------------------------
sortTriangulation = method()
sortTriangulation List := (T) -> sort for t in T list sort t

-- TODO: why codim2 and codim2s?
codim2 = (tri) -> (
    -- find all pairs whose intersection is all but one of the vertices.
    n := #tri_0;
    select(subsets(tri, 2), v -> length toList ((set (v#0)) * (set (v#1))) == n-1)
    )

codim2s = (tri) -> (
    n := #tri_0;
    C2 := unique apply(subsets(tri, 2), v -> sort toList (set v#0 + set v#1));
    select(C2, x -> #x == n+1)
    )

flipCandidates = method()

-- For each codim-2 wall of `tri` (a pair of maximal simplices whose union has
-- size d+1), return the signed kernel partition of those d+1 points as a list
-- {neg, pos} of column-index lists.  These are the affine circuits supported
-- on walls of the triangulation, and are the candidate inputs to bistellarFlip.
-- Some candidates do not correspond to a legal flip in `tri` (the wrong half
-- of the circuit is the one present); for those, bistellarFlip returns null.
flipCandidates(Matrix, List) := (Amat, tri) -> (
    -- Amat: d x n with the last row all 1's (homogenized point configuration),
    --   or d-1 x n which is auto-homogenized here for direct (Matrix, List)
    --   callers.  The Triangulation method already stores a d-row matrix, so
    --   the branch is a no-op on that path.
    d := #(tri#0);
    assert all(tri, t -> #t == d);
    if numrows Amat == d-1 then
      Amat = Amat || matrix{ toList(numcols Amat : 1) };
    c2 := codim2s tri;
    unique for c in c2 list (
        z := flatten entries syz Amat_c;
        rsort {c_(positions(z, zi -> zi > 0)), c_(positions(z, zi -> zi < 0))}
        )
    )

flipCandidates Triangulation := T -> flipCandidates(matrix T, max T)

-- For each distinct codim-2 circuit of `tri`, return a triple {inTri, notInTri, z}:
--   inTri    : circuit-support indices i with z_i > 0.
--   notInTri : circuit-support indices i with z_i < 0.
--   z        : integer kernel of length #(inTri | notInTri), indexed by
--              position in sort(inTri | notInTri).  Equivalently, z gives
--              the secondary-cone facet inequality
--                sum_{i in inTri | notInTri} z_i * w_i  >=  0
--              for every w that induces `tri` (viewing z as a vector of
--              length n = numcols A, zero outside the circuit support).
--
-- For a "balanced" circuit (z has both signs), inTri is exactly the half
-- of the support whose corresponding simplices c\{v} appear in `tri`.  For a
-- "totally cyclic" circuit (z one-sided -- which can occur e.g. when 0
-- lies in the interior of cone(c), as in many complete simplicial fans),
-- z is all-positive after orientation, inTri = circuit support, and
-- notInTri = {}; only some of the simplices c\{v} for v in the support are
-- present in `tri`, but the inequality is still valid.
--
-- Note: a single circuit can appear as the linear-dependence relation in
-- multiple (d+1)-walls when the circuit support is strictly smaller than
-- d+1 (i.e., z has zeros).  Such walls produce identical inequalities of
-- length n; we deduplicate so each distinct circuit appears once.
wallCircuits = method()
wallCircuits(Matrix, List) := (Amat, tri) -> (
    d := #(tri#0);
    assert all(tri, t -> #t == d);
    if numrows Amat == d-1 then
      Amat = Amat || matrix{ toList(numcols Amat : 1) };
    triset := set (tri/sort);
    -- True iff some c\{v} for v in S is in `tri`.
    sideInTri := (S, c) -> any(S, v -> member(sort toList(set c - set {v}), triset));
    seen := new MutableHashTable;
    for c in codim2s tri list (
        Z := syz Amat_c;
        assert(numcols Z == 1);
        z := flatten entries Z;
        -- Restrict z to its support; that is the actual circuit.
        suppPos := positions(z, zi -> zi != 0);
        circuit := c_suppPos;
        zSupp := z_suppPos;
        pos := circuit_(positions(zSupp, zi -> zi > 0));
        neg := circuit_(positions(zSupp, zi -> zi < 0));
        (inTri, notInTri, zSigned) := if sideInTri(pos, c) then (pos, neg, zSupp)
                                       else (neg, pos, apply(zSupp, x -> -x));
        key := {inTri, notInTri};
        if seen#?key then continue;
        seen#key = true;
        {inTri, notInTri, zSigned}
        )
    )

wallCircuits Triangulation := T -> (
    if T.cache#?(symbol wallCircuits) then T.cache#(symbol wallCircuits)
    else T.cache#(symbol wallCircuits) = wallCircuits(matrix T, max T)
    )

-- Charge matrix Q of a configuration A: an integer matrix whose rows form
-- a basis of ker A over ZZ.  Rows generate the lattice of linear relations
-- on the columns of A.  In the GLSM/CY context the rows are the U(1)
-- charges of the chiral fields.  Defined as transpose syz A; users who
-- want a reduced basis (e.g., LLL) can compute it themselves and pass it
-- via ChargeMatrix => myQ to secondaryCone.
chargeMatrix = method()
chargeMatrix Matrix := Matrix => A -> transpose syz A
chargeMatrix Triangulation := Matrix => T -> chargeMatrix matrix T

-- Secondary cone of a triangulation, returned as an integer inequality
-- matrix M such that the secondary cone (modulo lineality) is
--   { t in QQ^(N-d) : M * t >= 0 },
-- where t = Q * w represents weights modulo the row span of A (the
-- lineality space).  By default Q = chargeMatrix(matrix T) (rows are a
-- basis of ker A); pass ChargeMatrix => myQ to use a different basis.
-- Each wall-circuit row z (length N, in ker A) is expressed as z = z' * Q
-- with z' integer of length (N-d); the rows of M are these z'.
-- (A, tri) is regular iff this cone has nonempty interior, which can be
-- tested with rawConeInteriorPoint(raw -M).
secondaryCone = method(Options => {ChargeMatrix => null})
secondaryCone(Matrix, List) := Matrix => opts -> (A, tri) -> (
    -- Match wallCircuits' auto-homogenize convention so chargeMatrix and
    -- wallCircuits see the same matrix.
    d := #(tri#0);
    A1 := if numrows A == d-1 then A || matrix{ toList(numcols A : 1) } else A;
    Q := if opts.ChargeMatrix === null then chargeMatrix A1 else opts.ChargeMatrix;
    nQ := numrows Q;
    n := numcols A1;
    wcs := wallCircuits(A1, tri);
    if #wcs == 0 then return map(ZZ^0, ZZ^nQ, 0);
    -- Build the length-N inequalities row-by-row, then project to the
    -- charge lattice via z = z' * Q  <=>  z^T = Q^T * (z')^T.
    M := matrix for w in wcs list (
        c := sort flatten {w#0, w#1};
        z := w#2;
        cz := hashTable apply(#c, k -> c#k => z#k);
        for i from 0 to n-1 list (if cz#?i then cz#i else 0)
        );
    transpose ((transpose M) // (transpose Q))
    )

secondaryCone Triangulation := Matrix => opts -> T -> secondaryCone(matrix T, max T, opts)

-- previously in triangulations-code.m2,
link = method()
link(List, List) := (tau, triangulation) -> (
    S := select(triangulation, t -> isSubset(tau, t));
    sort for s in S list sort toList (set s - set tau)
    )

-- Note: a Triangulation stores its points as vectors (i.e. the hogenization is already done or not done).
-- Second, this returns all flips (not just "Fine" flips), i.e. ones that would change the number of vectors.
flips = method(Options => {RegularOnly => true})
flips Triangulation := List => opts -> T -> (
    topcomFlips(matrix T, max T, Homogenize => false, RegularOnly => opts.RegularOnly)
    )

bistellarFlip = method()

-- Perform the bistellar flip on `tri` determined by `affineCircuit`, where
-- `affineCircuit = {neg, pos}` is the signed partition of a circuit (typically
-- one returned by flipCandidates).  The two triangulations of the circuit are
-- T1 = {whole - {v} : v in neg} and T2 = {whole - {v} : v in pos}.  A flip is
-- legal iff exactly one of T1, T2 sits in `tri` with a common link L; in that
-- case we replace L*T_present with L*T_other.  Returns null if the candidate
-- is not flippable in `tri` (the simplices of one side are present but with
-- non-matching links, or neither side is fully present).
bistellarFlip(List,List) := (tri, affineCircuit) -> (
    whole := sort unique flatten affineCircuit;
    T1 := for v in affineCircuit#0 list sort toList (set whole - set {v});
    T2 := for v in affineCircuit#1 list sort toList (set whole - set {v});
    S1 := unique for tau in T1 list link(tau, tri);
    S2 := unique for tau in T2 list link(tau, tri);
    if #S1 > 1 or #S2 > 1 then return null;
    S1 = first S1;
    S2 = first S2;
    -- Both halves of the circuit cannot simultaneously be present in a
    -- triangulation; if they were, the d+1 points of `whole` would be
    -- triangulated two different ways at once.
    assert(#S1 == 0 or #S2 == 0);
    if #S1 == 0 then (
        S1 = S2;
        (T1,T2) = (T2,T1);
        );
    T := set tri;
    outgoing := set flatten for s in S1 list for tau in T1 list sort join(s,tau);
    incoming := set flatten for s in S1 list for tau in T2 list sort join(s,tau);
    sortTriangulation toList(T - outgoing + incoming)
    )

bistellarFlip(Triangulation, List) := (T, affineCircuit) -> (
    tri := bistellarFlip(max T, affineCircuit);
    if tri === null then null else
        triangulation(matrix T, tri, Homogenize => false)
    )

neighbors = method(Options => {Fine => true})
neighbors Triangulation := List => opts -> T -> (
    -- each element of the result is of the form:
    -- {circuit, triangulation}
    -- With Fine => true (default), only circuits with both sides of size >= 2
    -- are considered, so the support of the triangulation is preserved.
    -- With Fine => false, size-1 sides are also allowed, which can drop a
    -- vertex from the support (or, less commonly, add one).
    circuits0 := flipCandidates T;
    circuits := if opts.Fine then
        select(circuits0, z -> #z#0 > 1 and #z#1 > 1)
    else
        select(circuits0, z -> #z#0 >= 1 and #z#1 >= 1);
    for c in circuits list (
        T1 := bistellarFlip(T, c);
        if T1 === null then continue else {c, T1}
        )
    )

generateTriangulations = method(Options => {Limit=>infinity, RegularOnly=>false, Fine => true, Homogenize => true, Strategy => "engine"})
generateTriangulations Triangulation := opts -> T0 -> (
    -- BFS over the bistellar-flip graph starting at T0.
    -- 'seen' records every triangulation we have ever encountered, regardless of
    -- whether it was kept; this prevents repeated isRegularTriangulation calls on
    -- the same non-regular triangulation reached via different paths.
    -- 'queue' is both the BFS frontier and the result-so-far: a MutableList
    -- giving O(1) push (queue#(#queue) = x) and O(1) pop (advance nextIdx).
    -- Fine controls whether support-changing flips are considered (see neighbors).
    -- Strategy selects the regularity test: "engine" (default, uses
    -- rawConeInteriorPoint on secondaryCone) or "topcom" (subprocess).
    isReg := if opts.Strategy === "topcom" then topcomIsRegularTriangulation
             else isRegularTriangulation;
    seen := new MutableHashTable;
    seen#T0 = true;
    queue := new MutableList from {T0};
    nextIdx := 0;
    while nextIdx < #queue and #queue < opts.Limit do (
        cur := queue#nextIdx;
        nextIdx = nextIdx + 1;
        for cT1 in neighbors(cur, Fine => opts.Fine) do (
            if #queue >= opts.Limit then break;
            T1 := cT1#1;
            if seen#?T1 then continue;
            seen#T1 = true;
            if opts.RegularOnly and not isReg T1 then continue;
            queue#(#queue) = T1;
            );
        if debugLevel > 0 then
            << "todo = " << (#queue - nextIdx) << " and #triang = " << #queue << endl;
        );
    toList queue
    )
generateTriangulations Matrix := opts -> Amat -> (
    T := regularFineTriangulation(Amat, Homogenize => opts.Homogenize);
    generateTriangulations(T, opts)
    )
generateTriangulations(Matrix, List) := opts -> (Amat, triang) -> (
    T := triangulation(Amat, triang, Homogenize => opts.Homogenize); -- really want to Homogenize here?
    tris := generateTriangulations(T, opts);
    tris/max -- strip the triangulation class from these.  This matches (up to permutation) output of allTriangulations
    )

flipGraph = method(Options => {Limit=>infinity, RegularOnly=>false, Fine => true, Homogenize => true, Strategy => "engine"})
flipGraph Triangulation := HashTable => opts -> T0 -> (
    -- BFS over the bistellar-flip graph starting at T0, recording both the
    -- list of triangulations reached and the edges (i, j, circuit) connecting them.
    -- index#T encodes either the position in 'queue' (a non-negative integer)
    -- or -1 to mark a triangulation that was visited but rejected (e.g.
    -- non-regular under RegularOnly => true), so we don't re-test it.
    -- Edges are recorded once per undirected pair, when discovered from the
    -- lower-indexed endpoint (j > i guard skips back-edges to already-processed nodes).
    -- Strategy selects the regularity test (see generateTriangulations).
    isReg := if opts.Strategy === "topcom" then topcomIsRegularTriangulation
             else isRegularTriangulation;
    index := new MutableHashTable;
    index#T0 = 0;
    queue := new MutableList from {T0};
    edges := new MutableList from {};
    nextIdx := 0;
    while nextIdx < #queue and #queue < opts.Limit do (
        cur := queue#nextIdx;
        i := nextIdx;
        nextIdx = nextIdx + 1;
        for cT1 in neighbors(cur, Fine => opts.Fine) do (
            if #queue >= opts.Limit then break;
            (c, T1) := (cT1#0, cT1#1);
            if index#?T1 then (
                jSeen := index#T1;
                if jSeen === -1 then continue;
                if jSeen > i then edges#(#edges) = (i, jSeen, c);
                continue;
                );
            if opts.RegularOnly and not isReg T1 then (
                index#T1 = -1;
                continue;
                );
            jNew := #queue;
            index#T1 = jNew;
            queue#(#queue) = T1;
            edges#(#edges) = (i, jNew, c);
            );
        if debugLevel > 0 then
            << "todo = " << (#queue - nextIdx) << " and #triang = " << #queue << endl;
        );
    hashTable {
        symbol Triangulations => toList queue,
        symbol Edges => toList edges
        }
    )

flipGraph Matrix := HashTable => opts -> Amat -> (
    flipGraph(regularFineTriangulation(Amat, Homogenize => opts.Homogenize), opts)
    )
flipGraph(Matrix, List) := HashTable => opts -> (Amat, triang) -> (
    flipGraph(triangulation(Amat, triang, Homogenize => opts.Homogenize),
        Limit => opts.Limit, RegularOnly => opts.RegularOnly, Fine => opts.Fine,
        Strategy => opts.Strategy)
    )

-- Secondary fan: the flipGraph of fine regular triangulations of A,
-- augmented with each triangulation's secondaryCone in a shared
-- ChargeMatrix Q.  Returns a HashTable with keys
--   Triangulations  -- list of fine regular triangulations of A
--   Edges           -- list of (i, j, circuit) flips between them
--   Cones           -- list of integer matrices, one per triangulation,
--                      each giving the inequalities of the corresponding
--                      secondary cone in coordinates of Q
--   ChargeMatrix    -- the Q used (so callers can lift t in (N-d)-coords
--                      back to weights w in N-coords via solve(Q, t))
secondaryFan = method(Options => {
        Limit => infinity,
        ChargeMatrix => null,
        Strategy => "engine"
        })
secondaryFan Triangulation := HashTable => opts -> T -> (
    Q := if opts.ChargeMatrix === null then chargeMatrix T else opts.ChargeMatrix;
    G := flipGraph(T,
        Limit => opts.Limit,
        RegularOnly => true,
        Fine => true,
        Strategy => opts.Strategy);
    cones := apply(G.Triangulations, Ti -> secondaryCone(Ti, ChargeMatrix => Q));
    hashTable {
        symbol Triangulations => G.Triangulations,
        symbol Edges          => G.Edges,
        symbol Cones          => cones,
        symbol ChargeMatrix   => Q
        }
    )
secondaryFan Matrix := HashTable => opts -> A -> (
    secondaryFan(regularFineTriangulation A, opts)
    )

volumeVector = method()
volumeVector (Matrix, List) := (Amat, tri) -> (
    if #tri == 0 then error "expected at least one simplex";
    nelems := #tri#0;
    d := nelems-1;
    if not all(tri, f -> #f == nelems)
    then error "expected a triangulation";
    if numrows Amat =!= nelems then Amat = Amat || matrix{{(numcols Amat):1}};
    if numrows Amat =!= nelems then error "triangulation not compatible with matrix";
    H := hashTable for t in tri list t => (abs det Amat_t)/d!;
    if debugLevel > 0 then
        << "Volume = " << (sum values H)/d! << endl;
    for i from 0 to numColumns Amat - 1 list (
        T := select(tri, t -> member(i,t));
        sum for t in T list H#t
        )
    )
volumeVector Triangulation := T -> volumeVector(matrix T, max T)

gkzVector = volumeVector

beginDocumentation()

-*
      needsPackage "StringTorics"
      topes = kreuzerSkarke(5, Limit => 10)
      Q = reflexivePolytope topes_7
      isFavorable Q
      rays Q
      A = matrix topes_7
      P2 = polar convexHull A
      Amat = latticePointList P2

      -- one from h11=7
      --LP = {{-1, -1, -1, 1}, {-1, -1, -1, 2}, {-1, -1, 0, 1}, {-1, 0, -1, 1}, {-1, 0, 1, 0}, {-1, 0, 2, 0}, {-1, 1, 0, 0}, {-1, 2, 0, -1}, {0, -1, -1, 1}, {0, 1, 1, -1}, {2, 0, 0, -1}, {0,0,0,0}}      
*-

doc ///
  Key
    Triangulations
  Headline
    generating and manipulating triangulations of point or vector configurations
  Description
    Text
      {\bf Warning!} This package is experimental, and the interface
      may still be cleaned up and changed.  Use only if that bothers
      you not.
    Text
      @SUBSECTION "Data of a triangulation"@
    Text
      @UL {
          TO Triangulation,
          TO (max, Triangulation),
          TO (vectors, Triangulation),
          TO (matrix, Triangulation)
          }@
    Text
      @SUBSECTION "Creating triangulations"@
    Text
      @UL {
          TO (triangulation, Matrix, List),
          TO (regularFineTriangulation, Matrix),
          TO (fineStarTriangulation, Matrix, List),
          TO (regularFineStarTriangulation, Matrix),
          TO (generateTriangulations, Triangulation),
          TO (allTriangulations, Matrix)
          }@
    Text
      @SUBSECTION "Properties of triangulations"@
    Text
      @UL {
          TO (isWellDefined, Triangulation),
          TO (isRegularTriangulation, Triangulation),
          TO (regularTriangulationWeights, Triangulation),
          TO (isStar, Triangulation),
          TO (isFine, Triangulation),
          TO (naiveIsTriangulation, Triangulation)
          }@
    Text
      @SUBSECTION "Exploring the set of triangulations"@
    Text
      @UL {
          TO (flipCandidates, Triangulation),
          TO (flips, Triangulation),
          TO (bistellarFlip, Triangulation, List),
          TO (neighbors, Triangulation),
          TO (flipGraph, Triangulation)
          }@
    Text
      @SUBSECTION "Volumes and GKZ vectors"@
    Text
      @UL {
          TO (volumeVector, Triangulation)
          }@
    Text
      @SUBSECTION "Delaunay subdivisions"@
    Text
      @UL {
          TO (delaunayWeights, Matrix),
          TO (delaunaySubdivision, Matrix)
          }@
    Text
      @SUBSECTION "Chirotopes"@
    Text
      @UL {
          TO Chirotope,
          TO (chirotope, Matrix),
          TO (naiveChirotope, Matrix)
          }@
    Text
      This package is designed to help compute and explore the set of all (or many) triangulations
      of a point set or polytope.

      We give a sample use of this package.
    Example
      LP = {{-1, 0, -1, 1}, {-1, 0, 1, 0}, {-1, 0, 2, -1}, {-1, 1, -1, 0}, {1, 0, -1, 0}, {1, 0, 1, 0}, {2, -1, -1, 0}, {0, 0, 1, 0}, {1, 0, 0, 0}, {0,0,0,0}}      
      A = transpose matrix LP
      elapsedTime Ts = allTriangulations(A, Fine => true);
      select(Ts, T -> isStar T)
      #oo == 1
      #Ts == 51

      T = regularFineTriangulation A
      elapsedTime Ts2 = generateTriangulations T;
      #Ts2 == #Ts
  SeeAlso
    "Polyhedra::Polyhedra"
    "Topcom::Topcom"
    "ReflexivePolytopesDB::ReflexivePolytopesDB"
///

doc ///
  Key
    isRegularTriangulation
    (isRegularTriangulation, Triangulation)
    (isRegularTriangulation, Matrix, List)
    [isRegularTriangulation, ChargeMatrix]
  Headline
    determine if a given triangulation is a regular triangulation
  Usage
    isRegularTriangulation T
  Inputs
    T:Triangulation
      a triangulation of a point or vector configuration
    ChargeMatrix => Matrix
      an integer matrix whose rows are a $\mathbb{Z}$-basis of $\ker A$;
      passed through to @TO secondaryCone@.  Defaults to
      {\tt chargeMatrix T}
  Outputs
    :Boolean
      whether the given triangulation is regular
  Description
    Text
      A triangulation is called regular if it can be constructed in the following way: place the
      point set in one higher dimension at various heights in the new variable.  Compute the
      convex hull.  Collect the list of facets with downward pointing normal (last coordinate of normal vector
      is negative).  If each of these is a simplex, then these form a triangulation of the
      original point set.  A triangulation which arises this way is called {\it regular}.  See
      the book [deLoera et al] for more details and many beautiful properties of such triangulations.
    Text
      Tested via the engine LP {\tt rawConeInteriorPoint}: the triangulation
      is regular iff its @TO secondaryCone@ has nonempty interior.  The
      topcom-based test, kept for comparison, is available as
      @TO topcomIsRegularTriangulation@.
    Text
      The following example is one of the simplest examples of a non-regular
      triangulation.  Notice that {\tt tri} is a triangulation of the
      polytope which is the convex hull of the columns of $A$, which are
      the only points allowed in the triangulation.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      tri = {{0,1,2}, {1,3,5}, {2,3,4}, {0,1,5},
          {0,2,4}, {3,4,5}, {1,2,3}}
      T = triangulation(A, tri)
    Text
      We check that {\tt T} is indeed a triangulation, and whether it is a regular triangulation.
    Example
      isWellDefined T
      isRegularTriangulation T
  SeeAlso
    (regularTriangulationWeights, Triangulation)
    secondaryCone
    "Topcom::topcomIsRegularTriangulation"
    regularFineTriangulation
    (isWellDefined, Triangulation)
///

doc ///
  Key
    Triangulation
  Headline
    a triangulation of a point or vector configuration
  Description
    Text
      A {\tt Triangulation} packages a point (or vector) configuration $A$
      together with a triangulation of the columns of $A$, given as a list
      of maximal simplices.  Each simplex is a sorted list of column
      indices into $A$, and the outer list is sorted as well.
    Text
      Construct a {\tt Triangulation} with @TO triangulation@ from an
      explicit matrix and list, or with @TO regularFineTriangulation@ from
      just a matrix.  Inspect one with the accessors
      @TO (max, Triangulation)@, @TO (vectors, Triangulation)@, and
      @TO (matrix, Triangulation)@.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      T = regularFineTriangulation A
      max T
      matrix T
      vectors T
    Text
      By default, the matrix stored in {\tt T} is the {\it homogenized}
      configuration, with a final row of $1$'s appended; most algorithms
      in this package operate on homogenized vectors.  Pass
      {\tt Homogenize => false} to @TO triangulation@ if the input is
      already a vector configuration.
  SeeAlso
    triangulation
    regularFineTriangulation
    (max, Triangulation)
    (vectors, Triangulation)
    (matrix, Triangulation)
///

doc ///
  Key
    triangulation
    (triangulation, Matrix, List)
    (triangulation, List, List)
    [triangulation, Homogenize]
  Headline
    make a Triangulation object
  Usage
    triangulation(A, T)
    triangulation(vecs, T)
  Inputs
    A:Matrix
      whose columns are the points or vectors of the configuration
    vecs:List
      a list of points or vectors (each entry a list of integers or rationals);
      equivalent to passing {\tt transpose matrix vecs}
    T:List
      representing a triangulation of the columns of $A$ (each element in the list
      is a list of indices in the range $0, \ldots, n-1$, where $n$ is the number of
      columns of $A$)
    Homogenize => Boolean
      controls how $A$ is interpreted.  By default ({\tt null}) the
      interpretation is inferred from the size of the max simplices in $T$
      (see Description below).  Pass {\tt true} to force a point-set
      interpretation (a row of $1$'s is appended to $A$ before storing) or
      {\tt false} to force a vector-configuration interpretation (matrix
      stored as-is).  An explicit value inconsistent with the simplex sizes
      is an error.
  Outputs
    :Triangulation
      A @TO Triangulation@ object.  Very little computation is performed.  The matrix and list representing
      a triangulation is packaged into an object to make clear that it is a triangulation
  Description
    Text
      The {\tt Homogenize} option determines how $A$ is interpreted, not
      just how it is stored.  By default the constructor inspects the size
      of the max simplices in $T$:
    Text
      $\bullet$ if every max simplex has size {\tt numRows A + 1},
      $A$ is treated as a point set and the stored matrix is $A$ with an
      appended row of $1$'s (homogenisation);
    Text
      $\bullet$ if every max simplex has size {\tt numRows A},
      $A$ is already a vector configuration (e.g., a $d \times n$ matrix
      of rays in $\RR^d$) and is stored as given.
    Text
      Downstream routines (regularity, flips, chirotope, $\ldots$) read the
      ambient dimension from the stored matrix, so the two interpretations
      yield genuinely different triangulations of the same combinatorial $T$.
      Passing {\tt Homogenize => true} or {\tt false} forces a particular
      interpretation; an explicit value inconsistent with the inferred one
      is rejected.
    Example
      P = hypercube 3
      A = vertices P
      T = topcomRegularFineTriangulation A
      tri = triangulation(A, T)
      matrix tri
      vectors tri
      max tri
      isWellDefined tri
      netList flipCandidates tri
      isFine tri
      isStar tri
      isRegularTriangulation tri
    Text
      A vector configuration: simplex size matches the row count of $A$,
      so no homogenisation is applied.
    Example
      V = transpose matrix {{1,1,1,1},{0,1,1,1},{1,0,1,1},{1,1,0,1},{0,0,1,1},{0,1,0,1},{1,0,0,1},{0,0,0,1}}
      triV = {{0,1,2,3},{1,2,3,4},{1,3,4,5},{2,3,4,6},{3,4,5,6},{4,5,6,7}}
      Tv = triangulation(V, triV)
      matrix Tv
    Text
      The (List, List) form is convenient when the configuration is given
      as a list of vectors:
    Example
      triangulation({{0,0},{1,0},{0,1},{1,1}}, {{0,1,2},{1,2,3}})
  Caveat
  SeeAlso
    Triangulation
    (max, Triangulation)
    (vectors, Triangulation)
    (matrix, Triangulation)
    regularFineTriangulation
    (isWellDefined, Triangulation)
///

doc ///
  Key
    regularFineTriangulation
    (regularFineTriangulation, Matrix)
    [regularFineTriangulation, Homogenize]
  Headline
    a regular fine triangulation of a point or vector configuration
  Usage
    T = regularFineTriangulation A
  Inputs
    A:Matrix
      whose columns are the points of the configuration
    Homogenize => Boolean
      if true (default), $A$ is augmented with a final row of $1$'s and
      treated as a point set; if false, the columns are treated as a
      vector configuration in their own right
  Outputs
    T:Triangulation
      a regular triangulation that uses every column of $A$, or
      @TO null@ if no such triangulation exists
  Description
    Text
      Constructs a regular fine triangulation by calling
      @TO "Topcom::topcomRegularFineTriangulation"@ and wrapping the
      result in a @TO Triangulation@.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      T = regularFineTriangulation A
      isFine T
      isRegularTriangulation T
  SeeAlso
    triangulation
    isRegularTriangulation
    isFine
    "Topcom::topcomRegularFineTriangulation"
///

doc ///
  Key
    fineStarTriangulation
    (fineStarTriangulation, Matrix, List)
    [fineStarTriangulation, ConeIndex]
  Headline
    refine a triangulation to a fine star triangulation by coning over a center
  Usage
    star = fineStarTriangulation(A, tri)
  Inputs
    A:Matrix
      whose columns are the boundary points of a polytope (and possibly
      an interior cone center)
    tri:List
      a triangulation of the columns of $A$, typically a triangulation
      of the boundary
    ConeIndex => ZZ
      the column index to use as the cone center; if {\tt null} (default),
      uses {\tt numColumns A}, treating the cone center as a new point
      to be appended
  Outputs
    :List
      a list of simplices forming a star triangulation centered at the
      chosen index: each maximal simplex of {\tt tri}, intersected with
      each facet of $\mathrm{conv}(A)$, is appended with the cone index
  Description
    Text
      Given a triangulation {\tt tri} of (the boundary of) a polytope
      and an interior cone center, this function refines {\tt tri} into
      a star triangulation by joining each boundary simplex to the
      center.  The center is identified by its column index in $A$ (or,
      by default, treated as a new point at index {\tt numColumns A}).
    Example
      A = transpose matrix {{1,1},{-1,1},{-1,-1},{1,-1}}
      tri = topcomRegularFineTriangulation A
      fineStarTriangulation(A, tri)
  Caveat
    Returns a {\tt List} of simplices, not a @TO Triangulation@; wrap
    with @TO triangulation@ to attach the configuration matrix.
  SeeAlso
    regularFineStarTriangulation
    isStar
    triangulation
///

doc ///
  Key
    regularFineStarTriangulation
    (regularFineStarTriangulation, Matrix)
    [regularFineStarTriangulation, ConeIndex]
  Headline
    a regular fine star triangulation of a polytope's boundary
  Usage
    star = regularFineStarTriangulation A
  Inputs
    A:Matrix
      whose columns are the boundary points of a polytope
    ConeIndex => ZZ
      the column index to use as the cone center; if {\tt null} (default),
      the cone center is treated as a new point at index
      {\tt numColumns A}
  Outputs
    :List
      a list of simplices forming a star triangulation, equivalent to
      {\tt fineStarTriangulation(A, topcomRegularFineTriangulation A)}
  Description
    Text
      Convenience wrapper that computes a regular fine triangulation of
      $A$'s boundary via topcom and then cones each boundary simplex to
      the chosen center.
    Example
      A = transpose matrix {{1,1},{-1,1},{-1,-1},{1,-1}}
      regularFineStarTriangulation A
  Caveat
    Returns a {\tt List} of simplices, not a @TO Triangulation@.
  SeeAlso
    fineStarTriangulation
    regularFineTriangulation
    isStar
    triangulation
///

doc ///
  Key
    (max, Triangulation)
  Headline
    the maximal simplices of a triangulation
  Usage
    max T
  Inputs
    T:Triangulation
  Outputs
    :List
      of lists, each a sorted list of column indices defining a maximal
      simplex of $T$
  Description
    Text
      Returns the list of maximal simplices of the triangulation.  The
      outer list is sorted, and each inner list (a simplex) is sorted as
      well, so that two triangulations with the same simplices have
      identical {\tt max}.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      T = regularFineTriangulation A
      max T
  SeeAlso
    Triangulation
    (vectors, Triangulation)
    (matrix, Triangulation)
///

doc ///
  Key
    vectors
    (vectors, Triangulation)
  Headline
    the column vectors of the configuration underlying a triangulation
  Usage
    vectors T
  Inputs
    T:Triangulation
  Outputs
    :List
      whose $i$-th entry is the $i$-th column of @TO (matrix, Triangulation)@,
      represented as a list of integers
  Description
    Text
      Returns the columns of the matrix associated to $T$ as a list of
      column vectors.  If $T$ was constructed with the default
      {\tt Homogenize => true}, each vector ends with a $1$.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      T = regularFineTriangulation A
      vectors T
  SeeAlso
    Triangulation
    (matrix, Triangulation)
    (max, Triangulation)
///

doc ///
  Key
    (matrix, Triangulation)
  Headline
    the matrix whose columns are the points of a triangulation
  Usage
    matrix T
  Inputs
    T:Triangulation
  Outputs
    :Matrix
      whose columns are the points (or vectors) of the configuration
      that $T$ triangulates
  Description
    Text
      Returns the configuration matrix stored in $T$.  By default this is
      the homogenized matrix, with a final row of $1$'s appended; if $T$
      was constructed with {\tt Homogenize => false}, it is the original
      matrix passed to the constructor.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      T = regularFineTriangulation A
      matrix T
      numRows matrix T == numRows A + 1
  SeeAlso
    Triangulation
    (vectors, Triangulation)
    (max, Triangulation)
    triangulation
///

doc ///
  Key
    (isWellDefined, Triangulation)
  Headline
    test whether the simplices of a triangulation form a valid triangulation
  Usage
    isWellDefined T
  Inputs
    T:Triangulation
  Outputs
    :Boolean
      whether the simplices in {\tt max T} form a valid triangulation of
      the columns of {\tt matrix T}
  Description
    Text
      Defers to @TO "Topcom::topcomIsTriangulation"@ for the
      authoritative check.  A {\tt Triangulation} object is just
      packaging: its construction does not verify that the input simplices
      actually form a triangulation, so use this method to confirm.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      T = regularFineTriangulation A
      isWellDefined T
  SeeAlso
    naiveIsTriangulation
    isRegularTriangulation
    triangulation
///

doc ///
  Key
    isFine
    (isFine, Triangulation)
    (isFine, Matrix, List)
  Headline
    test whether a triangulation uses every point in the configuration
  Usage
    isFine T
    isFine(A, tri)
  Inputs
    T:Triangulation
    A:Matrix
      whose columns are the points of a configuration
    tri:List
      a triangulation of the columns of $A$
  Outputs
    :Boolean
      whether every column index appears in some maximal simplex of the
      triangulation
  Description
    Text
      A triangulation is {\it fine} if every point of the configuration is
      a vertex of some simplex.  Non-fine triangulations triangulate the
      convex hull but leave one or more points (typically interior or on
      the relative boundary) unused.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      Ts = allTriangulations A;
      tally apply(Ts, isFine)
  Caveat
    This function does not check that {\tt tri} is a valid triangulation;
    it only inspects the support.  See
    @TO (isWellDefined, Triangulation)@ or @TO naiveIsTriangulation@
    for that.
  SeeAlso
    isStar
    (isWellDefined, Triangulation)
    isRegularTriangulation
///

doc ///
  Key
    isStar
    (isStar, Triangulation)
    (isStar, Matrix, List)
  Headline
    test whether a triangulation is a star with respect to its last point
  Usage
    isStar T
    isStar(A, tri)
  Inputs
    T:Triangulation
    A:Matrix
    tri:List
  Outputs
    :Boolean
      whether every maximal simplex contains the last column index of
      the configuration
  Description
    Text
      A triangulation is a {\it star} (with respect to a distinguished
      point $p$) if every maximal simplex contains $p$ as a vertex.  This
      function uses the convention that $p$ is the {\bf last} column of
      the configuration: $T$ is a star iff every simplex of {\tt max T}
      contains the index {\tt numColumns(matrix T) - 1}.
    Text
      This convention is common when triangulating a reflexive polytope
      with the origin placed last: a fine star triangulation refines the
      polytope into simplices all sharing the origin.
    Example
      A = transpose matrix {{1,1},{-1,1},{-1,-1},{1,-1},{0,0}}
      T = regularFineTriangulation A
      max T
      isStar T
  Caveat
    This function does not check that {\tt tri} is a valid triangulation
    or that the last column is genuinely interior; it only inspects the
    indices.
  SeeAlso
    isFine
    (isWellDefined, Triangulation)
    fineStarTriangulation
    regularFineStarTriangulation
///

doc ///
  Key
    regularTriangulationWeights
    (regularTriangulationWeights, Triangulation)
    (regularTriangulationWeights, Matrix, List)
    [regularTriangulationWeights, ChargeMatrix]
  Headline
    height vector inducing a regular triangulation, if one exists
  Usage
    regularTriangulationWeights T
    regularTriangulationWeights(A, tri)
  Inputs
    T:Triangulation
    A:Matrix
    tri:List
    ChargeMatrix => Matrix
      an integer matrix whose rows are a $\mathbb{Z}$-basis of $\ker A$;
      passed through to @TO secondaryCone@.  Defaults to
      {\tt chargeMatrix T}
  Outputs
    :List
      of @TO RR@ values (machine reals), one per column of the
      configuration: heights whose lower envelope yields {\tt tri}; or
      @TO null@ if the triangulation is not regular
  Description
    Text
      A triangulation is regular iff there is a height vector such that
      lifting each point to that height and taking the lower facets of
      the resulting upper hull recovers exactly the maximal simplices of
      the triangulation.
    Text
      Computed via the engine LP {\tt rawConeInteriorPoint} on the
      @TO secondaryCone@: an interior point $t \in \mathbb{R}^{N-d}$ is
      lifted back to weights $w \in \mathbb{R}^N$ via the Moore-Penrose
      pseudo-inverse of the charge matrix, $w = Q^\top (Q Q^\top)^{-1} t$.
      Any $w$ with $Q w = t$ induces the same triangulation; this lift is
      the canonical representative.  For a rational/integer answer
      compatible with topcom's output, use
      @TO topcomRegularTriangulationWeights@.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      T = regularFineTriangulation A
      regularTriangulationWeights T
      isRegularTriangulation T
    Text
      For a non-regular triangulation, the function returns @TO null@:
    Example
      tri = {{0,1,2}, {1,3,5}, {2,3,4}, {0,1,5},
             {0,2,4}, {3,4,5}, {1,2,3}}
      Tnr = triangulation(A, tri)
      isRegularTriangulation Tnr
      regularTriangulationWeights Tnr
  SeeAlso
    isRegularTriangulation
    secondaryCone
    chargeMatrix
    "Topcom::topcomRegularTriangulationWeights"
///

doc ///
  Key
    naiveIsTriangulation
    (naiveIsTriangulation, Triangulation)
    (naiveIsTriangulation, Matrix, List)
    (naiveIsTriangulation, Matrix, List, List)
  Headline
    test whether a list of simplices is a triangulation, in pure Macaulay2
  Usage
    naiveIsTriangulation T
    naiveIsTriangulation(A, tri)
    naiveIsTriangulation(A, circuits, tri)
  Inputs
    T:Triangulation
    A:Matrix
      whose columns are the points of the configuration
    tri:List
      a putative triangulation of the columns of $A$
    circuits:List
      the oriented circuits of $A$; if omitted, computed via
      @TO "Topcom::orientedCircuits"@
  Outputs
    :Boolean
      whether {\tt tri} forms a valid triangulation of the convex hull of
      the columns of $A$
  Description
    Text
      A self-contained Macaulay2 implementation of triangulation
      validity, complementary to @TO (isWellDefined, Triangulation)@
      which defers to topcom.  Two checks are performed: each codim-1
      wall must either lie on the boundary of $\mathrm{conv}(A)$ and
      occur in exactly one simplex, or lie in the interior and occur in
      exactly two; and for each oriented circuit, at most one of the two
      sides may be fully present in the triangulation.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      T = regularFineTriangulation A
      naiveIsTriangulation T
      isWellDefined T
  Caveat
    This function carries internal {\tt TODO} comments expressing
    uncertainty about correctness in edge cases; for an authoritative
    answer, prefer @TO (isWellDefined, Triangulation)@.
  SeeAlso
    (isWellDefined, Triangulation)
    "Topcom::orientedCircuits"
///

doc ///
  Key
    (allTriangulations, Matrix)
    allTriangulations
    [allTriangulations, ConnectedToRegular]
    [allTriangulations, Fine]
    [allTriangulations, Homogenize]
    [allTriangulations, RegularOnly]
  Headline
    use topcom to generate all triangulations of a point or vector configuration
  Usage
    allTriangulations A
    allTriangulations(A, Homogenize => true, Fine => true, RegularOnly => true)
  Inputs
    A:Matrix
    Homogenize => Boolean
    ConnectedToRegular => Boolean
    Fine => Boolean
    RegularOnly => Boolean    
  Outputs
    :List
  Description
    Text
      This function constructs all triangulations of the point set corresponding to $A$
      (or triangulation of the cone over $A$, if {\tt Homogenize => false} is given.
      With no optional arguments, the default is to construct all regular triangulations.
      
      This function is a wrapper for the topcom function @TO "Topcom::topcomAllTriangulations"@,
      and has the same optional arguments as that function.
      This function returns a list of @TO Triangulation@'s.
      
      A triangulation is a list of list of the indices of the maximal simplices in the triangulation.
      (the index of the point corresponding to the $i$-th column (starting at $0$) is simply $i$.
      
      For example, the following point set is the smallest which has a non-regular triangulation.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      Ts = allTriangulations A
      #Ts == 16
      netList Ts
      tri = Ts#0
      isWellDefined tri
      isRegularTriangulation tri
      Ts/isRegularTriangulation

      regularTriangulationWeights tri
    Text
      The following code determines the support of each triangulation, and tallies them.
      Thus for example, we see that there are 6 regular fine triangulations.
    Example
      tally for tri in Ts list sort unique flatten max tri
    Text
      The method that topcom uses depends on the optional arguments {\tt Fine}, {\tt ConnectedToRegular}
      and {\tt RegularOnly}. 
    Example 
      options allTriangulations
    Text
      If the optional argument {\tt Fine} is set to true, then only 
      {\it fine} triangulations (i.e.
          those that involve every column of $A$) will be generated.
    Example 
      Ts = allTriangulations(A, Fine => true);
      #Ts == 6
    Text
      If the optional argument {\tt RegularOnly} is set to false, but
      {\tt ConnectedToRegular} is true, it will generally take less time,
      as the program doesn't need to check each triangulation to see if it is regular.
    Example
      T1s = allTriangulations(A, RegularOnly => true)
      T2s = allTriangulations(A, RegularOnly => false)
      #T1s
      #T2s
    Text
      The following search would also yield all triangulations, even those not connected via
      bistellar flips to regular triangulations.
    Example
      T3s = allTriangulations(A, RegularOnly => false, ConnectedToRegular => false)
      #T3s
    Text
      Given the list of triangulations, we can query them using other topcom functions.
      See also @TO "Triangulations::Triangulations"@ for other functionality.
    Example
      netList Ts
      for tri in Ts list isWellDefined tri
      for tri in Ts list isRegularTriangulation tri
      for tri in Ts list regularTriangulationWeights tri
  SeeAlso
    (topcomNumTriangulations, Matrix)
    generateTriangulations
///

doc ///
  Key
    generateTriangulations
    (generateTriangulations, Matrix)
    [generateTriangulations, Limit]
    [generateTriangulations, RegularOnly]
    [generateTriangulations, Homogenize]
    [generateTriangulations, Fine]
    [generateTriangulations, Strategy]
  Headline
    generate all triangulations with certain properties
  Usage
    Ts = generateTriangulations A
    Ts = generateTriangulations(A, tri)
    Ts = generateTriangulations T
  Inputs
    A:Matrix
      over the integers (or rationals?), whose columns are the
      points to use
    tri:List
      A list of lists of indices representing a triangulation of 
      the columns of $A$
    T:Triangulation
      packages both $A$ and {\tt tri} in one object
    Limit => ZZ
      Stop after constructing this many triangulations
    RegularOnly => Boolean
      Only generate regular triangulations
    Homogenize => Boolean
      set to false in the case the columns form a vector configuration.
      The default case is that the columns of $A$ are considered a point
      configuration (not used in the variant {\tt generateTriangulations T})
  Outputs
    Ts:List
        of lists of integers, each such list represents a triangulation
  Description
    Text
      This function can be used to generate a set of triangulations of a point set 
      or vector configuration $A$ (the points are the columns of $A$).
      
      It operates by starting with one triangulation (tri or T), if one is given, and if 
      not, it constructs a fine triangulation of the set of columns of $A$.
      
      After this, it uses bistellar flips to generate neighbors, and continues, until
      the limit is reached, or no new ones can be constructed.
      
      Important note! This function generally starts with a fine triangulation (i.e. one
      using all of the points in $A$), and only considers bistellar flips that give fine 
      triangulations.
    Example
      A = vertices hypercube 3
      T = topcomRegularFineTriangulation A
      tri = regularFineTriangulation A
      Ts1 = generateTriangulations A -- list of Triangulation's.
      Ts2 = generateTriangulations(A, T) -- list of list of subsets
      Ts3 = generateTriangulations triangulation(A, T) -- list of Triangulations
      Ts4 = generateTriangulations tri -- list of Triangulations
      all(Ts4, isFine)
      all(Ts4, isStar)
      all(Ts4, isRegularTriangulation)
      Ts4/isStar//tally
      Ts4/gkzVector
      volume convexHull A -- 8
      stars1 = select(Ts4, t -> (gkzVector t)#-1 == 8)
      stars2 = select(Ts4, isStar)
      stars1 == stars2
  Caveat
    This function is written in the top level Macaulay2 language, and so is much slower
    than @TO "allTriangulations"@, which calls the topcom code written in C++.  On the other hand,
    one can give this function a limit for the number of triangulations to generate,
    so can be used to generate triangulations in the case when the number is too large to 
    write down all of them.
  SeeAlso
    "allTriangulations"
///

doc ///
  Key
    bistellarFlip
    (bistellarFlip, Triangulation, List)
    (bistellarFlip, List, List)
  Headline
    perform a bistellar flip on a triangulation
  Usage
    T' = bistellarFlip(T, c)
    tri' = bistellarFlip(tri, c)
  Inputs
    T:Triangulation
    tri:List
      a triangulation, given as a list of lists of column indices
    c:List
      a pair $\{neg, pos\}$ of disjoint lists of column indices: the signed
      parts of an affine circuit, typically an entry of @TO flipCandidates@
  Outputs
    :Thing
      a @TO Triangulation@ (or, in the {\tt (List, List)} form, a {\tt List})
      obtained from $T$ by flipping along $c$, or @TO null@ if the circuit
      is not flippable in $T$
  Description
    Text
      A circuit on $d{+}1$ points admits two triangulations of its convex hull:
      $T_-$, omitting each vertex listed in {\tt neg}, and $T_+$, omitting each
      vertex listed in {\tt pos}.  Exactly one of these (say $T_-$) sits inside
      $T$, supported on a common link $L$; the bistellar flip replaces
      $L \star T_-$ with $L \star T_+$.
    Text
      If neither side has a common link in $T$ -- for example, the simplices
      of one side appear with mismatched links, or one side is only partially
      present -- the flip is undefined and this returns @TO null@.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      T = regularFineTriangulation A
      cs = flipCandidates T
      T' = bistellarFlip(T, cs#0)
      max T'
      isWellDefined T'
    Text
      Many candidate circuits returned by @TO flipCandidates@ are not
      flippable in any given $T$; those return @TO null@.
    Example
      for c in cs list bistellarFlip(T, c)
  SeeAlso
    flipCandidates
    flips
    neighbors
    flipGraph
///

doc ///
  Key
    flipCandidates
    (flipCandidates, Triangulation)
    (flipCandidates, Matrix, List)
  Headline
    candidate affine circuits for bistellar flips of a triangulation
  Usage
    cs = flipCandidates T
    cs = flipCandidates(A, tri)
  Inputs
    T:Triangulation
    A:Matrix
      whose columns are the points of the configuration, either of size
      $d \times n$ (homogenized, with last row all 1's) or $(d{-}1) \times n$
      (which is auto-homogenized internally)
    tri:List
      a triangulation of the columns of $A$
  Outputs
    :List
      of pairs $\{neg, pos\}$, where {\tt neg} and {\tt pos} are disjoint
      lists of column indices and ${\tt neg} \cup {\tt pos}$ spans a
      codim-2 wall of the triangulation
  Description
    Text
      For each pair of maximal simplices in $T$ that share a common ridge
      ({\it i.e.} whose union has $d{+}1$ points), this function returns the
      signed kernel partition of those points, regarded as an affine circuit.
      Each pair has the form $\{neg, pos\}$, the negative and positive parts
      of the kernel relation.
    Text
      Each returned circuit is a {\bf candidate} input for @TO bistellarFlip@,
      but not all candidates yield a valid flip in $T$; see @TO bistellarFlip@.
      In contrast, @TO "Topcom::orientedCircuits"@ returns {\it all} circuits
      of the underlying point configuration (most of which are not supported
      on walls of $T$), and @TO flips@ -- a wrapper around
      @TO "Topcom::topcomFlips"@ -- returns only those circuits that are
      actually flippable in $T$.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      T = regularFineTriangulation A
      flipCandidates T
  SeeAlso
    bistellarFlip
    flips
    neighbors
    "Topcom::orientedCircuits"
///

doc ///
  Key
    wallCircuits
    (wallCircuits, Triangulation)
    (wallCircuits, Matrix, List)
  Headline
    distinct codim-2 wall circuits of a triangulation, with sign convention
  Usage
    wcs = wallCircuits T
    wcs = wallCircuits(A, tri)
  Inputs
    T:Triangulation
    A:Matrix
      whose columns are the points or vectors of the configuration
    tri:List
      a triangulation of the columns of $A$
  Outputs
    :List
      of triples $\{inTri, notInTri, z\}$, one per distinct circuit
      supported on a codim-2 wall of the triangulation
  Description
    Text
      A codim-2 wall of $T$ is the union of two adjacent maximal simplices,
      a $(d{+}1)$-element subset $c$ of column indices.  The circuit
      supported on $c$ is the integer kernel relation $\sum_{i \in c} z_i A_i = 0$,
      restricted to its support (the indices $i$ with $z_i \ne 0$).  When the
      circuit support is strictly smaller than $d{+}1$ -- which happens when
      one of the wall vertices has $z = 0$ -- the same circuit can appear in
      several different walls; these duplicates are removed so each entry of
      the returned list is a distinct circuit.
    Text
      Each triple $\{inTri, notInTri, z\}$ records:
    Text
      $\bullet$ {\tt inTri}: the support indices with $z_i > 0$.
    Text
      $\bullet$ {\tt notInTri}: the support indices with $z_i < 0$.
    Text
      $\bullet$ {\tt z}: an integer kernel of length $\#({\tt inTri} \cup {\tt notInTri})$,
      indexed by position in {\tt sort(inTri | notInTri)}, signed so the
      facet inequality
        $$\sum_{i \in {\tt inTri} \cup {\tt notInTri}} z_i \, w_i \;\ge\; 0$$
      holds for every $w$ that induces $T$.
    Text
      For a {\bf balanced} circuit (both signs present), {\tt inTri} is exactly
      the half whose simplices $c \setminus \{v\}$ appear in $T$.  For a
      {\bf totally cyclic} circuit (e.g. when $0$ lies in the interior of
      $\mathrm{cone}(c)$, as in many complete simplicial fans), $z$ is one-sided:
      {\tt inTri} is the full circuit support and {\tt notInTri} is empty,
      yet the inequality still holds.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      T = regularFineTriangulation A
      netList wallCircuits T
  SeeAlso
    flipCandidates
    secondaryCone
///

doc ///
  Key
    chargeMatrix
    (chargeMatrix, Matrix)
    (chargeMatrix, Triangulation)
  Headline
    charge matrix Q whose rows generate ker A
  Usage
    Q = chargeMatrix A
    Q = chargeMatrix T
  Inputs
    A:Matrix
      a $d \times N$ integer configuration matrix
    T:Triangulation
  Outputs
    :Matrix
      an integer matrix of shape $(N - d) \times N$ whose rows are a
      $\mathbb{Z}$-basis of $\ker A$
  Description
    Text
      The charge matrix is computed as {\tt transpose syz A}.  In the
      GLSM/Calabi-Yau context the rows of $Q$ are the U(1) charges of
      the chiral fields under the gauge symmetries.  Mathematically,
      $Q$ provides explicit coordinates on the quotient
      $\mathbb{R}^N / \mathrm{rowspan}(A)$, which is where the secondary
      cone naturally lives.
    Text
      For $T = $ {\tt triangulation(A, tri)}, {\tt chargeMatrix T} uses
      the stored configuration matrix {\tt matrix T} (which is $A$
      auto-homogenized to one extra row of $1$'s when $A$ was supplied
      as a point set).
    Text
      Users who want a reduced basis (for instance, an LLL-reduced one
      with smaller integer entries) can compute it themselves from
      {\tt syz A} and pass it to @TO secondaryCone@ via the
      {\tt ChargeMatrix} option.
    Example
      A = transpose matrix {{0,0},{1,0},{0,1},{1,1}}
      T = triangulation(A, {{0,1,3},{0,2,3}})
      chargeMatrix T
  SeeAlso
    secondaryCone
    "syz"
///

doc ///
  Key
    secondaryCone
    (secondaryCone, Triangulation)
    (secondaryCone, Matrix, List)
    [secondaryCone, ChargeMatrix]
    ChargeMatrix
  Headline
    secondary cone of a triangulation, in charge-lattice coordinates
  Usage
    M = secondaryCone T
    M = secondaryCone(A, tri)
  Inputs
    T:Triangulation
    A:Matrix
      whose columns are the points or vectors of the configuration
    tri:List
      a triangulation of the columns of $A$
    ChargeMatrix => Matrix
      an integer matrix whose rows are a $\mathbb{Z}$-basis of $\ker A$;
      defaults to @TO chargeMatrix@ {\tt T} (i.e., {\tt transpose syz A})
  Outputs
    :Matrix
      an integer matrix $M$ of shape $m \times (N - d)$ where $m$ is the
      number of distinct codim-2 wall circuits and $N - d$ is the number
      of rows of the charge matrix; the secondary cone is
      $\{t \in \mathbb{Q}^{N-d} : M t \ge 0\}$
  Description
    Text
      Working modulo the lineality space.  In ambient $\mathbb{R}^N$, every
      weight vector $w$ inducing the triangulation $T$ yields the same
      regular subdivision when shifted by an element of $\mathrm{rowspan}(A)$
      (linear/affine functions of the configuration), so the secondary
      cone has lineality of dimension $\mathrm{rank}(A)$.  Quotienting by
      this lineality via $t = Q w$ -- where $Q$ is the charge matrix --
      produces a pointed cone in $\mathbb{R}^{N-d}$, the natural home of
      the secondary cone (and, in due course, the secondary fan).
    Text
      Each row of $M$ comes from one wall circuit $z$ of $T$ (see
      @TO wallCircuits@): solving $z = z' Q$ over $\mathbb{Z}$ gives the
      reduced inequality $z'$, and the rows of $M$ are these $z'$.  When
      the user supplies their own {\tt ChargeMatrix}, each row of $M$ is
      expressed in that basis instead.
    Text
      The triangulation is regular iff this cone has nonempty interior in
      $\mathbb{R}^{N-d}$, which can be tested with the engine LP
      {\tt rawConeInteriorPoint} (see @TO isRegularTriangulation@).
    Text
      To recover a weight vector $w \in \mathbb{R}^N$ from an interior
      point $t$ of the reduced cone, lift via any $w$ with $Q w = t$
      (for instance $w = \mathrm{solve}(Q, t)$).  Any such $w$ induces
      the triangulation; different lifts differ by an element of the
      lineality and produce the same triangulation.
    Example
      A = transpose matrix {{0,0},{1,0},{0,1},{1,1}}
      T1 = triangulation(A, {{0,1,3},{0,2,3}})
      T2 = triangulation(A, {{0,1,2},{1,2,3}})
      secondaryCone T1
      secondaryCone T2
    Text
      A vector configuration: 8 corners of a 3-cube, treated as a $4 \times 8$
      vector configuration (homogenized by hand with a row of $1$'s).  The
      reduced cone lives in $\mathbb{R}^{8-4} = \mathbb{R}^4$.
    Example
      V = transpose matrix {{1,1,1,1},{0,1,1,1},{1,0,1,1},{1,1,0,1},{0,0,1,1},{0,1,0,1},{1,0,0,1},{0,0,0,1}}
      Tv = triangulation(V, {{0,1,2,3},{1,2,3,4},{1,3,4,5},{2,3,4,6},{3,4,5,6},{4,5,6,7}})
      M = secondaryCone Tv
      Q = chargeMatrix Tv
      M * Q * (transpose matrix {regularTriangulationWeights Tv})
  SeeAlso
    chargeMatrix
    wallCircuits
    isRegularTriangulation
    regularTriangulationWeights
///

doc ///
  Key
    flips
    (flips, Triangulation)
    [flips, RegularOnly]
  Headline
    legal bistellar flips of a triangulation, computed via topcom
  Usage
    flips T
  Inputs
    T:Triangulation
    RegularOnly => Boolean
      restrict to flips between regular triangulations (default true)
  Outputs
    :List
      of circuit pairs $\{neg, pos\}$, one per legal bistellar flip of $T$
  Description
    Text
      This is a thin wrapper around @TO "Topcom::topcomFlips"@.  Unlike
      @TO flipCandidates@, which returns every circuit on a codim-2 wall
      of $T$ (not all of which are flippable), this function returns only
      the circuits that correspond to a legal bistellar flip of $T$,
      including support-changing flips.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      T = regularFineTriangulation A
      flips T
  SeeAlso
    flipCandidates
    bistellarFlip
    neighbors
    "Topcom::topcomFlips"
///

doc ///
  Key
    neighbors
    (neighbors, Triangulation)
    [neighbors, Fine]
  Headline
    triangulations adjacent to a given one in the bistellar-flip graph
  Usage
    neighbors T
  Inputs
    T:Triangulation
    Fine => Boolean
      if true (default), only flips that preserve the support of $T$ are
      considered; if false, support-changing flips are also returned
  Outputs
    :List
      of pairs $\{c, T'\}$, one per neighbor: $c$ is the affine circuit
      of the flip and $T'$ is the resulting @TO Triangulation@
  Description
    Text
      Two triangulations are neighbors if they differ by a single bistellar
      flip.  With the default {\tt Fine => true}, the support of $T$ is
      preserved: a fine triangulation has only fine neighbors.  With
      {\tt Fine => false}, flips that drop a vertex from the support
      (or, less commonly, add one) are also considered.
    Text
      This is the building block used by @TO generateTriangulations@ and
      @TO flipGraph@ to walk the bistellar-flip graph.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      T = regularFineTriangulation A
      ns = neighbors T
      #ns
      first ns
    Text
      With {\tt Fine => false}, support-changing flips are included:
    Example
      #neighbors(T, Fine => false)
  SeeAlso
    bistellarFlip
    flipCandidates
    flipGraph
    generateTriangulations
///

doc ///
  Key
    flipGraph
    (flipGraph, Triangulation)
    (flipGraph, Matrix)
    (flipGraph, Matrix, List)
    [flipGraph, Limit]
    [flipGraph, RegularOnly]
    [flipGraph, Fine]
    [flipGraph, Homogenize]
    [flipGraph, Strategy]
  Headline
    bistellar-flip graph of a point or vector configuration
  Usage
    G = flipGraph T
    G = flipGraph A
    G = flipGraph(A, tri)
  Inputs
    T:Triangulation
    A:Matrix
      whose columns are the points (or vectors) to triangulate
    tri:List
      a triangulation of the columns of $A$
    Limit => ZZ
      stop after this many triangulations have been visited
    RegularOnly => Boolean
      restrict the graph to regular triangulations
    Fine => Boolean
      if true (default), restrict to support-preserving flips
    Homogenize => Boolean
      used only by the {\tt Matrix} and {\tt (Matrix, List)} forms; see
      @TO triangulation@
  Outputs
    G:HashTable
      with keys @TT "Triangulations"@ and @TT "Edges"@: @TT "G.Triangulations"@
      is the list of @TO Triangulation@'s reached, and @TT "G.Edges"@ is a
      list of triples $(i, j, c)$ with $i < j$, where $i$ and $j$ index into
      @TT "G.Triangulations"@ and $c$ is the affine circuit of the flip
      between them
  Description
    Text
      Performs a breadth-first search over the bistellar-flip graph starting
      at $T$ (or at a regular fine triangulation of $A$ if no triangulation
      is given), recording both the triangulations reached and the edges
      between them.  Each undirected edge is recorded once, when discovered
      from its lower-indexed endpoint.
    Text
      This is the edge-aware companion of @TO generateTriangulations@, which
      returns the same triangulations but discards the connectivity.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      T = regularFineTriangulation A
      G = flipGraph T
      #G.Triangulations
      #G.Edges
      first G.Edges
  Caveat
    Like @TO generateTriangulations@, this function is implemented in the
    top-level Macaulay2 language and is much slower than the topcom-based
    @TO allTriangulations@.  It does, however, expose the flip-graph
    structure that {\tt allTriangulations} discards, and it accepts a
    {\tt Limit} for incremental exploration.
  SeeAlso
    generateTriangulations
    neighbors
    bistellarFlip
    allTriangulations
///

doc ///
  Key
    secondaryFan
    (secondaryFan, Triangulation)
    (secondaryFan, Matrix)
    [secondaryFan, Limit]
    [secondaryFan, ChargeMatrix]
    [secondaryFan, Strategy]
    Cones
  Headline
    secondary fan of a point or vector configuration, as flip graph + cones
  Usage
    F = secondaryFan T
    F = secondaryFan A
  Inputs
    T:Triangulation
      a fine regular triangulation; used as the BFS seed
    A:Matrix
      a configuration matrix; the seed is then {\tt regularFineTriangulation A}
    Limit => ZZ
      maximum number of triangulations to enumerate (default {\tt infinity})
    ChargeMatrix => Matrix
      shared charge matrix $Q$; defaults to {\tt chargeMatrix T}.  Every
      cone in the result is expressed in this Q's coordinates, so cones
      are directly comparable.
    Strategy => String
      either {\tt "engine"} (default; uses @TO isRegularTriangulation@,
      no subprocess) or {\tt "topcom"} (uses
      @TO topcomIsRegularTriangulation@) for the regularity test in the
      flip-graph BFS
  Outputs
    F:HashTable
      with keys {\tt Triangulations}, {\tt Edges}, {\tt Cones},
      {\tt ChargeMatrix}: see Description
  Description
    Text
      Returns a flip-graph-shaped representation of the secondary fan
      restricted to fine regular triangulations.  Each fine regular
      triangulation gives a maximal cone of the fan; adjacent cones
      (sharing a wall) correspond to triangulations connected by a
      bistellar flip.  Non-fine subdivisions live on lower-dimensional
      faces and are not enumerated by this function.
    Text
      The returned hash table has four entries:
    Text
      $\bullet$ {\tt Triangulations}: list of fine regular triangulations
      reached from the seed.
    Text
      $\bullet$ {\tt Edges}: list of triples {\tt (i, j, circuit)} giving
      the bistellar flips, in the same format as @TO flipGraph@.
    Text
      $\bullet$ {\tt Cones}: list of integer matrices, one per
      triangulation; row $k$ of {\tt F.Cones#i} is the inequality
      $z_k' \cdot t \ge 0$ defining the $k$-th facet of the
      $i$-th secondary cone, in the shared $Q$-coordinate system.
    Text
      $\bullet$ {\tt ChargeMatrix}: the integer matrix $Q$ used for all
      the cones (so callers can lift $t \in \mathbb{R}^{N-d}$ back to
      weights $w \in \mathbb{R}^N$ via {\tt solve(Q, t)}).
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      F = secondaryFan A
      # F.Triangulations
      # F.Edges
      F.ChargeMatrix
      first F.Cones
  Caveat
    The flip-graph BFS only enumerates triangulations reachable from the
    seed via support-preserving flips ({\tt Fine => true}).  Pass
    {\tt Limit => N} to bound the search on large examples.
  SeeAlso
    flipGraph
    secondaryCone
    chargeMatrix
    isRegularTriangulation
    "Topcom::topcomIsRegularTriangulation"
///

doc ///
  Key
    volumeVector
    (volumeVector, Triangulation)
    (volumeVector, Matrix, List)
    gkzVector
  Headline
    the GKZ vector (per-vertex/per-ray volume sum) of a triangulation
  Usage
    volumeVector T
    volumeVector(A, tri)
    gkzVector T
    gkzVector(A, tri)
  Inputs
    T:Triangulation
    A:Matrix
      whose columns are the points or vectors of the configuration
    tri:List
      a triangulation of the columns of $A$
  Outputs
    :List
      whose $i$-th entry is the sum of $|\det A_t| / d!$ over all
      maximal simplices $t$ of {\tt tri} containing column $i$
  Description
    Text
      For each maximal simplex $t$, the function computes
      $|\det A_t| / d!$.  The output's $i$-th entry is the sum of these
      values over all $t$ containing $i$.  This is the {\bf GKZ vector}
      (or {\it volume vector}) of the triangulation; the two names
      {\tt volumeVector} and {\tt gkzVector} are aliases for the same
      method.
    Text
      The geometric reading of $|\det A_t|/d!$ depends on whether $A$
      is a point or vector configuration:
    Text
      @UL {
          {BOLD "Point configuration", " (homogenized matrix, last
            row of $1$'s; e.g. a Triangulation built with the default
            ", TT "Homogenize => true", "): ", TT "|det A_t|/d!", " is
            the $d$-dimensional volume of the simplex with vertices
            the columns of ", TT "A_t", "; the result is the GKZ
            vector in the polytope / secondary-fan sense."},
          {BOLD "Vector configuration", " (e.g. a Triangulation built
            with ", TT "Homogenize => false", ", whose columns span
            the rays of a simplicial fan): ", TT "|det A_t|/d!", " is
            the normalized lattice volume of the cone, equivalently
            the index of the sublattice generated by the rays in ",
            TT "A_t", "; the result is the per-ray sum of cone
            indices."}
          }@
    Text
      If $A$ has fewer rows than $d{+}1$ where $d{+}1 =
      |t|$ for any simplex $t \in {\tt tri}$, $A$ is auto-homogenized
      with a final row of $1$'s before the determinant is taken.
    Text
      Example: a fine regular triangulation of a planar point set,
      regarded as a point configuration:
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      T = regularFineTriangulation A
      volumeVector T
    Text
      Example: the rays of the fan of $\mathbb{P}^1 \times \mathbb{P}^1$,
      as a vector configuration:
    Example
      B = matrix{{1,0,-1,0},{0,1,0,-1}}
      TB = triangulation(B, {{0,1},{1,2},{2,3},{0,3}}, Homogenize => false)
      gkzVector TB
  SeeAlso
    Triangulation
    triangulation
///

doc ///
  Key
    delaunayWeights
    (delaunayWeights, Matrix)
  Headline
    squared-norm weights inducing the Delaunay subdivision of a point set
  Usage
    w = delaunayWeights A
  Inputs
    A:Matrix
      whose columns are points in $\mathbb{R}^d$
  Outputs
    w:Matrix
      a $1 \times n$ matrix whose $i$-th entry is $\|v_i\|^2$, the
      squared Euclidean norm of the $i$-th column of $A$
  Description
    Text
      Returns the standard weight vector for the @TO delaunaySubdivision@
      of a point set: the $i$-th entry is $\|v_i\|^2$, the squared
      Euclidean norm of the $i$-th column of $A$.  Lifting each point
      $v_i$ to $(v_i, \|v_i\|^2) \in \mathbb{R}^{d+1}$ and projecting
      the lower faces of the resulting convex hull back to
      $\mathbb{R}^d$ yields the Delaunay subdivision; passing these
      weights to @TO "Polyhedra::regularSubdivision"@ is the standard
      way to realise that construction.
    Example
      A = transpose matrix {{0,0},{1,0},{2,0},{0,1},{1,1},{2,1}}
      delaunayWeights A
  Caveat
    Delaunay is intrinsically a Euclidean concept on a point set, so
    this function is meaningful only for point configurations -- not
    for vector configurations.  No input-shape check is performed.
  SeeAlso
    delaunaySubdivision
    "Polyhedra::regularSubdivision"
///

doc ///
  Key
    delaunaySubdivision
    (delaunaySubdivision, Matrix)
  Headline
    the Delaunay subdivision of a point set
  Usage
    delaunaySubdivision A
  Inputs
    A:Matrix
      whose columns are points in $\mathbb{R}^d$
  Outputs
    :List
      of lists of column indices: the maximal cells of the Delaunay
      subdivision of the columns of $A$
  Description
    Text
      The {\bf Delaunay subdivision} of a finite point set
      $P \subset \mathbb{R}^d$ is the polyhedral subdivision of the
      convex hull of $P$ characterized by the {\it empty-sphere
      property}: a $d$-simplex with vertices in $P$ is a maximal cell
      iff the open ball circumscribed about it contains no point of
      $P$ in its interior.  Equivalently, it is the projection of the
      lower faces of the {\it lifted paraboloid} -- the convex hull of
      the points $(v_i, \|v_i\|^2) \in \mathbb{R}^{d+1}$.  When the
      points are in general position (no $d{+}2$ of them cospherical),
      the Delaunay subdivision is a triangulation; otherwise some
      cells may be non-simplicial.  It is dual to the Voronoi diagram
      of $P$ and is widely used in computational geometry, mesh
      generation, and interpolation.
    Text
      This function realises the lifted-paraboloid construction by
      calling @TO "Polyhedra::regularSubdivision"@ with the squared
      norms produced by @TO delaunayWeights@: lifting heights to
      $\|v_i\|^2$, taking the lower envelope, and projecting back.
    Example
      A = transpose matrix {{0,0},{1,0},{0,1},{1,1}}
      delaunaySubdivision A
  Caveat
    Like @TO delaunayWeights@, this function is meaningful only for
    point configurations, not vector configurations.
  SeeAlso
    delaunayWeights
    "Polyhedra::regularSubdivision"
///

doc ///
  Key
    ConeIndex
  Headline
    optional argument: index of the cone center for star triangulations
  Description
    Text
      Used by @TO fineStarTriangulation@ and
      @TO regularFineStarTriangulation@ to specify which column of the
      input matrix should serve as the cone center.  If {\tt null}
      (the default), the functions treat the cone center as a new
      point at index {\tt numColumns A}; otherwise the value must be
      an integer in $0, \ldots, n$, where $n$ is the number of columns.
  SeeAlso
    fineStarTriangulation
    regularFineStarTriangulation
///

doc ///
  Key
    Chirotope
  Headline
    the chirotope (oriented matroid) of a point or vector configuration
  Description
    Text
      A {\tt Chirotope} packages the chirotope of a configuration: for
      each $d$-subset $S$ of the columns, the sign of $\det A_S$, stored
      as a topcom-format string.  Two configurations have the same
      chirotope iff they have the same oriented matroid -- in particular,
      the same set of triangulations.
    Text
      Construct one with @TO chirotope@ from a matrix (via topcom) or
      with @TO naiveChirotope@ (a slower pure-Macaulay2 alternative,
      useful for verification).  The {\tt String} form of @TO chirotope@
      wraps a precomputed topcom string.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      C = chirotope A
      C == naiveChirotope A
  SeeAlso
    chirotope
    naiveChirotope
    "Topcom::chirotopeString"
///

doc ///
  Key
    chirotope
    (chirotope, Matrix)
    (chirotope, String)
    [chirotope, Homogenize]
  Headline
    compute the chirotope of a point or vector configuration
  Usage
    C = chirotope A
    C = chirotope s
  Inputs
    A:Matrix
      whose columns are the points (or vectors) of the configuration
    s:String
      a precomputed topcom-format chirotope string
    Homogenize => Boolean
      if true (default), augment $A$ with a final row of $1$'s before
      computing (treating columns of $A$ as a point set); if false, treat
      the columns of $A$ as a vector configuration as-is
  Outputs
    C:Chirotope
  Description
    Text
      Computes the chirotope by calling
      @TO "Topcom::chirotopeString"@.  The {\tt String} form is a thin
      wrapper that lets you construct a {\tt Chirotope} from an
      already-computed topcom string -- useful when caching results.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      C = chirotope A
      toString C
  SeeAlso
    Chirotope
    naiveChirotope
    "Topcom::chirotopeString"
///

doc ///
  Key
    naiveChirotope
    (naiveChirotope, Matrix)
    [naiveChirotope, Homogenize]
  Headline
    compute the chirotope of a configuration via determinants in Macaulay2
  Usage
    C = naiveChirotope A
  Inputs
    A:Matrix
    Homogenize => Boolean
      if true (default), augment $A$ with a final row of $1$'s before
      computing (treating columns of $A$ as a point set); if false, treat
      the columns of $A$ as a vector configuration as-is
  Outputs
    C:Chirotope
  Description
    Text
      Computes the chirotope directly in Macaulay2 by enumerating all
      $d$-subsets of the columns and recording the sign of each
      $d \times d$ minor.  Useful for verifying the topcom-based
      @TO chirotope@.
    Example
      A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
      chirotope A == naiveChirotope A
  Caveat
    Slower than @TO chirotope@ for configurations with many points.
  SeeAlso
    chirotope
    Chirotope
///

-*
  restart
  debug needsPackage "Triangulations"
*-
TEST ///
-- of homogenization and need for it.
  -- test of isRegularTriangulation
  A = transpose matrix {{-1,-1},{-1,1},{1,-1},{1,1},{0,0}}
  regularFineTriangulation(A, Homogenize=>false) -- returns null.  What does this mean?
  T = regularFineTriangulation(A, Homogenize=>true) -- this is good.
  needsPackage "Polyhedra"
  assert(volume convexHull A == 4) -- actual volume
  assert(sum (for t in max T list volume convexHull A_t) == 4)

  A1 = transpose matrix {{-1,-1,1},{-1,1,1},{1,-1,1},{1,1,1},{0,0,1}}
  T1 = regularFineTriangulation(A1, Homogenize=>false)
  assert(T1 === T)
  assert(max T === {{0, 1, 4}, {0, 2, 4}, {1, 3, 4}, {2, 3, 4}}) -- they are sorted, so it should be this.
  assert isRegularTriangulation T
  regularTriangulationWeights T == {1,1,0,0,0}
///

-*
  restart
  needsPackage "Triangulations"
*-
TEST ///
  A = transpose matrix {{-1,-1},{-1,1},{1,-1},{1,1},{0,0}}
  T = regularFineTriangulation A
  naiveIsTriangulation T -- TODO: doc this, and allow A to be homogenized? Same with topcomIsTriangulation
  -- XX
  assert(set flips T === set{{{0, 3}, {4}}, {{1, 2}, {4}}})
  orientedCircuits A
  assert isSubset(flips T, orientedCircuits A)
  orientedCocircuits A
  chirotope A
  assert(naiveChirotopeString A === chirotopeString A)
  topcomNumTriangulations(A, RegularOnly => false, ConnectedToRegular => false) -- this should really be the default?
  allTriangulations A
///

-*
  restart
  needsPackage "Triangulations"
*-
TEST ///
-- TODO: this is a test for Topcom, it seems?
  -- test of isRegularTriangulation
  A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
  tri = {{0,1,2}, {1,3,5}, {2,3,4},
         {0,1,5}, {0,2,4}, {3,4,5},
         {1,2,3}}
  assert not isRegularTriangulation(A,tri)
  assert(null === regularTriangulationWeights(A,tri))
  topcomNumTriangulations A
  allTriangulations A  
  allTriangulations(A, Fine=>true)
  allTriangulations(A, Fine=>true, RegularOnly=>false)
  A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{7,-2}}
  tri = {{0,1,2}, {1,3,5}, {2,3,4},
         {0,1,5}, {0,2,4}, {3,4,5},
         {1,2,3}}
  assert isRegularTriangulation(A,tri)
  regularTriangulationWeights(A,tri) -- Question: how to test that this is correct
    -- TODO: need a function which takes a point set, weights, and creates the lift (easy)
    --       compute the lower hull of this polytope.

  assert(toString chirotope A === naiveChirotopeString A)
  orientedCircuits A
  orientedCocircuits A
  A = transpose matrix {{1,0},{0,1}}
  tri = {{0,1}}
  assert isRegularTriangulation(A,tri)
  regularTriangulationWeights(A,tri) == {0,1} -- TODO: check that this is the correct answer
  
  A = transpose matrix {{0}}
  tri = {{0}}
  assert isRegularTriangulation(A,tri)
  regularTriangulationWeights(A,tri) == {1}
///

-*
  restart
  needsPackage "Triangulations"
*-
///
XXXdifhdifhdsihfds
-- TODO: This test needs to be made to assert correct statements
-- How to test that triangulations are correct?  What I thought worked does not.
  needsPackage "Triangulations"
  A = transpose matrix{{-1,-1},{-1,1},{1,-1},{1,1},{0,0}}
  regularFineTriangulation A  
  tri = {{0, 2, 4}, {2, 3, 4}, {0, 1, 4}, {1, 3, 4}}
  tri = {{0, 2, 4}, {2, 3, 4}, {0, 1, 4}, {1, 2, 3}}
  isRegularTriangulation(A, tri) -- Wrong!!

  badtri = {{0, 2, 4}, {2, 3, 4}, {0, 1, 4}, {1, 2, 3}}
  debugLevel = 6
  isRegularTriangulation(A,badtri) -- this should fail! But it doesn't seem to do so. BUG in something!!!
  debugLevel = 0
  -- hmmm, we can make non-sensical triangulations, without it noticing.
  -- this should be a bug?  
  A = transpose matrix {{0,0},{0,1},{1,0},{1,1}}
  tri = {{0,1,2},{0,2,3}}
  assert isRegularTriangulation(A,tri)  
  tri = {{0,1,2},{1,2,3}}
  assert isRegularTriangulation(A,tri) 
///

-*
  restart
  needsPackage "Triangulations"
*-
TEST ///  
  needsPackage "Polyhedra"
  
  A = transpose matrix {{-1,-1,2},{-1,0,1},{-1,1,1},{0,-1,2},{0,1,1},{1,-1,3},{1,0,-1},{1,1,-2}}
  debugLevel = 0
  T = regularFineTriangulation A
  assert isRegularTriangulation T
  assert(regularTriangulationWeights T =!= null)

  A = transpose matrix {{-1, 0, -1, -1}, {-1, 0, 0, -1}, {-1, 1, 2, -1}, {-1, 1, 2, 0}, {1, -1, -1, -1}, {1, -1, -1, 1}, {1, 0, -1, 2}, {1, 0, 1, 2}}
  P2 = polar convexHull A
  C = matrix {latticePoints P2}
  tri = regularFineTriangulation C
  assert isRegularTriangulation tri
  regularTriangulationWeights tri -- is this correct?  Some weights have negative values??
///


-*
  restart
  needsPackage "Triangulations"
*-
TEST ///
-- simple example of chirotope
  A = transpose matrix {{-1,-1},{-1,1},{1,-1},{1,1},{0,0}}
  tri = {{0, 2, 4}, {2, 3, 4}, {0, 1, 4}, {1, 3, 4}}
  ch1 = chirotope A
  ch2 = naiveChirotope A
  assert(ch1 === ch2)
///

TEST ///
-- Bad triangulations of the square
V = transpose matrix {{0,0},{1,0},{0,1},{1,1}}
T1 = {{0,1,2}}
T2 = {{0,1,2},{0,1,3}}
T3 = {{0,1,2,3}}
assert(not naiveIsTriangulation(V, T1))
assert(not naiveIsTriangulation(V, T2))
assert(not naiveIsTriangulation(V, T3))
-- assert(not topcomIsTriangulation(V, T1)) -- topcom signals an error here
-- assert(not topcomIsTriangulation(V, T2)) -- topcom signals an error here
assert(not topcomIsTriangulation(V, T3))
///

-- This example is a good one, but takes too long to be run automatically
-- Actually, this is a test of Topcom....
///
restart
  needsPackage "Topcom"  
  needsPackage "Polyhedra"
  pts =  {{-1,0,0,-1},{-1,0,1,-1},{-1,0,1,0},{-1,1,0,-1},{-1,1,0,0},{-1,1,1,2},{1,-1,0,-1},{1,0,-1,1},{1,-1,-1,-1},{0,0,0,-1}}
  A = transpose matrix pts 

  debugLevel = 7

  elapsedTime n1 = numTriangulations(A, Fine=>true, ConnectedToRegular=>true) -- 6.9 sec, 408 of these CORRECT
  elapsedTime n2 = numTriangulations(A, Fine=>true, ConnectedToRegular=>false, RegularOnly => false) -- 116 sec, 448 of these
  elapsedTime n3 = numTriangulations(A, Fine=>false, ConnectedToRegular=>true)  -- 8 sec, 520 of these CORRECT
  elapsedTime n4 = numTriangulations(A, Fine=>false, ConnectedToRegular=>false, RegularOnly => false) -- 115 sec, 564 of these

  elapsedTime n5 = numTriangulations(A, Fine=>true, ConnectedToRegular=>true, RegularOnly=>false) -- .09 sec, 448 of these
  --this is n2 above.  elapsedTime n6 = numTriangulations(A, Fine=>true, ConnectedToRegular=>false, RegularOnly=>false) -- 115.5 sec, 448 of these
  elapsedTime n7 = numTriangulations(A, Fine=>false, ConnectedToRegular=>true, RegularOnly=>false)  -- .11 sec, 564 of these
  -- this is n4 above. elapsedTime n8 = numTriangulations(A, Fine=>false, ConnectedToRegular=>false, RegularOnly=>false) -- 116 sec, 564 of these

  elapsedTime set1 = allTriangulations(A, Fine=>true, ConnectedToRegular=>true); -- 6.9 sec, 408  CORRECT
  elapsedTime set2 = allTriangulations(A, Fine=>true, ConnectedToRegular=>false, RegularOnly => false); -- 118 sec, 448
  elapsedTime set3 = allTriangulations(A, Fine=>false, ConnectedToRegular=>true); -- 8.1 sec, 520 CORRECT
  elapsedTime set4 = allTriangulations(A, Fine=>false, ConnectedToRegular=>false, RegularOnly => false); -- 116 sec.  564 of these.

  elapsedTime set5 = allTriangulations(A, Fine=>true, ConnectedToRegular=>true, RegularOnly=>false); -- .15 sec, 448 of these
  --elapsedTime set6 = allTriangulations(A, Fine=>true, ConnectedToRegular=>false, RegularOnly=>false); -- 116 sec, 448 of these
  elapsedTime set7 = allTriangulations(A, Fine=>false, ConnectedToRegular=>true, RegularOnly=>false); -- .22 sec, 564 of these
  --elapsedTime set8 = allTriangulations(A, Fine=>false, ConnectedToRegular=>false, RegularOnly=>false); -- 117 sec, 564 of these

  assert((n1,n2,n3,n4,n5,n7) == (#set1, #set2, #set3, #set4, #set5, #set7))
  fineTris = select(set4, x -> # unique flatten x == numColumns A);
  regularFineTris = select(fineTris, x -> isRegularTriangulation(A, x));
  regularTris = select(set4, x -> isRegularTriangulation(A, x));

  assert(#regularFineTris == 408)
  assert(#fineTris == 448)
  assert(#regularTris == 520)  

  assert(set set5 === set set2) -- in general, this doesn't need to hold, but it is rare for this to be the case
  assert(set set7 === set set4) -- same: rare for this to not hold
  elapsedTime assert(set select(set7, x -> isRegularTriangulation(A, x)) === set set3)
  elapsedTime assert(set select(set5, x -> isRegularTriangulation(A, x)) === set set1)

-- the rest of this test needs to be checked, and cleaned up
  set5_0
  elapsedTime for tri in set5 list naiveIsTriangulation(A, tri)

  numFlips(A, set5_0)  
  topcomFlips(A, set5_0)
  -- now let's see about the naive way of getting regular star triangulations 
  -- i.e. we add in the origin
  
  pts1 =  {{-1,0,0,-1},{-1,0,1,-1},{-1,0,1,0},{-1,1,0,-1},{-1,1,0,0},{-1,1,1,2},{1,-1,0,-1},{1,0,-1,1},{1,-1,-1,-1},{0,0,0,-1},{0,0,0,0}}
  A1 = transpose matrix pts1
  --elapsedTime tris1 = allTriangulations(A1, Fine=>true, ConnectedToRegular=>true, RegularOnly=>false); -- 
  elapsedTime tris1 = allTriangulations(A1, Fine=>false, ConnectedToRegular=>true, RegularOnly=>false); -- 
  fineTris1 = select(tris1, x -> # unique flatten x == numColumns A1);
  regTris1 = select(tris1, x -> isRegularTriangulation(A1, x));  
  fineRegTris1 = select(regTris1, x -> # unique flatten x == numColumns A1);
  stars1 = select(tris1, x -> all(x, x1 -> member(10, x1))); -- 100 here
  starsFine1 = select(stars1, x -> # unique flatten x == numColumns A1);
  RST = select(stars1, x -> isRegularTriangulation(A1,x)); -- 80 here...
  FSRT = select(starsFine1, x -> isRegularTriangulation(A1,x)); -- 48 here...!


  unique for tri in set5 list fineStarTriangulation(A, tri);
  --select(oo, tri -> isRegularTriangulation(A1, tri))  -- this isn't defined here...

  -- let's test this one for being a triangulation:
  oA = orientedCircuits A
  tri = set5_3
  tally flatten for t in tri list subsets(t,4)
  for z in oA list (
      # select(tri, t -> isSubset(z_0, t)),
      # select(tri, t -> isSubset(z_1, t))
      )
  -- todo:
  -- 1. routine to check that a triangulation is a triangulation
  -- 2. routine to turn a regular, fine triangulation, into a star (fine, regular) triangulation. How general is this? DONE, I think.
  -- 3. perform bistellar flips to get new triangulations.
///

-*
  restart
  needsPackage "Triangulations"  
*-
///
  needsPackage "ReflexivePolytopesDB"
  needsPackage "Topcom"  
  needsPackage "Polyhedra"
  debug needsPackage "Triangulations"
  
 str = "4 18  M:53 18 N:11 10 H:6,45 [-78]
        1   0   0  -2   0   2   1   3  -2   2  -2   2   3  -1   0  -2   0  -1
        0   1   0   2   0   0   1  -2   1  -2   0   0  -1   0  -2   0  -2  -1
        0   0   1   1   0  -1  -1  -2   2  -2   0  -2  -2   2  -1   2   1   2
        0   0   0   0   1  -1  -1   0  -1   1   1  -1  -1  -1   2  -1   0  -1"
 A = matrix first kreuzerSkarke str
 P = convexHull A
 P2 = polar P
 A1 = vertices P2
 LP = matrix{select(latticePoints P2, x -> x != 0)}
 numTriangulations(LP, Fine => true) == 731
 elapsedTime tris = allTriangulations(LP, Fine=>true);
 #tris == 731
 numTriangulations(LP)
 allTriangulations(LP);
 
 T = regularFineTriangulation LP
 elapsedTime trisT = generateTriangulations T; -- much slower, and includes 10 non-regular triangulations.
 assert(#trisT == 741)
 assert((trisT/(T -> isRegularTriangulation T)//tally) === new Tally from {true => 731, false => 10})

 (24) * volume P2  == 29

///

-*
  restart
  needsPackage "Triangulations"  
*-
TEST ///
-- this is an example used in 
-- https://people.inf.ethz.ch/fukudak/lect/mssemi/reports/03_rep_ClemensPohle.pdf
-- (accessed 2 June 2022)
  debug needsPackage "Topcom"  

  -- this tests construction of the chirotope.
  A = transpose matrix"0,0;1,1;3,1;5,0;1,5"
  OM = chirotope A
  -- "foo-input.in" << topcomPoints A << endl << close;
  -- directString = get("!"|(topcompath|"points2chiro <foo-input.in "));
  -- assert(directString == toString chirotope A)
  assert(toString OM == "5,3:\n--+-++-+++\n")

  -- now get one triangulation
  -- TODO: return Triangulation.  sort triangulation.  get weights? (optionally?)
  elapsedTime T = regularFineTriangulation A -- sort the output of this?
  assert((max T)/sort//sort == ({{0, 1, 2}, {0, 2, 3}, {1, 2, 4}, {0, 1, 4}, {2, 3, 4}})/sort//sort)

  -- the above T is the same here as the placing triangulation (not often the same, I think).
  -- elapsedTime tri = value get("!"|(topcompath|"points2placingtriang <foo-input.in "));
  -- assert(tri/sort//sort == ({{0, 1, 2}, {0, 2, 3}, {1, 2, 4}, {0, 1, 4}, {2, 3, 4}})/sort//sort)

  -- now find the possible flips.  Can we get topcom to do the flips?
  assert(set flips T === set {{{0, 2, 4}, {1}}, {{1, 3}, {0, 2}}})
  assert(isSubset((flips T)/sort, (orientedCircuits A)/sort))
  -- I don't see how to get topcom to use these though.
///

-*
  restart
  needsPackage "Triangulations"
*-
TEST ///
  -- test of bistellar flips code.
  
  -- example 1.
  debug needsPackage "Topcom"
  A = transpose matrix"0,0;1,1;3,1;5,0;1,5"
  tri = regularFineTriangulation A
  max tri
  assert(max tri == (max tri)/sort//sort)
  
  flips tri -- TODO: return list of "affine circuits"
  possibles = flipCandidates tri
  tri2 = bistellarFlip(tri, possibles#0)
  tri3 = bistellarFlip(tri, possibles#1)
  bistellarFlip(tri, possibles#2) -- null
  bistellarFlip(tri, possibles#3) -- null

  neighbors tri
  nbors = possibles/(c -> c => bistellarFlip(tri, c))
  24 * gkzVector tri
  for t in nbors list if last t === null then continue else {t, gkzVector last t}
  gkzVector tri    
///

TEST /// -- test of functions here on the square and the cube
-*
  restart
  needsPackage "Triangulations"
*-
  square = transpose matrix{{1,1},{-1,1},{-1,-1},{1,-1}}
  
  topcomRegularFineTriangulation square
  assert(# allTriangulations square == 2)
    
  -- Now consider all of the lattice points of the square
  sq9 = matrix {{-1, -1, 1, 1, -1, 0, 0, 1, 0}, {-1, 1, -1, 1, 0, -1, 1, 0, 0}}

  -- test function from Topcom.
  t1 = topcomRegularFineTriangulation sq9
  regularTriangulationWeights(sq9, t1)
  fineStarTriangulation(sq9, t1) -- has central element removed.  Don't do that?
  delaunaySubdivision sq9 -- not a triangulation (4 squares).
  orientedCircuits sq9 -- many of these are not useful when considering only fine triangulations.

  -- Polyhedra command, bug fix currently in StringTorics.
  t2 = regularSubdivision(sq9, matrix{{4,4,4,4,1,1,1,1,0}})
  assert(
    t2 
    == 
    {{0, 4, 5}, {1, 4, 6}, {2, 5, 7}, {3, 6, 7}, {4, 5, 8}, {4, 6, 8}, {5, 7, 8}, {6, 7, 8}}
    )

  t3 = regularSubdivision(sq9, matrix{{4,4,4,4,1,1,1,1,-4}})
  t4 = regularSubdivision(sq9, matrix{{1,1,1,1,1,1,1,1,-4}})
  
  flipCandidates(sq9, t3)
  elapsedTime flipCandidates(sq9, t1)
  
  elapsedTime tris = allTriangulations sq9; -- computes all triangulations, Fine => false, regular.
  assert(387 == #tris) -- check this number. 
  elapsedTime tris1 = allTriangulations(sq9, Fine => true);
  assert(64 == #tris1)
  elapsedTime tris2 = generateTriangulations triangulation(sq9, t1);
  elapsedTime tris2a = generateTriangulations(triangulation(sq9, t1), RegularOnly => true); -- slow...
  #tris2 == #tris2a

  -- via topcom: (too long for general test)
  ----elapsedTime assert(387 == # select(tris, t -> topcomIsRegularTriangulation(sq9, max t))) -- slow
  -- via Polyhedra/ (too long for a general test)
  ----elapsedTime assert(387 == # select(tris, t -> null =!= regularTriangulationWeights t)) -- slow, same as Polyhedra, I think (same code is being used)
  -- 64 fine triangulations
  assert(64 == # select(tris, isFine))
  assert(16 == # select(tris, isStar))
  assert(1 == # select(tris, t -> isStar t and isFine t))

  -- generate only fine 
  elapsedTime finetris = allTriangulations(sq9, Fine => true);
  -- elapsedTime finetris1 = generateTriangulations(sq9, t1, Fine => true); -- TODO
  elapsedTime finetris1 = generateTriangulations triangulation(sq9, t1);
  assert(finetris/max//sort == finetris1/max//sort)

  finetris/volumeVector
  
  -- TODO: audit/allow "A" matrices to be homogeneous or not?  THIS MIGHT BE DONE...
  --   one way: default is not, and Homogenize => true will add the extra row.
  --   use this for most routines taking point configurations or vector configurations.
///

TEST ///
  -- triangulation code, test 1.  
  -- some triangulation code is in Topcom, Polyhedra
  -- (comes from KS database, h11=3, #232.
-*
  restart
  needsPackage "Triangulations"
*-  

  A0 = matrix {{1, 1, 1, 1, -11}, {0, 2, 2, 2, -10}, {0, 0, 4, 4, -8}, {0, 0, 0, 12, -12}}
  P2 = polar convexHull A0
  -- the floowing are the lattice points of P2 (from 'LP = latticePointList P2', but this uses StringTorics...)



  LP = {{-1, 0, 0, 0}, {-1, 0, 0, 1}, {-1, 0, 3, -1}, {-1, 2, -1, 0}, {1, -1, 0, 0}, {-1, 0, 1, 0}, {-1, 1, 0, 0}, {0, 0, 0, 0}}  
  A  = transpose matrix LP
  -- We want triangulations of the point configuration given by the columns of A.
  T = topcomRegularFineTriangulation A
  assert isFine(A, T)
  assert isStar(A, T)
  assert isRegularTriangulation(A, T)

  assert naiveIsTriangulation(A, T)      
  assert topcomIsTriangulation(A, T)      

  elapsedTime tris = allTriangulations A;
  FRST = first select(tris, t -> isFine t and isStar t and isRegularTriangulation t)
  volumeVector FRST
  elapsedTime tris = generateTriangulations(A, T);

  wts = regularTriangulationWeights(A, T)
  assert(regularSubdivision(A, matrix{wts}) == T//sort)
  assert isRegularTriangulation(A, T) -- from Topcom.
  assert topcomIsTriangulation(A, T)

  -- also check Topcom's code...
  neighbors triangulation(A, T) -- not unpacked... TODO: need to understand and document what this gives

  --A1 = A || splice matrix{{numColumns A: 1}}
  --A1 = augment A
  A1 = A || matrix{{numcols A: 1}}
  --volumeVector(A, T) -- need to homogenize A? at bottom or at top?
  volumeVector(A1, T) -- need to homogenize A? at bottom or at top?
  neighbors triangulation(A1, T)
  --regularFineStarTriangulation A1 -- this one fails
  regularFineStarTriangulation A -- this one works
  
  -- check that T is a triangulation?
  circs = flipCandidates(A, T)
  bistellarFlip(T, {{5}, {0, 1, 2}})
  for c in circs list bistellarFlip(T, c)
///

TEST ///
-- tests for generateTriangulations
-*
  restart
  needsPackage "Triangulations"
*-
  -- 5-pt configuration in 2D: 2 triangulations
  A = transpose matrix"0,0;1,1;3,1;5,0;1,5"
  T = regularFineTriangulation A
  tris = generateTriangulations T
  assert(#tris == 2)
  assert(member(T, tris))

  -- 9 lattice points of the square: 64 fine triangulations, all regular
  sq9 = matrix {{-1, -1, 1, 1, -1, 0, 0, 1, 0}, {-1, 1, -1, 1, 0, -1, 1, 0, 0}}
  T = regularFineTriangulation sq9
  tris = generateTriangulations T
  assert(#tris == 64)
  assert(set (tris/max) === set ((allTriangulations(sq9, Fine => true))/max))

  -- RegularOnly: same 64 in this case (all are regular)
  trisR = generateTriangulations(T, RegularOnly => true)
  assert(#trisR == 64)

  -- Limit is exact, including the boundary cases
  assert(#generateTriangulations(T, Limit => 1) == 1)
  assert(generateTriangulations(T, Limit => 1) == {T})
  assert(#generateTriangulations(T, Limit => 17) == 17)
  assert(#generateTriangulations(T, Limit => 1000) == 64) -- no overshoot when Limit > #available

  -- Matrix entry points produce the same set of triangulations
  trisA = generateTriangulations sq9
  assert(set (trisA/max) === set (tris/max))
  trisAL = generateTriangulations(sq9, max T)
  assert(set trisAL === set (tris/max))

  -- Vector configuration: 4 rays in R^3, ray 3 = e1+e2+e3 in interior of cone(e1,e2,e3).
  -- Only one support-preserving triangulation exists (any flip would remove ray 3).
  V = matrix"1,0,0,1; 0,1,0,1; 0,0,1,1"
  Tv = triangulation(V, {{0,1,3},{0,2,3},{1,2,3}}, Homogenize => false)
  assert(#generateTriangulations Tv == 1)
///

TEST ///
-- Fine => false: square with center.  The only fine triangulation has 4
-- simplices meeting at the center; both flips drop the center, so with the
-- default Fine => true we get just T0, but with Fine => false we should
-- reach the two non-fine triangulations of the square (split along each
-- diagonal), giving 3 in total.
-*
  restart
  needsPackage "Triangulations"
*-
  A = transpose matrix {{-1,-1},{-1,1},{1,-1},{1,1},{0,0}}
  T = regularFineTriangulation A
  assert(#generateTriangulations T == 1)
  assert(#generateTriangulations(T, Fine => false) == 3)

  -- The Fine => false count should match allTriangulations on the same
  -- configuration (all 3 are regular, all are connected to the regular
  -- fine triangulation).
  assert(#generateTriangulations(T, Fine => false) == # allTriangulations A)

  -- 9 lattice points of the square: with Fine => false, the count grows
  -- beyond the 64 fine triangulations.
  sq9 = matrix {{-1, -1, 1, 1, -1, 0, 0, 1, 0}, {-1, 1, -1, 1, 0, -1, 1, 0, 0}}
  Tsq = regularFineTriangulation sq9
  assert(#generateTriangulations Tsq == 64)
  trisAll = generateTriangulations(Tsq, Fine => false);
  assert(#trisAll > 64)
  -- Every triangulation found should be reachable from T via flips, and
  -- since all triangulations of sq9 are regular, this should match
  -- allTriangulations restricted to those connected to a regular fine.
  assert(#trisAll == # allTriangulations sq9)
///

TEST ///
-- flipGraph: smoke test on the same examples used for generateTriangulations.
-*
  restart
  needsPackage "Triangulations"
*-
  -- 5-pt configuration: 2 fine triangulations connected by a single flip.
  A = transpose matrix"0,0;1,1;3,1;5,0;1,5"
  T = regularFineTriangulation A
  G = flipGraph T
  assert(#G.Triangulations == 2)
  assert(#G.Edges == 1)
  e = first G.Edges
  assert(e#0 == 0 and e#1 == 1)
  -- The triangulations agree (as a set) with generateTriangulations.
  assert(set G.Triangulations === set generateTriangulations T)

  -- All edges should respect i < j.
  sq9 = matrix {{-1, -1, 1, 1, -1, 0, 0, 1, 0}, {-1, 1, -1, 1, 0, -1, 1, 0, 0}}
  Tsq = regularFineTriangulation sq9
  Gsq = flipGraph Tsq
  assert(#Gsq.Triangulations == 64)
  assert(all(Gsq.Edges, e -> e#0 < e#1))
  assert(all(Gsq.Edges, e -> 0 <= e#0 and e#1 < 64))

  -- Square with center: Fine => false gives 3 triangulations.  T0 (fine)
  -- connects to each of the two diagonal triangulations T1, T2 by a
  -- size-1 flip that drops the center; and T1 <-> T2 are connected by a
  -- support-preserving 2-2 flip among {0,1,2,3} (not involving vertex 4),
  -- so we expect 3 edges.
  Asq = transpose matrix {{-1,-1},{-1,1},{1,-1},{1,1},{0,0}}
  Tsq2 = regularFineTriangulation Asq
  Gsq2 = flipGraph(Tsq2, Fine => false)
  assert(#Gsq2.Triangulations == 3)
  assert(#Gsq2.Edges == 3)
  assert(all(Gsq2.Edges, e -> e#0 < e#1))
///

-- The following is too long for a test.
///
  -- how many triangulations at h11=8? (roughly?)
  needsPackage "ReflexivePolytopesDB"  
  topes = kreuzerSkarke 8;
  A = matrix topes_500
  P = convexHull A
  P2 = polar P
  LP = matrix{latticePoints P2}
  T = regularFineTriangulation LP

  T = {T}
  T1 = T/neighbors/(t -> t/last)//flatten
  T1/gkzVector

  #T1
  T2 = T1/(t -> neighbors t)//flatten/last//unique;
  T2/gkzVector//matrix
  T1 = T2;
  


    
  methods allTriangulations
  options allTriangulations
  debugLevel = 1
  elapsedTime trisLP = allTriangulations(LP, Fine => true, RegularOnly => true);  
  frsts = for t in trisLP list if all(t, t1 -> member(numcols LP - 1, t1)) then t else continue;
  #frsts == 108
  isStar(t, numcols LP - 1) then t else continue;
  T = regularFineTriangulation LP
  debug Triangulations
  debugLevel = 0
  elapsedTime generateTriangulations(T, Limit => 20000);
///

TEST ///
-- secondaryCone / wallCircuits

-- Square: 4 points, one circuit, two regular triangulations.
-- The configuration matrix A is 2x4, augmented to 3x4 (rank 3),
-- so the reduced secondary cone lives in R^(4-3) = R^1.
-- The two triangulations give opposite half-lines.
A = transpose matrix {{0,0},{1,0},{0,1},{1,1}};
T1 = triangulation(A, {{0,1,3},{0,2,3}});
T2 = triangulation(A, {{0,1,2},{1,2,3}});
M1 = secondaryCone T1;
M2 = secondaryCone T2;
assert(numrows M1 == 1 and numcols M1 == 1);
assert(M1 == -M2);

-- Sign canary: regularTriangulationWeights, projected to charge-lattice
-- coordinates t = Q*w, lands in the closed reduced cone.
Q1 = chargeMatrix T1;
t1 = Q1 * (transpose matrix {regularTriangulationWeights T1});
t2 = Q1 * (transpose matrix {regularTriangulationWeights T2});
assert all(flatten entries (M1 * t1), x -> x >= 0);
assert all(flatten entries (M2 * t2), x -> x >= 0);

-- Single-simplex triangulation: no walls, empty matrix, full quotient cone.
-- A is 2x3, augmented to 3x3 of rank 3, so N-d = 0 and the reduced cone
-- lives in R^0 (a 0x0 inequality matrix).
T3 = triangulation(matrix {{0,1,0},{0,0,1}}, {{0,1,2}});
assert(numrows secondaryCone T3 == 0);
assert(numcols secondaryCone T3 == 0);

-- wallCircuits: orientation, sign convention.
wcs = wallCircuits T1;
assert(#wcs == 1);
(inTri, notInTri, z) = toSequence first wcs;
-- For balanced circuits, every present simplex c\{v} has v in inTri.
c = sort flatten {inTri, notInTri};
trisetT1 = set (max T1)/sort;
present = select(c, v -> member(sort toList(set c - set {v}), trisetT1));
assert(isSubset(set present, set inTri));
assert(#z == #c);
assert all(inTri, v -> z#(position(c, x -> x == v)) > 0);
assert all(notInTri, v -> z#(position(c, x -> x == v)) < 0);
///

TEST ///
-- secondaryCone on a complete simplicial fan with totally cyclic circuits.
-- A reflexive polytope's polar lattice points (excluding 0) span R^4
-- with positive linear dependences, producing one-sided kernel relations
-- on some codim-2 walls.  The resulting secondary cone must still
-- contain regularTriangulationWeights componentwise.
needsPackage "ReflexivePolytopesDB";
tope = KSEntry "4 10  M:73 10 N:9 7 H:4,58 [-108] id:100
   1   1   1   0   1   1  -3   1   1  -9
   0   2   0   0   3  -1  -3  -1   4  -6
   0   0   2   0  -1   3  -3   4  -1  -6
   0   0   0   1  -1  -1   1  -1  -1   4
   ";
P = polar convexHull matrix tope;
A = matrix {select(latticePoints P, x -> x != 0)};
t0 = regularFineTriangulation(A, Homogenize => false);
M = secondaryCone t0;
Q = chargeMatrix t0;
w = transpose matrix {regularTriangulationWeights t0};
assert all(flatten entries (M * (Q * w)), x -> x >= 0);
-- At least one totally cyclic wall exists in this example.
wcs = wallCircuits t0;
assert any(wcs, t -> #(t#0) == 0 or #(t#1) == 0);
///

TEST ///
-- secondaryCone of a non-regular triangulation has no interior point.
-- Verified via the engine LP rawConeInteriorPoint: the fullDim flag is 0
-- iff the secondary cone has empty interior iff the triangulation is
-- not regular.  Cross-checked against isRegularTriangulation (Topcom).
debug Core
A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}};
tri = {{0,1,2}, {1,3,5}, {2,3,4}, {0,1,5}, {0,2,4}, {3,4,5}, {1,2,3}};
Ta = triangulation(A, tri);
Ma = secondaryCone Ta;
assert(not isRegularTriangulation Ta);
ra = map(RR_53, rawConeInteriorPoint raw(-Ma));
assert((entries ra)#0#0 == 0.0);

-- Same combinatorics on a perturbed point set gives a regular
-- triangulation, whose secondary cone has full-dimensional interior.
Ab = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{7,-2}};
Tb = triangulation(Ab, tri);
Mb = secondaryCone Tb;
assert(isRegularTriangulation Tb);
rb = map(RR_53, rawConeInteriorPoint raw(-Mb));
assert((entries rb)#0#0 == 1.0);
///

TEST ///
-- triangulation constructor: auto-detect Homogenize from max-simplex sizes,
-- (List, List) form, and rejection of inconsistent inputs.
A = transpose matrix {{0,0},{1,0},{0,1},{1,1}}
T1 = triangulation(A, {{0,1,2},{1,2,3}})
assert(numRows matrix T1 == 3)             -- point set: augmented
assert(T1.cache#"point set" === true)

V = transpose matrix {{1,1,1,1},{0,1,1,1},{1,0,1,1},{1,1,0,1},{0,0,1,1},{0,1,0,1},{1,0,0,1},{0,0,0,1}}
triV = {{0,1,2,3},{1,2,3,4},{1,3,4,5},{2,3,4,6},{3,4,5,6},{4,5,6,7}}
Tv = triangulation(V, triV)
assert(numRows matrix Tv == 4)             -- vector config: stored as-is
assert(Tv.cache#"point set" === false)

T3 = triangulation({{0,0},{1,0},{0,1},{1,1}}, {{0,1,2},{1,2,3}})  -- (List, List)
assert(max T3 == max T1)
assert(matrix T3 == matrix T1)

-- Explicit Homogenize matching the inferred value still works.
assert(max triangulation(A, {{0,1,2},{1,2,3}}, Homogenize => true) == max T1)
assert(max triangulation(V, triV, Homogenize => false) == max Tv)

-- Inconsistent explicit option errors.
assert try (triangulation(V, triV, Homogenize => true); false) else true
assert try (triangulation(A, {{0,1,2},{1,2,3}}, Homogenize => false); false) else true

-- Empty triangulation errors.
assert try (triangulation(A, {}); false) else true

-- Mixed simplex sizes error.
assert try (triangulation(A, {{0,1,2},{0,1}}); false) else true
///

TEST ///
-- engine-LP isRegularTriangulation / regularTriangulationWeights agree
-- with the topcom path on a few canonical examples; engine weights are
-- integer and induce the right triangulation via Polyhedra.
A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
T = regularFineTriangulation A
assert(isRegularTriangulation T == topcomIsRegularTriangulation T)
assert(isRegularTriangulation T)
wEngine = regularTriangulationWeights T
assert(class first wEngine === ZZ)
assert(regularSubdivision(matrix T, matrix {wEngine}) == sort \ sort max T)

-- Non-regular: both paths return false / null.
triNR = {{0,1,2}, {1,3,5}, {2,3,4}, {0,1,5}, {0,2,4}, {3,4,5}, {1,2,3}}
Tnr = triangulation(A, triNR)
assert(isRegularTriangulation Tnr == topcomIsRegularTriangulation Tnr)
assert(not isRegularTriangulation Tnr)
assert(regularTriangulationWeights Tnr === null)

-- interiorLatticePoint directly: cube secondary cone, check {x : M x >= 0}.
V = transpose matrix {{1,1,1,1},{0,1,1,1},{1,0,1,1},{1,1,0,1},{0,0,1,1},{0,1,0,1},{1,0,0,1},{0,0,0,1}}
Tv = triangulation(V, {{0,1,2,3},{1,2,3,4},{1,3,4,5},{2,3,4,6},{3,4,5,6},{4,5,6,7}})
M = secondaryCone Tv
x = interiorLatticePoint M
assert(class first x === ZZ)
assert all(flatten entries (M * (transpose matrix {x})), v -> v > 0)

-- Empty cone (no walls): interiorLatticePoint returns the zero vector.
M0 = map(ZZ^0, ZZ^3, 0)
assert(interiorLatticePoint M0 == {0,0,0})
///

TEST ///
-- secondaryFan and the engine/topcom Strategy switch produce identical
-- flipGraph data on a small example.  Cones are full-dim (we asked for
-- regular triangulations only) and live in the same Q-coords.
A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
Feng = secondaryFan(A, Strategy => "engine")
Ftc  = secondaryFan(A, Strategy => "topcom")
assert(set Feng.Triangulations === set Ftc.Triangulations)
assert(# Feng.Edges == # Ftc.Edges)
assert(Feng.ChargeMatrix == Ftc.ChargeMatrix)
-- Each cone has the same column count (= numrows Q), since Q is shared.
nQ = numrows Feng.ChargeMatrix
assert all(Feng.Cones, M -> numcols M == nQ)
-- Every cone is full-dim (interior point exists).
assert all(Feng.Cones, M -> interiorLatticePoint M =!= null)
-- Each edge connects two adjacent (different) triangulations.
assert all(Feng.Edges, e -> e#0 != e#1 and e#0 < # Feng.Triangulations and e#1 < # Feng.Triangulations)
///

-*
restart
needsPackage "Triangulations"
*-
///
  -- testing generateTriangulations, and flipGraph (not written yet!)
  -- Let's start with a reflexive polytope of dim 4, with h11=4.
  needsPackage "ReflexivePolytopesDB"
  --tope = (kreuzerSkarke 4)#100
  tope = KSEntry "4 10  M:73 10 N:9 7 H:4,58 [-108] id:100
     1   1   1   0   1   1  -3   1   1  -9
     0   2   0   0   3  -1  -3  -1   4  -6
     0   0   2   0  -1   3  -3   4  -1  -6
     0   0   0   1  -1  -1   1  -1  -1   4
     "
  P = polar convexHull matrix tope
  A = matrix {select(latticePoints P, x -> x != 0)}
  -- Ts = allTriangulations(A, Homogenize => false) -- 24 total here.
  -- for T in Ts list # sort unique flatten max T

  t0 = regularFineTriangulation(A, Homogenize => false) -- calls topcom
  secondaryCone t0
  
  -- assert isWellDefined t0 -- calls topcom
  -- assert naiveIsTriangulation t0 -- FAILS
  -- assert isRegularTriangulation t0 -- calls topcom
  -- regularTriangulationWeights t0 
///

TEST /// -- wallCircuits with degenerate codim-2 walls (circuit < d+1).
  -- 8 corners of a unit 3-cube as a vector configuration (homogenized to 4
  -- rows by appending 1's).  Each codim-2 face of the triangulation has a
  -- 4-element circuit (a face of the cube), but the (d+1) = 5-element wall
  -- obtained from a pair of adjacent simplices includes one extra
  -- "extension" point whose kernel coordinate is zero.  That extension is
  -- what used to make distinct walls produce the same circuit twice.
  vecs = {{1,1,1,1},{0,1,1,1},{1,0,1,1},{1,1,0,1},{0,0,1,1},{0,1,0,1},{1,0,0,1},{0,0,0,1}}
  maxsimps = {{0,1,2,3},{1,2,3,4},{1,3,4,5},{2,3,4,6},{3,4,5,6},{4,5,6,7}}
  tri = triangulation(vecs, maxsimps)
  assert isWellDefined tri
  assert(transpose matrix vecs == transpose matrix vectors tri)
  assert(#flipCandidates tri == 4)
  assert(#wallCircuits tri == 4)
  -- Each circuit has support of size 4 (one face of the cube).
  assert all(wallCircuits tri, w -> #(w#0) + #(w#1) == 4 and #(w#2) == 4)
  -- secondaryCone returns the reduced inequality matrix in Q-coordinates:
  -- one row per distinct circuit, one column per charge basis vector.
  -- Here d=4, N=8, so the cone lives in R^(8-4) = R^4.
  M = secondaryCone tri
  Q = chargeMatrix tri
  assert(numrows M == 4 and numcols M == 4)
  assert(numrows Q == 4 and numcols Q == 8)
  -- Sign canary: project the regularity weights to charge-lattice coordinates,
  -- then check every facet inequality.
  w = transpose matrix {regularTriangulationWeights tri}
  t = Q * w
  assert all(flatten entries (M * t), x -> x >= 0)
///

end----------------------------------------------------

restart
uninstallPackage "Triangulations"
restart
needsPackage "Triangulations"
restart
installPackage "Triangulations"
restart
check "Triangulations"

TEST /// 
  debug needsPackage "Topcom"
  -- points2chiro
  toppath = "/opt/homebrew/bin/"
  A = transpose matrix {{-1,-1,1},{-1,1,1},{1,-1,1},{1,1,1},{0,0,1}}
  tri = {{0, 2, 4}, {2, 3, 4}, {0, 1, 4}, {1, 3, 4}}
  run (toppath|"/points2chiro"|" -h")
  "topcomfoo.in" << topcomPoints(A, Homogenize=>false) << endl << close;
  chiro = get ("!"|toppath|"/points2chiro"|" <topcomfoo.in")
  #chiro
  chiro2 = "5,3:\n" | (concatenate for s in sort subsets(5,3) list (
      d := det A_s;
      if d > 0 then "+" else if d == 0 then "0" else "-"
      )) | "\n"
  chiro == chiro2
  -- notes: a chirotope for topcom:
  --  5,3:  (number of vertices, dim)
  --  a string of "-","+","0", maybe cut over a number of lines.
  -- should we make a type out of this (so we can read and write it to a file)

  -- chiro2circuits
  "topcomfoo.in" << chiro << endl << [] << endl << close;
  circs = get ("!"|toppath|"/chiro2circuits"|"  <topcomfoo.in")
  cocircs = get ("!"|toppath|"/chiro2cocircuits"|"  <topcomfoo.in")
  drop(drop(circs, 2), -1)
  oo/value

chiro = "5, 3:"

r12'chiro = "12, 4:
-+--+++---++---++-+---++-+++-----++--++-++++--++---+++-+++--+---++---++--++-++++
--+---++-+++--+++--++--+----+-+++--+++--++--+----+---++--++-++++-++--+-----+----
-++---++---+++-+++--+---++---++--++-++++--+---++-+++--+++--++--+----+-+++--+++--
++--+----+---++--++-++++---++-+++++-+++++--++++---++-+++--+++--++--+----+-+++--+
++--++--+----+---++--++-++++---++-+++++-+++++--+++---++---++--++-++++-+++--++--+
----+++--+-----+-----++--+++--++--+----+++--+-----+-----++----++-+++++-+++++--++
--------++--++-
"
  chiro = r12'chiro
  -- chiro2alltriangs, chiro2nalltriangs
  "topcomfoo.in" << "5, 3:" << endl << chiro << endl << [] << endl << close;
  "topcomfoo.in" << chiro << [] << endl << close;
  get ("!"|toppath|"/chiro2placingtriang"|" -v <topcomfoo.in")
  get ("!"|toppath|"/chiro2circuits"|" <topcomfoo.in")
  get ("!"|toppath|"/chiro2cocircuits"|" <topcomfoo.in")
  get ("!"|toppath|"/chiro2alltriangs"|" <topcomfoo.in")
  get ("!"|toppath|"/chiro2ntriangs"|" <topcomfoo.in")
  get ("!"|toppath|"/chiro2finetriang"|" <topcomfoo.in")
  get ("!"|toppath|"/chiro2finetriangs"|" <topcomfoo.in") -- what is the format of the output here??
  get ("!"|toppath|"/chiro2nfinetriangs"|" -v <topcomfoo.in")
  
///

TEST ///
  restart
  debug needsPackage "Topcom"
  needsPackage "ReflexivePolytopesDB"
  needsPackage "StringTorics"
  polytopes = kreuzerSkarke(50, Limit=>10);
  tope = polytopes_5
  A = matrix tope
  P = convexHull A
  P2 = polar P
  A = matrix{latticePoints P2}

  LP = drop(latticePointList P2, -1);
  A = transpose matrix LP;
  debugLevel = 6
  elapsedTime tri = regularFineTriangulation A;
  
  -- XXX
  augment A
  "topcomfoo.in" << topcomPoints(augment A, Homogenize=>false) << endl << close;
  chiro = get ("!"|toppath|"/points2chiro"|" <topcomfoo.in")

  "topcomfoo.in" << chiro << "[]" << endl << close;
  get ("!"|toppath|"/chiro2circuits"|" <topcomfoo.in")
  get ("!"|toppath|"/chiro2ntriangs"|" <topcomfoo.in")
  --get ("!"|toppath|"/chiro2alltriangs"|"  <topcomfoo.in")
  get ("!"|toppath|"/chiro2cocircuits"|" <topcomfoo.in")    
///

TEST ///
-- how to check a triangulation?  I don't think that Topcom has this implemented for general use.
-*
  restart
  debug needsPackage "Topcom"
*-
  -- test of isRegularTriangulation
  toppath = "/Users/mike/src/M2-master/M2/BUILD/dan/builds.tmp/as-mth-indigo.local-master/libraries/topcom/build/topcom-0.17.8/src/"
  A = transpose matrix {{-1,-1,1},{-1,1,1},{1,-1,1},{1,1,1},{0,0,1}}
  badtri = {{0, 2, 4}, {2, 3, 4}, {0, 1, 4}, {1, 2}}
  debugLevel = 6

  -- a regular triangulation
  A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{7,-2}}
  tri = {{0,1,2}, {1,3,5}, {2,3,4},
         {0,1,5}, {0,2,4}, {3,4,5},
         {1,2,3}}
  "topcomfoo.in" << topcomPoints(A, Homogenize=>true) << endl << "[]" << endl << tri << endl << close;
  run (topcompath|"checkregularity"|" --heights <topcomfoo.in >topcomfoo.out")  

  -- points2chiro
  "topcomfoo.in" << topcomPoints(A, Homogenize=>false) << endl << "[]" << endl << badtri << endl << close;
  print (toppath|"/points2alltriangs"|" --checktriang -v <topcomfoo.in") 


  A = transpose matrix {{0,3},{0,1},{-1,-1},{1,-1},{-4,-2},{4,-2}}
  tri = {{0,1,2}, {1,3,5}, {2,3,4},
         {0,1,5}, {0,2,4}, {3,4,5},
         {1,2,3}}
  "topcomfoo.in" << topcomPoints(A, Homogenize=>true) << endl << "[]" << endl << tri << endl << close;
  run (topcompath|"checkregularity"|" --heights <topcomfoo.in >topcomfoo.out")
  assert not isRegularTriangulation(A,tri)

///

///  
  -- now let's see about the naive way of getting regular star triangulations 
  -- i.e. we add in the origin
  
  pts1 =  {{-1,0,0,-1},{-1,0,1,-1},{-1,0,1,0},{-1,1,0,-1},{-1,1,0,0},{-1,1,1,2},{1,-1,0,-1},{1,0,-1,1},{1,-1,-1,-1},{0,0,0,-1},{0,0,0,0}}
  A1 = transpose matrix pts1
  elapsedTime tris1 = allTriangulations(A1, Fine=>false, ConnectedToRegular=>true, RegularOnly=>false); -- 
  fineTris1 = select(tris1, x -> # unique flatten x == numColumns A1);
  regTris1 = select(tris1, x -> isRegularTriangulation(A1, x));  
  fineRegTris1 = select(regTris1, x -> # unique flatten x == numColumns A1);
  stars1 = select(tris1, x -> all(x, x1 -> member(10, x1))); -- 100 here
  starsFine1 = select(stars1, x -> # unique flatten x == numColumns A1);
  RST = select(stars1, x -> isRegularTriangulation(A1,x)); -- 80 here...
  FSRT = select(starsFine1, x -> isRegularTriangulation(A1,x)); -- 48 here...!
  #tris1 == 2254
  #RST == 80
  #FRST = 48

  unique for tri in set5 list (
      tri1 := fineStarTriangulation(A, tri);
      newtri := for t in tri1 list append(t, 10);
      newtri
      );
  select(oo, tri -> isRegularTriangulation(A1, tri))  

  -- let's test this one for being a triangulation:
  oA = orientedCircuits A
  tri = set5_3
  tally flatten for t in tri list subsets(t,4)
  for z in oA list (
      # select(tri, t -> isSubset(z_0, t)),
      # select(tri, t -> isSubset(z_1, t))
      )
///

TEST ///
-- Bad triangulations of the square
  V = transpose matrix {{0,0},{1,0},{0,1},{1,1}}
  T1 = {{0,1,2}}
  T2 = {{0,1,2},{0,1,3}}
  T3 = {{0,1,2,3}}
  assert(not topcomIsTriangulation(V, T1))
  assert(not topcomIsTriangulation(V, T2))
  assert(not topcomIsTriangulation(V, T3))

  debug needsPackage "Triangulations" -- TODO: isTriangulation should be exported, or called naiveIsTriangulation.
  assert(not isTriangulation(V, T1))
  assert(not isTriangulation(V, T2))
  assert(not isTriangulation(V, T3)) -- gives error, should give false!
///


end--

restart
uninstallPackage "Topcom"
restart
needsPackage "Topcom"
installPackage "Topcom"
restart
check "Topcom"
viewHelp

///
-- generate examples to use for this package
-- from reflexive polytopes of dim 4
  restart
  needsPackage "StringTorics"

  str = getKreuzerSkarke(10, Limit=>5)
  str = getKreuzerSkarke(20, Limit=>5)
  str = getKreuzerSkarke(30, Limit=>5)
  polytopes = parseKS str
  tope = polytopes_4_1
  A = matrixFromString tope
  P = convexHull A
  P2 = polar P
  LP = drop(latticePointList P2, -1)
  A1 = transpose matrix LP
  A2 = transpose matrix latticePointList P2
  tri = regularFineTriangulation A1
  tri2 = regularFineTriangulation A2
  #tri
  #tri2
  elapsedTime chiro1 = chirotope A1;
  elapsedTime chiro2 = chirotope A2;
  elapsedTime # orientedCircuits chiro1
  elapsedTime # orientedCircuits chiro2
  elapsedTime # orientedCocircuits chiro1
  elapsedTime # orientedCocircuits chiro2
  (select(orientedCocircuits A2, f -> #f#0 == 0 or #f#1 == 0))/first
  netList annotatedFaces P2  
  tri2
  -- fine:
  assert(sort unique flatten tri2 == toList (0..14))
  walls = tri2/(x -> subsets(x, #x-1))//flatten
  nfacets = tally walls
  facs = (select((annotatedFaces P2), x -> x_0 == 3))/(x -> x#2)
  walls = partition(k -> nfacets#k, keys nfacets)
  for w in walls#1 list (
      # select(facs, f -> isSubset(w, f))
      )
  for w in walls#2 list (
      # select(facs, f -> isSubset(w, f))
      )
  -- check overlaps of elements of tri2:
  C = orientedCircuits A2;
  elapsedTime for c in C list (
      val1 := select(tri2, x -> isSubset(c_0, x));
      val2 := select(tri2, x -> isSubset(c_1, x));
      if #val1 > 0 and #val2 > 0 then print (c, val1, val2);
      (c, #val1, #val2));
  
  tri_0
  
///

-*
restart
needsPackage "Triangulations"
*-
///
  -- testing generateTriangulations, and flipGraph (not written yet!)
  -- Let's start with a reflexive polytope of dim 4, with h11=4.
  needsPackage "ReflexivePolytopesDB"
  --tope = (kreuzerSkarke 4)#100
  tope = KSEntry "4 10  M:73 10 N:9 7 H:4,58 [-108] id:100
     1   1   1   0   1   1  -3   1   1  -9
     0   2   0   0   3  -1  -3  -1   4  -6
     0   0   2   0  -1   3  -3   4  -1  -6
     0   0   0   1  -1  -1   1  -1  -1   4
     "
  P = polar convexHull matrix tope
  A = matrix {select(latticePoints P, x -> x != 0)}
  Ts = allTriangulations(A, Homogenize => false) -- 24 total here.
  for T in Ts list # sort unique flatten max T

  t0 = regularFineTriangulation(A, Homogenize => false) -- calls topcom
  assert isWellDefined t0 -- calls topcom
  assert naiveIsTriangulation t0 -- FAILS
  assert isRegularTriangulation t0 -- calls topcom
  regularTriangulationWeights t0 

  -- secondary cone?
  secondaryCone(transpose matrix vectors t0, max t0)
  
  -- now let's find flip graph...
  flipGraph t0
  G = flipGraph(t0, Fine => false)
  assert(#G.Triangulations == #allTriangulations(A, Homogenize => false))

  
  flips t0
  neighbors t0
  flipCandidates(A, max t0)
  orientedCircuits A
  N0 = neighbors t0 -- only the subgraph of fine triangulations
  t1 = last N0#0
  t2 = last N0#1
  neighbors t1

  -- now using neighbor's that include non-fines.
  Nall0 = neighbors t0 -- 5: t1, t2, t3, t4, t5
  t3 = Nall0_0_1
  t3 == t0
  t3 == t1
  t3 == t2
  t4 = Nall0_3_1
  t5 = Nall0_4_1
  assert(Nall0_2_1 === t2)
  assert(Nall0_1_1 === t1)
  neighbors t3

  Ts = generateTriangulations t0
  Ts_0 == t0
  Ts_1 == t1
  Ts_2 == t2

  
///

BENCHMARK = str -> null

-*
restart
needsPackage "Triangulations"
*-
BENCHMARK ///
  -- testing generateTriangulations, and flipGraph (not written yet!)
  -- Let's start with a reflexive polytope of dim 4, with h11=4.
  needsPackage "ReflexivePolytopesDB"
  --tope = (kreuzerSkarke 6)#3
  tope = KSEntry "4 15  M:32 15 N:11 10 H:6,27 [-42] id:3
   1   0   1  -1   0   1   0   0  -1   2   3  -2   1  -3  -1
   0   1  -1   1   0   1   0   0   2  -1  -3   1  -2   3  -1
   0   0   0   0   1  -1   0   1   0  -1  -2   2  -1   1   0
   0   0   0   0   0   0   1  -1  -1   1   3  -3   3  -3   0
   "
  P = polar convexHull matrix tope
  A = matrix {select(latticePoints P, x -> x != 0)}
  elapsedTime Ts = allTriangulations(A, Homogenize => false); -- 4900 triangulations, 5.4 sec


  debugLevel = 1
  t0 = Ts_1
  isFine t0
  elapsedTime G = flipGraph(t0, Fine => true);
  #G.Triangulations == 2968
  netList G.Edges
  #G.Edges

  elapsedTime Ts' = generateTriangulations(t0, Homogenize => false, Fine => false, RegularOnly => true); -- 174 sec

  elapsedTime Gall = flipGraph(t0, Fine => false); -- 71 sec
  #Gall.Triangulations == 8387
  #Gall.Edges

  elapsedTime Greg = flipGraph(t0, Fine => false, RegularOnly => true); -- 184 sec
  #Greg.Triangulations == 4900 -- this is great!  Matches topcom
  #Greg.Edges == 15040

  -- this check uses topcom. 119 sec
  elapsedTime for t in Gall.Triangulations list isRegularTriangulation t;
  tally oo -- 4900 regular, 3487 not.

  -- The following two topcom calls crash.  Probably the same problem.
  -- regularFineTriangulation(A, Homogenize => false) -- crashes in topcom
  -- elapsedTime Tsfine = allTriangulations(A, Fine => true, Homogenize => false);
  
  -- TODO: secondary cones
  --       
///

-- Some notes for me for next time using claude:
 So if I start again later, what should I say to start in on the todo list (signature-review notes accumulated section)?

⏺ Just say something like:

  ▎ "Let's start on the Triangulations signature-review TODO list."

  That phrase ("signature-review") matches the index entry in MEMORY.md, so I'll pull up the full file (project_triangulations-signature-review.md) on the first turn and see all 12
  items.

  Two things to know:

  1. You need to be in the computegv branch's directory (/Users/mike/src/M2-current-branches/M2-computegv/M2 or its parent) for the per-project memory to load. If you start me from
  a different repo, the TODO list won't be in scope — ~/.claude/CLAUDE.md will still load the M2 conventions, but the Triangulations-specific items won't.
  2. Optional sharpening, if you have a preference:
    - "...starting with item 1 (FlipGraph keying)." — if you want to tackle a specific one
    - "...go through them in order, one commit per item." — gives me a working cadence
    - "...read the list and tell me which look like the cheapest wins." — if you want me to triage first
    - "...skip anything that ripples into reflexive-polytope or external code; just the local-impact items." — to scope down

  Without sharpening, I'll most likely open with a short triage of the 12 items and ask which one you want first. Either is fine.

 Saved. To resume tomorrow, just say:

  ▎ "Let's start the secondary cone project for Triangulations."

  The phrase "secondary cone" matches the MEMORY.md index entry, so I'll pull up project_triangulations-secondary-cone.md on the first turn and have the full plan: math contract
  (hyperplane = signed circuit, oriented by which half is in $T$), the PR split (a) secondaryCone first, (b) secondaryFan after, your naive interior-point idea (sum the rays of the
  cone for an interior weight), the verification approach (regularTriangulationWeights T should land inside secondaryCone T), and the deferral of HiGHS to PR 4.

  Make sure you launch from this branch's directory (or its parent), so the per-project memories load. Good night!
  
