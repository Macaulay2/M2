---------------------------------------------------------------------------
-- PURPOSE : Compute elementary operations of ideals, modules, and chain
--           complexes over local rings. The main lemma involves lifting
--           the objects from R_P back to R, performing the operation, then
--           localizing by tensoring with R_P and pruning the resolution.
--
-- PROGRAMMERS : Localization at a maximal ideal was implemented by Mike Stillman
--               and David Eisenbud (legacy code was moved to LocalRings/legacy.m2).
--               Support for prime ideals added by Mahrud Sayrafi and Mike Stillman.
--
-- UPDATE HISTORY : created 1 July 2008;
-- 	     	    updated 4 January 2017; last update 25 October 2017.
-- 	     	    updated 12 May 2021
--
-- ISSUE TRACKER : https://github.com/orgs/Macaulay2/projects/5
--
-- TODO : 1. Hilbert-Samuel Polynomial
--        2. Implement a prime filtration
--        3. Define a variety over an open cover:
-- Given C in Proj R = Proj kk[x_0,...,x_n]
-- Store L = {R_(x_0),...,R_(x_n)}
-- along with generators of C_(x_0),...,C_(x_n)
-- and gluing maps from C_(x_i) <-- C_(x_j)
--        4. hookify quotientRemainder and add hook for local rings
--        5. add hooks for radical, minimalPrimes, associatedPrimes, etc.
--        6. what does flattenRing mean over a local ring?
---------------------------------------------------------------------------
newPackage(
    "LocalRings",
    Version => "2.1",
    Date => "May 08, 2021",
    Authors => {
        {Name => "Mahrud Sayrafi", Email => "mahrud@umn.edu",        HomePage => "https://math.umn.edu/~mahrud/"},
        {Name => "Mike Stillman",  Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike/"},
        {Name => "David Eisenbud", Email => "de@msri.org",           HomePage => "http://www.msri.org/~de/"}
        },
    Headline => "operations over a local ring R_p",
    Keywords => {"Commutative Algebra"},
    PackageExports => {"PruneComplex", "Saturation"},
    AuxiliaryFiles => true
    )

importFrom_Core { "ContainmentHooks", "ReduceHooks", "printerr",
    "raw", "rawLiftLocalMatrix", "rawMatrixRemake2", "rawSource" }

-- see m2/localring.m2
exportFrom_Core { "LocalRing" }

export {
    "localRing",
    "liftUp",
    "presentationComplex", -- TODO: what was this for?
    "hilbertSamuelFunction",
    -- Legacy
    "setMaxIdeal",
    "localComplement",
    "localsyz",
    "localMingens",
    "localModulo",
    "localPrune",
    "localResolution",
    "residueMap", -- not documented
    "maxIdeal" -- not documented
    }

-- << "--------------------------------------------------------------------------------------" << endl;
-- << "-- The LocalRings package is experimental, but old methods are still available.     --" << endl;
-- << "-- See the documentation and comments in the package to learn more.                 --" << endl;
-- << "--------------------------------------------------------------------------------------" << endl;

--===================================== LocalRing type =====================================--

-- used to be in the Core
load "./LocalRings/localring.m2"

--=================================== Basic Constructors ===================================--

PolynomialRing _ Ideal := LocalRing => (R, P) -> ( -- assumes P is prime
    if ring P =!= R then error "expected a prime ideal in the same ring for localization";
    localRing(R, P))

PolynomialRing _ RingElement := LocalRing => (R, f) -> (
    if ring f =!= R then error "expected a ring element in the same ring for localization";
    error "localizing at a hypersurface is not yet implemented")

isWellDefined LocalRing := R -> isPrime R.maxIdeal -- cached in the maximal ideal

baseRing LocalRing := Ring => RP -> ( R := RP; while instance(R, LocalRing) do R = last R.baseRings; R )

--==================================== Basic Operations ====================================--
-- Note: The following methods are extended to local rings in this package:
-- syz                 -> localSyzHook,
-- mingens             -> localMingensHook,
-- minimalPresentation -> localMinimalPresentationHook,
-- length              -> localLengthHook,
-- trim
-- resolution
-- (quotient, Matrix, Matrix)
-- inducedMap
-- (remainder, Matrix, Matrix)
-- isSubset
-- quotient            -> localQuotient -- Note: quotient does not work as a synonym of symbol:
-- saturate            -> localSaturate
-- annihilator
-- Note: various elementary operations are defined in LocalRings/localring.m2.
-- Note: map, modulo, subquotient, kernel, cokernel, image, homology, Hom, Ext, Tor, and many
--       other methods that rely only on the methods above work for local rings automatically.
-- Note: certain methods, such as symbol%, radical, minimalPrimes, etc. are not yet implemented.
--       If you need specific methods that do not work, please inform Mahrud Sayrafi.

-- Lifts objects over R_P to an object over R such that tensoring with R_P results
-- in the original object.
-- TODO: implement this in engine for mutable matrices as well
-- TODO: add option to return a row of common denominators to be used in syz
liftUp = method()
liftUp Thing                :=  T     -> liftUp(T, last (ring T).baseRings)
liftUp(Ideal, Ring)         := (I, R) -> ideal liftUp(gens I, R)
liftUp(Module, Ring)        := (M, R) -> (
    g := generators M;
    r := relations  M;
    c := (if M.?generators then 1 else 0) +
         (if M.?relations  then 1 else 0) * 2;
    if c == 0 then return R^(-degrees M);                          --freemodule
    if c == 1 then return subquotient(liftUp(g, R),             ); --image
    if c == 2 then return subquotient(            , liftUp(r, R)); --coker
    if c == 3 then return subquotient(liftUp(g, R), liftUp(r, R)); --subquotient
    )
liftUp(Matrix, Ring)        := (m, R) ->
    map(liftUp(target m, R),
        liftUp(source m, R),
        rawLiftLocalMatrix(raw R, raw m))
liftUp(MutableMatrix, Ring) := (m, R) -> mutableMatrix liftUp(matrix m, R)
liftUp(RingElement, Ring)   := (r, R) -> (liftUp(matrix {{r}}, R))_(0,0)

-- Computes syzygies of a matrix over local rings
-- TODO: don't compute too much if certain options are given
localSyzHook = method(Options => options syz)
localSyzHook Matrix := Matrix => opts -> m -> (
    RP := ring m;
    f' := liftUp m;
    g' := syz f';
    h' := syz g';
    g := g' ** RP;
    h := h' ** RP;
    C := {g, h};
    C = first pruneComplex(C, 1, Direction => "right", PruningMap => false);
    f := C#0;
    -- Dot product with the denominators of lift
    N := transpose entries m;
    for i from 0 to numcols m - 1 do
      rowMult(f, i, N_i/denominator//lcm);
    -- TODO make sure other options are treated correctly
    if opts.SyzygyLimit < numcols f then f = submatrix(f, , 0 ..< opts.SyzygyLimit);
    if opts.SyzygyRows < numrows f then matrix submatrix(f, 0 ..< opts.SyzygyRows, )
    else map(source m, , matrix f)
    )

-- Computes mingens of modules over local rings
-- TODO: if presentationComplex exists, skip some steps
localMingensHook = method(Options => options mingens)
localMingensHook Module := Matrix => opts -> M -> (
    RP := ring M;
    F := ambient M;
    c := (if M.?generators then 1 else 0) + 2 * (if M.?relations then 1 else 0);
    if c == 0 then return id_F; --freemodule
    if c == 1 then (            --image
        f := generators M;
        f' := liftUp f;
        g' := syz f';
        g := g' ** RP;
        C := {f, g};
        C = first pruneComplex(C, 1, Direction => "right", PruningMap => false);
        return map(F, , matrix C#0);
        );
    if c == 2 then (            --coker
        f = id_F;
        g = relations M;
        C = {f, g};
        C = first pruneComplex(C, 1, Direction => "right", PruningMap => false);
        return map(F, , matrix C#0);
        );
    if c == 3 then (            --subquotient
        f = generators M;
        g = relations M;
        f' = liftUp f;
        g' = liftUp g;
        h' := modulo (f', g');
        h := h' ** RP;
        C = {f, h};
        C = first pruneComplex(C, 1, Direction => "right", PruningMap => false);
        return map(F, , matrix C#0);
        );
    )

-- Computes minimalPresentation of modules over local rings
-- TODO: if presentationComplex exists, skip stuff
localMinimalPresentationHook = method(Options => options minimalPresentation ++ {PruningMap => true})
localMinimalPresentationHook Module := Module => opts -> M -> (
    RP := ring M;
    c := (if M.?generators then 1 else 0) + 2 * (if M.?relations then 1 else 0);
    if c == 0 then return M; --freemodule
    if c == 1 then (         --image
        f := generators M;
        f' := liftUp f;
        g' := syz f';
        h' := syz g';
        g := g' ** RP;
        h := h' ** RP;
        (C, P) := ({g, h}, null);
        (C, P)  = pruneComplex(C, PruningMap => true);
        phi := map(M, , matrix P#0);
        N := coker map(source phi, , matrix C#0);
        phi = map(M, N, phi);
        N.cache.pruningMap = phi;
        M.cache.presentationComplex = toChainComplex C;
        return N;
        );
    if c == 2 then (         --coker
        f = relations M;
        f' = liftUp f;
        g' = syz f';
        g = g' ** RP;
        C = {f, g};
        (C, P) = pruneComplex(C, PruningMap => true);
        phi = map(M, , matrix P#0);
        N = coker map(source phi, , matrix C#0);
        phi = map(M, N, phi);
        N.cache.pruningMap = phi;
        M.cache.presentationComplex = toChainComplex C;
        return N;
        );
    if c == 3 then (         --subquotient
        f = generators M;
        g = relations M;
        f' = liftUp f;
        g' = liftUp g;
        h' = modulo (f', g');
        e' := syz h';
        h = h' ** RP;
        e := e' ** RP;
        C = {h, e};
        (C, P) = pruneComplex(C, PruningMap => true);
        phi = map(M, , matrix P#0);
        N = coker map(source phi, , matrix C#0);
        phi = map(M, N, phi);
        N.cache.pruningMap = phi;
        M.cache.presentationComplex = toChainComplex C;
        return N;
        );
    )

--===================== Length and Hilbert-Samuel Polynomial Polynomial =====================--

-- TODO: check that it's Artinian first
-- test based on when hilbertSamuelFunction(M, n) == 0?
-- Maybe http://stacks.math.columbia.edu/tag/00IW ?
isFiniteLength = x -> (
    if debugLevel >= 1 then printerr "isFiniteLength is not implemented; assuming the input has finite length"; true)

-- Computes the length of an ideal or module over local rings
-- Note: If computing length is slow, try summing hilbertSamuelFunction for short ranges
-- TODO: implement partial caching
localLengthHook = method(Options => {Strategy => null, Limit => 1000})
localLengthHook Ideal  := ZZ => opts -> I -> localLengthHook(opts, module I)
localLengthHook Module := ZZ => opts -> M -> (
    if not isFiniteLength M then return infinity;
    m := max ring M;
    sum for i to opts.Limit list (
	if M == 0 then break;
        if debugLevel >= 2 then printerr("step ", toString i);
        if opts.Strategy === Hilbert
        then hilbertSamuelFunction(M, i) -- really should be M/mM, but by Nakayama it's the same
        else (
            N := localMinimalPresentationHook(M, PruningMap => false);
            if i < opts.Limit then M = m * N;
            numgens N)
        )
    )

-- Computes the Hilbert-Samuel function for modules over local ring, possibly using a parameter ideal.
-- Note:
--   If computing at index n is fast but slows down at n+1, try computing at range (n, n+1).
--   On the other hand, if computing at range (n, n+m) is slow, break up the range.
-- TODO: implement the fast powering algorithm
-- TODO: switch the order of the inputs
-- TODO: implement partial caching
hilbertSamuelFunction = method()
hilbertSamuelFunction (Module, ZZ)            := ZZ   => (M, n) -> first hilbertSamuelFunction(M,n,n)
-- Eisenbud 1995, Chapter 12:
-- Input:  finitely generated (R,m)-module M, integer n0, n1
-- Output: H_M(i) := dim_{R/q}( m^n M / m^{n+1} M ) for i from n0 to n1
hilbertSamuelFunction (Module, ZZ, ZZ)        := List => (M, n0, n1) -> (
    RP := ring M;
    if class RP =!= LocalRing then error "expected objects over a local ring";
    m := max RP;
    M = m^n0 * M;
    for i from n0 to n1 list (
        if debugLevel >= 1 then printerr("computing HSF_", toString i);
        N := localMinimalPresentationHook(M, PruningMap => false);  -- really should be N/mN, but by Nakayama it's the same
        if i < n1 then M = m * N;
        numgens N
        )
    )
hilbertSamuelFunction (Ideal, Module, ZZ)     := ZZ   => (q, M, n) -> first hilbertSamuelFunction(q,M,n,n)
-- Eisenbud 1995, Section 12.1:
-- Input:  parameter ideal q, finitely generated (R,m)-module M, integers n0, n1
-- Output: H_{q, M}(i) := length( q^n M / q^{n+1} M ) for i from n0 to n1
hilbertSamuelFunction (Ideal, Module, ZZ, ZZ) := List => (q, M, n0, n1) -> (
    RP := ring M;
    if class RP =!= LocalRing then error "expected objects over a local ring";
    if ring q =!= RP          then error "expected objects over the same ring";
    if q == max RP            then return hilbertSamuelFunction(M, n0, n1);
    M = localMinimalPresentationHook(M, PruningMap => false);
    M = q^n0 * M;
    for i from n0 to n1 list (
        if debugLevel >= 1 then printerr("computing HSF_", toString i);
        N := localMinimalPresentationHook(M, PruningMap => false);  -- really should be N/mN, but by Nakayama it's the same
        if i < n1 then M = q * N;
        localLengthHook (N/(q * N))
        )
    )

--===================================== addHooks Section =====================================--

-- res, resolution
addHook((resolution, Module), Strategy => Local, (opts, M) -> (
        RP := ring M;
        if instance(RP, LocalRing) then (
            M' := liftUp M;
            C := resolution(M', opts);
            CP := C ** RP;
            CP = if isHomogeneous M'
              then pruneComplex(CP, UnitTest => isScalar, PruningMap => false)
              else pruneComplex(CP, PruningMap => false);
            CP)
        ))

-- syz
addHook((syz, Matrix), Strategy => Local, (opts, m) ->
    if instance(ring m, LocalRing) then localSyzHook(opts, m))

-- mingens
addHook((mingens, Module), Strategy => Local, (opts, M) ->
    if instance(ring M, LocalRing) then localMingensHook(opts, M))

-- minimalPresentation
addHook((minimalPresentation, Module), Strategy => Local, (opts, M) ->
    if instance(ring M, LocalRing) then localMinimalPresentationHook(opts,M))

-- length
addHook((length, Module), Strategy => Local, M ->
    if instance(ring M, LocalRing) then localLengthHook M)

-- trim
addHook((trim, Module), Strategy => Local, (opts, M) ->
    if instance(ring M, LocalRing) then subquotient(ambient M,
	if M.?generators then localMingensHook(opts, image generators M),
	if M.?relations  then localMingensHook(opts, image relations M)))

load "./LocalRings/LU.m2"

-- (symbol//, Matrix, Matrix)
-- Here is the algorithm:
--   given matrices  [f], [g] with the same target
--   want to compute  f // g such that f = g * (f // g)
--   compute  h = syz(f | g) = [A || B] so that f*A + g*B = 0 and f = -g*B*A^-1
--   compute an LU decompostion of h to get [ id || B*A^-1 ] and return -B*A^-1
-- Note: this is not always possible, in which case f - g * (f // g) will be the remainder
-- TODO: does this work over a prime ideal?
addHook((quotient, Matrix, Matrix), Strategy => Local, (opts, f, g) -> (
    RP := ring f;
    if instance(RP, LocalRing) then (
        r := numColumns f;
        s := numColumns g;
        if debugLevel >= 2 then printerr "beginning syzygy computation";
        -- TODO: why are columns of h sometimes ordered incorrectly?
        -- if they were sorted correctly, we would only need r columns here
        h := mutableMatrix(syz(liftUp(fg := f | g), SyzygyLimit => infinity -* r *- ) ** RP);
        n := numColumns h;
        -- Dot product with the denominators of lift
        N := transpose entries fg;
        for i in 0 ..< #N do rowMult(h, i, N_i/denominator//lcm);
        -- see test near line 252 of tests.m2; here is a naive sort:
        h0 := mutableMatrix RP.residueMap matrix h;
        c := 0;
        for i in 0 ..< r + s do (
            if c == r then break;
            scan(c ..< n, j -> if isUnit h0_(i, j)
                then ( columnSwap(h, c, j); columnSwap(h0, c, j); c = c + 1; break )));
        n = c; -- TODO: is this the correct rank?
        -- initiating LU-decomposition matrices
        if debugLevel >= 2 then printerr "beginning LU decomposition";
        P := new MutableList from (0 ..< r + s);
        -- TODO: can we do the LU after reducing the top portion to the residue field?
        LU := mutableMatrix map(RP^(r + s), RP^(n + 1), 0);
        for i in 0 ..< n do incrLU(P, LU, h_{i}, i);
        (L, U) := extractLU(LU, r + s, n);
        for i in 0 ..< n do colReduce(L, i);
        m := - submatrix(L, {r ..< r + s}, {0 ..< n});
        columnPermute(m, 0, (toList P)_{0 ..< n});
        -- padding is necessary when image f \nin image g, so we get a remainder
        m  = matrix m | map(RP^s, RP^(max(0, r - n)), 0);
        map(source g, source f, m,
            Degree => degree matrix f - degree matrix g)  -- set the degree in the engine instead
        )))

-- inducedMap
addHook((inducedMap, Module, Module, Matrix), Strategy => Local, (opts, N', M', f) -> (
    RP := ring f;
    if instance(RP, LocalRing) then (
        N := target f;
        M := source f;
        g := generators N * cover f * (generators M' // mingens image generators M);
        f' := g // mingens image generators N';
        f' = map(N',M',f',Degree => if opts.Degree === null then degree f else opts.Degree);
	(f', g, mingens N, mingens M))))

-- Caution: this is only correct in the sense that f = g * (f // g) + r
-- but over a local ring, this may not be the correct reduction of f modulo image of g
addHook((remainder, Matrix, Matrix), Strategy => Local, (f, g) ->
    if instance(ring f, LocalRing) then f - g * (f // g))

-- TODO: first hookify quotientRemainder
--addHook((quotientRemainder, Matrix, Matrix), Strategy => Local, (f, g) -> (
--    RP := ring f;
--    if instance(RP, LocalRing) then error "remainder over local rings is not implemented"))

-- TODO: probably can do better here
-- see issub in m2/modules2.m2
addHook(ContainmentHooks, Strategy => Local, (f, g) -> (
    RP := ring f;
    if not instance(RP, LocalRing) then return null;
    all(numColumns f, i -> (
	    L := flatten entries syz(liftUp(f_{i} | g), SyzygyRows => 1);
	    any(L, u -> isUnit promote(u, RP))))
    ))

-- Warning: does not return a normal form over local rings
-- see reduce in m2/matrix.m2
addHook(ReduceHooks, Strategy => Local, (tar, rawF) -> (
    RP := ring tar;
    if not instance(RP, LocalRing) then return null;
    f := map(RP, rawF);
    g := presentation tar;
    m := apply(numColumns f, i -> (
	    L := flatten entries syz(liftUp(f_{i} | g), SyzygyRows => 1);
	    if any(L, u -> isUnit promote(u, RP)) then map(tar, RP^1, 0) else f_{i}));
    rawMatrixRemake2(raw cover tar, rawSource rawF, degree rawF, raw matrix{m}, 0)
    ))

--======================================= Experimental =======================================--

-- (symbol:, Thing, Thing)
-- We rely on the fact that ideal and module quotients commute with localization
localQuotient := (opts, A, B) -> (
    RP := ring A;
    if instance(RP, LocalRing) then (
        R := baseRing RP;
        A' := liftUp(A, R);
        B' := liftUp(B, R);
        C' := quotient(A', B', opts);
        C' ** RP))

-- saturate
-- We rely on the fact that ideal and module saturations commute with localization
localSaturate := (opts, A, B) -> (
    RP := ring A;
    if instance(RP, LocalRing) then (
        R := baseRing RP;
        A' := liftUp(A, R);
        B' := liftUp(B, R);
        C' := saturate(A', B', opts);
        C' ** RP))

-- annihilator
-- We rely on the fact that ideal and module annihilators commute with localization
localAnnihilator := (opts, A) -> (
    RP := ring A;
    if instance(RP, LocalRing) then (
        R := baseRing RP;
        -- TODO: is this theoretically correct?
        A' := liftUp(A, R);
        B' := annihilator(A', opts);
        B' ** RP))

--============================= addHooks Section for Saturation =============================--

-- Installing local hooks for quotient and saturate
scan({	(quotient, Ideal,  Ideal),
	(quotient, Module, Ideal),
	(quotient, Module, Module)}, key -> addHook(key, localQuotient,    Strategy => Local))
scan({	(saturate, Ideal,  Ideal),
	(saturate, Ideal,  RingElement),
	(saturate, Module, Ideal)},  key -> addHook(key, localSaturate,    Strategy => Local))
scan({	(annihilator, Module)},      key -> addHook(key, localAnnihilator, Strategy => Local))

--================================= Tests and Documentation =================================--

load ("./LocalRings/legacy.m2")
load ("./LocalRings/tests.m2")
beginDocumentation()
load ("./LocalRings/doc.m2")

end--

--==================================== Under Development ====================================--
--TODO: implement a prime filtration

--TODO: localRingQuot
-- Returns the local quotient ring RP/I
-- TODO should we insert this as a hook in (quotient, Ideal)?
localRingQuot = (RP,I) -> I.cache.QuotientRing = (
    if ring I =!= RP then error "expected ideal of the same ring";
    if I.cache.?QuotientRing then return I.cache.QuotientRing;
    if I == 0 then return RP;
    -- TODO eg: ZZ[x,y]/ideal(2)
    R := RP;
    m := max RP;
    -- TODO isWellDefined should do the following:
    if not isSubset(I, m) then return 0; -- FIXME zero ring, not element
    while instance(R, LocalRing) do R = last R.baseRings;
    J := liftUp(I, R);
    Q := R / J;
    n := promote(liftUp(m, R), Q);
    QP := localRing(Q, n)
    )

LocalRing / Ideal := LocalRing => (RP,I) -> (
    if ring I === RP then localRingQuot(RP,I) else localRingQuot(RP,promote(I, RP)))

/// -- FIXME -- See examples.m2 Eisenbud 12.2
  restart
  needsPackage "LocalRings"
  R =ZZ/32003[x,y,z,w]
  P =ideal"x,y,w"    -- z -> unit
  RP=localRing(R, P)
  QP=RP/ideal"x"     -- x -> 0

  rawQuotientRing(raw RP, raw generators gb generators ideal x) -- (RP/J)
  use R
  Q = R/ideal x
  rawLocalRing(raw Q, raw gb ideal gens Q) -- (R/I)_Q

  assert(x+y+z === y+z)
  assert(y/(z+x) + x/z + w/(z+y) === y/z + w/(z+y))
  assert(ideal"z" === ideal 1_QP)          --FIXME
  assert(ideal"y":ideal"z,y" === ideal"y") --FIXME
  assert(ideal"y":ideal"x,y" === ideal 1_QP)
  assert(ideal"x,y":ideal"x" === ideal 1_QP)
  assert(generators QP === {y,w})         -- FIXME don't show 0 and z as generators

  use RP
  QP = RP/ideal"z" -- this should make the whole ring 0 because we're quotienting by a unit, I think?
  assert(generators QP === {}) -- FIXME should be empty, or only zero
///

-- TODO: adapt these two examples to local rings
-- They require local quotient rings
/// -- from Macaulay2Doc/test/ann.m2
  S=QQ[x_0..x_4]
  R=S/(ideal(x_0,x_1)*ideal(x_2,x_3))
  J=ideal vars R
  M=R^1/J
  d=3
  N=(R^1)/(J^d)
  assert( annihilator Tor_1(M,N) == annihilator Tor_1(N,M) )
///

/// -- from Macaulay2Doc/test/ann2.m2
  S=QQ[x_0..x_3]
  R=S/ideal(x_0*x_1-x_2*x_3)
  J=ideal vars R
  M=R^1/J
  d=3
  N=(R^1)/(J^d)
  assert( annihilator Tor_1(M,N)==  annihilator Tor_1(N,M) )
///

/// --For another example, we compute the annihilator of an element in a quotient ring
  R = QQ[a..d];
  RP = localRing(R, ideal gens R)
  SP = RP/(a*b,a*c,a*d)
  ann a
///
