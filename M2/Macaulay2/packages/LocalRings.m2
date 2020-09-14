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
--
-- TODO : 1. Hilbert-Samuel Polynomial
--        2. Implement a prime filtration
--        3. Define a variety over an open cover:
-- Given C in Proj R = Proj kk[x_0,...,x_n]
-- Store L = {R_(x_0),...,R_(x_n)}
-- along with generators of C_(x_0),...,C_(x_n)
-- and gluing maps from C_(x_i) <-- C_(x_j)
---------------------------------------------------------------------------
newPackage(
    "LocalRings",
    Version => "2.0",
    Date => "January 14, 2017",
    Authors => {
        {Name => "Mahrud Sayrafi", Email => "mahrud@berkeley.edu",   HomePage => "http://ocf.berkeley.edu/~mahrud/"},
        {Name => "Mike Stillman",  Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike/"},
        {Name => "David Eisenbud", Email => "de@msri.org",           HomePage => "http://www.msri.org/~de/"}
        },
    Headline => "operations over a local ring (R, P)",
    PackageExports => {"PruneComplex"},
    AuxiliaryFiles => true
    )

-- These two are defined in m2/localring.m2
exportFrom_Core { "LocalRing", "localRing" }

export {
    "liftUp",
    "presentationComplex",
    "hilbertSamuelFunction",
    -- Legacy
    "setMaxIdeal",
    "localComplement",
    "localsyz",
    "localMingens",
    "localModulo",
    "localPrune",
    "localResolution",
    "residueMap",
    "maxIdeal"
    }

-- << "--------------------------------------------------------------------------------------" << endl;
-- << "-- The LocalRings package is experimental, but old methods are still available.     --" << endl;
-- << "-- See the documentation and comments in the package to learn more.                 --" << endl;
-- << "--------------------------------------------------------------------------------------" << endl;

debug Core;

--==================================== Basic Operations ====================================--
-- Note: The following methods are extended to local rings in this package:
-- syz                 -> localSyzHook,
-- mingens             -> localMingensHook,
-- minimalPresentation -> localMinimalPresentationHook,
-- length              -> localLengthHook,
-- trim
-- resolution
-- symbol// (quotient)
-- inducedMap
-- symbol:             -> localQuotient -- Note: quotient does not work as a synonym of symbol:
-- saturate            -> localSaturate
-- annihilator
-- Note: various elementary operations are defined in m2/localring.m2.
-- Note: isSubset symbol== are fixed in m2/modules2.m2 and reduce is fixed in m2/matrix.m2.
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

-- Computes syzygies of a matrix over local rings
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
    if opts.SyzygyRows < numrows f then (
        f = submatrix(f, 0..(opts.SyzygyRows-1),);
        matrix f
        )
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

-- Computes the length of an ideal or module over local rings
-- Note: If computing length is slow, try summing hilbertSamuelFunction for short ranges
-- TODO: check that it's Artinian first
-- test based on when hilbertSamuelFunction(M, n) == 0?
-- Maybe http://stacks.math.columbia.edu/tag/00IW ?
localLengthHook = method(Options => {Strategy => null, Limit => 1000})
localLengthHook Ideal  := ZZ => opts -> I -> localLengthHook(opts, module I)
localLengthHook Module := ZZ => opts -> M -> (
    RP := ring M;
    m := max RP;
    if class RP =!= LocalRing then error "expected objects over a local ring";
--    if not isFiniteLength M   then return -1;
    if debugLevel >= 1        then  << "isFiniteLength is not implemented" << endl;
    sum for i from 0 to opts.Limit list (
        if debugLevel >= 1    then  << i << endl;
        if opts.Limit == i    then (<< "maximum limit for computing length is reached" << endl; break);
        if opts.Strategy === Hilbert
        then n := hilbertSamuelFunction(M, i) -- really should be M/mM, but by Nakayama it's the same
        else (
            M = localMinimalPresentationHook(M, PruningMap => false);
            n = numgens M;
            M = m * M;
            n
            );
        if n == 0 then break else n
        )
    )

-- Computes the Hilbert-Samuel function for modules over local ring, possibly using a parameter ideal.
-- Note:
--   If computing at index n is fast but slows down at n+1, try computing at range (n, n+1).
--   On the other hand, if computing at range (n, n+m) is slow, break up the range.
-- TODO: implement the fast powering algorithm
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
        if debugLevel >= 1 then << i << endl;
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
        if debugLevel >= 1 then << i << endl;
        N := localMinimalPresentationHook(M, PruningMap => false);  -- really should be N/mN, but by Nakayama it's the same
        if i < n1 then M = q * N;
        localLengthHook (N/(q * N))
        )
    )

--===================================== addHooks Section =====================================--

-- syz
-- this method doesn't have hooks so we redefine it to allow runHooks
oldSyz = lookup(syz, Matrix)
syz Matrix := Matrix => opts -> m -> (
    c := runHooks(Matrix, symbol syz, (opts,m));
    if c =!= null then return c;
    error "syz: no method implemented for this type of matrix"
    )

addHook(Matrix, symbol syz, (opts,m) -> break (oldSyz opts)(m))

addHook(Matrix, symbol syz, (opts,m) -> (
        if instance(ring m, LocalRing) then break localSyzHook(opts,m)
        ))

-- res, resolution
-- this method already has hooks installed, so we can simply add a new one
addHook(Module, symbol resolution, (opts,M) -> (
        RP := ring M;
        if instance(RP, LocalRing) then (
            M' := liftUp M;
            C := resolution(M', opts);
            CP := C ** RP;
            CP = if isHomogeneous M'
              then pruneComplex(CP, UnitTest => isScalar, PruningMap => false)
              else pruneComplex(CP, PruningMap => false);
            break CP)
        ))

-- mingens
-- this method doesn't have hooks so we redefine it to allow runHooks
oldMingens = lookup(mingens, Module)
mingens Module := Matrix => opts -> (cacheValue symbol mingens) (M -> (
        c := runHooks(Module, symbol mingens, (opts, M));
        if c =!= null then return c;
        error "mingens: no method implemented for this type of module"
        ))

addHook(Module, symbol mingens, (opts,M) -> break (oldMingens opts)(M))

addHook(Module, symbol mingens, (opts,M) -> (
        if instance(ring M, LocalRing) then break localMingensHook(opts,M)
        ))

-- minimalPresentation
-- this method already has hooks installed, so we can simply add a new one
addHook(Module, symbol minimalPresentation, (opts,M) -> (
        if instance(ring M, LocalRing) then break localMinimalPresentationHook(opts,M)
        ))

-- length
-- this method doesn't have hooks so we redefine it to allow runHooks
oldLength = lookup(length, Module)
length Module := ZZ => (cacheValue symbol trim) (M -> (
    c := runHooks(Module, symbol length, M);
    if c =!= null then return c;
    error "length: no method implemented for this type of module"
    ))

addHook(Module, symbol length, M -> break oldLength M)

addHook(Module, symbol length, M -> (
        if instance(ring M, LocalRing) then break (localLengthHook M)
        ))

-- trim
-- this method doesn't have hooks so we redefine it to allow runHooks
oldTrim = lookup(trim, Module)
trim Module := Module => opts -> (cacheValue symbol trim) (M -> (
    c := runHooks(Module, symbol trim, (opts,M));
    if c =!= null then return c;
    error "trim: no method implemented for this type of module"
    ))

addHook(Module, symbol trim, (opts,M) -> break (oldTrim opts)(M))

addHook(Module, symbol trim, (opts,M) -> (
        if instance(ring M, LocalRing) then (
            if isFreeModule M then break M;
            N := subquotient(mingens M, if M.?relations then mingens image relations M);
            N.cache.trim = N;
            break N)
        ))

-- (symbol//, Matrix, Matrix)
-- Caution: this method is only correct when f = g * (f//g),
--          otherwise may not be the correct reduction of f modulo image of g
-- Here is the algorithm:
--   Given two matrices F = [f1 ... fn], G = [g1 ... gm] with the same target,
--   we wish to computer F // G such that F = G * (F // G).
--   We compute H = syz(F | G). Each column looks like H_j = [h_1 .. h_n .. h_(n+m)]^T.
--   Now for each column F_i of F, look for a unit in the i-th row of H.
--   (Question: if there are multiple units, which one to choose?)
--   Let's say in the unit u in the column H_j, then we replace F_i by -1/u times the column
--   [h_(n+1) h_(n+2) ... h_(n+m)]^T, remove the column H_j from H, and move on to F_(i+1).
--   If there aren't any units in the i-th column, we replace F_i by a 0 column and move on to F_(i+1)
oldQuotient = lookup(quotient, Matrix, Matrix)
quotient(Matrix,Matrix) := Matrix => opts -> (f, g) -> (
    RP := ring f;
    if ring g =!= RP then error "expected objects of the same ring";
    if target f =!= target g then error "expected maps with the same target";
    if instance(RP, LocalRing) then (
        G := syz liftUp(f | g);
        mat := for i from 0 to numColumns f - 1 list (
            col := f_{i};
            n := scan(numColumns G, j -> if isUnit promote(G_(i,j), RP) then break j);
            if n === null then matrix map(source g, RP^1, 0) else (
                col = -submatrix(G,{numColumns f..numRows G-1},{n}) * G_(i,n)^-1;
                G = submatrix(G, ,{0..n-1, n+1..numColumns G-1});
                (col ** RP)
                )
            );
        m := if mat === {} then 0_RP else raw matrix{mat};
        map(source g, source f, m,
	    Degree => degree matrix f - degree matrix g)  -- set the degree in the engine instead
        )
    else (oldQuotient opts)(f, g)
    )

-- inducedMap
-- TODO: verification of induced maps over local rings when opts.Verify = true
oldInducedMap = lookup(inducedMap,Module,Module,Matrix)
inducedMap(Module,Module,Matrix) := Matrix => opts -> (N',M',f) -> (
    RP := ring f;
    if ring N' =!= RP or ring M' =!= RP then error "inducedMap: expected modules and map over the same ring";
    if instance(RP, LocalRing) then (
        N := target f;
        M := source f;
        if isFreeModule N and isFreeModule M and (N =!= ambient N' and rank N === rank ambient N' or M =!= ambient M' and rank M === rank ambient M')
        then f = map(N = ambient N', M = ambient M', f)
        else (
	    if ambient N' =!= ambient N then error "inducedMap: expected new target and target of map provided to be subquotients of same free module";
	    if ambient M' =!= ambient M then error "inducedMap: expected new source and source of map provided to be subquotients of same free module";
	    );
        g := generators N * cover f * (generators M' // mingens image generators M);
        f' := g // mingens image generators N';
        f' = map(N',M',f',Degree => if opts.Degree === null then degree f else opts.Degree);
        if false and opts.Verify then ( -- FIXME this is set to false because % doesn't work over local rings
            if relations M % relations M' != 0 then error "inducedMap: expected new source not to have fewer relations";
            if relations N % relations N' != 0 then error "inducedMap: expected new target not to have fewer relations";
            if generators M' % mingens M != 0 then error "inducedMap: expected new source not to have more generators";
            if g % mingens N' != 0 then error "inducedMap: expected matrix to induce a map";
            if not isWellDefined f' then error "inducedMap: expected matrix to induce a well-defined map";
            );
        f')
    else (oldInducedMap opts)(N',M',f)
    )

--======================================= Experimental =======================================--

-- (symbol:, Thing, Thing)
-- We rely on the fact that ideal and module quotients commute with localization
-- TODO: find a way to handle the options from quotient
localQuotient = (A, B) -> (
    RP := ring A;
    if ring B =!= RP then error "expected objects of the same ring";
    if instance(RP, LocalRing) then (
        if isSubset(B, A) then (
            if class A === Ideal or class B === Module
            then return ideal 1_RP
            else return ambient A;
            );
        R := RP;
        while class R === LocalRing do R = last R.baseRings;
        A' := liftUp(A, R);
        B' := liftUp(B, R);
        C' := quotient(A', B');
        C' ** RP
        )
    else quotient(A, B)
    )

--FIXME: get quotient to work, currently A:B works
--quotient(Ideal ,Ideal      ) := Ideal  => opts -> (I,J) -> (quotientIdeal opts)(I,J)
--quotient(Ideal ,RingElement) := Ideal  => opts -> (I,f) -> (quotientIdeal opts)(I,ideal(f))
--quotient(Module,Ideal      ) := Module => opts -> (M,I) -> (quotientModule opts)(M,I)
--quotient(Module,RingElement) := Module => opts -> (M,f) -> (quotientModule opts)(M,ideal(f))
--quotient(Module,Module     ) := Ideal  => opts -> (M,N) -> (quotientAnn opts)(M,N)

Ideal  : Ideal       := Ideal  => (I,J) -> localQuotient(I,J)
Ideal  : RingElement := Ideal  => (I,r) -> localQuotient(I,ideal(r))
Module : Ideal       := Module => (M,I) -> localQuotient(M,I)
Module : RingElement := Module => (M,r) -> localQuotient(M,ideal(r))
Module : Module      := Ideal  => (M,N) -> localQuotient(M,N)

-- annihilator
-- We rely on the fact that ideal and module annihilators commute with localization
oldAnnihilator = lookup(annihilator, Module)
annihilator Module := Ideal => opts -> (cacheValue symbol annihilator) (M -> (
    RP := ring M;
    if instance(RP, LocalRing) then (
        if M == 0 then return ideal 1_RP;
        R := RP;
        while class R === LocalRing do R = last R.baseRings;
        -- Does this theoretically work?
        M' := liftUp(M, R);
        N' := annihilator(M', opts);             -- Any options we need to care about?
        N' ** RP
        )
    else (oldAnnihilator opts)(M)
    ))

-- saturate
-- We rely on the fact that ideal and module saturations commute with localization
oldSaturateIdeal  = lookup(saturate, Ideal,  Ideal)
oldSaturateModule = lookup(saturate, Module, Ideal)
localSaturate := opts -> (A, B) -> (
    RP := ring A;
    if ring B =!= RP then error "expected objects of the same ring";
    if instance(RP, LocalRing) then (
        if isSubset(B, A) then (
            if class A === Ideal
            then return ideal 1_RP
            else return ambient A;
            );
        R := RP;
        while class R === LocalRing do R = last R.baseRings;
        A' := liftUp(A, R);
        B' := liftUp(B, R);
        C' := saturate(A', B', opts);
        C' ** RP             -- Any options we need to care about?
        )
    else if class A === Ideal
    then (oldSaturateIdeal  opts)(A, B)
    else (oldSaturateModule opts)(A, B)
    )

saturate Ideal                := Ideal  => opts ->  I     -> (localSaturate opts)(I, ideal vars ring I)
saturate(Ideal, Ideal)        := Ideal  => opts -> (I, J) -> (localSaturate opts)(I, J)
saturate(Ideal, RingElement)  := Ideal  => opts -> (I, f) -> (localSaturate opts)(I, ideal(f))
saturate Module               := Module => opts ->  M     -> (localSaturate opts)(M, ideal vars ring M)
saturate(Module, Ideal)       := Module => opts -> (M, I) -> (localSaturate opts)(M, I)
saturate(Module, RingElement) := Module => opts -> (M, f) -> (localSaturate opts)(M, ideal(f))

--================================= Tests and Documentation =================================--

load ("./LocalRings/legacy.m2")
load ("./LocalRings/tests.m2")
beginDocumentation()
load ("./LocalRings/doc.m2")

end--
