---------------------------------------------------------------------------
-- PURPOSE : Compute elementary operations of ideals, modules, and chain
--           complexes over local rings. The main trick involves lifting
--           the objects from R_P back to R, performing the operation, then
--           promoting from R to R_P and pruning the resolution.
--
-- PROGRAMMERs : Localization at a maximal ideal was implemented by Mike Stillman
--               and David Eisenbud (legacy code was moved to Localization.m2).
--               Support for prime ideals added by Mike Stillman and Mahrud Sayrafi.
--               
-- UPDATE HISTORY : created 1 July 2008
-- 	     	    updated 4 January 2017, and later
--
-- TODO :
---------------------------------------------------------------------------
newPackage(
    "LocalRings",
    Version => "1.0", 
    Date => "January 14, 2017",
    Authors => {
      {Name => "Mike Stillman",  Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike/"},
      {Name => "Mahrud Sayrafi", Email => "mahrud@berkeley.edu",   HomePage => "http://ocf.berkeley.edu/~mahrud/"}
      },
    Headline => "Local operations",
    PackageExports => {"Localization", "PruneComplex"},
    DebuggingMode => true,
    AuxiliaryFiles => true
    )

export {
    "liftUp",
--   "newMingens",
    "newMinimalPresentation",
    "presentationComplex",
    "localQuotient",
    "localSaturate",
    "localAnnihilator",
    "localLength",
    "hilbertSamuelFunction"
    }

--============================ Elementary Operations ===================================--

-- FIXME what about a module? Is this correct for general modules?
liftUp = method() -- Lifts things over R_P to R, columnwise
liftUp Ideal                :=  I         -> ideal liftUp gens I
liftUp(Ideal, Ring)         := (I, R)     -> ideal liftUp(gens I, R)
liftUp List                 :=  L         -> for M in L list liftUp M
liftUp(List, Ring)          := (L, R)     -> for M in L list liftUp(M, R)
liftUp ChainComplex         :=  C         -> toChainComplex liftUp toMutable C
liftUp(ChainComplex, Ring)  := (C, R)     -> toChainComplex liftUp(toMutable C, R)
liftUp Matrix               :=  M         -> matrix liftUp mutableMatrix M
liftUp(Matrix, Ring)        := (M, R)     -> matrix liftUp(mutableMatrix M, R)
liftUp Module               :=  M         -> liftUp(M, last (ring M).baseRings)
liftUp(Module,Ring)         := (M,R)      -> prune subquotient (liftUp(generators M, R), liftUp(relations M, R))
liftUp MutableMatrix        :=  M         -> liftUp(M, last (ring M).baseRings)
liftUp(MutableMatrix, Ring) := (M, R)     -> (
    if instance(ring M, LocalRing) then (
      N := transpose entries M; 
      for i from 0 to numcols M - 1 do 
        columnMult(M, i, N_i/denominator//lcm);
      );
    mutableMatrix lift(matrix M, R)
    )

-- TODO: if presentationComplex exists, skip stuff
newMinimalPresentation = method()
newMinimalPresentation Module := M -> (
    if true or instance(ring M, LocalRing) then (
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
            C := {mutableMatrix g, mutableMatrix h};
--            pruneComplex(mutableComplex, PruningMaps => true);
            (C1, P1) := pruneDiff(C, 0, PruningMaps => true);
    -- FIXME, should this be C1?  v
            (C2, P2) := pruneDiff(C, 1, PruningMaps => P1);
            N := coker matrix C2#0;
--            M.cache.pruningMap = map(M, N, phi);
-- FIXMEFIXMEFIXME RIGHT NOW
--            phi := map(image liftUp generators M,
--                       coker liftUp relations N,
--                       liftUp matrix P2#0);                  -- FIXME liftUp hack
--            N.cache.pruningMap = matrix phi ** RP;           -- FIXME how should this be given?
            M.cache.presentationComplex = toChainComplex C2;
            return (N, P2);
            );
        if c == 2 then (         --coker
            f = relations M;
            f' = liftUp f;
            g' = syz f';
            g = g' ** RP;
            C = {mutableMatrix f, mutableMatrix g};
--            pruneComplex(C, PruningMaps => true);
            (C1, P1) = pruneDiff(C, 0, PruningMaps => true);
            (C2, P2) = pruneDiff(C, 1, PruningMaps => P1);
            N = coker matrix C2#0;
--            M.cache.pruningMap = map(M, N, phi);
-- FIXMEFIXMEFIXME RIGHT NOW
--            phi = map(coker liftUp relations M,
--                      coker liftUp relations N,
--                      liftUp matrix P2#0);                   -- FIXME hack
--            N.cache.pruningMap = matrix phi ** RP;
            M.cache.presentationComplex = toChainComplex C2;
            return (N, P2);
            );
        if c == 3 then (         --subquotient
            f = generators M;
            g = relations M;
            f' = liftUp f;
            g' = liftUp g;
            h' = modulo (f, g);
            e' := syz h';
            h = h' ** RP;
            e := e' ** RP;
            C = {mutableMatrix h, mutableMatrix e};
            -- TODO
--            error "debug me";
--            pruneComplex(C, PruningMaps => true);
            (C1, P1) = pruneDiff(C, 0, PruningMaps => true);
            (C2, P2) = pruneDiff(C, 1, PruningMaps => P1);
            N = coker matrix C2#0;
--            M.cache.pruningMap = map(M, N, matrix P2#0);
--            N.cache.pruningMap = matrix P2#0;                -- FIXME, this is not actually what we want
            M.cache.presentationComplex = toChainComplex C2;
            return (N, P2);
            );
        );
    )

newMingens = method()
newMingens Module := M -> (
    if true or instance(ring M, LocalRing) then (
        RP := ring M;
        c := (if M.?generators then 1 else 0) + 2 * (if M.?relations then 1 else 0);
        if c == 0 then return generators M; --freemodule
        if c == 1 then (         --image
            f := generators M;
            f' := liftUp f;
            g' := syz f';
            g := g' ** RP;
            C := {mutableMatrix f, mutableMatrix g};
            pruneDiff(C, 1);
            return matrix C#0;
            );
        if c == 2 then (         --coker
            f = relations M;
            f' = liftUp f;
            g' = syz f';
            g = g' ** RP;
            C = {mutableMatrix f, mutableMatrix g};
            pruneDiff(C, 1);
            return matrix C#0;
            );
        if c == 3 then (         --subquotient
            f = generators M;
            g = relations M;
            f' = liftUp f;
            g' = liftUp g;
            h' := modulo (f, g);
            h := h' ** RP;
            C = {mutableMatrix h};
            pruneComplex C;
            return matrix C#0;
            );
        );
    )

--============================ addHooks Sections ===================================--

-- res, resolution
-- this method already has hooks installed, so we can simply add a new one
addHook(Module, symbol resolution, (opts, M) -> (
        RP := ring M;
        if instance(RP, LocalRing) then (
            M' := coker liftUp presentation M;
            C := resolution(M', opts);
            CP := C ** RP;
            CP = pruneComplex CP;
            break CP)
        ))

-- minimalPresentation
-- this method already has hooks installed, so we can simply add a new one
addHook(Module, symbol minimalPresentation, (opts,M) -> (
        RP := ring M;
        if instance(RP, LocalRing) then (
            (N, P) := newMinimalPresentation M;
            break N)
        ))

-- mingens
-- this method doesn't have hooks so we redefine it to allow runHooks
oldMingens = lookup(mingens, Module)
mingens Module := Matrix => opts -> (cacheValue symbol mingens) ((M) -> (
        c := runHooks(Module, symbol mingens, (opts, M));
        if c =!= null then return c;
        error "mingens: no method implemented for this type of module"
        ))

addHook(Module, symbol mingens, (opts,M) -> (
        break ((oldMingens(opts))(M))
        ))

addHook(Module, symbol mingens, (opts,M) -> (
        RP := ring M;
        if instance(RP, LocalRing) then (
            m := newMingens M;
            break m)
        ))

-- syz
-- this method doesn't have hooks so we redefine it to allow runHooks
oldSyz = lookup(syz, Matrix)
syz Matrix := Matrix => opts -> (m) -> (
    c := runHooks(Matrix, symbol syz, (opts, m));
    if c =!= null then return c;
    error "syz: no method implemented for this type of matrix"
    )

addHook(Matrix, symbol syz, (opts,m) -> (
        break ( ( oldSyz(opts) )(m) )
        ))

addHook(Matrix, symbol syz, (opts,m) -> (
        RP := ring m;
        if instance(RP, LocalRing) then (
            f' := liftUp m;
            g' := syz(f', opts);          -- we're running blind on the options here ... the crucial one is SyzygyRows
            h' := syz g';
            g := g' ** RP;
            h := h' ** RP;
            C := {mutableMatrix g, mutableMatrix h};
            pruneDiff(C, 1);
            break (matrix C#0))
        ))


-- TODO http://stacks.math.columbia.edu/tag/00IW
-- check that it's Artinian first?
isFiniteLength = method()
isFiniteLength Module := M -> (
    RP := ring M;
    if class RP =!= LocalRing then error "expected objects over a local ring";
    -- test based on when hilbertSamuelFunction(M, n) == 0?
    true
    )

localLength = method()
localLength Ideal  := I -> localLength module I
localLength Module := M -> (
    RP := ring M;
    if class RP =!= LocalRing then error "expected objects over a local ring";
    if not isFiniteLength M   then return -1;
    m := promote(max RP, RP);
    n := -1;
    l := while n != 0 list (
        M = minimalPresentation M; -- really should be M/mM, but by Nakayama it's the same
        n = numgens M;
        if debugLevel >= 1 then << n << endl;
        M = m * M; 
        n
        );
    l//sum
    )

hilbertSamuelFunction = method()
hilbertSamuelFunction (Module, ZZ)        :=    (M, n) -> (
-- Eisenbud 1995, Chapter 12:
-- Input:  finitely generated (R,m)-module M, integer n
-- Output: H_M(n) := dim_{R/q}( m^n M / m^{n+1} M )
    RP := ring M;
    if class RP =!= LocalRing then error "expected objects over a local ring";
    m := promote(max RP, RP);
    -- FIXME this is very costly. Perhaps try divide and conquere?
    N := m^n * M;
    numgens minimalPresentation N -- really should be N/mN, but by Nakayama it's the same
    )
hilbertSamuelFunction (Ideal, Module, ZZ) := (q, M, n) -> (
-- Eisenbud 1995, Section 12.1:
-- Input:  parameter ideal q, finitely generated (R,m)-module M, integer n
-- Output: H_{q, M}(n) := length( q^n M / q^{n+1} M )
    RP := ring M;
    if class RP =!= LocalRing then error "expected objects over a local ring";
    if ring q =!= RP          then error "expected objects over the same ring";
    M = minimalPresentation M;
    -- FIXME this is very costly. Perhaps try divide and conquere?
    M = q^n * M;
    localLength (M/(q * M))
    )

--##################### Work in progress ####################--

--FIXME: gens RP/I gives error

--TODO insert this as a hook in (quotient, Ideal)
localRingQuot = I -> I.cache.QuotientRing = (
    RP := ring I;
    if I.cache.?QuotientRing then return I.cache.QuotientRing;
    if I == 0 then return RP;
    --TODO if I is a unit ideal, return 0. FIXME 1 % 1
    M := max RP;
    R := RP;
    while class R === LocalRing do R = last R.baseRings;
    J := liftUp(I, R);
    Q := R / J;
    N := promote(liftUp(M, R), Q);
    QP := localRing(Q, N)
    )

LocalRing / Ideal := LocalRing => (RP, I) -> (
    if ring I =!= RP then localRingQuot I else localRingQuot promote(I, RP))


localQuot := opts -> (A, B) -> (
    RP := ring A;
    if ring B =!= RP then error "expected objects of the same ring";
    if B == 0 then return RP;
    --TODO if J \subset I, return RP

    R := RP;
    while class R === LocalRing do R = last R.baseRings;
    A' := liftUp(A, R);
    B' := liftUp(B, R);
    C' := quotient(A', B', opts);
    C := promote(C', RP)             -- Any options we need to care about?
    )

localQuotient = method(
    Options => {
        DegreeLimit => {},
        BasisElementLimit => infinity,
        PairLimit => infinity,
        MinimalGenerators => true,
        Strategy => Iterate
	})
localQuotient(Ideal , Ideal      ) := Ideal  => opts -> (I, J) -> (localQuot opts)(I, J)
localQuotient(Ideal , RingElement) := Ideal  => opts -> (I, f) -> (localQuot opts)(I, ideal(f))
--- ASK MIKE or DE how are these defined?
localQuotient(Module, Ideal      ) := Module => opts -> (M, I) -> (localQuot opts)(M, I)
localQuotient(Matrix, Ideal      ) := Module => opts -> (M, I) -> (localQuot opts)(M, I)
localQuotient(Module, RingElement) := Module => opts -> (M, f) -> (localQuot opts)(M, ideal(f))
localQuotient(Module, Module     ) := Ideal  => opts -> (M, N) -> (localQuot opts)(M, N)

TEST ///
  R =ZZ/32003[a,b,c,d,e,f]
  I =quotient(ideal"abc,abd,ade,def", ideal"abf")
  RP=localRing(R, ideal gens R)
  J =localQuotient(ideal"abc,abd,ade,def", ideal"abf")
  assert(promote(I, RP) === J)
  assert(I === liftUp(J, R))
///

TEST ///
--  restart
--  needsPackage "LocalRings"
  R =ZZ/32003[x,y,z,w]
  P =ideal"x,y,w"    -- z -> unit
  RP=localRing(R, P)
  QP=RP/ideal"x"     -- x -> 0
  
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


--TODO: localSaturate
localSat := opts -> (A, B) -> (
    RP := ring A;
    if ring B =!= RP then error "expected objects of the same ring";
    if B == 0 then return RP;

    R := RP;
    while class R === LocalRing do R = last R.baseRings;
    A' := liftUp(A, R);
    B' := liftUp(B, R);
    C' := saturate(A', B', opts);
    C := promote(C', RP)             -- Any options we need to care about?
    )

localSaturate = method(
    Options => {
        DegreeLimit => {},
        BasisElementLimit => infinity,
        PairLimit => infinity,
        MinimalGenerators => true,
        Strategy => null
        })
localSaturate Ideal                := Ideal  => opts ->  I     -> (localSat opts)(I, ideal vars ring I)
localSaturate(Ideal , Ideal      ) := Ideal  => opts -> (I, J) -> (localSat opts)(I, J)
localSaturate(Ideal , RingElement) := Ideal  => opts -> (I, f) -> (localSat opts)(I, ideal(f))
localSaturate Module               := Module => opts ->  M     -> (localSat opts)(M, ideal vars ring M)
--ASK MIKE or DE how are these defined?
localSaturate(Module, Ideal      ) := Module => opts -> (M, I) -> (localSat opts)(M, I)         
localSaturate(Module, RingElement) := Module => opts -> (M, f) -> (localSat opts)(M, ideal(f))


///--FIXME add some tests for localSaturate
  restart
  needsPackage "LocalRings"
  debug LocalRings

  R = ZZ/32003[a,b,c,d,e,f]
  quotient(ideal"abc,abd,ade,def", ideal"abf")
  RP =localRing(R, ideal"a,b,c,d,e,f")
///


localAnn := opts -> A -> (
    RP := ring A;
    --if A == 0 then return RP;
    --TODO if J \subset I, return RP

    R := RP;
    while class R === LocalRing do R = last R.baseRings;
    -- Does this theoretically work?
    A' := liftUp(A, R);
    B := annihilator(A', opts)
    )

localAnnihilator = method(
    Options => {
        Strategy => Intersection			    -- or Quotient
        })
localAnnihilator Module      := Ideal => opts -> M -> (localAnn opts) M
localAnnihilator Ideal       := Ideal => opts -> I -> (localAnn opts) module I
localAnnihilator RingElement := Ideal => opts -> f -> (localAnn opts) ideal f

///
  --FIXME adapt this example to the local case
  restart
  needsPackage "LocalRings"
  
  --As an example, we compute the annihilator of the canonical module of the rational quartic curve.
  R = QQ[a..d];
  J = monomialCurveIdeal(R,{1,3,4})
  M = Ext^2(R^1/J, R)
  annihilator M

  --For another example, we compute the annihilator of an element in a quotient ring
  A = R/(a*b,a*c,a*d)
  ann a
  --Macaulay2 uses two algorithms to compute annihilators.  The default version is to compute the annihilator of
  --each generator of the module TT "M", and to intersect these two by two.  Each annihilator is done using a 
  --submodule quotient.  The other algorithm computes the annihilator in one large computation and is used if
  -- TT "Strategy => Quotient", is specified.
  annihilator(M, Strategy=>Quotient)
///




--   FIXME: Do we need any of this? for localQuotient?
--     gensI := generators I;
--     gensgbI := generators gb gensI;
--     S := new QuotientRing from rawQuotientRing(raw R, raw gensgbI);
--     S#"raw creation log" = Bag { FunctionApplication {rawQuotientRing, (raw R, raw gensgbI)} };
--     S.cache = new CacheTable;
--     S.basering = R.basering;
--     S.FlatMonoid = R.FlatMonoid;
--     S.numallvars = R.numallvars;
--     S.ideal = I;
--     S.baseRings = append(R.baseRings,R);
--     commonEngineRingInitializations S;
--     S.relations = gensI;
--     S.isCommutative = R.isCommutative;
--     if R.?SkewCommutative then S.SkewCommutative = R.SkewCommutative;
--     S.generators = apply(generators S, m -> promote(m,S));
--     if R.?generatorSymbols then S.generatorSymbols = R.generatorSymbols;
--     if R.?generatorExpressions then S.generatorExpressions = (
--	  R.generatorExpressions
--	  -- apply(R.generatorExpressions,S.generators,(e,x) -> new Holder2 from {e#0,x})
--	  );
--     if R.?indexStrings then S.indexStrings = applyValues(R.indexStrings, x -> promote(x,S));
--     if R.?indexSymbols then S.indexSymbols = applyValues(R.indexSymbols, x -> promote(x,S));
--     expression S := lookup(expression,R);
--     S.use = x -> (
----	  try monoid S;
----	  if S.?monoid then (
----	       M := S.monoid;
----	       M + M := (m,n) -> S#1 * m + S#1 * n;
----	       M - M := (m,n) -> S#1 * m - S#1 * n;
----	       - M := m -> (- S#1) * m;
----	       scan(S.baseRings, A -> (
----		    A + M := (i,m) -> promote(i, S) + m;
----		    M + A := (m,i) -> m + promote(i, S);
----		    A - M := (i,m) -> promote(i, S) - m;
----		    M - A := (m,i) -> m - promote(i, S);
----		    A * M := (i,m) -> promote(i, S) * m;
----		    M * A := (m,i) -> m * promote(i, S);
----		    ));
----	       );
----	  );
--     runHooks(R,QuotientRingHook,S);
--     S)


--============================ Tests Sections ===================================--

UNTEST = method()
UNTEST String := (str) -> null

fixmatrix = (m) -> (R := ring m; map(R^(numrows m), R^(numcols m), m)) -- FIXME this is a hack to remove grading

TEST ///
  R = ZZ/32003[vars(0..3)]
  I = monomialCurveIdeal(R, {1, 3, 4})
  C = res I
  P = ideal"a,b,c";
  RP = localRing(R, P);
  IP = ideal ((gens I) ** RP)
  CP = res IP
  D = C ** RP
  DP = pruneComplex (D)
  assert(DP == CP)
///

TEST ///
  R = ZZ/32003[vars(0..3)]
  I = monomialCurveIdeal(R, {1, 3, 4})
  C = res I
  P = ideal"a,b,c";
  RP = localRing(R, P);

  F = transpose C.dd_3 ** RP;
  G = transpose C.dd_2 ** RP;
  F' = ker F
  
  debug LocalRings
  G' = fixmatrix(G)
  G'' = image G'
  F'/G''
  
  D = C ** RP
  E = pruneComplex D
  D.dd
  F = D.dd_2 ** RP  
///


TEST ///
  R = ZZ/32003[vars(0..5)]
  I = ideal"abc-def,ab2-cd2-c,-b3+acd";
  C = freeRes I
  C' = pruneComplex(C, UnitTest=>isScalar)
  P = ideal"a,b,c,d,e,f";
  RP = localRing(R, P);
  F = C.dd_2
  FP = F ** RP

  GP = syz FP
  assert(FP * GP == 0)

  use R
  G = F ** a                              -- TODO come up with better tests
  GP = FP ** a
  oldmodulo = modulo(F, G)
  newmodulo = modulo(FP, GP)
  -- FIXME
--  assert(mingens image (oldmodulo ** RP) - newmodulo == 0)  -- in reality only the images and number of generators should be the same
///

TEST ///
  R = ZZ/32003[vars(0..5)]
  I = ideal"abc-def,ab2-cd2-c,-b3+acd";
  C = freeRes I
  P = ideal"a,b,c,d,e,f";
  RP = localRing(R, P);
  F = C.dd_2 ** RP
  M = image F
  N = minimalPresentation M
--  assert(target N.cache.pruningMap === M)       -- Fails until map can handle LocalRings.
--  assert(source N.cache.pruningMap === N)  
  M = coker F
  N = minimalPresentation M
--  assert(target N.cache.pruningMap === M)
--  assert(source N.cache.pruningMap === N)
  M = RP^5
  N = minimalPresentation M
  assert(N.cache.pruningMap == 1)
///

TEST ///
  R = ZZ/32003[a..f]
  I = ideal"abc-def,ab2-cd2-c,-b3+acd";
  C = freeRes I
  P = ideal"a,b,c,d,e,f";
  RP = localRing(R, P);
  F = C.dd_2
  FP = C.dd_2 ** RP
  N = image F
  NP = image FP
    minimalPresentation N
    minimalPresentation NP
    mingens N
    mingens NP
  M = coker F
  MP = coker FP
    minimalPresentation M
    minimalPresentation MP
    mingens M
    mingens MP
  E = R^4
  EP = R^4 ** RP
    minimalPresentation E
    minimalPresentation EP
    mingens E
    mingens EP
///


TEST ///
  R = ZZ/32003[a..f]
  I = ideal"abc-def,ab2-cd2-c,-b3+acd";
  C = freeRes I
  P = ideal"a,b,c,d,e,f";
  RP = localRing(R, P);
  D = pruneComplex(C ** RP, UnitTest => isUnit)
  D.dd;

  F = C.dd_2 ** RP
  N = image F
    minimalPresentation N
    mingens N
  M = coker F
    minimalPresentation M
    mingens M
  E = R^4 ** RP
    minimalPresentation E
    mingens E

  use R
  setMaxIdeal ideal gens R
  localResolution coker C.dd_2                    -- FIXME something is fishy here.
///


TEST ///
  R = ZZ/32003[a..d]
  I = monomialCurveIdeal(R,{1,3,4})
  C = res I
  f = syz transpose C.dd_3
  g = transpose C.dd_2
  modulo(f,g)
  prune coker oo
  --TODO: finish this example
///

TEST ///
  S = ZZ/32003[r,s,t]
  R = ZZ/32003[a..e]
  phi = map(S,R,{r^3, s^3, t^3-1, r*s*t-r^2-s, s^2*t^2})
  I = ker phi
  assert not isHomogeneous I
 
  C = freeRes I 
  C = res I
  f = syz transpose C.dd_3;
  g = transpose C.dd_2;
  modulo(f,g);
  prune coker oo
///

TEST ///
  R = ZZ/32003[vars(0..5)]
  I = ideal"abc-def,ab2-cd2-c,-b3+acd";
  C = freeRes I
  P = ideal"a,b,c,d,e,f";
  RP = localRing(R, P);
  F = C.dd_2 ** RP

  M = image F
  (N, P) = newMinimalPresentation M
  assert(matrix P#0 - N.cache.pruningMap == 0) -- FIXME why is this zero?
  assert(M.cache.presentationComplex.dd_1 - relations N == 0)
  A = matrix P#0 * M.cache.presentationComplex.dd_1;
  B = syz liftUp generators M * matrix P#1;
  assert(A-B==0);
  A = liftUp matrix P#0
  B = liftUp N.cache.pruningMap * liftUp generators N  -- FIXME pruningMap isn't right
  assert(A-B==0);
  
  M = coker F
  (N, P) = newMinimalPresentation M
  assert(matrix P#0 - N.cache.pruningMap == 0) -- FIXME why is this zero?
  assert(M.cache.presentationComplex.dd_1 - relations N == 0)
  A = matrix P#0 * M.cache.presentationComplex.dd_1;
  B = relations M * matrix P#1;
  assert(A-B==0);
  A = liftUp matrix P#0
  B = liftUp N.cache.pruningMap * liftUp generators N -- FIXME pruningMap isn't right
  assert(A-B==0);
  A = syz liftUp relations M * matrix P#2
  B = matrix P#1 * matrix M.cache.presentationComplex.dd_2
  assert(A-B==0);
///



------------------------- localLength and hilbertSamuelPolynomial ---------------------------

TEST /// -- Intersection Theory: Geometric Multiplicity
R = ZZ/32003[x,y];
C = ideal"y-x2"; -- parabola
D = ideal"y-x";  -- line
E = ideal"y";    -- line

use R;
P = ideal"y-1,x-1";
RP = localRing(R, P);
assert(localLength (RP^1/promote(C+D, RP)) == 1)
assert(localLength (RP^1/promote(C+E, RP)) == 0)

use R;
P = ideal"x,y";  -- origin
RP = localRing(R, P);
assert(localLength(RP^1/promote(C+D, RP)) == 1)
assert(localLength(RP^1/promote(C+E, RP)) == 2)
///

TEST ///
R = ZZ/32003[x,y];
C = ideal"y-x3";
D = ideal"y-x2";
E = ideal"y";

use R;
P = ideal"x,y";
RP = localRing(R, P);
assert(localLength(RP^1/promote(C+D, RP)) == 2)
assert(localLength(RP^1/promote(C+E, RP)) == 3)

use R;
P = ideal"x-1,y-1";
RP = localRing(R, P);
assert(localLength(RP^1/promote(C+D, RP)) == 1)
assert(localLength(RP^1/promote(C+E, RP)) == 0)
///

TEST /// -- Hilbert-Samuel Function
R = ZZ/32003[x,y];
RP = localRing(R, ideal gens R);
N = RP^1
q = ideal"x2,y3"
assert({1,2,3,4,5,6} == for i from 0 to 5 list hilbertSamuelFunction(N, i)) -- n+1
assert({6,12,18,24,30,36} == for i from 0 to 5 list hilbertSamuelFunction(q, N, i)) -- 6(n+1)
///

TEST ///-- Computations in Algebraic Geometry with Macaulay2 pp 61
R = QQ[x,y,z];
RP = localRing(R, ideal gens R);
I = ideal"x5+y3+z3,x3+y5+z3,x3+y3+z5"
M = RP^1/I
assert(localLength(RP^1/I) == 27) -- 1.5 seconds
assert((for i from 0 to 6 list hilbertSamuelFunction(M, i))//sum == 27) -- 9.2 seconds
--elapsedTime assert((for i from 0 to 6 list hilbertSamuelFunction(promote(max ring M, ring M),M, i))//sum == 27) -- 11.1 seconds
///

TEST /// -- test from Mengyuan
    R = ZZ/32003[x,y,z,w]
    P = ideal "  yw-z2,   xw-yz,  xz-y2"
    I = ideal "z(yw-z2)-w(xw-yz), xz-y2"
    codim I == codim P    --Hence this is finite, thus I is artinian in R_P, i.e. RP/IP is an artinian ring.
    C = freeRes I
    radical I == P

    RP = localRing(R, P)
    IP = promote(I, RP)
    N = RP^1/IP
    M = promote(P, RP)
    
    -- FIXME ASKMIKE
    --assert(liftUp ideal (pruneComplex res (M*M*M)).dd_1 == liftUp ideal (pruneComplex res (M^3)).dd_1)
    
    assert(localLength(N) == 2)
    assert(localLength(N) == degree I / degree P)

    assert({1,1,0} == for i from 0 to 2 list hilbertSamuelFunction(N, i))
    assert({1,1,0} == for i from 0 to 2 list hilbertSamuelFunction(promote(max RP, RP), N, i))
    -- TODO: Find the polynomial from this
///




beginDocumentation()
load (currentFileDirectory | "LocalRings/doc.m2")

end--

///
  restart
--path = prepend("~/src/M2-local-rings/M2/Macaulay2/packages/", path)           -- Mike
  needsPackage "LocalRings"
  needsPackage "PruneComplex"
  debug LocalRings
  debug PruneComplex
  check LocalRings
  check PruneComplex

  uninstallPackage "LocalRings"
  restart
  installPackage "LocalRings"
  viewHelp "LocalRings"
///










------------------------------------- Testing under development ------------------------------------

UNTEST ///  
  --XXX
  kk = ZZ/32003
  R = kk[a..d]  
  R = kk[a..c]  
  matrix for i from 0 to 2 list for j from 0 to 2 list (random(2, R)+random(1,R))
  I = minors(2,oo)
  
  RP = localRing(R, ideal gens R)

  C0 = freeRes I
  C01 = pruneComplex C0
  C = res I
  C1 = pruneComplex C
  C2 = C1 ** RP
  D = pruneComplex C2
  M1 = liftUp(C2.dd_2, R)
  mingens image liftUp(C2.dd_2, R)

  IP = ideal((gens I) ** RP);
  elapsedTime m1 = mingens image gens IP; -- expensive?
  elapsedTime m2 = presentation minimalPresentation image m1; -- expensive: 18 seconds
  elapsedTime m3 = presentation minimalPresentation image m2;
  elapsedTime m4 = presentation minimalPresentation image m3;
  assert(m1 * m2 == 0)
  assert(m2 * m3 == 0)

  -- by hand compute Ext^1(RP/IP,RP) == 0  
  f1 = relations minimalPresentation image transpose D.dd_2
  f2 = transpose D.dd_1  
  f1 = map(target f2,,f1)
  M = subquotient(f1,f2)
  assert(minimalPresentation M == 0)
  assert(mingens M == 0)

  -- by hand compute Ext^2(RP/IP,RP) == 0  
  f1 = relations minimalPresentation image transpose D.dd_3
  f2 = transpose D.dd_2
  f1 = map(target f2,,f1)
  M = subquotient(f1,f2)
  assert(mingens M == 0)  -- very slow... modulo(f,g) doesn't work so well here.
  assert(minimalPresentation M == 0) -- also very slow.

  ker transpose C.dd_3 / image transpose C.dd_2
  prune oo -- very fast, compared to the above, why?

  -- now try LocalRings via setMaxIdeal
  setMaxIdeal ideal gens R  
  I = ideal I_*; -- clear out any cached values
  elapsedTime localResolution I -- 7.6 seconds
  elapsedTime m1 = localMingens gens I;
  elapsedTime m2 = localsyz m1;
  elapsedTime m3 = localsyz m2;
  elapsedTime m4 = localsyz m3; -- this is the one that takes almost all of the time

  -- now try Mora algorithm
  A = kk{a..d}
  IA = sub(I,A);
  m1 = gens gb IA;
  see ideal m1
  m1 = mingens gb IA;
  m2 = mingens gb syz m1; -- not good.  Is this easy to fix in engine code?
///

///
  needsPackage "SingularInterface"
  print toSingular ring I
  print toSingular I

  -- Singular code for the above example.
  -- Computes sres over dp, very quickly.
  ring R1 = 32003,(a,b,c,d),ls;
  ideal I1 = 
   -15013*a^4+4284*a^3*b+15166*a^2*b^2+11833*a*b^3-8028*b^4-6644*a^3*c-2411*a^2*b*c+6207*a*b^2*c+11377*b^3*c-8298*a^2*c^2-12608*a*b*c^2+451*b^2*c^2+6767*a*c^3-11974*b*c^3+562*c^4-102*a^3*d-12226*a^2*b*d-11077*a*b^2*d-5303*b^3*d-15892*a^2*c*d-7572*a*b*c*d+12138*b^2*c*d-7594*a*c^2*d+2479*b*c^2*d+2928*c^3*d+8290*a^2*d^2-7678*a*b*d^2-8442*b^2*d^2-9802*a*c*d^2-12261*b*c*d^2-25*c^2*d^2-4150*a*d^3-5129*b*d^3+12771*c*d^3+8333*d^4+1587*a^3+9527*a^2*b-13921*a*b^2-10277*b^3-13311*a^2*c+13113*a*b*c+3852*b^2*c+3037*a*c^2+3815*b*c^2-5731*c^3+5189*a^2*d-2813*a*b*d+13435*b^2*d-1122*a*c*d-11166*b*c*d-9320*c^2*d-8865*a*d^2+7584*b*d^2+2402*c*d^2-14866*d^3+15413*a^2+2050*a*b-2105*b^2+15177*a*c+14501*b*c+14413*c^2+13025*a*d-6632*b*d-13385*c*d-9440*d^2,
   5986*a^4+10598*a^3*b+13625*a^2*b^2+7289*a*b^3-14228*b^4-7783*a^3*c-10162*a^2*b*c+9815*a*b^2*c-6669*b^3*c+14610*a^2*c^2+8827*a*b*c^2-9124*b^2*c^2+1159*a*c^3-15594*b*c^3-10802*c^4+15844*a^3*d+10048*a^2*b*d-2657*a*b^2*d-12807*b^3*d+2001*a^2*c*d-2892*a*b*c*d-7426*b^2*c*d-8751*a*c^2*d-3843*b*c^2*d+13467*c^3*d-8998*a^2*d^2-11962*a*b*d^2+1400*b^2*d^2-3897*a*c*d^2+2246*b*c*d^2+1764*c^2*d^2+4594*a*d^3+7158*b*d^3+4455*c*d^3-789*d^4+8098*a^3+5534*a^2*b-1951*a*b^2+2558*b^3-13731*a^2*c-3872*a*b*c-12508*b^2*c-720*a*c^2-1306*b*c^2+9637*c^3-11649*a^2*d+7923*a*b*d+7051*b^2*d-5120*a*c*d+9253*b*c*d+1661*c^2*d+7967*a*d^2+1303*b*d^2+13891*c*d^2+15598*d^3-9617*a^2+7799*a*b+15278*b^2+14005*a*c-3518*b*c+6089*c^2+9564*a*d-13104*b*d-12975*c*d+2188*d^2,
   -12311*a^4+12314*a^3*b+6025*a^2*b^2-13088*a*b^3+6328*b^4+11704*a^3*c+15472*a^2*b*c+5458*a*b^2*c+1053*b^3*c+14662*a^2*c^2-2586*a*b*c^2+13612*b^2*c^2-12362*a*c^3+3760*b*c^3-6785*c^4+4186*a^3*d-7660*a^2*b*d+1285*a*b^2*d+3421*b^3*d-2475*a^2*c*d-255*a*b*c*d-1740*b^2*c*d-2177*a*c^2*d-609*b*c^2*d-13232*c^3*d-3225*a^2*d^2+2293*a*b*d^2-49*b^2*d^2-8569*a*c*d^2+15710*b*c*d^2+11951*c^2*d^2+14616*a*d^3-2293*b*d^3-694*c*d^3+1726*d^4+11583*a^3-839*a^2*b-1448*a*b^2+9596*b^3+4866*a^2*c+9119*a*b*c-2142*b^2*c-8952*a*c^2+827*b*c^2-10481*c^3+6414*a^2*d+15649*a*b*d+4364*b^2*d+10139*a*c*d-5773*b*c*d+9832*c^2*d-7387*a*d^2-875*b*d^2-5314*c*d^2+4273*d^3-15657*a^2-6155*a*b-2688*b^2+2595*a*c+13183*b*c+7997*c^2+6618*a*d+751*b*d+10466*c*d+6348*d^2,
   -139*a^4+2755*a^3*b-2351*a^2*b^2+8951*a*b^3+9388*b^4-4944*a^3*c-2098*a^2*b*c-3557*a*b^2*c+2176*b^3*c-13643*a^2*c^2+4791*a*b*c^2+13307*b^2*c^2+15993*a*c^3-2578*b*c^3-3944*c^4+14226*a^3*d+15045*a^2*b*d-4574*a*b^2*d-8155*b^3*d+14788*a^2*c*d+7712*a*b*c*d-7380*b^2*c*d-329*a*c^2*d-5937*b*c^2*d-3657*c^3*d-2894*a^2*d^2+4933*a*b*d^2-1752*b^2*d^2+15951*a*c*d^2+14095*b*c*d^2+11815*c^2*d^2+14629*a*d^3-15675*b*d^3+14521*c*d^3-10610*d^4+13952*a^3+4908*a^2*b+14431*a*b^2-1161*b^3+4282*a^2*c+12468*a*b*c+1219*b^2*c+1928*a*c^2-14531*b*c^2-12354*c^3+12065*a^2*d+6949*a*b*d+12558*b^2*d-9696*a*c*d-4505*b*c*d-15550*c^2*d+13030*a*d^2+3541*b*d^2-1822*c*d^2+14145*d^3+2176*a^2-6182*a*b-15427*b^2-2082*a*c-341*b*c+11146*c^2-4602*a*d+4052*b*d+11895*c*d+1381*d^2,
   -4259*a^4-4863*a^3*b+4833*a^2*b^2+9184*a*b^3-2811*b^4+5992*a^3*c+170*a^2*b*c-2800*a*b^2*c-6237*b^3*c+12813*a^2*c^2-3239*a*b*c^2-6517*b^2*c^2-734*a*c^3+7175*b*c^3+1409*c^4-7075*a^3*d+14298*a^2*b*d+5961*a*b^2*d+13922*b^3*d-4157*a^2*c*d+5316*a*b*c*d+8499*b^2*c*d+9142*a*c^2*d+2191*b*c^2*d-13629*c^3*d+61*a^2*d^2+806*a*b*d^2+7128*b^2*d^2-15303*a*c*d^2-15172*b*c*d^2-4416*c^2*d^2-6644*a*d^3+4460*b*d^3+9879*c*d^3-15402*d^4-385*a^3+13802*a^2*b-12490*a*b^2+14131*b^3+7531*a^2*c-11234*a*b*c-2004*b^2*c-13151*a*c^2+14686*b*c^2-2348*c^3+14362*a^2*d-2747*a*b*d+7079*b^2*d+7761*a*c*d-2755*b*c*d+7630*c^2*d+4762*a*d^2-4275*b*d^2+7493*c*d^2+11546*d^3+457*a^2-15678*a*b+14517*b^2+12751*a*c+350*b*c-14832*c^2-8206*a*d-2027*b*d+7305*c*d-124*d^2,
   5530*a^4+14528*a^3*b+7452*a^2*b^2+14209*a*b^3+6672*b^4+8260*a^3*c-2514*a^2*b*c-15006*a*b^2*c-5574*b^3*c+5562*a^2*c^2-5905*a*b*c^2+8922*b^2*c^2-13258*a*c^3-2209*b*c^3-2657*c^4+2828*a^3*d+6792*a^2*b*d+8730*a*b^2*d-9243*b^3*d+6238*a^2*c*d+12003*a*b*c*d-4701*b^2*c*d+3852*a*c^2*d-4792*b*c^2*d-3155*c^3*d+8734*a^2*d^2-933*a*b*d^2+11798*b^2*d^2-14339*a*c*d^2-13037*b*c*d^2+5019*c^2*d^2-5016*a*d^3-4866*b*d^3+7404*c*d^3+7775*d^4-9696*a^3-8456*a^2*b-1733*a*b^2-228*b^3-5975*a^2*c-6093*a*b*c-10815*b^2*c-15469*a*c^2-159*b*c^2+7680*c^3-6825*a^2*d+15263*a*b*d+5697*b^2*d+5922*a*c*d-12809*b*c*d+15879*c^2*d-605*a*d^2+10582*b*d^2-2397*c*d^2+2321*d^3+11055*a^2-1057*a*b+14165*b^2-8225*a*c-13790*b*c-6275*c^2+184*a*d+15178*b*d+15672*c*d+109*d^2,
   6460*a^4-10759*a^3*b-15393*a^2*b^2-4212*a*b^3+4033*b^4+859*a^3*c+13541*a^2*b*c+10032*a*b^2*c+2266*b^3*c+1224*a^2*c^2+13106*a*b*c^2+11500*b^2*c^2-11779*a*c^3-12672*b*c^3-283*c^4-7289*a^3*d-15496*a^2*b*d-3076*a*b^2*d-12900*b^3*d+684*a^2*c*d+1182*a*b*c*d-8806*b^2*c*d-13566*a*c^2*d+6478*b*c^2*d-10445*c^3*d-15333*a^2*d^2+6735*a*b*d^2-1015*b^2*d^2-12649*a*c*d^2+3587*b*c*d^2-2466*c^2*d^2-12985*a*d^3-1933*b*d^3-4919*c*d^3-4361*d^4-12067*a^3-11183*a^2*b-6916*a*b^2-2844*b^3-4629*a^2*c+10887*a*b*c-588*b^2*c-6859*a*c^2+9644*b*c^2+9725*c^3-1338*a^2*d+13218*a*b*d+3124*b^2*d-8631*a*c*d-6316*b*c*d-13125*c^2*d+2590*a*d^2+8466*b*d^2+15431*c*d^2-279*d^3-11609*a^2-786*a*b-14970*b^2-15734*a*c+15621*b*c-7119*c^2+9192*a*d+7235*b*d+1816*c*d+8576*d^2,
   12010*a^4-5395*a^3*b-5712*a^2*b^2-11141*a*b^3-3135*b^4+1502*a^3*c+11458*a^2*b*c+6910*a*b^2*c+13339*b^3*c-9769*a^2*c^2+6845*a*b*c^2+3372*b^2*c^2-13016*a*c^3+13163*b*c^3+7867*c^4-2362*a^3*d+5986*a^2*b*d+15333*a*b^2*d-14530*b^3*d+33*a^2*c*d-13205*a*b*c*d-5998*b^2*c*d-5503*a*c^2*d-7031*b*c^2*d+576*c^3*d+12274*a^2*d^2+8918*a*b*d^2+2690*b^2*d^2+12336*a*c*d^2+5123*b*c*d^2-741*c^2*d^2+12201*a*d^3+5160*b*d^3-267*c*d^3+3280*d^4+7136*a^3+13875*a^2*b-10280*a*b^2-2430*b^3+13180*a^2*c+1742*a*b*c+3522*b^2*c-2988*a*c^2+14973*b*c^2+9227*c^3-3827*a^2*d-14884*a*b*d-6412*b^2*d-3906*a*c*d-15487*b*c*d+1019*c^2*d-8448*a*d^2+12740*b*d^2+15104*c*d^2-9530*d^3-12307*a^2+7258*a*b+7762*b^2+6445*a*c+2926*b*c-4131*c^2-7205*a*d-7270*b*d-13882*c*d+2528*d^2,
   -9300*a^4-3611*a^3*b-4932*a^2*b^2+7100*a*b^3-3999*b^4+7779*a^3*c+8978*a^2*b*c+10915*a*b^2*c+13211*b^3*c+4644*a^2*c^2-8639*a*b*c^2+5978*b^2*c^2-2458*a*c^3+12240*b*c^3+6028*c^4-15046*a^3*d-11101*a^2*b*d+14807*a*b^2*d+13699*b^3*d-8257*a^2*c*d-1745*a*b*c*d-11510*b^2*c*d-15099*a*c^2*d+14413*b*c^2*d+10273*c^3*d-8155*a^2*d^2+3061*a*b*d^2+2550*b^2*d^2+14282*a*c*d^2+2043*b*c*d^2-4334*c^2*d^2+7995*a*d^3+1253*b*d^3-7378*c*d^3-6732*d^4-13168*a^3-13819*a^2*b-941*a*b^2+3675*b^3-3021*a^2*c+5044*a*b*c+14898*b^2*c+1963*a*c^2+4764*b*c^2-3850*c^3+14720*a^2*d-6942*a*b*d-15024*b^2*d-8435*a*c*d-5470*b*c*d-5344*c^2*d+2177*a*d^2+15216*b*d^2+14832*c*d^2+11709*d^3-7969*a^2+6488*a*b+15474*b^2-769*a*c+15923*b*c-5524*c^2+9353*a*d+3998*b*d+10181*c*d+7094*d^2;
ideal J = std(I1);
resolution rI = sres(J,0); // didn't finish quickly
resolution minI = minres(rI);
minI;
print(list(minI));
///

UNTEST ///
-- this one doesn't even parse correctly?
  --XXX
  kk = ZZ/32003
  S = kk[t]
  R = kk[a..d]  
  phi = map(S,R,{t^2-t^4, t-t^7, t^2+2*t^5, t^8})
  phi = map(S,R,{t^2-t^4, t^3-t^4-t^7, t^5+2*t^6, t^8})
  
  RP = localRing(R, ideal gens R)

  C = freeRes I
  C1 = pruneComplex C
  C2 = C1 ** RP
  pruneComplex C2

  IP = ideal((gens I) ** RP)
  elapsedTime m1 = mingens image gens IP
  elapsedTime m2 = presentation minimalPresentation image m1  
  elapsedTime m3 = presentation minimalPresentation image m2
  assert(m1 * m2 == 0)
  assert(m2 * m3 == 0)
///

---------------------------------
-- Tests of minimalPresentation, mingens --
---------------------------------
UNTEST ///
  --XXX
  kk = ZZ/32003
  S = kk[s,t]
  R = kk[a..d]  
  phi = map(S,R,{s^2*t-t, s*t-s, t^3-s*t^2-s, s^2*t^2})
  I = ker phi

  RP = localRing(R, ideal gens R)

  C = freeRes I
  C1 = pruneComplex C
  C2 = C1 ** RP
  pruneComplex C2

  IP = ideal((gens I) ** RP)
  elapsedTime m1 = mingens image gens IP
  elapsedTime m2 = presentation minimalPresentation image m1   -- ERROR
  assert(m1 * m2 == 0)

  units = findAllUnits(mutableMatrix C2.dd_3)
  hashTable for u in units list u => C2.dd_3_u
  
  C = res I
  C = C ** RP
  pruneComplex C

  -- test of free modules
  F = RP^5
  assert(minimalPresentation F === F)
  assert(mingens F == id_F)

  -- test of submodules of R
  f = gens IP
  mingens image f
  minimalPresentation image f

  -- test of cokernel modules
  M = coker f
  mingens M
  assert(presentation minimalPresentation M ==   mingens image f)
///

UNTEST ///
  --XXX
  R = ZZ/32003[a..d]
  F = (a^3-b*c*d-3*b*c)^3-a^3-b^2-(b+d)^4-c^5
  I = ideal jacobian matrix{{F}}
  setMaxIdeal ideal gens R
  localResolution I -- nope...
  
  RP = localRing(R, ideal gens R)
  IP = ideal((gens I) ** RP)

  C = freeRes I
  C1 = pruneComplex C
  C2 = C1 ** RP
  units = findAllUnits(mutableMatrix C2.dd_3)
  hashTable for u in units list u => C2.dd_3_u
  
  C = res I
  C = C ** RP
  pruneComplex C

  -- test of free modules
  F = RP^5
  assert(minimalPresentation F === F)
  assert(mingens F == id_F)

  -- test of submodules of R
  f = gens IP
  mingens image f
  minimalPresentation image f

  -- test of cokernel modules
  M = coker f
  mingens M
  assert(presentation minimalPresentation M ==   mingens image f)

  -- 
  C = freeRes coker f
  D = pruneComplex C
  
  Rloc = (ZZ/32003){a,b,c,d}
  Iloc = sub(I, Rloc)
  m1 = mingens Iloc
  m2 = syz m1
  gens gb Iloc

  use R
  I2 = ideal"
  d3+3bd2+3b2d+b3-7994b3c3-15997b3c3d-8000b3c3d2+15997a3b2c2+16000a3b2c2d-8000a6bc,
  c4-12785b3c2-12785b3c2d+6406b3c2d2-6400b3c2d3-12812a3b2c+12794a3b2cd+12800a3b2cd2+12803a6b-6400a6bd,
  b-15961b2c3-15961b2c3d-15988b2c3d2-16000b2c3d3+15988b3c3-9b3c3d+16000b3c3d2-27a3bc2-18a3bc2d-3a3bc2d2+9a3b2c2+3a3b2c2d-15997a6c-16000a6cd+16000a6bc,
  a2
  "
  C3 = freeRes I2
  C4 = pruneComplex C3
  D = C4 ** RP
  pruneComplex D
///

/// singular code for the above example, this gives a resolution with no real effort.
  // This is at least partly because the initial ideal in the local ring is (b, a^2, d^3, c^4).  So the Schreyer frame gives a minimal resolution
  ring R1 = 32003,(a,b,c,d),ls;  
  ideal I1 = 9*a^8-18*a^5*b*c*d+9*a^2*b^2*c^2*d^2-54*a^5*b*c+54*a^2*b^2*c^2*d+81*a^2*b^2*c^2-3*a^2,
         -3*a^6*c*d+6*a^3*b*c^2*d^2-3*b^2*c^3*d^3-9*a^6*c+36*a^3*b*c^2*d-27*b^2*c^3*d^2+54*a^3*b*c^2-81*b^2*c^3*d-81*b^2*c^3-4*b^3-12*b^2*d-12*b*d^2-4*d^3-2*b,
         -3*a^6*b*d+6*a^3*b^2*c*d^2-3*b^3*c^2*d^3-9*a^6*b+36*a^3*b^2*c*d-27*b^3*c^2*d^2+54*a^3*b^2*c-81*b^3*c^2*d-81*b^3*c^2-5*c^4,
         -3*a^6*b*c+6*a^3*b^2*c^2*d-3*b^3*c^3*d^2+18*a^3*b^2*c^2-18*b^3*c^3*d-27*b^3*c^3-4*b^3-12*b^2*d-12*b*d^2-4*d^3;  
  ideal J = std(I1);
  resolution rI = sres(J,0);
  resolution minI = minres(rI);
  print(list(minI));
///
