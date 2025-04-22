---------------------------------------------------------------------------
-- PURPOSE : to compute direct summands of modules and coherent sheaves
--
-- UPDATE HISTORY : created Oct 2023
--
-- TODO :
-- 1. implement over Dmodules and other non-commutative rings
-- 2. implement diagonalize for matrices (and later, complexes)
-- 3. find a way to restrict and pass End to the summands
-- 4. make summands work over ZZ (currently rank fails)
-- 5. compute a general endomorphism without computing End
-- 6. speed up computing eigenvalues over ZZ/p and GF(q)
-- 7. once an indecomposable summand is found, call summands(N, M)
---------------------------------------------------------------------------
newPackage(
    "DirectSummands",
    Version => "0.3",
    Date => "April 20th 2025",
    Headline => "decompositions of modules and coherent sheaves",
    Authors => {
	{ Name => "Devlin Mallory", Email => "malloryd@math.utah.edu", HomePage => "https://math.utah.edu/~malloryd/"},
	{ Name => "Mahrud Sayrafi", Email => "mahrud@umn.edu",         HomePage => "https://math.umn.edu/~mahrud/"}
	},
    Keywords => { "Commutative Algebra" },
    PackageImports => {
	"Isomorphism",     -- for isIsomorphic
	"Polyhedra",       -- for coneFromVData and coneComp
	"PushForward",     -- only for frobenius.m2
	"RationalPoints2", -- for rationalPoints in findIdempotent
	"Truncations",     -- for effGenerators
	"Varieties",
	},
    AuxiliaryFiles => true,
    DebuggingMode => false
    )

export {
    -- methods
    "isIndecomposable",
    "directSummands", "summands" => "directSummands",
    "findIdempotent", "findIdem" => "findIdempotent",
    "findProjectors",
    "findSplitInclusion",
    "generalEndomorphism",
    "isomorphismTally",
    "tallySummands",
    "isomorphism",
    -- symbols
    "Tries",
    -- frobenius methods
    "frobeniusMap",
    "frobeniusRing",
    "frobeniusPullback",
    "frobeniusPushforward",
    "frobeniusTwist",
    "potentialExtension",
    "changeBaseField"
    }

importFrom_Core {
    "raw", "rawReshape",
    "rawNumberOfColumns",
    "rawNumberOfRows",
    "tryHooks",
    "sortBy",
    }

-----------------------------------------------------------------------------
-* Code section *-
-----------------------------------------------------------------------------

-- defined here and used in idempotents.m2 and homogeneous.m2
DirectSummandsOptions = new OptionTable from {
    Limit             => null, -- used in directSummands(Module, Module)
    Strategy          => 7,    -- Strategy is a bitwise sum of the following:
    -- 1  => use degrees of generators as heuristic to peel off line bundles first
    -- 2  => check generators of End_0 as heuristic for finding idempotents
    -- 4  => whether we are splitting coherent sheaves, so skip line bundles
    -- 8  => precompute Homs before looking for idempotents
    -- 16 => use summandsFromIdempotents even in graded case
    Tries             => null, -- see defaultNumTries below
    Verbose           => false -- whether to print extra debugging info
}

-- the default number of attempts for randomized algorithms
-- e.g. 145 tries in char 2, 10 in char 32003, and 1 in char 0
defaultNumTries = p -> ceiling(0.1 + 100 / log p)
--apply({2, 32003, 0}, defaultNumTries)

-- helpers for computing Frobenius pushforwards of modules and sheaves
-- TODO: move to PushForward package?
load "./DirectSummands/frobenius.m2"
-- helpers for finding random idempotents of a module for the local case
load "./DirectSummands/idempotents.m2"
-- helpers for finding random projectors of a module for the graded case
load "./DirectSummands/homogeneous.m2"

-----------------------------------------------------------------------------
-- Things to move to the Core
-----------------------------------------------------------------------------

-- return the submatrix with given degrees of target and source
submatrixByDegrees(Matrix, Sequence) := (m, degs) -> (
    (tar, src) := degs;
    col := if src =!= null then positions(degrees source m, deg -> member(deg, src));
    row := if tar =!= null then positions(degrees target m, deg -> member(deg, tar));
    submatrix(m, row, col))

-- this defines sorting on modules and sheaves
CoherentSheaf ? CoherentSheaf :=
Module ? Module := (M, N) -> if rank M != rank N then rank M ? rank N else degrees M ? degrees N

position(ZZ, Function) := o -> (n, f) -> position(0..n-1, f)
-- TODO: this is different from position(List,List,Function)
position' = method()
position'(VisibleList, VisibleList, Function) := (B, C, f) -> for b in B do for c in C do if f(b, c) then return (b, c)
position'(ZZ,          ZZ,          Function) := (B, C, f) -> position'(0..B-1, 0..C-1, f)

char SheafOfRings := O -> char variety O

GF'ZZ'ZZ = memoize (lookup(GF, ZZ, ZZ)) (options GF)
GF'Ring  = memoize (lookup(GF, Ring))   (options GF)
GF(ZZ, ZZ) := GaloisField => opts -> GF'ZZ'ZZ
GF(Ring)   := GaloisField => opts -> GF'Ring

-- TODO: what generality should this be in?
-- WANT:
--   R ** ZZ/101 to change characteristic
--   R ** S to change coefficient ring
-- TODO: can you change the ground field but keep the tower structure?
QuotientRing   ** GaloisField :=
PolynomialRing ** GaloisField := (R, L) -> (
    -- TODO: in general we may want to keep part of the ring tower
    A := first flattenRing(R, CoefficientRing => null);
    quotient sub(ideal A, L monoid A))

changeBaseField = method()
-- TODO: add more automated methods, e.g. where minors of the presentation factor
changeBaseField(ZZ, Module) :=
changeBaseField(ZZ, CoherentSheaf)   := (e, M) -> changeBaseField(GF(char ring M, e), M)
changeBaseField(GaloisField, Module) := (L, M) -> M.cache#(symbol changeBaseField, L) ??= (
    S := first flattenRing(ring M, CoefficientRing => null);
    K := coefficientRing S;
    if class K =!= GaloisField then return (
        R0 := quotient sub(ideal S, L monoid S);
	M' := directSum apply(cachedSummands M,
	    N -> coker sub(presentation N, R0));
        M.cache#(symbol changeBaseField, L) = M');
    -- don't needlessly create new rings:
    if K.order === L.order then return M;
    i0 := map(L, K);
    LS := L(monoid S);
    i1 := map(LS, ring ideal S, gens LS | { i0 K_0 });
    R := quotient i1 ideal S;
    i := map(R, S, i1);
    directSum apply(cachedSummands M, N -> i ** N))

-- TODO: come up with a better way to extend ground field of a variety
-- TODO: does this also need to be used for frobenius pushforward?
sheaf' = (X, M) -> try sheaf(X, M) else (
    if instance(X, ProjectiveVariety) then sheaf(Proj ring M, M) else
    if instance(X,     AffineVariety) then sheaf(Spec ring M, M) else
    error "extension of the coefficient field of the base variety is not implemented")

changeBaseField(GaloisField, CoherentSheaf) := (L, F) -> sheaf'(variety F, changeBaseField(L, module F))

nonzero = x -> select(x, i -> i != 0)
nonnull = x -> select(x, i -> i =!= null)

checkRecursionDepth = () -> if recursionDepth() > recursionLimit - 20 then printerr(
    "Warning: the recursion depth limit may need to be extended; use `recursionLimit = N`")

-----------------------------------------------------------------------------
-- things to move to Isomorphism package
-----------------------------------------------------------------------------

module Module := identity

-- give an isomorphism between two free modules with same degrees
-- FIXME: because of https://github.com/Macaulay2/M2/issues/3719,
-- this might not give the most "natural" isomorphism
isisofree = o -> (M, N0) -> (
    (d1, d2) := (degrees M, degrees N0);
    if #d1 =!= #d2 then return (false, null);
    if o.Strict then N := N0 else d2 = degrees(
	N = N0 ** (ring N0)^{min d2 - min d1});
    if sort d1 != sort d2 then return (false, null);
    p1 := first \ (sortBy last) toList pairs d1;
    p2 := first \ (sortBy last) toList pairs d2;
    (true, map(M, N0, id_N^p2 // id_M^p1)))

isiso = lookup(isIsomorphic, Module, Module)
isIsomorphic(Module, Module) := Sequence => o -> (M, N) -> (
    if isFreeModule M and isFreeModule N
    then (isisofree o)(M, N)
    else (isiso o)(M, N))

protect Isomorphisms
isIsomorphic' = method(Options => options isIsomorphic ++ { Tries => null })
isIsomorphic'(Module, Module) := opts -> (M, N) -> (
    M.cache.Isomorphisms ??= new MutableHashTable;
    if M.cache.Isomorphisms#?N then return true;
    tries := opts.Tries ?? defaultNumTries char ring M;
    opts' := selectKeys(opts, k -> (options isIsomorphic)#?k);
    -- TODO: parallelize
    for c to tries - 1 do (
	(bool, isom) := isIsomorphic(M, N, opts');
	if bool then ( M.cache.Isomorphisms#N = isom; return true ));
    false)

isomorphism = method(Options => options isIsomorphic ++ { Tries => null })
isomorphism(Module, Module) := Matrix => o -> (M, N) -> (
    if not isIsomorphic'(M, N, o) then error "modules not isomorphic";
    M.cache.Isomorphisms#N)

-- TODO: speed this up
-- TODO: implement isIsomorphic for sheaves
-- TODO: add strict option
tallySummands = L -> tally (
    opts := Homogeneous => all(L, isHomogeneous);
    L  = new MutableList from module \ L;
    b := new MutableList from #L : true;
    for i to #L-2 do if b#i then for j from i+1 to #L-1 do if b#j then (
	if isIsomorphic'(L#i, L#j, opts)
	then ( b#j = false; L#j = L#i ));
    new List from L)

isomorphismTally = method()
isomorphismTally List := L -> (
    if not uniform L then error "expected list of elements of the same type";
    if not (class L_0 === Module or class L_0 === CoherentSheaf ) then error "expected list of modules or sheaves";
    opts := Homogeneous => all(L, isHomogeneous);
    --L = new MutableList from L;
    j := 0;
    while j < #L list (
	i := j + 1;
	c := 1;
	while i < #L do (
	    if isIsomorphic'(L#j, L#i, opts)
	    then (
		L = drop(L, {i, i});
		c = c + 1)
	    else i = i + 1);
	j = j + 1;
	(L#(j-1), c)))

-----------------------------------------------------------------------------
-- methods for finding general endomorphisms of degree zero
-----------------------------------------------------------------------------

importFrom_Truncations { "effGenerators" }

coneComp = (C, u, v) -> (
    --if u == v                            then symbol== else
    if contains(C, matrix vector(v - u)) then symbol <= else
    if contains(C, matrix vector(u - v)) then symbol > else incomparable)

-- TODO: add this as a strategy to basis
smartBasis = (deg, M) -> (
    -- TODO: try splitting coker {{a, b^3}, {-b^3, a}} over ZZ/32003[a..b]/(a^2+b^6)
    if M == 0 then return map(M, 0, 0);
    if instance(deg, ZZ) then deg = {deg};
    degs := if #deg == 1 then select(unique degrees M, d -> d <= deg) else (
	-- FIXME: in the multigraded case sometimes just using basis is faster:
	return basis(deg, M);
        -- in the multigraded case, coneMin and coneComp can be slow
	-- but for sufficiently large modules they are still an improvement
        -- TODO: make them faster
        C := coneFromVData effGenerators ring M;
        --elapsedTime compMin(C, unique degrees M) -- TODO: this is not the right thing
        select(unique degrees M, d -> coneComp(C, d, deg) == symbol <=));
    if degs === {deg} then return map(M, , submatrixByDegrees(gens cover M, (, degs)));
    M' := subquotient(ambient M,
	if M.?generators then submatrixByDegrees(gens M, (, degs)),
	if M.?relations  then relations M);
    M'.cache.homomorphism = M.cache.homomorphism;
    basis(deg, M')) -- caching this globally causes issues!

-- matrix of (degree zero) generators of End M
-- TODO: rename this
-- also see gensHom0
gensEnd0 = M -> M.cache#"End0" ??= (
    -- TODO: need to pass options from Hom + choose the coefficient field
    zdeg := if isHomogeneous M then degree 0_M;
    if 0 < debugLevel then stderr << " -- computing End module ... " << flush;
    A := Hom(M, M,
	DegreeLimit       => zdeg,
	MinimalGenerators => false);
    if 0 < debugLevel then stderr << "done!" << endl;
    if isHomogeneous M
    then smartBasis(zdeg, A)
    else inducedMap(A, , gens A))

-- give a random vector in a module over a local ring
localRandom = (M, opts) -> (
    R := ring M;
    -- TODO: which coefficient ring do we want?
    K := try coefficientRing R else R;
    v := random(cover M ** K, module K, opts);
    -- TODO: sub should be unnecessary, but see https://github.com/Macaulay2/M2/issues/3638
    vector inducedMap(M, , generators M * sub(v, R)))

random(ZZ,   Module) :=
random(List, Module) := Vector => o -> (d, M) -> vector map(M, , random(cover M, (ring M)^{-d}, o))
random       Module  := Vector => o ->     M  -> (
    if isHomogeneous M then random(degree 1_(ring M), M, o) else localRandom(M, o))

generalEndomorphism = method(Options => options random)
generalEndomorphism Module := Matrix => o -> M -> (
    B := gensEnd0 M;
    r := if isHomogeneous M
    then random(source B, o)
    else localRandom(source B, o);
    homomorphism(B * r))
-- the sheaf needs to be pruned to prevent missing endomorphisms
generalEndomorphism CoherentSheaf := SheafMap => o -> F -> (
    sheaf generalEndomorphism(module prune F, o))

-- overwrite two existing hooks, to be updated in Core
addHook((quotient, Matrix, Matrix), Strategy => Default,
    -- Note: this strategy only works if the remainder is zero, i.e.:
    -- homomorphism' f % image Hom(source f, g) == 0
    (opts, f, g) -> (
	opts = new OptionTable from {
	    DegreeLimit       => opts.DegreeLimit,
	    MinimalGenerators => opts.MinimalGenerators };
	map(source g, source f, homomorphism(homomorphism'(f, opts) // Hom(source f, g, opts)))))

addHook((quotient', Matrix, Matrix), Strategy => Default,
    -- Note: this strategy only works if the remainder is zero, i.e.:
    -- homomorphism' f % image Hom(g, target f) == 0
    (opts, f, g) -> (
	opts = new OptionTable from {
	    DegreeLimit       => opts.DegreeLimit,
	    MinimalGenerators => opts.MinimalGenerators };
	map(target f, target g, homomorphism(homomorphism'(f, opts) // Hom(g, target f, opts)))))

importFrom_Core {"Hooks", "HookPriority"}
Matrix.Hooks#(quotient,  Matrix, Matrix).HookPriority = drop(Matrix.Hooks#(quotient,  Matrix, Matrix).HookPriority, -1)
Matrix.Hooks#(quotient', Matrix, Matrix).HookPriority = drop(Matrix.Hooks#(quotient', Matrix, Matrix).HookPriority, -1)

-- left inverse of a split injection
-- TODO: figure out if we can ever do this without computing End source g
leftInverse = inverse' = method(Options => options Hom)
leftInverse Matrix := opts -> g -> g.cache.leftInverse ??= quotient'(id_(source g), g, opts)

-- right inverse of a split surjection
-- FIXME: inverse may fail for a general split surjection:
-- c.f. https://github.com/Macaulay2/M2/issues/3738
-- TODO: figure out if we can ever do this without computing End target g
rightInverse = method(Options => options Hom)
rightInverse Matrix := opts -> g -> g.cache.rightInverse ??= quotient(id_(target g), g, opts)

-- given N and a split injection inc:N -> M,
-- we use precomputed endomorphisms of M
-- to produce a general endomorphism of N
generalEndomorphism(Matrix, Nothing, Matrix) := Matrix => o -> (N, null, inc) -> (
    -- assert(N === source inc);
    inv := leftInverse(inc,
	DegreeLimit => degree 0_N,
	-- FIXME: setting this to false sometimes
	-- produces non-well-defined inverses
	MinimalGenerators => true);
    inc * generalEndomorphism(target inc, o) * inv)

-- given N and a split surjection pr:M -> N,
-- we use precomputed endomorphisms of M
-- to produce a general endomorphism of N
generalEndomorphism(Module, Matrix) := Matrix => o -> (N, pr) -> (
    -- assert(N === target pr);
    -- TODO: currently this still computes End_0(N)
    -- figure out a way to compute the inverse without doing so
    inv := rightInverse(pr,
	DegreeLimit => degree 0_N,
	-- FIXME: setting this to false sometimes
	-- produces non-well-defined inverses
	MinimalGenerators => true);
    pr * generalEndomorphism(source pr, o) * inv)

generalEndomorphism(Module, Matrix, Nothing) := Matrix => o -> (N, pr, null) -> generalEndomorphism(N, pr, o)
generalEndomorphism(Module, Matrix, Matrix)  := Matrix => o -> (N, pr, inc) -> (
    -- assert(N === target pr and source pr === target inc and N === source inc);
    pr * generalEndomorphism(source pr, o) * inc)

-----------------------------------------------------------------------------
-- helpers for splitting and caching projection maps
-----------------------------------------------------------------------------

findSplitInclusion = method(Options => { Tries => 50 })
--tests if M is a split summand of N
findSplitInclusion(Module, Module) := opts -> (M, N) -> (
    h := for i to opts.Tries - 1 do (
        b := homomorphism random Hom(M, N, MinimalGenerators => false);
        c := homomorphism random Hom(N, M, MinimalGenerators => false);
        if isIsomorphism(c * b) then break b);
    if h === null then return "not known" else return h)

-- helper for splitting a free module and setting the split surjections
splitFreeModule = (M, opts) -> apply(numgens M, i -> target(M.cache#(symbol ^, [i]) = M^{i}))

-- helper for splitting free summands by observing the degrees of generators
splitFreeSummands = (M, opts) -> M.cache#"FreeSummands" ??= (
    directSummands(R := ring M; apply(-unique degrees M, d -> R^{d}), M, opts))

-- helper for splitting a module with known components
-- (in particular, the components may also have summands)
-- TODO: can we sort the summands here?
splitComponents = (M, comps, splitfunc) -> (
    n := #comps;
    c := -1; -- summand counter
    projs := apply(n, i -> M^[i]);
    flatten apply(comps, projs, (N, p) -> (
	L := splitfunc N;
	-- Projection maps to the summands
	if #L > 1 then apply(#L, i ->
	    M.cache#(symbol ^, [c += 1]) = N^[i] * p)
	else M.cache#(symbol ^, [c += 1]) = p;
	-- Inclusion maps are computed on-demand
	L)))

-- these are computed on-demand from the cached split surjection
oldinclusions = lookup(symbol_, Module, Array)
Module _ Array := Matrix => (M, w) -> M.cache#(symbol _, w) ??= (
    if not M.cache#?(symbol ^, w) then oldinclusions(M, w)
    else rightInverse(M.cache#(symbol ^, w),
	DegreeLimit       => degree 0_M,
	MinimalGenerators => true))

-- TODO: can we return cached summands from the closest subfield?
cachedSummands = M -> M.cache.summands ?? components M

-----------------------------------------------------------------------------
-- directSummands
-----------------------------------------------------------------------------

-- Note: M may need to be extended to a field extensions
directSummands = method(Options => DirectSummandsOptions)
directSummands Module := List => opts -> M -> M.cache.summands ??= (
    checkRecursionDepth();
    strategy := opts.Strategy;
    R := ring M;
    -- Note: End does not work for WeylAlgebras or AssociativeAlgebras yet, nor does basis
    if not isCommutative R and not isWeylAlgebra R then error "expected a commutative base ring";
    -- Note: rank does weird stuff if R is not a domain
    if opts.Verbose then printerr("splitting module of rank: ", toString rank M);
    if rank cover M <= 1 then return M.cache.summands = { M.cache.isIndecomposable = true; M };
    if isDirectSum  M    then return M.cache.summands = splitComponents(M, components M, directSummands_opts);
    if isFreeModule M    then return M.cache.summands = splitFreeModule(M, opts);
    if strategy & 1 == 1 then return M.cache.summands = (
	directSummands(directSum splitFreeSummands(M, opts), opts, Strategy => strategy - 1));
    -- TODO: where should indecomposability check happen?
    -- for now it's here, but once we figure out random endomorphisms
    -- without computing Hom, this would need to move.
    -- Note: this may return null if it is inconclusive
    if strategy & 2  == 2  and isIndecomposable M === true then { M } else
    if strategy & 16 != 16 and isHomogeneous M
    then summandsFromProjectors(M, opts)   -- see DirectSummands/homogeneous.m2
    else summandsFromIdempotents(M, opts)) -- see DirectSummands/idempotents.m2

directSummands CoherentSheaf := List => opts -> F -> apply(
    directSummands(module prune F, opts), N -> sheaf(variety F, N))

-----------------------------------------------------------------------------

-- tests whether L (perhaps a line bundle) is a summand of M
-- Limit => N _recommends_ stopping after peeling N summands of L
-- FIXME: it's not guaranteed to work, e.g. on X_4 over ZZ/2
-- TODO: cache projection/inclusion maps
-- TODO: cache this
directSummands(Module, Module) := List => opts -> (L, M) -> (
    checkRecursionDepth();
    if ring L =!= ring M then error "expected objects over the same ring";
    if rank L  >  rank M then return {M};
    limit := opts.Limit ?? numgens M;
    tries := opts.Tries ?? defaultNumTries char ring M;
    if 1 < #cachedSummands M then return flatten apply(cachedSummands M, N -> directSummands(L, N, opts));
    if 1 < limit then (
	if opts.Verbose then stderr << " -- splitting free summands: " << flush;
	comps := new MutableList from {M};
	comps = for i to limit - 1 do (
	    LL := directSummands(L, M, opts, Limit => 1);
	    if #LL == 1 then break sort toList comps;
	    comps#(#comps-1) = L;
	    comps##comps = M = LL#1);
	if opts.Verbose then stderr << endl << flush;
	return comps
    );
    zdeg := degree 0_M;
    -- TODO: can we detect multiple summands of L at once?
    -- perhaps find a projector onto a summand with several copies?
    gensHom0 := (N, M) -> (
	H := Hom(N, M,
	    DegreeLimit => zdeg,
	    MinimalGenerators => false);
	smartBasis(zdeg, H));
    if isFreeModule L then (
	B := gensHom0(M, L); if numcols B == 0 then return {M};
	-- Previous alternative:
	-- h := for i from 0 to numcols B - 1 do (
	--     isSurjective(b := homomorphism B_{i}) ...)
	h := for i to tries - 1 do (
	    b := homomorphism(B * random source B);
	    if isSurjective b then break matrix {L_0} // b))
    else (
        -- we look for a composition L -> M -> L which is the identity
	B  = gensHom0(L, M); if numcols B == 0 then return {M};
	C := gensHom0(M, L); if numcols C == 0 then return {M};
        -- attempt to find a random isomorphism
        h = for i to tries - 1 do (
	    b := homomorphism(B * random source B);
	    c := homomorphism(C * random source C);
            --TODO: change isIsomorphism to isSurjective?
	    if isIsomorphism(c * b) then break b);
        if h === null then h = (
	    -- TODO: is it worth doing the following lines? when does the random strategy above fail?
	    printerr "summands got to this part, it's probably useful!";
	    if opts.Strategy & 8 == 8 then (
	        -- precomputing the Homs can sometimes be a good strategy
		-- TODO: confirm that this injectivity check is worthwhile and not slow
		Bhoms := select(apply(numcols B, i -> homomorphism B_{i}), isInjective);
		Choms := select(apply(numcols C, i -> homomorphism C_{i}), isSurjective);
		pos := position'(Choms, Bhoms, (c, b) -> isIsomorphism(c * b));
	        if pos =!= null then last pos)
	    else (
	        -- and sometimes too memory intensive
	        ind := position'(numcols C, numcols B, (c, b) ->
		    isSurjective homomorphism C_{c}
		    and isInjective homomorphism B_{b}
		    and isIsomorphism(homomorphism C_{c} * homomorphism B_{b}));
	        if ind =!= null then homomorphism B_{last ind})
	));
    if h === null then return {M};
    if opts.Verbose then stderr << concatenate(rank L : ".") << flush;
    {L, prune coker h})

directSummands(CoherentSheaf, CoherentSheaf) := List => opts -> (L, G) -> apply(
    directSummands(module prune L, module prune G, opts), N -> sheaf(variety L, N))

-- attempt to peel off summands from a given list of modules
directSummands(List, CoherentSheaf) :=
directSummands(List, Module) := List => opts -> (Ls, M) -> sort (
    if 1 < #cachedSummands M then flatten apply(cachedSummands M, N -> directSummands(Ls, N, opts))
    else fold(Ls, {M}, (L, LL) -> join(drop(LL, -1), directSummands(L, last LL, opts))))

-----------------------------------------------------------------------------
-- isIndecomposable
-----------------------------------------------------------------------------

-- returns false if an easy decomposition can be found
-- (but does _not_ run the full directSummands algorithm)
-- returns true if the module is certifiably indecomposable
-- returns null for non-conclusive results
isIndecomposable = method(Options => { Strategy => null })
isIndecomposable CoherentSheaf := o -> F -> isIndecomposable(module prune F, o)
isIndecomposable Module := o -> M -> M.cache.isIndecomposable ??= tryHooks(
    (isIndecomposable, Module), (o, M), (o, M) -> (
	if 1 < debugLevel then printerr("isIndecomposable was inconclusive. ",
	    "Try extending the field or pruning the sheaf.")))

-- this strategy checks if:
-- * M has only one degree zero endomorphism, namely identity, or
-- * all degree zero endomorphisms of M are zero mod maximal ideal
-- if a non-identity idempotent is found, it is cached in M
addHook((isIndecomposable, Module), Strategy => "IdempotentSearch", (opts, M) -> (
	(idemp, certified) := findBasicIdempotent M;
	if idemp =!= null then ( if 1 < debugLevel then printerr "module is decomposable!";  false )
	else if certified then ( if 1 < debugLevel then printerr "module is indecomposable!"; true )
    ))

-- TODO
--addHook((isIndecomposable, Module), Strategy => TorsionFree, (opts, M) -> (
--	if isTorsionFree M and isDomain M
--	and degree M // degree R <= 1 then return true))

-----------------------------------------------------------------------------
-* Development section *-
-----------------------------------------------------------------------------

--directSummands Matrix  := List => opts -> f -> apply(directSummands(coker f,  opts), presentation)
-* TODO: not done yet
diagonalize = M -> (
    m := presentation M;
    elapsedTime A := End M; -- most time consuming step
    elapsedTime B := basis(degree 1_(ring M), A);
    h = generalEndomorphism M;
    N0 = image homomorphism B_{idem};
    N = prune N0;
    psi = inverse N.cache.pruningMap; -- map N0 --> N
    phi = map(N0, M, homomorphism B_{idem}); -- map M --> N0
    f = psi * phi; -- map M --> N
    h' = f * h * inverse f;
    source h' == N;
    target h' == N;
    h'
    )
*-

--directSummands Complex := List => opts -> C -> () -- TODO: should be functorial

-----------------------------------------------------------------------------
-* Test section *-
-----------------------------------------------------------------------------

load "./DirectSummands/tests.m2"

-----------------------------------------------------------------------------
-* Documentation section *-
-----------------------------------------------------------------------------

beginDocumentation()

load "./DirectSummands/docs.m2"

end--

restart
check needsPackage "DirectSummands"

uninstallPackage "DirectSummands"
restart
viewHelp installPackage "DirectSummands"
viewHelp directSummands
viewHelp

end--
restart
debug needsPackage "DirectSummands"


R = kk[x,y,z];
n = 1000
d = {100}
elapsedTime smartBasis(0, Hom(R^n, R^d));
elapsedTime smartBasis(0, Hom(R^n, R^d, DegreeLimit => 0));


--------
-- summand of 4th syzygy of residue field of ring defined by
-- ideal(y*z,x*z,y^3,x*y^2+z^3,x^2*y,x^3) is indecomposable,
-- but the current method doesn't really show that definitively
