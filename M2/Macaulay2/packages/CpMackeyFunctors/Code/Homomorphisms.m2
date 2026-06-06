protect symbol Domain
protect symbol Codomain

MackeyFunctorHomomorphism = new Type of HashTable
MackeyFunctorHomomorphism.synonym = "Mackey Functor homomorphism"

-- For magic
isMorphism MackeyFunctorHomomorphism := f -> true
-- Hack to make matrix() work; kind of justified!
ring MackeyFunctorHomomorphism := f -> ZZ
-- Hack to make matrix() work; unclear if this is justified.
promote (MackeyFunctorHomomorphism, ZZ) := (f,R) -> f
-- This is true, we don't have gradings
degree MackeyFunctorHomomorphism := f -> 0

isWellDefined MackeyFunctorHomomorphism := Boolean => F ->(
    -- Verify the keys are correct
    if not (F#?Domain and F#?Codomain and F#?UnderlyingMap and F#?FixedMap) then return false;

    -- Verify domain and codomain are indeed Mackey functors
    if not (class F.Domain === CpMackeyFunctor and class F.Codomain === CpMackeyFunctor) then return false;

    -- Verify that the primes are the same
    if not (F.Domain.PrimeOrder == F.Codomain.PrimeOrder) then return false;

    -- Verify F.UnderlyingMap and F.FixedMap have the right domain and codomain
    if not (source F.UnderlyingMap == F.Domain.Underlying and target F.UnderlyingMap == F.Codomain.Underlying) then return false;
    if not (source F.FixedMap == F.Domain.Fixed and target F.FixedMap == F.Codomain.Fixed) then return false;

    -- Check commutes with restriction
    if not (F.UnderlyingMap * F.Domain.Res == F.Codomain.Res * F.FixedMap) then (print " -- the given morphism does not commute with restriction"; return false);

    -- Check commutes with transfer
    if not (F.FixedMap * F.Domain.Trans == F.Codomain.Trans * F.UnderlyingMap) then (print " -- the given morphism does not commute with transfer"; return false);

    -- Check commutes with conjugation
    if not (F.UnderlyingMap * F.Domain.Conj == F.Codomain.Conj * F.UnderlyingMap) then (print " -- the given morphism does not commute with conjugation"; return false);

    true
)

-- Arguments:
-- 1. Target
-- 2. Source
-- 3. Underlying map
-- 4. Fixed-point map
map(CpMackeyFunctor, CpMackeyFunctor, Matrix, Matrix) := MackeyFunctorHomomorphism => opts -> (N,M,u,f) -> (
    F := new MackeyFunctorHomomorphism from {
        symbol Codomain => N,
        symbol Domain => M,
        symbol UnderlyingMap => map(N.Underlying,M.Underlying,u),
        symbol FixedMap =>  map(N.Fixed,M.Fixed,f),
        symbol cache => new CacheTable
        };
    if assertLevel > 0 and not isWellDefined F then error "Mackey Functor homomorphism is not well-defined";
    F
)

-- Allows you construct the zero map, and also k*id
map(CpMackeyFunctor,CpMackeyFunctor,ZZ) := MackeyFunctorHomomorphism => opts -> (N,M,a) -> (
    if not M.PrimeOrder == N.PrimeOrder then error "Mackey functors for different primes";
    if a==0 then return map(N,M,map(N.Underlying,M.Underlying,0),map(N.Fixed,M.Fixed,0));
    if N==M then return map(N,M, map(N.Underlying,M.Underlying,a),map(N.Fixed,M.Fixed,a));
    error "Mackey functors are not the same and integer is not zero."
)

-- Print behavior
-- This is just some adhoc editing, sorry to anybody trying to decipher this!
-- But the basics are: "string | string" will adjoin things horizontally, and
-- "string || string" will adjoin things vertically. Everything done here is
-- gluing strings together with whitespace depending on the width/heights of
-- the modules/matrices here.
-- For those trying to understand, worth pointing out that the HEIGHT of a string
-- is obtained via "length" and NOT "height".
lineAbove := (s, n) -> concatenate(n : "-") || s
lineBelow := (s, n) -> s || concatenate(n : "-")
horzSpace := n -> (s := " "; if n == 0 then return ""; if n == 1 then return s else for i to n-2 do s = s | " "; s)
vertSpace := n -> (s := ""; if n == 1 then return s else for i to n-2 do s = s || ""; s)
extraLines := n -> (s := "| | "; if n == 1 then return s else for i to n-2 do s = s || "| | "; s)

objHelper := (M,n) -> (
    (f,u) := toSequence { M.Fixed, M.Underlying }/net/width;
    arrowShift := floor((max {f,u})/2) - 1;
    fShift := if f >= u then 0 else floor((u-f)/2)+1;
    uShift := if u >= f then 0 else floor((f-u)/2)+1;
    arrows := horzSpace(arrowShift) | ("^ | " || extraLines(n) || "| v ");
    loopArrow := horzSpace(arrowShift) | ("^ | " || "└-┘ ");
    str := (horzSpace(fShift) | (net M.Fixed)) || arrows || (horzSpace(uShift) | (net M.Underlying)) || loopArrow;
    str
)

net MackeyFunctorHomomorphism := f -> (
    n := 6 + max ({f.FixedMap, f.UnderlyingMap}/net/width);
    h1 := if (f.FixedMap) == 0 then 1 else numRows (f.FixedMap);
    (s,t) := toSequence { (source f).Fixed, (target f).Fixed }/net/length;
    h2 := 2 + max{s,t};
    hs := if s >= t then 1 else t - s + 1;
    ht := if t >= s then 1 else s - t + 1;
    horizontalJoin(
	vertSpace(h1) || objHelper(target f, ht), vertSpace(h1) || " <--" || vertSpace(h2) || " <--",
	lineBelow("fix : " | net (f.FixedMap), n) || vertSpace(h2) || lineAbove("und : " | net (f.UnderlyingMap), n),
	vertSpace(h1) || "-- " || vertSpace(h2) || "-- ", vertSpace(h1) || objHelper(source f, hs)
	)
    )

-*
debug needsPackage "CpMackeyFunctors"
makeRandomMackeyFunctorHomomorphism(makeRandomCpMackeyFunctor 3, makeRandomCpMackeyFunctor 3)
f = complexLinearizationMap 3
*-

-- tries to induce from the identity
inducedMap(CpMackeyFunctor, CpMackeyFunctor) := MackeyFunctorHomomorphism => opts -> (N,M) -> (
    map(N,M,inducedMap(N.Underlying, M.Underlying), inducedMap(N.Fixed, M.Fixed))
)

-- tries to induce from components of f
inducedMap(CpMackeyFunctor, CpMackeyFunctor, MackeyFunctorHomomorphism) := MackeyFunctorHomomorphism => opts -> (N,M,f) -> (
    map(N,M,inducedMap(N.Underlying, M.Underlying, f.UnderlyingMap), inducedMap(N.Fixed, M.Fixed, f.FixedMap))
)

-- tries to induce from u and f
inducedMap(CpMackeyFunctor, CpMackeyFunctor, Matrix, Matrix) := MackeyFunctorHomomorphism => opts -> (N,M,u,f) -> (
    map(N,M,inducedMap(N.Underlying, M.Underlying, u), inducedMap(N.Fixed, M.Fixed, f))
)

source MackeyFunctorHomomorphism := CpMackeyFunctor => F -> F.Domain
target MackeyFunctorHomomorphism := CpMackeyFunctor => F -> F.Codomain

CpMackeyFunctor#id = X -> map(X, X, id_(X.Underlying), id_(X.Fixed))

-- This is the linearization map A -> RU
complexLinearizationMap = method()
complexLinearizationMap(ZZ) := MackeyFunctorHomomorphism => p -> (
    map(makeComplexRepresentationMackeyFunctor p, makeBurnsideMackeyFunctor p, matrix {{1}}, matrix {{1,1}} || matrix (for i to p-2 list {0,1}))
)

-- This is the linearization map A -> RO
realLinearizationMap = method()
realLinearizationMap(ZZ) := MackeyFunctorHomomorphism => p -> (
    RO := makeRealRepresentationMackeyFunctor p;
    map(RO, makeBurnsideMackeyFunctor p, matrix {{1}}, matrix {{1,1}} || matrix (for i to (rank (RO.Fixed) - 2) list {0,1}))
)

makeUniversalMapFixed = method()
-- Given a Mackey functor M and vector x in fixed module, produce map A -> M
makeUniversalMapFixed(CpMackeyFunctor,Vector) := MackeyFunctorHomomorphism => (M,x) -> makeUniversalMapFixed(M,matrix x)
-- Given a Mackey functor M and a matrix of n vectors in fixed module, produce map A^n -> M
makeUniversalMapFixed(CpMackeyFunctor,Matrix) := MackeyFunctorHomomorphism => (M,x) -> (
    n := numColumns x;
    if n == 0 then return map(M, makeZeroMackeyFunctor(M.PrimeOrder), 0);
    L := {for i to n-1 list (
        X := inducedMap(M.Fixed, , matrix x_i);
        p := M.PrimeOrder;
        A := makeBurnsideMackeyFunctor(p);
        U := M.Res * X;
        F := X | (M.Trans * M.Res * X);
        map(M, A, U, F)
	)};
    matrix L
)

makeUniversalMapUnderlying = method()
-- Given a Mackey functor M and vector x in underlying module, produce map B -> M
makeUniversalMapUnderlying(CpMackeyFunctor,Vector) := MackeyFunctorHomomorphism => (M,x) -> makeUniversalMapUnderlying(M,matrix x)
-- Given a Mackey functor M and a matrix of n vectors in underlying module, produce map B^n -> M
makeUniversalMapUnderlying(CpMackeyFunctor,Matrix) := MackeyFunctorHomomorphism => (M,x) -> (
    n := numColumns x;
    if n == 0 then return map(M, makeZeroMackeyFunctor(M.PrimeOrder), 0);
    L := {for i to n-1 list (
        X := inducedMap(M.Underlying, , matrix x_i);
        p := M.PrimeOrder;
        B := makeUnderlyingFreeMackeyFunctor(p);
        U := matrix {for i to p-1 list ((M.Conj)^i) * X};
        F := M.Trans * X;
        map(M, B, U, F)
	)};
    matrix L
)

-- Given a Mackey functor M,
-- a matrix of n elements X in underlying, and
-- a matrix of m elements Y in fixed,
-- return the universal map B^n ++ A^m -> M
makeUniversalMap = method()
makeUniversalMap(CpMackeyFunctor,Matrix,Matrix) := MackeyFunctorHomomorphism => (M,X,Y) -> (
    return makeUniversalMapUnderlying(M,X) | makeUniversalMapFixed(M,Y)
)

-- Arithmetic operations
-- ZZ-linear operations
MackeyFunctorHomomorphism + MackeyFunctorHomomorphism := MackeyFunctorHomomorphism => (f,g) -> (
    if source f != source g then error("-- sources of maps must agree");
    if target f != target g then error("-- targets of maps must agree");
    map(target f, source g, f.UnderlyingMap + g.UnderlyingMap, f.FixedMap + g.FixedMap)
)

ZZ * MackeyFunctorHomomorphism := MackeyFunctorHomomorphism => (n, f) -> (
    map(target f, source f, n * f.UnderlyingMap, n * f.FixedMap)
)

- MackeyFunctorHomomorphism := MackeyFunctorHomomorphism => f -> (
    (-1)*f
)

MackeyFunctorHomomorphism - MackeyFunctorHomomorphism := MackeyFunctorHomomorphism => (f,g) -> (
    f + (-g)
)

-- Function composition
MackeyFunctorHomomorphism * MackeyFunctorHomomorphism := MackeyFunctorHomomorphism => (G,F) ->(
    map(G.Codomain, F.Domain, G.UnderlyingMap * F.UnderlyingMap, G.FixedMap * F.FixedMap)
)

-- Direct sums of homomorphisms
MackeyFunctorHomomorphism.directSum = args -> (
    if not same apply(args, f -> (source f).PrimeOrder) then error "-- Prime not compatible";
    Src := directSum(args/source);
    Tgt := directSum(args/target);
    B := directSum(apply(args,a->a.UnderlyingMap));
    T := directSum(apply(args,a->a.FixedMap));

    map(Tgt,Src,B,T)
    )
MackeyFunctorHomomorphism ++ MackeyFunctorHomomorphism := MackeyFunctorHomomorphism => (F, G) -> MackeyFunctorHomomorphism.directSum(F, G)
directSum MackeyFunctorHomomorphism := MackeyFunctorHomomorphism => F -> MackeyFunctorHomomorphism.directSum(1 : F)

-- Checking if a morphism is an iso.
isIsomorphism(MackeyFunctorHomomorphism) := Boolean => F -> (
    isTrivialMackeyFunctor ker F and isTrivialMackeyFunctor coker F
)

-- If it is an isomorphism, then we can invert it.
inverse MackeyFunctorHomomorphism := MackeyFunctorHomomorphism => f -> (
    if not isIsomorphism f then error("-- map must be invertible");
    fT := inverse f.FixedMap;
    fB := inverse f.UnderlyingMap;
    map(source f, target f, fB, fT)
    )

-- Power function composition, including negative powers.
MackeyFunctorHomomorphism ^ ZZ := MackeyFunctorHomomorphism => (f,n) -> (
    if n == 1 then return f;
    if n == 0 then return id_(source f);
    if n == -1 then (if isIsomorphism f then inverse f else error("-- f not invertible"));
    if source f != target f then error("-- can only iterate self-maps");
    g := if n < 0 then inverse f else f;
    for i to abs(n)-2 do g = f * g;
    g
)

isTrivialMackeyFunctor = method()
isTrivialMackeyFunctor(CpMackeyFunctor) := Boolean => F -> (
    F.Fixed == 0 and F.Underlying == 0
)

-- Equality of morphisms
MackeyFunctorHomomorphism == MackeyFunctorHomomorphism := Boolean => (f,g) -> (
    if source f != source g then return false;
    if target f != target g then return false;
    if f.UnderlyingMap != g.UnderlyingMap then return false;
    if f.FixedMap != g.FixedMap then return false;
    true
    )

-- Pruning morphisms
prune MackeyFunctorHomomorphism := MackeyFunctorHomomorphism => opts -> f -> (
    src := prune source f;
    tgt := prune target f;
    srcPrune := src.cache.pruningMap;
    tgtPrune := tgt.cache.pruningMap;
    (inverse tgtPrune) * f * srcPrune
    )

-- block homomorphisms
MackeyFunctorHomomorphism | MackeyFunctorHomomorphism := MackeyFunctorHomomorphism => MackeyFunctorHomomorphism.concatCols = maps -> (
    if not all(maps, f -> target f === target maps#0) or not all(maps, f -> (source f).PrimeOrder === (source maps#0).PrimeOrder) then
        error "MackeyFunctorHomomorphism.concatCols: all maps must have the same target and prime order";
    if #maps === 0 then
        error "MackeyFunctorHomomorphism.concatCols: no maps provided";
    map(target maps#0, directSum apply(maps, source), concatCols apply(maps, f -> f.UnderlyingMap), concatCols apply(maps, f -> f.FixedMap))
)

MackeyFunctorHomomorphism || MackeyFunctorHomomorphism := MackeyFunctorHomomorphism => MackeyFunctorHomomorphism.concatRows = maps -> (
    if not all(maps, f -> source f === source maps#0) or not all(maps, f -> (source f).PrimeOrder === (source maps#0).PrimeOrder) then
        error "MackeyFunctorHomomorphism.concatRows: all maps must have the same source and prime order";
    if #maps === 0 then
        error "MackeyFunctorHomomorphism.concatRows: no maps provided";
    map(directSum apply(maps, target), source maps#0, concatRows apply(maps, f -> f.UnderlyingMap), concatRows apply(maps, f -> f.FixedMap))
)

MackeyFunctorHomomorphism.concatBlocks = maps -> MackeyFunctorHomomorphism.concatRows apply(maps, MackeyFunctorHomomorphism.concatCols)
MackeyFunctorHomomorphism.matrix = opts -> MackeyFunctorHomomorphism.concatBlocks

--map--
map(CpMackeyFunctor,CpMackeyFunctor,ZZ) := MackeyFunctorHomomorphism => opts -> (N,M,a) -> (
    if not M.PrimeOrder == N.PrimeOrder then error "Mackey functors for different primes";
    if a==0 then return map(N,M,map(N.Underlying,M.Underlying,0),map(N.Fixed,M.Fixed,0));
    if N==M then return map(N,M, map(N.Underlying,M.Underlying,a),map(N.Fixed,M.Fixed,a));
    error "Mackey functors are not the same and integer is not zero."
)
