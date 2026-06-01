-- TODO:
--  finish doc
--  how to interact with pushForward?
--   issues: pushForward seems somewhat faster, in the homogeneous case...
--           also, are these stashed in that case?  (They are not here, yet).

newPackage(
    "PushForward",
    Version => "0.6",
    Date => "May 14, 2021",
    Authors => {
        {Name => "Claudiu Raicu",
            Email => "craicu@nd.edu",
            HomePage => "http://www3.nd.edu/~craicu"},
        {Name => "David Eisenbud",
            Email => "de@msri.org",
            HomePage => "http://www.msri.org/~de"},
        {Name => "Mike Stillman",
            Email => "mike@math.cornell.edu",
            HomePage => "http://pi.math.cornell.edu/~mike"}
        },
    Headline => "push forwards of finite ring maps",
    Keywords => {"Commutative Algebra"},
    AuxiliaryFiles => true
)

-- note, this version has a slight change added by Karl Schwede.  It has an option to turn off the prune calls.
-- Recently, David Eisenbud and Mike Stillman have extended it, fixing some bugs too.

export {
    "isModuleFinite",
    "pushFwd",
    "NoPrune",
    "pushforward",
    "pushforward'"
}
protect \ {PUSHFORWARDMODULES, PUSHFORWARDMAPS, PUSHFORWARDMAP'}



-- pushforward method --
-- map elements from a ring/module to it's pushforward
pushforward = method()
pushforward(RingMap, Matrix) := (f, n) -> (
    N := getModuleAux n;
    Ps := getPushforwards(N, f);
    if Ps === null or #Ps == 0 then (
        M := pushFwd(f, N);
        P := getPushforwardByModule(N, M);
        return P n;
    );
    if #Ps > 1 then error "more than one pushFwd exists for this ring map. specify pushFwd module.";

    P = first Ps;
    P n
)
pushforward(RingMap, Vector) := (f, v) -> pushforward(f, matrix v)
pushforward(Matrix) := (n) -> pushforward(map(ring target n, coefficientRing ring target n), n)
pushforward(Vector) := (v) -> pushforward(matrix v)
pushforward(RingMap, RingElement) := (f, r) -> pushforward(f, matrix r)
pushforward(RingElement) := (r) -> pushforward(map(ring r, coefficientRing ring r), r)
pushforward(Module, Matrix) := (M, n) -> (
    N := getModuleAux n;
    P := getPushforwardByModule(N, M);
    if P === null then error "argument module is not the pushforward of the module of argument element.";
    P n
);
pushforward(Module, Vector) := (M, v) -> pushforward(M, matrix v);
pushforward(Module, RingElement) := (M, r) -> pushforward(M, matrix r);

-- pushforward' method --
-- map elements from a pushforward module to the module that was pushed
pushforward' = method()
pushforward'(Matrix) := (m) -> (
    P' := getPushforward'(module target m);
    if P' === null then error "expected an element of a module of the form pushFwd(N)";
    P' m
)
pushforward'(Vector) := (v) -> pushforward' matrix v


-- pushFwd method
-- central export of this package. compute the push forward of various objects
-- over a ring map if possible.
pushFwd = method(Options => {NoPrune => false})
pushFwd Ring := Sequence => o -> B -> pushFwd(map(B, coefficientRing B), o)
pushFwd Module := Module => o -> M -> pushFwd(map(ring M, coefficientRing ring M), M, o)
pushFwd Matrix := Matrix => o -> d -> pushFwd(map(ring d, coefficientRing ring d), d, o)

-- output is (pfB, matB, mapf) where
--   fB is B^1 as an A-module
--   matB is the set of monomials in B that form a set of generators as an A-module
--   mapf is a method that takes a ring element of B, and returns an element of pfB
pushFwd RingMap := Sequence => o -> (f) ->
(
    B := target f;
    if (cachedModule := getPushFwdModule(B, f, o)) =!= null then (
        -- the extra complexity of this functions return type necessitates a
        -- little bit of work on top of the cache.
        return (
            cachedModule,
            pushforward' cachedModule_{0..numgens cachedModule - 1},
            getPushforwardWithOpts(B, f, o)
        );
    );

    (matB, mapfaux) := pushAuxHgs f;
    (pfB, pfmat', pf) := makeModule(module B, f, matB, mapfaux); -- pf is redundant with mapfaux so is unused below

    g := map(pfB, , gens pfB);
    ringpf := (b) -> g*(mapfaux b);
    setPushforwardCache(B, f, o, ringpf);
    setPushforwardCache(module B, f, o, ringpf);
    setPushforwardByModuleCache(module B, pfB, ringpf);
    setPushforwardCache'(pfB, (a) -> (
        -- coerce to matrix over B
        coeffs := map(module B, B^(numcols a), pfmat'* a);
        -- this try is to handle the case where coeffs has a RingMap attached
        -- which has carried over from the pfmat' construction. in the case
        -- where f is id_R it carries the RingMap through for some reason even
        -- though in other cases it does not. If this fails we attempt to
        -- rebuild the matrix from it's coefficients to get rid of any dangling
        -- RingMap metadata.
        try(map(module B, , coeffs)) else map(module B, , matrix entries coeffs)
    ));

    (pfB, matB, ringpf)
)

pushFwd(RingMap, Module) := Module => o -> (f, N) -> (
    if (cachedModule := getPushFwdModule(N, f, o)) =!= null then
        return cachedModule;

    A := source f;
    B := target f;
    B' := B/ann N; -- N is finite over A iff A -> B' is a finite ring extension
    quot := map(B', B);
    g := quot * f;

    -- you might think we want to just compute pushFwd g inside makeModule but
    -- that triggers infinite recursion makeModule <> pushFwd(RingMap)
    (unusedModule, matB, ringpf) := pushFwd g;
    (pfN, pfmat', pf) := makeModule(N ** B', g, matB, ringpf);

    -- diagram chase

    liftToN := map(N, N ** B', map(B, B'), N_{0..numgens N - 1});
    auxmat := map(N, pfN, f, liftToN * pfmat');

    -- patch auxmat into a function M -> N
    mapb := (m) -> (
        -- m is a map A^1 -> pfN so auxmapb*m is a map A^1 -> N.
        -- since we want the source to be B^1 we have to do some shenanigans
        -- here.
        result := map(N, B^(numcols m), auxmat * m);
        -- if needed we let map fix the degrees to make the result homogeneous.
        if isHomogeneous m then map(N, , result) else result
    );
    setPushforwardCache'(pfN, mapb);

    -- patch pf into a function N -> M
    mapf := (n) -> pf(n ** B');
    setPushforwardCache(N, f, o, mapf);
    setPushforwardByModuleCache(N, pfN, mapf);

    if (o.NoPrune == false) then (
        pfNPruned := prune pfN;
        pruningmap := pfNPruned.cache.pruningMap;
        -- patch up our maps to work with the pruned module instead
        -- todo(dodgejoel): consider placing this diagram chase in the pushforward / pushforward'
        -- methods and just returning the bare pruned module here instead?  that
        -- way you could actually pushforward['] out of a module you pruned by
        -- hand...
        p := (n) -> pruningmap^-1 * mapf(n);
        setPushforwardCache(N, f, o, p);
        setPushforwardCache'(pfNPruned, (m) -> mapb(pruningmap * m));
        setPushforwardByModuleCache(N, pfNPruned, p);
        pfNPruned
    ) else (
        pfN
    )
)


pushFwd(RingMap, Matrix) := Matrix => o -> (f, F) -> (
    M := pushFwd(f, source F, o);
    N := pushFwd(f, target F, o);
    if o.NoPrune == false then (
        N' := target N.cache.pruningMap;
        map(N, M, N.cache.pruningMap^-1 * pushforward(N', F * pushforward' M_{0..numgens M - 1}))
    ) else
        map(N, M, pushforward(N, F * pushforward' M_{0..numgens M - 1}))
)



-- makeModule
-- internal function which implements the push forward of a module.
-- input:
--   N      : Module, a module over B
--   f      : RingMap, A --> B
--   matB   : matrix over B, with one row, whose entries form a basis for B over A.
--           in fact, it can be any desired subset of A-generators of B, as well.
--   ringpf : FunctionClosure sending elements of B to elements of pushFwd B
-- output:
--   (M, F, p) : Sequence
--   M      : the module N as an A-module.
--   F      : Matrix N <- M which provides one direction of the bijection between M and N.
--   p      : FunctionClosure M <- N providing the inverse of the bijection
-- notes:
--   if A is a field, this should be easier?
--   the map mp is basically
--     A^k --> auxN (over B)
--   and its kernel are the A-relations of the elements auxN
--   TODO: stash the matB, pf?  Make accessor functions to go to/from gens of R over A, or M to M_A.
makeModule = method()
makeModule(Module, RingMap, Matrix, FunctionClosure) := (N, f, matB, ringpf) -> (
    N = prune N;
    auxN := ambient N/image relations N;
    A := source f;
    k := (numgens ambient N) * (numgens source matB);
    sourceGens := gens N ** matB;
    mp := if isHomogeneous f then
        try(map(auxN, , f, sourceGens)) else map(auxN, A^k, f, sourceGens)
    else
        map(auxN, A^k, f, sourceGens);

    rels := kernel mp;
    ambientSpace := super rels;

    -- add in any relations coming from f.
    -- note we'll get the wrong answer if the kernel is nontrivial but computing it fails.
    try(kernalAux := kernel f) then rels += kernalAux * ambientSpace;

    -- some rings have can't trim so fallback to not trimming.  can raise `gcd: unimplemented for this ring`.
    rels = try(trim rels) else rels;
    M := ambientSpace / rels;
    pfmat' := N.cache.pruningMap * map(N, M, f, sourceGens);

    pf := (n) -> ( -- pf: N --> M
        n = N.cache.pruningMap^-1 * n;
        -- a bit hacky: we want to transpose without applying antipode
        n' := transpose matrix for row in entries n list for c in row list antipode(c);
        -- apply ringpf and stack as vectors
        results := for i from 0 to numrows n' - 1 list reshape(A^(numgens M), A^1, ringpf n'^{i});
        if isHomogeneous n then
            map(M, , matrix {results})
        else
            map(M, A^(numcols n), matrix {results})
    );

    (M, pfmat', pf)
)

-- what if B is an algebra over A (i.e. A is the coefficient ring of B)
-*
    TODO.
    g = gens gb ideal L
    m = lift(matB, ring g)
    coker last coefficients(g, Monomials => m)
*-

-- helper method that extracts common concerns between isModuleFinite(RingMap)
-- and pushFwd(RingMap)
-- f:RingMap
-- input:
--   f                  : RingMap
-- output: (optional (matB, mapf), optional errorString)
--   matB               : matrix over B, with one row, whose entries form a basis for B over A.
--   mapf               : a method that takes b \in B and returns a matrix of A-coefficients
--                        for b with respect to matB.
--   errorString        : in case of validation failure an error string will be
--                        returned and the first return value is null is null.
ERRORNOTFINITE = "not a finite map";
pushFwdRingHelper = (f) -> (
    A := source f;
    B := target f;

    (FA, phiA) := flattenRing A;
    iFA := ideal FA;
    varsA := flatten entries phiA^-1 vars FA;
    RA := try(ring source presentation FA) else FA;

    (FB, phiB) := flattenRing B;
    iFB := ideal FB;
    varsB := flatten entries phiB^-1 vars FB;
    RB := try(ring source presentation FB) else FB;

    R := try(tensor(RB, RA, Join => false)) else tensor(RB, RA, Join => true);
    m := numgens FA;
    n := numgens FB;
    pols := (f.matrix)_{0..(m-1)};
    xvars := (gens R)_{n..n+m-1};
    yvars := (gens R)_{0..n-1};

    iA := sub(ideal FA, matrix {xvars});
    iB := sub(ideal FB, matrix {yvars});
    iGraph := ideal(matrix {xvars} - sub(pols, matrix {yvars}));
    I := iA + iB + iGraph;
    inI := leadTerm I;
    rels := ideal(sub(inI, matrix{yvars | splice{m:0}}));

    -- skew variables don't show up as explicit relations but are nilpotent so
    -- don't need to be checked here.
    skewInds := set(if isSkewCommutative FB then FB.SkewCommutative else {});
    for i from 1 to n do
        if (
            not member(i - 1, skewInds) and
            ideal(sub(gens rels, matrix {{(i-1):0, 1_R, (m+n-i):0}})) != ideal(1_R)
        ) then error ERRORNOTFINITE;

    mat := lift(basis(R / (rels + ideal(xvars))), R);
    matB := sub(mat, matrix {varsB | toList(m:0_B)});

    phi := map(R, B, matrix{yvars});
    toA := map(A, R, flatten{n:0_A, varsA});
    mapf := (b) -> (
        cfs := last coefficients(phi b % I, Monomials => mat, Variables => yvars);
        toA cfs
    );

    (matB, mapf)
)


pushAuxHgs = method()
pushAuxHgs(RingMap) := (f) -> (
    if isInclusionOfCoefficientRing f then (
        if not isModuleFinite target f then error "inclusion of coefficientRing not a finite map.";

        A := source f;
        B := target f;

        matB := basis(B, Variables => 0 .. numgens B - 1);
        mapf := if isHomogeneous f then (b) -> (
            cfs := last coefficients(b, Monomials => matB);
            try
                lift(cfs, A)
            else (
                -- lifting can fail even in the homogeneous case when the
                -- coefficientRing is graded.
                cfs = map(B^(numrows cfs), B^(numcols cfs), cfs);
                lift(cfs, A)
            )
        )
        else (b) -> (
            cfs := last coefficients(b, Monomials => matB);
            -- strip degrees on the target, as otherwise, with differing degrees
            -- in A and B, the degree cannot always be lifted.
            cfs = map(B^(numrows cfs), B^(numcols cfs), cfs);
            lift(cfs, A)
        );
        matB, mapf
    ) else (
        pushFwdRingHelper(f)
    )
)

isInclusionOfCoefficientRing = method()
isInclusionOfCoefficientRing RingMap := Boolean => inc -> (
    -- checks whether the map is the inclusion of the coefficientRing
    if source inc =!= coefficientRing target inc then return false;
    inc vars source inc == promote (vars source inc, target inc)
)


-- isModuleFinite method
-- compute whether a ring is module finite over source of a ring homomorphism
isModuleFinite = method()
isModuleFinite Ring := Boolean => R -> (
    -- flatten R to gather all of the relevant relations
    (fR, phiR) := flattenRing R;
    I := leadTerm ideal fR;

    R' := ring I;
    flatRels := join(
        flatten select(I_*/support, ell -> #ell == 1),
        -- skew commuting variables don't contribute to failure of module-finiteness
        try(apply(R'.SkewCommutative, (i) -> R'_i)) else {}
    );
    relsR := apply(flatRels, g -> phiR^-1 g);

    -- these are the variables that relations need to cut down
    gensR := gens(R, CoefficientRing => coefficientRing R);

    -- this can be a strict subset
    isEmpty(gensR - set relsR)
)
isModuleFinite RingMap := Boolean => (f) -> (
    if isInclusionOfCoefficientRing f then return isModuleFinite target f;

    (val, err) := trap pushFwdRingHelper(f);
    if err =!= null then (
        if toString err === ERRORNOTFINITE then return false else error err;
    );
    true
)

------------------------
-- internal utilities --
------------------------

-- cache manipulation --
-*
use of cache in this package:
when the pushforward of a module is computed - pushFwd(f, M) - we also generate
maps to translate elements between M and pushFwd(f, M) and these methods are
stored in various caches of related objects.

Caced on M:
- M.cache.PUSHFORWARDMAPS is a CacheTable whose keys have type (RingMap,
OptionTable) and whose values are functions. when (f, o) => p then p is a
function from M to pushWd(f, M, o)
- M.cache.PUSHFORWARDMODULES is a CacheTable whose keys are Modules and whose
values are a functions. When pushFwd(f, M) => p, then p is a function M ->
pushFwd(f, M).

Cached on pushFwd(f, M):
- when N = pushFwd(f, M), then N.cache.PUSHFORWARDMAP' is a function N -> M

Cached on target f:
- If R = target f, then when pushFwd(f) is computed, we populate
R.cache.PUSHFORWARDMAPS is a CacheTable whose kets have type (RingMap,
OptionTable) and values are functions - the same as the first case for M above.
*-

-- setters
setPushforwardCache = (X, f, o, v) -> (
    X.cache.PUSHFORWARDMAPS ??= new CacheTable;
    X.cache.PUSHFORWARDMAPS#(f, o) = v;
)
setPushforwardCache' = (X, v) -> X.cache.PUSHFORWARDMAP' = v
setPushforwardByModuleCache = (X, M, p) -> (
    X.cache.PUSHFORWARDMODULES ??= new CacheTable;
    X.cache.PUSHFORWARDMODULES#M = p;
)

-- getters
getPushforwards = (X, f) -> (
    -- todo - structure cache in a way that doesn't require this scan over pairs
    -- nested CacheTable?
    if X.cache.?PUSHFORWARDMAPS then (
        fmatches := select(pairs(X.cache.PUSHFORWARDMAPS), (k, v) -> first k === f);
        apply(fmatches, (k, v) -> v)
    )
)
getPushforwardWithOpts = (X, f, o) -> (
    if X.cache.?PUSHFORWARDMAPS and X.cache.PUSHFORWARDMAPS#?(f, o) then
        X.cache.PUSHFORWARDMAPS#(f, o)
)
getPushforward' = (X) -> (
    if X.cache.?PUSHFORWARDMAP' then X.cache.PUSHFORWARDMAP'
)
getPushforwardByModule = (X, M) -> (
    if X.cache.?PUSHFORWARDMODULES and X.cache.PUSHFORWARDMODULES#?M then
        X.cache.PUSHFORWARDMODULES#M
)
getPushFwdModule = (X, f, o) -> (
    cached := getPushforwardWithOpts(X, f, o);
    -- 0_X is a nice canonical element to pushforward and see where we end up
    -- todo - better strategy for looking up the module which is the pushforward along (f, o)
    if cached =!= null then target cached matrix 0_X
)

-- in rank 1 free case use THE rank 1 free module for ring N
getModuleAux = (n) -> (
    N := module target n;
    if N == module ring N then module ring N else N
)

-----------
-- TESTS --
-----------
load "./PushForward/test.m2"

-------------------
-- DOCUMENTATION --
-------------------
beginDocumentation()
load "./PushForward/doc.m2"



-------------------
end
-------------------

restart
uninstallPackage"PushForward"
restart
installPackage"PushForward"
x = symbol x;y= symbol y;
check PushForward
viewHelp PushForward



target oo == pr_0
pushFwd(map(R',R), R'^1)
---
A = QQ
B = QQ[x]/(x^2)
N = B^1 ++ (B^1/(x))
f = map(B,A)
pushFwd(f,N)
pushFwd f

-- example bug -----------------------------------
-- DE + MES

///
  restart
  needsPackage "PushForward"


  -- This one works
  kk = ZZ/101
  A = kk[s,t]
  C = A[x,y,z]/(x^2, y^2, z^2)
  phi = map(C,A)
  f = map(C^1, A^4, phi, {{x,s*y,t*y, z}})
  ker f

  -- This one fails, degrees are screwed up.
  kk = ZZ/101
  A = kk[s,t]
  B = frac A
  C = B[x,y,z]/(x^2, y^2, z^2)
  phi = map(C,B)
  f = map(C^1, B^3, phi, {{x,s*y,z}})
  ker f
///

TEST ///
-*
  restart

  needsPackage "NoetherNormalForm"
*-
  needsPackage "PushForward"
  s = symbol s; t = symbol t
  kk = ZZ/101
  A = frac(kk[s,t])
  L = A[symbol a.. symbol d]/(d-t, a-s, b*c-s*t, b^2-(s/t)*c^2)
  describe L
  ML = pushFwd(map(L,frac A), L^1) -- dim 4, free -- FAILS

  -- simpler example which fails
  -- FIX THIS: should not create a graph ring.
  restart
  debug needsPackage "PushForward"
  s = symbol s; t = symbol t
  kk = ZZ/101
  A = frac(kk[s,t])
  L = A[symbol b, symbol c]/(b*c-s*t, b^2-(s/t)*c^2)
  basis L
  describe L
  inc = map(L, A)
  assert isInclusionOfCoefficientRing inc
  assert isModuleFinite L
  pushFwd inc
  ML = pushFwd(map(L,frac A), L^1)

  -- FIX THIS: should not create a graph ring.
  -- FIX ME?
  restart
  debug needsPackage "PushForward"
  s = symbol s; t = symbol t
  A = QQ
  L = A[symbol b, symbol c]/(b*c-13, b^3-c^2)
  describe L
  inc = map(L, A)
  assert isInclusionOfCoefficientRing inc
  assert isModuleFinite L
  (LA, bas, pf) = pushFwd inc -- this works
  pf(b^2+c^2) -- maybe a better way?


  restart
  debug needsPackage "PushForward"
  s = symbol s; t = symbol t
  kk = ZZ/101
  A = frac(kk[s,t])
  L = A[symbol b, symbol c]/(b^2-(s/t)*c^2 - c, c^3)
  basis L
  describe L
  inc = map(L, A)
  pushForward(inc, A^1) -- now fails...
  pushFwd inc
///


///
-- Case 1.
-- ring map is f : A --> B = A[xs]/I, A is a polynomial ring, quotient field, basic field.

///

///
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