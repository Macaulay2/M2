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
    AuxiliaryFiles => true,
    PackageImports => {
        "Complexes" -- compute resolution for Ext
    }
)

-- Note, this version has a slight change added by Karl Schwede.  It has an option to turn off the prune calls.
-- Recently, David Eisenbud and Mike Stillman have extended it, fixing some bugs too.
-- Joel Dodge made a significant refactor of this package and extended its functionality to cover
-- more ring maps and to implement the natural bijections between a module and it's
-- pushforward.

export {
    "pushFwd",
    "pushforward",
    "pushforward'",
    "isModuleFinite",
    "pushFwdGens",
    "pushFwdRingMapShim"
}

-------------
-- pushFwd --
-------------
-- central export of this package. compute the push forward of various objects
-- over a ring map if possible.
pushFwd = method(Options => {MinimalGenerators => true})
pushFwd RingMap := Module => o -> (f) -> pushFwd(f, module target f, o)
pushFwd Ring := Module => o -> R -> pushFwd(map(R, coefficientRing R), module R, o)
pushFwd Module := Module => o -> M -> pushFwd(map(ring M, coefficientRing ring M), M, o)
pushFwd Matrix := Matrix => o -> F -> pushFwd(map(ring F, coefficientRing ring F), F, o)

protect computing
pushFwd(RingMap, Ideal) := Module => o -> (f, I) -> pushFwd(f, module I)
pushFwd(RingMap, Module) := Module => o -> (f, N) -> N.cache#(pushFwd, f, o) ??= (
    N.cache#(computing, pushFwd) = true;
    N' := asDirectSum N;
    Cs := components N';

    (pfN, pf', pf) := if #Cs > 1 then (
        pfCs := apply(Cs, C -> pushFwd(f, C));
        auxpfN := directSum pfCs;
        auxpf' := (m) -> (
            map(N, N', id_N) * sum for i from 0 to #Cs - 1 list N'_[i] * pushforward'(map(pfCs#i, , auxpfN^[i] * m))
        );
        auxpf := (n) -> (
            sum for i from 0 to #Cs - 1 list auxpfN_[i] * pushforward(pfCs#i, map(Cs#i, , N'^[i] * map(N', N, id_N') * n))
        );
        (auxpfN, auxpf', auxpf)
    ) else makeModule(f, N);

    result := if o.MinimalGenerators then (
        -- prune and then push our pf / pf' maps through the pruningMap
        pfNPruned := prune pfN;
        pruningmap := pfNPruned.cache.pruningMap;
        pfNPruned.cache#pushforward' = (m) -> pf'(pruningmap * m);
        pfNPruned.cache.formation = FunctionApplication { pushFwd, (f, N, o) };
        N.cache#(pushforward, pfNPruned) = (n) -> pruningmap^-1 * pf(n);

        N.cache#(pushFwd, f, o) = pfNPruned
    ) else (
        pfN.cache#pushforward' = pf';
        pfN.cache.formation = FunctionApplication { pushFwd, (f, N, o) };
        N.cache#(pushforward, pfN) = pf;

        N.cache#(pushFwd, f, o) = pfN
    );

    remove(N.cache, (computing, pushFwd));
    result
)

pushFwd(RingMap, Matrix) := Matrix => o -> (f, F) -> (
    M := pushFwd(f, source F, o);
    N := pushFwd(f, target F, o);
    map(N, M, pushforward(N, F * pushFwdGens(M)))
)

-----------------
-- pushforward --
-----------------
-- map elements from a ring/module to it's pushforward
pushforward = method(Options => options pushFwd)
-- accepts ring map and computes pushforward module if necessary
pushforward(RingMap, RingElement) := Matrix => opts -> (f, r) -> pushforward(f, map(module ring r, module ring r, matrix r), opts);
pushforward(RingMap, Vector) := Matrix => opts -> (f, x) -> pushforward(f, matrix x, opts)
pushforward(RingMap, Matrix) := Matrix => opts -> (f, n) -> pushforward(pushFwd(f, target n, opts), n, opts)
-- pushforward to explicit module
-- opts are ignored in these overrides
pushforward(Module, RingElement) := Matrix => opts -> (M, r) -> pushforward(M, map(module ring r, module ring r, matrix r), opts);
pushforward(Module, Vector) := Matrix => opts -> (M, v) -> pushforward(M, matrix v, opts);
pushforward(Module, Matrix) := Matrix => opts -> (M, n) -> (
    N := module target n;
    if not N.cache#?(pushforward, M) then error "expected an element of a module of the form pushFwd(N)"
    else N.cache#(pushforward, M)(n)
);

------------------
-- pushforward' --
------------------
-- map elements from a pushforward module to the module that was pushed
pushforward' = method()
pushforward'(Vector) := (v) -> pushforward' matrix v
pushforward'(Matrix) := (m) -> (
    M := module target m;
    if not M.cache#?pushforward' then error "expected an element of a module of the form pushFwd(N)"
    else M.cache#pushforward' m
)

--------------------
-- isModuleFinite --
--------------------
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
    -- we are removing 0_R as this appears in the degenerate case of the zero-ring.
    isEmpty(gensR - set relsR - set {0_R})
)
isModuleFinite RingMap := Boolean => (f) -> (
    if isInclusionOfCoefficientRing f then return isModuleFinite target f;

    (val, err) := trap pushFwdRingHelper(f);
    if err =!= null then (
        if toString err === ERRORNOTFINITE then return false else error err;
    );
    true
)

-- exported to shim over a change to the method signature for pushFwd(RingMap) and pushFwd(Module)
-- these two overrides used to construct some auxiliary data and return it but now just return the module.
-- call sites using this shim ought to be rewritten to appopriately use the below constructions instead.
pushFwdRingMapShim = method()
pushFwdRingMapShim(RingMap) := Sequence => (f) -> (
    M := pushFwd(f, module target f);
    matB := pushFwdGens(M);
    ringpf := (b) -> (module target f).cache#(pushforward, M) matrix b;
    (M, matB, ringpf)
)

--------------
-- INTERNAL --
--------------
pushFwdGens = method()
pushFwdGens(Module) := Matrix => (M) -> M.cache.pushFwdGens ??= pushforward' M_{0..numgens M - 1}

-- makeModule
-- helper implementing the core pushforward computation
-- input:
--   f      : RingMap, S -> R
--   N      : Module, a module over R
-- output:
--   (M, pf', pf) : Sequence
--   M      : the module N as an S-module.
--   pf'    : FunctionClosure N <- M which provides one direction of the bijection between M and N.
--   pf     : FunctionClosure M <- N providing the inverse of the pf'
-- notes:
--   if A is a field, this should be easier?
--   the map mp is basically
--     S^k --> auxN (over R)
--   and its kernel are the S-relations of the elements auxN
-- lift a basis for the a pushforward module M to the module it was pushed from
makeModule = method()
makeModule(RingMap, Module) := (f, N) -> (
    (R, S) := (target f, source f);

    -- replace R^1 with module R so we benefit from caching
    if N === module R then N = module R;

    if isRankOneFree N and not inComputation N then (
        -- this reduces to computing cached pushFwd of module R
        return makeModuleRankOneFree(f, N)
    );

    q := map(R / ann N, R);
    qinv := map(R, R / ann N);

    (matR, ringpf) := pushAuxHgs(q * f);
    ringpf' := (r) -> ringpf q r;
    matR = qinv matR; -- lift S-gens for R / ann N to R

    prunedN := prune N;
    auxN := ambient prunedN/image relations prunedN;
    k := (numgens ambient prunedN) * (numgens source matR);
    sourceGens := gens prunedN ** matR;
    mp := if isHomogeneous f then
        try(map(auxN, , f, sourceGens)) else map(auxN, S^k, f, sourceGens)
    else
        map(auxN, S^k, f, sourceGens);

    rels := kernel mp;
    rels = try(trim rels) else rels;
    M := super rels / rels;

    pf := (n) -> ( -- pf: N --> M
        if numrows n === 0 then return map(M, S^(numcols n), 0);

        n = prunedN.cache.pruningMap^-1 * n;
        -- a bit hacky: we want to transpose without applying antipode
        n' := transpose matrix for row in entries n list for c in row list antipode(c);
        -- apply ringpf and stack as vectors
        results := for i from 0 to numrows n' - 1 list reshape(S^(numgens M), S^1, ringpf' n'^{i});
        if isHomogeneous n then
            map(M, , matrix {results})
        else
            map(M, S^(numcols n), matrix {results})
    );

    pfmat' := prunedN.cache.pruningMap * map(prunedN, M, f, sourceGens);
    pf' := (m) -> (
        -- source m == S^1 and we want a map with source R^1 so do some map
        -- shenanigans here.
        result := map(N, R^(numcols m), pfmat' * m);
        -- if needed we let map fix the degrees to make the result homogeneous.
        -- the try here is to handle a strange case when there is a ring map attached to result
        if isHomogeneous m then (try(map(N, , result)) else map(N, , matrix entries result))
        else result
    );

    (M, pf', pf)
)

-- this is to reduce pushFwd of a free module to pushFwd of module target f
makeModuleRankOneFree = (f, N) -> (
    (R, S) := (target f, source f);
    if not isRankOneFree N then error "expected rank one free module";
    X := pushFwd(f, module R);
    auxpfN := if degreeGroup R == degreeGroup S then X ** S^(degrees N) else X;
    if X != auxpfN then (
        return (
            auxpfN,
            (m) -> pushforward'(map(X, auxpfN, gens X) * m),
            (n) -> map(auxpfN, X, gens auxpfN) * pushforward(X, n)
        );
    ) else (
        return (
            X,
            X.cache#pushforward',
            N.cache#(pushforward, X)
        );
    );
)

inComputation = (M) -> M.cache#?(computing, pushFwd)

asDirectSum = (N) -> (
    if (N == 0 or not isFreeModule N or #components N == numgens N) then N
    else directSum apply(degrees N, d -> (ring N)^{-d})
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
-- output: (matB, mapf)
--   matB               : matrix over B, with one row, whose entries form a basis for B over A.
--   mapf               : a method that takes b \in B and returns a matrix of A-coefficients
--                        for b with respect to matB.
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
pushAuxHgs(RingMap) := (f) -> f.cache.pushAuxHgs ??= (
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
        (matB, mapf)
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

-- various convenience methods to unpack formation data from a pushFwd module
pushFwdSource = (M) -> (
    if not M.cache.?formation then return null;
    if M.cache.formation#0 =!= pushFwd then return null;
    M.cache.formation#1#1
)

pushFwdOpts = (M) -> (
    if not M.cache.?formation then return null;
    if M.cache.formation#0 =!= pushFwd then return null;
    M.cache.formation#1#2
)

pushFwdRingMap = (M) -> (
    if not M.cache.?formation then return null;
    if M.cache.formation#0 =!= pushFwd then return null;
    M.cache.formation#1#0
)

isRankOneFree = (M) -> isFreeModule M and rank M == 1

---------------
-- Hom / Ext --
---------------
load "./PushForward/Hom.m2"

-----------
-- TESTS --
-----------
load "./PushForward/test.m2"

-------------------
-- DOCUMENTATION --
-------------------
beginDocumentation()
load "./PushForward/doc.m2"
load "./PushForward/Hom-doc.m2"

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
A = QQ
B = QQ[x]/(x^2)
N = B^1 ++ (B^1/(x))
f = map(B,A)
pushFwd(f,N)
pushFwd f