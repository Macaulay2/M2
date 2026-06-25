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
    "NoPrune"
}

-------------
-- pushFwd --
-------------
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
    pfB := pushFwd(f, module B, o);
    matB := pushforward' pfB_{0..numgens pfB - 1};
    ringpf := (b) -> (module B).cache#(pushforward, pfB) matrix b;

    (pfB, matB, ringpf)
)

pushFwd(RingMap, Module) := Module => o -> (f, N) -> N.cache#(pushFwd, f, o) ??= (
    A := source f;
    B := target f;
    B' := B / ann N;
    quot := map(B', B);
    g := quot * f;
    (pfN, pfmat', pf) := makeModule(N ** B', g);

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
        -- the try here is to handle a strange case when there is a ring map attached to result
        if isHomogeneous m then (try(map(N, , result)) else map(N, , matrix entries result))
        else result
    );

    -- patch pf into a function N -> M
    mapf := (n) -> pf(n ** B');

    if (o.NoPrune == false) then (
        pfNPruned := prune pfN;
        pruningmap := pfNPruned.cache.pruningMap;
        -- patch up our maps to work with the pruned module instead
        -- todo(dodgejoel): consider placing this diagram chase in the pushforward / pushforward'
        -- methods and just returning the bare pruned module here instead?  that
        -- way you could actually pushforward['] out of a module you pruned by
        -- hand...
        pfNPruned.cache#pushforward' = (m) -> mapb(pruningmap * m);
        pfNPruned.cache.formation = FunctionApplication { pushFwd, (f, N, o) };
        N.cache#(pushforward, pfNPruned) = (n) -> pruningmap^-1 * mapf(n);

        N.cache#(pushFwd, f, o) = pfNPruned
    ) else (
        pfN.cache#pushforward' = mapb;
        pfN.cache.formation = FunctionApplication { pushFwd, (f, N, o) };
        N.cache#(pushforward, pfN) = mapf;

        N.cache#(pushFwd, f, o) = pfN
    )
)


pushFwd(RingMap, Matrix) := Matrix => o -> (f, F) -> (
    M := pushFwd(f, source F, o);
    N := pushFwd(f, target F, o);
    map(N, M, pushforward(N, F * pushforward' M_{0..numgens M - 1}))
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

----------------------
-- internal methods --
----------------------
-- makeModule
-- internal function which implements the push forward of a module.
-- input:
--   N      : Module, a module over B
--   f      : RingMap, A --> B
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
makeModule = method()
makeModule(Module, RingMap) := (N, f) -> (
    (matB, ringpf) := pushAuxHgs(f);
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
    rels = try(trim rels) else rels;
    M := super rels / rels;

    pfmat' := N.cache.pruningMap * map(N, M, f, sourceGens);
    pf := (n) -> ( -- pf: N --> M
        if numrows n === 0 then return map(M, A^(numcols n), 0);

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