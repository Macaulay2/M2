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



isInclusionOfCoefficientRing = method()
isInclusionOfCoefficientRing RingMap := Boolean => inc -> (
    --checks whether the map is the inclusion of the coefficientRing
    if source inc =!= coefficientRing target inc then return false;
    inc vars source inc == promote (vars source inc, target inc)
    )

isFinite1 = (f) -> (
    A := source f;
    B := target f;
    matB := null;
    mapf := null;
    pols := f.matrix;
    (FA, phiA) := flattenRing A;
    iFA := ideal FA;
    varsA := flatten entries phiA^-1 vars FA;
    RA := try(ring source presentation FA) else FA;
    (FB, phiB) := flattenRing B;
    iFB := ideal FB;
    varsB := flatten entries phiB^-1 vars FB;
    RB := try(ring source presentation FB) else FB;
    m := numgens FA;
    n := numgens FB;
    pols = pols_{0..(m-1)};
    R := try(tensor(RB, RA, Join => false)) else tensor(RB, RA, Join => true);
    xvars := (gens R)_{n..n+m-1};
    yvars := (gens R)_{0..n-1};
    iA := sub(ideal FA,matrix{xvars});
    iB := sub(ideal FB,matrix{yvars});
    iGraph := ideal(matrix{xvars}-sub(pols,matrix{yvars}));
    I := iA+iB+iGraph;
    inI := leadTerm I;
    r := ideal(sub(inI,matrix{yvars | splice{m:0}}));     
    for i from 1 to n do
        if ideal(sub(gens r,matrix{{(i-1):0,1_R,(m+n-i):0}}))!=ideal(1_R) then
            return false;
    true
    )

isModuleFinite = method()
isModuleFinite Ring := Boolean => R -> (
    I := ideal leadTerm ideal R;
    ge := flatten select(I_*/support, ell -> #ell == 1);
    set ge === set gens ring I
    )
isModuleFinite RingMap := Boolean => f -> (
    if isInclusionOfCoefficientRing f then
        isModuleFinite target f
    else
        isFinite1 f
    )

pushFwd=method(Options => {NoPrune => false})
pushFwd RingMap := Sequence => o -> (f) ->
--pfB is B^1 as an A-module
--matB is the set of monomials in B that form a set of generators as an A-module
--mapf takes as arg an element of B, and returns ??
(
     A:=source f;
     B:=target f;
     deglenA:=degreeLength A;
     deglenB:=degreeLength B;
     (matB, mapfaux) := pushAuxHgs f;

     pfB := makeModule(B^1,f,matB);
     g := map(pfB,,gens pfB);
     mapf := (b) -> g*(mapfaux b); 
     (pfB,matB,mapf)
     )

pushFwd Ring := Sequence => o -> B -> pushFwd(map(B, coefficientRing B), o)
pushFwd Module := Module => o -> M -> pushFwd(map(ring M, coefficientRing ring M), M, o)
pushFwd Matrix := Matrix => o -> d -> pushFwd(map(ring d, coefficientRing ring d), d, o)

pushFwd(RingMap,Module):=Module=>o->(f,N)->
(
     B:=target f;
     aN:=ann N;
     C:=B/aN;
     bc:=map(C,B);
     g:=bc*f;
     
     matB:=(pushAuxHgs g)_0;
     if (o.NoPrune == false) then prune makeModule(N**C,g,matB) else makeModule(N**C,g,matB)
     )

pushFwd(RingMap,Matrix):=Matrix=>o->(f,d)->
(
     A:=source f;
     B:=target f;
     pols:=f.matrix;
     pM:=source d;
     pN:=target d;
     
     amn:=intersect(ann pM,ann pN);
     C:=B/amn;
     bc:=map(C,B);
     g:=bc*f;     
     M:=pM**C;
     N:=pN**C;
   
     psh:=pushAuxHgs g;
     matB:=psh_0;
     mapf:=psh_1;     
          
     pushM:=makeModule(M,g,matB);
     pushN:=makeModule(N,g,matB);
     
     matMap:=symbol matMap;
     gR:=matB**matrix d;
     c:=numgens source gR;
     l:=numgens target gR;
     k := numcols matB;
     matMap=mutableMatrix(A,k*l,c);
     
     for i1 from 0 to c-1 do
     	  for i2 from 0 to l-1 do
	  (
       	       e:=mapf(gR_i1_i2);
	       for i3 from 0 to k-1 do matMap_(i2+l*i3,i1)=e_0_i3;	       
	   );

          if (o.NoPrune == false) then prune map(pushN,pushM,matrix matMap) else map(pushN,pushM,matrix matMap)
     )


-- TODO: stash the matB, pf?  Make accessor functions to go to/from gens of R over A, or M to M_A.
-- TODO: given: M = pushFwd N, get the maps from N --> M (i.e. stash it somewhere).
--   also, we want the map going backwards too: given an element of M, lift it to N.


-- makeModule
-- internal function which implements the push forward of a module.
-- input: 
--   N     : Module, a module over B
--   f     : RingMap, A --> B
--   matB  : matrix over B, with one row, whose entries form a basis for B over A.
--           in fact, it can be any desired subset of A-generators of B, as well.
-- output:
--   the module N as an A-module.
-- notes:
--   if A is a field, this should be easier?
--   the map mp is basically
--     A^k --> auxN (over B)
--   and its kernel are the A-relations of the elements auxN

makeModule=method()
makeModule(Module,RingMap,Matrix):=(N,f,matB)->
(
     N = trim N;
     auxN:=ambient N/image relations N;
     A:=source f;
     k:=(numgens ambient N) * (numgens source matB);
     --mp:=try(map(auxN,,f,matB**gens N)) else map(auxN,A^k,f,matB**gens N);
     mp := if isHomogeneous f then 
               try(map(auxN,,f,matB**gens N)) else map(auxN,A^k,f,matB**gens N)
           else
               map(auxN,A^k,f,matB**gens N);
     ke:=kernel mp;
     (super ke)/ke
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