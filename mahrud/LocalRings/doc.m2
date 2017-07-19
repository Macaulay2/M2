doc ///
Key
  LocalRings
  (localRing, Ring, Ideal)
Headline
  Localizing polynomial rings at a prime ideal
Description
  Text
    The following functions currently work with local rings: syz, resolution, mingens, minimalPresentation.
  Example
    R = ZZ/32003[a..d];
    "rational quartic curve in P^3:";
    I = monomialCurveIdeal(R,{1,3,4})
    C = res I
    M = ideal"a,b,c,d"; "maximal ideal at the origin";
    P = ideal"a,b,c"; "prime ideal";
    RM = localRing(R, M);
    D = C ** RM;
    E = pruneComplex(D, UnitTest => isUnit)
    "That is to say, the rational quartic curve is not locally Cohen-Macaulay at the origin";
    "Therefore the curve is not Cohen-Macaulay";
    RP = localRing(R, P);
    D' = C ** RP;
    E' = pruneComplex(D', UnitTest => isUnit)
    "However, the curve is Cohen-Macaulay at the prime ideal P (and in fact any other prime ideal)";
Caveat
  Currently limited to localization at prime ideals rather than any multiplicatively closed set.
  Quotients of local rings are not implemented yet. Moreover, certain functions (such as remainder and 
  leadingCoefficient) are ambiguous or not well defined.
SeeAlso
  PruneComplex
///

doc ///
Key
  liftUp
  (liftUp, ChainComplex)
  (liftUp, ChainComplex, Ring)
  (liftUp, Ideal)
  (liftUp, Ideal, Ring)
  (liftUp, List)
  (liftUp, List, Ring)
  (liftUp, Matrix)
  (liftUp, Matrix, Ring)
  (liftUp, MutableMatrix)
  (liftUp, MutableMatrix, Ring)
Headline
  Lifts various objects over R_P to R.
Description
  Text
    Given an object, for instance an ideal IP, over a local ring (RP, P), this method returns the 
    preimage of that object under the cannonical map R -> RP after clearing denominators of IP.
    
    For matrices (hence most other objects as well), clearing denominators is performed columnwise.
    
    Note that (liftUp, List) and (liftUp, List, Ring) expect a list of mutable matrices;
    i.e., a mutableComplex.
    
    In conjunction with pruneComplex or pruneDiff, liftUp is used to implement many of the elementary
    operations over local rings such as syz (see the example below).
  Example
    -- Computing the syzygy over a local ring using liftUp and pruneDiff
    R = ZZ/32003[vars(0..5)];
    I = ideal"abc-def,ab2-cd2-c,-b3+acd";
    C = res I;
    M = ideal"a,b,c,d,e,f";
    RM = localRing(R, M);
    F = C.dd_2;
    FM = F ** RM
    --GM = syz FM
     f' = liftUp FM;
     g' = syz f';
     h' = syz g';
     g = g' ** RM;
     h = h' ** RM;
     C = {mutableMatrix g, mutableMatrix h};
     pruneDiff(C, 1);
    GM = matrix C#0
    assert(FM * GM == 0);
Caveat
  This is NOT the same as lift.
  Does not work with quotients properly. Not implemented for modules yet.
SeeAlso
///

end--

--####################### Even older stuff? ##########################--

-- This routine, localRing, is not functional yet -----
--localRing = new method(
--     Options => {
--          MonomialOrder => null
--          }
--     )
--localRing Ring := R -> localRing(R, ideal vars R)
--localRing(Ring,Ideal) := (R,P) -> (
--     R        -- at first, we do nothing
--     )


restart
loadPackage "LocalRings"
kk=ZZ/101
S=kk[t,x,y,z]
setMaxIdeal ideal vars S
assert(S.residueMap === map(S,S,{0,0,0,0}))

m=matrix"x,y2;z3,x4"
M=coker m
assert(localsyz m == 0)
C = localResolution M -- wrong answer, OK now: MES

N=coker map (S^{-2,0},S^{-3,-4}, m)
--betti res N --without this line some further errors are produced below, different than with this line!
localResolution N -- gives error msg "key not found in hash table", OK mow: MES


betti (FFM=res (M, LengthLimit => 4)) -- correct, nonminimal
FFM.dd
betti (FF=localResolution M)
FF.dd -- this is WRONG, not even composable maps
localResolution M

betti res N --without the res N line above, this doesn't work! -- MES: YET TO BE FIXED
betti (FF=localResolution N) -- same for this, MES: THIS SEEMS TO WORK NOW
localResolution N -- and this MES: WORKING NOW
NN=coker map (S^2, S^2, m)
resolution NN
localResolution NN




restart
loadPackage "LocalRings"
kk=ZZ/101
S=kk[t,x,y,z]
setMaxIdeal ideal vars S
m=matrix"x,y2;z3,x4"
M=coker m
C = localResolution M -- wrong answer, OK now: MES
N=coker map (S^{-2,0},S^{-3,-4}, m)

localResolution N
errorDepth = 0
res N






restart
loadPackage "LocalRings"
kk = ZZ/32003
R = kk[x,y,z,w,SkewCommutative=>true]
m = matrix{{x,y*z},{z*w,x}}
setMaxIdeal(ideal(x,y,z,w))
C = localResolution(coker m, LengthLimit=>10)
C = localResolution(coker m)
C^2
C.dd_4

kk = QQ
R = kk[a,b,c,d]
setMaxIdeal(ideal(a-1,b-2,c-3,d-4))
R.residueMap
I = ideal(a*(1+a), a*(1+b))
I = ideal(a*(a-1), a*(b-2))
I = ideal((1+a), (1+b))
localResolution coker gens I
module I
localPrune module I
localMingens gens I
decompose I

---------------------------------------
-- MES: tests of local ring routines --
---------------------------------------
restart
R = ZZ/32003[a,b,c,Weights=>{-1,-1,-1},Global=>false]
I = ideal(a^2-b^3+a*c^4, a*b-c^3-b^4)
gens gb I
mingens I
J = I + ideal((1+a-b^2)*I_0)
mingens J
res J

restart
loadPackage "BGG"
loadPackage "LocalRings"
A=ZZ/101[a,Weights=>{-1},Global=>false]/a^2
S=ZZ/101[a,x,y,Weights=>{-1,1,1},Global=>false,Degrees=>{0,1,1}]/a^2
F = map(S,A)
m=matrix"ay,0;x,0;-y,x;0,-y"
isHomogeneous m
E=setupBGG(F,{e,f})
describe E
F = symmetricToExterior m
--E = (ZZ/101 [a, e, f, Weights=>{-1,1,1},Global=>false,Degrees => {{0}, {1}, {1}}, SkewCommutative => {1, 2}])/(a^2)
setMaxIdeal(ideal(a,e,f))
F = substitute(F,E)
sF = matrix entries syz F
sF = localMingens sF
localResolution(coker sF, LengthLimit=>10)

syz oo
ker F
prune F
res coker oo

A = ZZ/101[a,MonomialOrder=>Weights=>{-1},Global=>false]
B = A[x,y,z]

----------------------------
-- below this is possibly:
-- under development by dan
--------------------------------

newPackage "LocalRings"
export {"LocalRing","localRing","ambientRing","maxIdeal"}
LocalRing = new Type of EngineRing
debug Core
presentation LocalRing := (R) -> presentation R.ambientRing
generators LocalRing := opts -> (R) -> generators R.ambientRing
isCommutative LocalRing := (R) -> isCommutative R.ambientRing
degreeLength LocalRing := (R) -> degreeLength R.ambientRing
numgens LocalRing := (R) -> numgens R.ambientRing
-- promote(ZZ,LocalRing) := promote(RingElement,LocalRing) := (r,R) -> promote(r,R.ambientRing)

localRing = method()

localRing(EngineRing,Ideal) := (R,m) -> (                                            -- R = poly ring, m = max ideal
     S := new LocalRing from raw R;
     S.ambientRing = R;
     S.maxIdeal = m;
     S.baseRings = append(R.baseRings, R);
     commonEngineRingInitializations S;
     expression S := s -> expression lift(s,R);
     S)
endPackage "LocalRings"

-- end
-- try it out immediately

errorDepth = 0
R = localRing (QQ[x,y], ideal(x,y))
M = R^4
C = res M



--------- potentially needed, or not? ----------------
------------------------------------------------------
LocalRing = new Type of EngineRing
LocalRing.synonym = "Local ring"

expression LocalRing := S -> new FunctionApplication from { localRing, (expression ambient S, S.MaximalIdeal ) }
undocumented (expression, LocalRing)

toExternalString LocalRing := R -> toString expression R
undocumented (toExternalString, LocalRing),

debug Core
toString LocalRing := R -> (
     if hasAttribute(R,ReverseDictionary) then toString getAttribute(R,ReverseDictionary)
     else toString expression R)
undocumented (toString, LocalRing)

net LocalRing := R -> (
     if hasAttribute(R,ReverseDictionary) then toString getAttribute(R,ReverseDictionary)
     else net expression R)
undocumented (net, LocalRing)

LocalRingElement = new Type of RingElement

ambient LocalRing := (AP) -> ring AP.MaximalIdeal

localRing = method()
localRing(Ring,Ideal) := (A,P) -> (
     if not (A.?Engine and A.Engine) 
     then error "expected coefficient ring handled by the engine";
     AP := new LocalRing of LocalRingElement;
     AP.RawRing = frac A;
     AP#1 = 1_AP;
     AP#0 = 0_AP;
     AP.baseRings = append(A.baseRings,A);
     AP.generators = {};
     AP.numgens = numgens A;
     AP.degreeLength = 0;
     AP.MaximalIdeal = P;
     --the basic features of AP are coded at the engine level
     -- TODO: is this true??
     commonEngineRingInitializations AP;
     ONE := AP#1; -- ??
     if A.?char then AP.char = A.char;
     toExternalString AP := r -> toString expression r;
     expression AP := f -> (
         << "called expression" << endl; 
         hold "oops"
         );
     listForm AP := (f) -> (
         << "called listForm" << endl; 
         {});
     AP.use = AP -> (); -- todo
     AP
     )
