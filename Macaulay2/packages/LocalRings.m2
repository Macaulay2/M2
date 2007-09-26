newPackage(
	"LocalRings",
    	Version => "1.0", 
    	Date => "September 27, 2006",
    	Authors => {
	     {Name => "David Eisenbud", Email => "de@msri.org", HomePage => "http://www.msri.org/~de/"},
	     {Name => "Mike Stillman", Email => "", HomePage => ""}
	     },
    	Headline => "Local rings at the origin",
    	DebuggingMode => true
    	)

export {
     setMaxIdeal,
     localComplement,
     localsyz,
     localMingens,
     localModulo,
     localPrune,
     localResolution,
     residueMap,
     maxIdeal
     }

-- This routine, localRing, is not functional yet -----
--localRing = new method(
--     Options => {
--	  MonomialOrder => null
--	  }
--     )
--localRing Ring := R -> localRing(R, ideal vars R)
--localRing(Ring,Ideal) := (R,P) -> (
--     R	-- at first, we do nothing
--     )
--------------------------------------------------------
setMaxIdeal = method()
setMaxIdeal(Ideal) := (maxI) -> (
     R := ring maxI;
     R.residueMap = map(R,R,vars R % maxI);
     R.maxIdeal = maxI
     )

localComplement = method()
localComplement Matrix := Matrix => (m) -> (
     n := transpose syz transpose ((ring m).residueMap m);
     id_(target n) // n)

defaultResolutionLength := (R) -> (
     numgens R + 1 + if ZZ === ultimate(coefficientRing, R) then 1 else 0
     )

resolutionLength := (R,options) -> (
     if options.LengthLimit == infinity then defaultResolutionLength R else
options.LengthLimit
     )

localsyz = method()
localsyz(Matrix) := (m) -> (
     localMingens syz m
     --C = res(coker m,LengthLimit=>3);
     --C.dd_2 * localComplement(C.dd_3)
     )

localMingens = method()
localMingens(Matrix) := Matrix => (m) -> (
     -- warning: this routine should perhaps take a Module...
     m * localComplement syz m
     )

localModulo = method()
localModulo(Matrix,Matrix) := Matrix => (m,n) -> (
     P := target m;
     Q := target n;
     if P != Q then error "expected maps with the same target";
     if not isFreeModule P or not isFreeModule Q
     or not isFreeModule source m or not isFreeModule source n
     then error "expected maps between free modules";
     localMingens syz(m|n, SyzygyRows => numgens source m)
     )


localPrune = method()
localPrune Module := (M) -> (
     p = presentation M;
     p1 = localComplement p;
     p2 = localModulo(p1,p);
     N := coker(p2);
     N.cache.pruningMap = map(M,N,p1);
     N
     )

localResolution = method(Options => options resolution)
localResolution Module := options -> (M) -> (
     R := ring M;
     maxlength := resolutionLength(R,options);
     if M.cache.?resolution
     then (C := M.cache.resolution;
     C.length = length C)
     else (
	  C = new ChainComplex;
	  C.ring = R;
	  f := presentation M;
	  C#0 = target f;
	  C#1 = source f;
	  C.dd#1 = f;
	  M.cache.resolution = C;
	  C.length = 1;
	  );
     i := C.length;
     while i < maxlength and C.dd#i != 0 do (
	  g := localsyz C.dd#i;
	  shield (
	       i = i+1;
	       C.dd#i = g;
	       C#i = source g;
	       C.length = i;
	       );
	  );
     C)

beginDocumentation()

document { Key => LocalRings,
     Headline => "Polynomial rings localized at a maximal ideal",
     EM "LocalRings", " is a package for finding minimal generators, syzygies and resolutions
     for polynomial rings localized at a maximal ideal.",
     }

document { 
     Key => setMaxIdeal,
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }

document { 
     Key => localComplement,
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }

document { 
     Key => localsyz,
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }

document { 
     Key => localMingens,
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }

document { 
     Key => localModulo,
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }

document { 
     Key => localPrune,
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }

document { 
     Key => localResolution,
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }

end
-- ###
restart
loadPackage "LocalRings"
kk=ZZ/101
S=kk[t,x,y,z]
setMaxIdeal ideal vars S
assert(S.residueMap == map(S,S,{0,0,0,0}))

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
export {LocalRing,localRing,ambientRing,maxIdeal}
LocalRing = new Type of EngineRing
debug Core
presentation LocalRing := (R) -> presentation R.ambientRing
generators LocalRing := opts -> (R) -> generators R.ambientRing
isCommutative LocalRing := (R) -> isCommutative R.ambientRing
degreeLength LocalRing := (R) -> degreeLength R.ambientRing
numgens LocalRing := (R) -> numgens R.ambientRing
-- promote(ZZ,LocalRing) := promote(RingElement,LocalRing) := (r,R) -> promote(r,R.ambientRing)

localRing = method()

localRing(EngineRing,Ideal) := (R,m) -> (					    -- R = poly ring, m = max ideal
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

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages NAMEOFPACKAGE=Local install-one"
-- End:
