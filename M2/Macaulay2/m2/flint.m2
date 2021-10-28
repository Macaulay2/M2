needs "engine.m2"
needs "enginering.m2"
needs "mutablemat.m2"
needs "quotring.m2" -- for initializeEngineLinearAlgebra

ZZFlintRing = new Type of EngineRing
QQFlintRing = new Type of EngineRing
--toString ZZFlintRing := R -> toString R.RawRing

makeZZFlint = () -> (
     rawF := try rawARingZZFlint() else null;
     if rawF === null then return null;
     F := new ZZFlintRing from rawARingZZFlint();
     F.baseRings = {};
     commonEngineRingInitializations F;
     F.isBasic = true;
     F.dim = 1;
     F.char = 0;
     F.degreeLength = 0;
     --factor F := options -> f -> factor numerator f / factor denominator f;
     toString F := x -> toString expression x;
     net F := x -> net expression x;
     --baseName F := (f) -> (
	 -- if denominator f != 1 
	 -- then error "expected a generator"
	 -- else baseName numerator f);
     expression F := (f) -> toString raw f;
     --numerator F := (f) -> new R from rawNumerator raw f;
     --denominator F := (f) -> new R from rawDenominator raw f;
     --fraction(F,F) := F / F := (x,y) -> if y != 0 then x//y else error "division by 0";
     --fraction(R,R) := (r,s) -> new F from rawFraction(F.RawRing,raw r,raw s);
     --F % F := (x,y) -> if y == 0 then x else 0_F;	    -- not implemented in the engine, for some reason
     F.generators = {};
     --if R.?generatorSymbols then F.generatorSymbols = R.generatorSymbols;
     --if R.?generators then F.generators = apply(R.generators, r -> promote(r,F));
     --if R.?generatorExpressions then F.generatorExpressions = (
	 -- R.generatorExpressions
	 -- -- apply(R.generatorExpressions,F.generators,(e,x)->new Holder2 from {e#0,x})
	 -- );
     --if R.?indexSymbols then F.indexSymbols = applyValues(R.indexSymbols, r -> promote(r,F));
     --if R.?indexStrings then F.indexStrings = applyValues(R.indexStrings, r -> promote(r,F));
     initializeEngineLinearAlgebra F;
     precision F := (f) -> infinity;
     precision ZZFlintRing := F -> infinity;
     F)

ZZFlint = makeZZFlint()
--toString ZZFlintRing := R -> toString R.RawRing

makeQQFlint = () -> (
     rawF := try rawARingQQFlint() else null;
     if rawF === null then return null;
     F := new ZZFlintRing from rawF;
     F.baseRings = {ZZFlint};
     R := ZZFlint;
     R.frac = F;
     commonEngineRingInitializations F;
     F.isBasic = true;
     F.dim = 0;
     F.char = 0;
     F.degreeLength = 0;
     factor F := options -> f -> factor numerator f / factor denominator f;
     toString F := x -> toString expression x;
     net F := x -> net expression x;
     --baseName F := (f) -> (
	 -- if denominator f != 1 
	 -- then error "expected a generator"
	 -- else baseName numerator f);
     expression F := (f) -> toString raw f;
     numerator F := (f) -> new R from rawNumerator raw f;
     denominator F := (f) -> new R from rawDenominator raw f;
     fraction(F,F) := F / F := (x,y) -> if y != 0 then x//y else error "division by 0";
     fraction(R,R) := (r,s) -> new F from rawFraction(F.RawRing,raw r,raw s);
     --F % F := (x,y) -> if y == 0 then x else 0_F;	    -- not implemented in the engine, for some reason
     F.generators = {};
     --if R.?generatorSymbols then F.generatorSymbols = R.generatorSymbols;
     --if R.?generators then F.generators = apply(R.generators, r -> promote(r,F));
     --if R.?generatorExpressions then F.generatorExpressions = (
	 -- R.generatorExpressions
	 -- -- apply(R.generatorExpressions,F.generators,(e,x)->new Holder2 from {e#0,x})
	 -- );
     --if R.?indexSymbols then F.indexSymbols = applyValues(R.indexSymbols, r -> promote(r,F));
     --if R.?indexStrings then F.indexStrings = applyValues(R.indexStrings, r -> promote(r,F));
     initializeEngineLinearAlgebra F;
     F)

QQFlint = makeQQFlint();


end
restart
debug Core

load "~/src/M2-git-linalg/M2/Macaulay2/m2/flint.m2"
R = ZZFlint
a = 1_R
b = 3_R
b^100
a % b -- nope
assert(a // b == 0)
assert(b // a == 3)
3*b -- needs promotion to work...

b % a
M = mutableMatrix(R, 4, 4)
fillMatrix M
M * M
assert(M + M == 2*M)
assert(M + M != 0)
assert(M - M == 0)
assert(3*M == M + M + M)
(3_R) * M 

makeZZFlint = () -> (
  ZZFlint = new Type of EngineRing;
  ZZF = new ZZFlint from rawARingZZFlint();
  )
ZZF.RawRing = 
ZZFlint.RawRing = 
ZZFlint.isBasic = true
new ZZFlint from RawRingElement := (ZZFlint,n) -> 
raw ZZ := x -> rawFromNumber(rawZZ(), x)

ZZFlint#1 = new ZZFlint from rawFromNumber(ZZFlint.RawRing, 1)
ZZFLint#0 = 0
ZZ.char = 0
ZZ.InverseMethod = x -> 1/x
ZZ.dim = 1
ZZ.Engine = true
ZZ.baseRings = {}
ZZ.degreeLength = 0
ZZ.frac = QQ
round ZZ := identity
lift(ZZ,ZZ) := opts -> (i,ZZ) -> i
promote(ZZ,ZZ) := (i,ZZ) -> i
ZZ.random = opts -> ZZ -> rawRandomZZ opts.Height

ZZFlint_0

ZZF = new Type of EngineRing
ZZF1 = new ZZF from rawZZ()
