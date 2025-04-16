--		Copyright 1996-2006 by Daniel R. Grayson and Michael E. Stillman

needs "enginering.m2"
needs "polyrings.m2"
needs "matrix1.m2"

-----------------------------------------------------------------------------
-- QuotientRing type declaration and basic methods
-----------------------------------------------------------------------------

QuotientRing = new Type of EngineRing
QuotientRing.synonym = "quotient ring"

isQuotientRing = method(TypicalValue => Boolean)
isQuotientRing Ring := R -> false
isQuotientRing QuotientRing := R -> true

isFinitePrimeField = method(TypicalValue => Boolean)
isFinitePrimeField Ring := F -> isQuotientRing F and ambient F === ZZ and F.?char

isQuotientOf = method(TypicalValue => Boolean)
isQuotientOf(Ring,Ring) := (R,S) -> false
isQuotientOf(Ring,QuotientRing) := (R,S) -> R === ambient S or isQuotientOf(R,ambient S)
isQuotientOf(Type,Ring) := (X,S) -> false
isQuotientOf(Type,QuotientRing) := (X,S) -> instance(ambient S,X) or isQuotientOf(X,ambient S)

isHomogeneous QuotientRing := R -> isHomogeneous ideal R
isWeylAlgebra QuotientRing := R -> isWeylAlgebra ambient R
isSkewCommutative QuotientRing := R -> isSkewCommutative ambient R

coefficientRing QuotientRing := (cacheValue coefficientRing) (R -> coefficientRing ambient R)
ambient QuotientRing := Ring => (cacheValue ambient) (R -> last R.baseRings)
monoid QuotientRing := o -> (cacheValue monoid) (S -> monoid ambient S)
ideal QuotientRing := R -> R.ideal

degreesRing   QuotientRing := (cacheValue degreesRing)   (S -> degreesRing   ambient S)
degreesMonoid QuotientRing := (cacheValue degreesMonoid) (S -> degreesMonoid ambient S)
degreeLength QuotientRing := S -> degreeLength ambient S
degreeGroup  QuotientRing := S -> degreeGroup  ambient S

degrees QuotientRing := R -> degrees ambient R

precision QuotientRing := precision @@ ambient
numgens QuotientRing := (cacheValue numgens) (S -> numgens ambient S)
options QuotientRing := R -> options ambient R

random QuotientRing := opts -> S -> (
     if S.baseRings === {ZZ} then (random char S)_S
     else notImplemented())

-- printing
printRels := S -> if #(I := toSequence flatten entries S.relations) === 1 then first I else I
describe   QuotientRing := S -> Describe Divide { expression ambient S, expression printRels S }
expression QuotientRing := S -> (
    if hasAttribute(S, ReverseDictionary)
    then expression getAttribute(S, ReverseDictionary)
    else new Divide from { unhold expression ambient S, unhold expression printRels S })
-- TODO: add AfterPrint for QuotientRing

-----------------------------------------------------------------------------

savedQuotients = new MutableHashTable

liftZZmodQQ := (r,S) -> (
     needsPackage "LLLBases";
     v := (value getGlobalSymbol "LLL") syz matrix {{-1,lift(r,ZZ),char ring r}};
     v_(0,0) / v_(1,0))

--------------------------------
ZZp = method(Options=> {Strategy => "Flint"}) -- values allowed: "Flint", "Ffpack", "Aring", "Old".
ZZp ZZ := opts -> (n) -> ZZp(ideal n, opts)
ZZp Ideal := opts -> (I) -> (
     typ := opts#Strategy;
     gensI := generators I;
     if ring gensI =!= ZZ then error "expected an ideal of ZZ";
     n := gcd flatten entries gensI;
     if n < 0 then n = -n;
     if n === 0 then 
         ZZ
     else if savedQuotients#?(typ, n) then
         savedQuotients#(typ, n)
     else (
	  if not isPrime n or n > 2^64
	  then return ZZ[DegreeRank => 0]/n;
	  S := new QuotientRing from 
      if typ === "Ffpack" then rawARingGaloisField(n,1)  
        else if typ === "Flint" then rawARingZZpFlint n
        else if typ === "Aring" then rawARingZZp n
        else if typ === "Old" then rawZZp n
        else error("unknown implementation choice: ", typ, ". ", newline,
	    ///Choices are "Flint" (default), "Ffpack", "Aring", "Old"///);
	  S.cache = new CacheTable;
	  S.isBasic = true;
	  S.ideal = I;
	  S.baseRings = {ZZ};
   	  commonEngineRingInitializations S;
      initializeEngineLinearAlgebra S;
	  S.relations = gensI;
	  S.isCommutative = true;
	  S.presentation = matrix{{n}};
	  S.order = S.char = n;
	  S.dim = 0;					    -- n != 0 and n!= 1
	  expression S := x -> expression rawToInteger raw x;
	  fraction(S,S) := S / S := (x,y) -> if y === 0_S then error "division by zero" else x//y;
	  S.frac = S;		  -- ZZ/n with n PRIME!
      savedQuotients#(typ, n) = S;
      lift(S,QQ) := opts -> liftZZmodQQ;
	  S))

initializeEngineLinearAlgebra = method()
initializeEngineLinearAlgebra Ring := (R) -> (
    R#"EngineLinearAlgebra" = true;
    R.determinant = (f) -> (
        -- The following information 
        -- f is a Matrix in the ring R
        -- f should be a square matrix, with free modules for both source and target
         m := mutableMatrix(f, Dense=>true);
         new R from rawLinAlgDeterminant raw m
         );
    R.inverse = (f) -> (
        A := mutableMatrix(f, Dense=>true);
        R := ring A;
        if numRows A =!= numColumns A then error "expected square matrix";
        matrix map(R,rawLinAlgInverse(raw A))
        );
    R#"solveLinear" = (f,g) -> (
        -- solve f*X = g
        if ring f =!= ring g then error "expected same base rings";
        A := mutableMatrix(f, Dense=>true);
        B := mutableMatrix(g, Dense=>true);
        R := ring A;
        result := map(R,rawLinAlgSolve(raw A,raw B));
        matrix result
        );
    )

isBasicMatrix Matrix := (f) -> isFreeModule source f and isFreeModule target f
basicDet Matrix := (f) -> (
    if not isBasicMatrix f then error "expected a matrix with free source and target";
    m := mutableMatrix(f, Dense=>true);
    promote(rawLinAlgDeterminant raw m, ring f) -- use promote to work with real and complex fields
    )
basicInverse Matrix := (f) -> (
    if not isBasicMatrix f then error "expected a matrix with free source and target";    
    A := mutableMatrix(f, Dense=>true);    
    R := ring A;
    if numRows A =!= numColumns A then error "expected square matrix";
    matrix map(R,rawLinAlgInverse(raw A))
    )
basicRank Matrix := (f) -> (
    if not isBasicMatrix f then error "expected a matrix with free source and target";
    m := mutableMatrix(f, Dense=>true);
    rawLinAlgRank(raw m)
    )

initializeEngineLinearAlgebra QQ

-----------------------------------------------------------------------------
-- Main quotient ring constructor
-----------------------------------------------------------------------------

Ring / Ideal := QuotientRing => (R,I) -> I.cache.QuotientRing = (
     if ring I =!= R then error "expected ideal of the same ring";
     if I.cache.?QuotientRing then return I.cache.QuotientRing;
     if I == 0 then return R;
     if R === ZZ then return ZZp(I);
     error "can't form quotient of this ring";
     )

EngineRing / Ideal := QuotientRing => (R,I) -> I.cache.QuotientRing = (
     if ring I =!= R then error "expected ideal of the same ring";
     if I.cache.?QuotientRing then return I.cache.QuotientRing;
     if I == 0 then return R;
     -- recall that ZZ is NOT an engine ring.
     A := R;
     while class A === QuotientRing do A = last A.baseRings;
     gensI := generators I;
     gensgbI := generators gb gensI;
     S := new QuotientRing from rawQuotientRing(raw R, raw gensgbI);
     S#"raw creation log" = Bag { FunctionApplication {rawQuotientRing, (raw R, raw gensgbI)} };
     S.cache = new CacheTable;
     S.BaseRing   = R.BaseRing;
     S.FlatMonoid = R.FlatMonoid;
     S.numallvars = R.numallvars;
     S.ideal = I;
     S.baseRings = append(R.baseRings,R);
     commonEngineRingInitializations S;
     S.relations = gensI;
     S.isCommutative = R.isCommutative;
     if R.?SkewCommutative then S.SkewCommutative = R.SkewCommutative;
     S.generators = apply(generators S, m -> promote(m,S));
     if R.?generatorSymbols then S.generatorSymbols = R.generatorSymbols;
     if R.?generatorExpressions then S.generatorExpressions = R.generatorExpressions;
     if R.?index        then S.index = R.index;
     if R.?indexStrings then S.indexStrings = applyValues(R.indexStrings, x -> promote(x,S));
     if R.?indexSymbols then S.indexSymbols = applyValues(R.indexSymbols, x -> promote(x,S));
     expression S := lookup(expression,R);
     S.use = x -> (
--	  try monoid S;
--	  if S.?monoid then (
--	       M := S.monoid;
--	       M + M := (m,n) -> S#1 * m + S#1 * n;
--	       M - M := (m,n) -> S#1 * m - S#1 * n;
--	       - M := m -> (- S#1) * m;
--	       scan(S.baseRings, A -> (
--		    A + M := (i,m) -> promote(i, S) + m;
--		    M + A := (m,i) -> m + promote(i, S);
--		    A - M := (i,m) -> promote(i, S) - m;
--		    M - A := (m,i) -> m - promote(i, S);
--		    A * M := (i,m) -> promote(i, S) * m;
--		    M * A := (m,i) -> m * promote(i, S);
--		    ));
--	       );
	  );
     runHooks(R,QuotientRingHook,S);
     S)

Ring / ZZ := (R,f) -> R / ideal f_R

Ring / RingElement := QuotientRing => (R,f) -> (
     try f = promote(f,R);
     if ring f =!= R then error "expected element of the same ring or promotable to it";
     R / ideal f)

Ring / Module := QuotientRing => (R,I) -> (
     if ambient I != R^1 or I.?relations
     then error ("expected ", toString I, " to be an ideal of ", toString R);
     R / ideal I)

Ring / List := Ring / Sequence := QuotientRing => (R,f) -> R / promote(ideal f, R)

-----------------------------------------------------------------------------

presentation PolynomialRing := Matrix => R -> map(R^1, R^0, 0)
presentation QuotientRing   := Matrix => R -> R.presentation ??= (
	  S := ambient R;
	  f := generators ideal R;
	  while class S === QuotientRing do (		    -- untested code
	       f = lift(f,ambient S) | generators ideal S;
	       S = ambient S;
	       );
	  f
    )

presentation(QuotientRing,   QuotientRing)   :=
presentation(QuotientRing,   PolynomialRing) :=
presentation(PolynomialRing, QuotientRing)   :=
presentation(PolynomialRing, PolynomialRing) := (R,S) -> (
     if not (R === S or isQuotientOf(R,S)) then error "expected ring and a quotient ring of it";
     v := map(R^1,R^0,0);
     while S =!= R do (
	  v = v | lift(S.relations,R);
	  S = last S.baseRings;
	  );
     v)

-----------------------------------------------------------------------------

dim QuotientRing := (R) -> (
     if isField R then 0
     else if R.?SkewCommutative then notImplemented()
     else (
	  I := flattenRing(R, Result => Ideal);
	  cd := codim I;
	  if cd === infinity then -1 else dim ring I - codim I
	  )
     )

generators QuotientRing := opts -> (S) -> (
     if opts.CoefficientRing === S then {}
     else apply(generators(ambient S,opts), m -> promote(m,S)))
char QuotientRing := (stashValue symbol char) ((S) -> (
     p := char ambient S;
     if p == 1 then return 1;
     if isPrime p or isMember(QQ,S.baseRings) then return if S == 0 then 1 else p;
     relns := presentation S;
     if relns == 0 then return char ring relns;
     if coefficientRing S =!= ZZ then notImplemented();
     g := generators gb relns;
     if g == 0 then return char ring g;
     m := g_(0,0);
     lift(m,ZZ,Verify=>false) ?? 0))

singularLocus = method()
singularLocus(Ring) := QuotientRing => (R) -> (
     f := presentation R;
     A := ring f;
     A / (ideal f + minors(codim(R,Generic=>true), jacobian presentation R)))

singularLocus(Ideal) := QuotientRing => (I) -> singularLocus(ring I / I)

-----------------------------------------------------------------------------

toField = method()
toField Ring := R -> (
     if isField R then error "toField: ring is already a field";
     if R.?Engine and R.Engine then (
	  S := R[MonomialOrder => Position => Up, DegreeRank => 0, Join => false, DegreeMap => x -> {}, DegreeLift => x -> toList(degreeLength R : 0)];
	  rawDeclareField raw S;
	  remove(S,"has quotient elements");
	  S.toField = true;
	  S.isBasic = true;
	  S / S := (x,y) -> x * y^-1;
	  S.use = S -> if R.?generators and R.?generatorSymbols then scan(R.generatorSymbols,R.generators,(sym,val) -> sym <- promote(val,S));
	  S)
     else error "toField: no method for declaring this type of ring to be a field")

getNonUnit = R -> if R.?Engine and R.Engine then (
     r := rawGetNonUnit raw R;
     if r != 0 then new R from r)

-- nextPrime and getPrimeWithRootOfUnity, written by Frank Schreyer.
nextPrime=method(TypicalValue=>ZZ)
nextPrime Number := nextPrime0 @@ ceiling

getPrimeWithRootOfUnity = method(Options=>{Range=>(10^4,3*10^4)})
getPrimeWithRootOfUnity(ZZ,ZZ) := opt-> (n,r1) -> (
     a := opt.Range_0;
     b := opt.Range_1;
     --(a,b)=(100,200),n=12,r=12
     if b<a+2*log a or b<2 or not ( class a === ZZ and class b === ZZ) then 
         error "the range (a,b) should be an integral interval of diameter b-a > 2*log a";
     if n==2 then return (nextPrime(a+random(b-a)),-1);
     r2 := r1-1;
     primeFactors := apply(toList factor n, f-> first f); -- the prime factors of n
     ds := apply(primeFactors, d->lift(n/d,ZZ)); -- the largest factors of n
     L := toList factor(r2^n-1);
     q := last L;
     p := first q;
     while (
         while p>b or p<a do (
                 if #L>1 and p>=a then (
                     L = delete(q,L);
                     q = last L;
                     p = first q
                     ) 
                 else (
                     r2 = r2+1; 
                     L = toList factor(r2^n-1);
                     q = last L;
                     p = first q
                     );
                 );
         -- r2 is now a root of unity in ZZ/p with a <= p <= b  
         #select(ds, d-> (r2^d)%p == 1) !=0)  --check for primitive
     do ( 
         r2 = r2+1; 
         L = toList factor(r2^n-1);
         q = last L;
         p = first q
         );
     (p,r2)
     );

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
