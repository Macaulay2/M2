--		Copyright 1996-2006 by Daniel R. Grayson and Michael E. Stillman

QuotientRing = new Type of EngineRing
QuotientRing.synonym = "quotient ring"
ideal QuotientRing := R -> R.ideal
isQuotientRing = method(TypicalValue => Boolean)
monoid QuotientRing := o -> R -> R.monoid
isQuotientRing Ring := R -> false
isQuotientRing QuotientRing := R -> true
coefficientRing QuotientRing := (cacheValue coefficientRing) (R -> coefficientRing ambient R)
options QuotientRing := R -> options ambient R
isHomogeneous QuotientRing := R -> isHomogeneous ideal R
isQuotientOf = method(TypicalValue => Boolean)
isQuotientOf(Ring,Ring) := (R,S) -> false
isQuotientOf(Ring,QuotientRing) := (R,S) -> R === ambient S or isQuotientOf(R,ambient S)
isQuotientOf(Type,Ring) := (X,S) -> false
isQuotientOf(Type,QuotientRing) := (X,S) -> instance(ambient S,X) or isQuotientOf(X,ambient S)
degreeLength QuotientRing := S -> degreeLength ambient S
vars QuotientRing := (cacheValue vars) (S -> map(S^1,, table (1, numgens S, (i,j) -> S_j)))
numgens QuotientRing := (cacheValue numgens) (S -> numgens ambient S)
pretty := relns -> (
     s := toSequence flatten entries relns;
     if #s === 1 then s = first s;
     s)
toExternalString QuotientRing := S -> toString describe S
random QuotientRing := opts -> S -> (
     if S.baseRings === {ZZ} then (random char S)_S
     else notImplemented())
expression QuotientRing := S -> if hasAttribute(S,ReverseDictionary) then expression getAttribute(S,ReverseDictionary) else new Divide from { unhold expression ambient S, unhold expression pretty S.relations }
describe QuotientRing := S -> Describe Divide { expression ambient S, expression pretty S.relations }
ambient PolynomialRing := R -> R
ambient QuotientRing := Ring => (cacheValue ambient) (R -> last R.baseRings)
degrees QuotientRing := R -> degrees ambient R
precision QuotientRing := precision @@ ambient
isSkewCommutative QuotientRing := R -> isSkewCommutative ambient R

Ring / Module := QuotientRing => (R,I) -> (
     if ambient I != R^1 or I.?relations
     then error ("expected ", toString I, " to be an ideal of ", toString R);
     R / ideal I)

savedQuotients = new MutableHashTable

liftZZmodQQ := (r,S) -> (
     needsPackage "LLLBases";
     v := (value getGlobalSymbol "LLL") syz matrix {{-1,lift(r,ZZ),char ring r}};
     v_(0,0) / v_(1,0))

--------------------------------
ZZp = method(Options=> {Strategy => null}) -- values allowed: "Flint", "Ffpack", "Aring", "Old".
ZZp ZZ := opts -> (n) -> ZZp(ideal n, opts)
ZZp Ideal := opts -> (I) -> (
      typ := opts#Strategy;
      if typ === null then typ = "Flint";
     gensI := generators I;
     if ring gensI =!= ZZ then error "expected an ideal of ZZ";
     n := gcd flatten entries gensI;
     if n < 0 then n = -n;
     if n === 0 then 
         ZZ
     else if savedQuotients#?(typ, n) then
         savedQuotients#(typ, n)
     else (
	  if not isPrime n
	  then error ("ZZ/n not implemented yet for composite n = ", toString n);
	  S := new QuotientRing from 
	    if typ === "Ffpack" then rawARingGaloisField(n,1)  
        else if typ === "Flint" then rawARingZZpFlint n
        else if typ === "Aring" then rawARingZZp n
        else if typ === "Old" then rawZZp n
        else error("unknown implementation choice: "|typ|///. Choices are "Flint" (default), "Ffpack", "Aring", "Old"///);
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
	  fraction(S,S) := S / S := (x,y) -> x//y;
	  S.frac = S;		  -- ZZ/n with n PRIME!
      savedQuotients#(typ, n) = S;
      lift(S,QQ) := opts -> liftZZmodQQ;
	  S))

isFinitePrimeField = F -> isQuotientRing F and ambient F === ZZ and F.?char

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
--------------------------------

ZZquotient := (R,I) -> (
     gensI := generators I;
     if ring gensI =!= ZZ then error "expected an ideal of ZZ";
     n := gcd flatten entries gensI;
     if n < 0 then n = -n;
     if n === 0 then ZZ
     else if savedQuotients#?n 
     then savedQuotients#n
     else (
	  if n > 32767 then error "large characteristics not implemented yet";
	  if n > 1 and not isPrime n
	  then error "ZZ/n not implemented yet for composite n";
	  S := new QuotientRing from rawZZp n;
	  S.cache = new CacheTable;
	  S.isBasic = true;
	  S.ideal = I;
	  S.baseRings = {R};
     	  commonEngineRingInitializations S;
          initializeEngineLinearAlgebra S;
	  S.relations = gensI;
	  S.isCommutative = true;
	  S.presentation = matrix{{n}};
	  S.order = S.char = n;
	  S.dim = 0;					    -- n != 0 and n!= 1
	  expression S := x -> expression rawToInteger raw x;
	  fraction(S,S) := S / S := (x,y) -> x//y;
	  S.frac = S;		  -- ZZ/n with n PRIME!
	  savedQuotients#n = S;
	  lift(S,QQ) := opts -> liftZZmodQQ;
	  S))

Ring / Ideal := QuotientRing => (R,I) -> I.cache.QuotientRing = (
     if ring I =!= R then error "expected ideal of the same ring";
     if I.cache.?QuotientRing then return I.cache.QuotientRing;
     if I == 0 then return R;
     if R === ZZ then return ZZp(I);
     error "can't form quotient of this ring";
     )

predecessors := method()
predecessors Ring := R -> {R}
predecessors QuotientRing := R -> append(predecessors last R.baseRings, R)

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
     S.basering = R.basering;
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
     if R.?generatorExpressions then S.generatorExpressions = (
	  R.generatorExpressions
	  -- apply(R.generatorExpressions,S.generators,(e,x) -> new Holder2 from {e#0,x})
	  );
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

Ring / List := Ring / Sequence := QuotientRing => (R,f) -> R / promote(ideal f, R)

presentation PolynomialRing := Matrix => R -> map(R^1, R^0, 0)
presentation QuotientRing   := Matrix => R -> (
     if R.?presentation then R.presentation else R.presentation = (
	  S := ambient R;
	  f := generators ideal R;
	  while class S === QuotientRing do (		    -- untested code
	       f = lift(f,ambient S) | generators ideal S;
	       S = ambient S;
	       );
	  f
	  )
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

dim QuotientRing := (R) -> (
     if isField R then 0
     else if R.?SkewCommutative then notImplemented()
     else (
	  I := flattenRing(R, Result => Ideal);
	  cd := codim I;
	  if cd === infinity then -1 else dim ring I - codim I
	  )
     )

monoid QuotientRing := o -> (cacheValue monoid) (S -> monoid ambient S)
degreesMonoid QuotientRing := (cacheValue degreesMonoid) (S -> degreesMonoid ambient S)
degreesRing QuotientRing := (cacheValue degreesRing) (S -> degreesRing ambient S)
QuotientRing_String := (S,s) -> if S#?s then S#s else (
     R := ultimate(ambient, S);
     S#s = promote(R_s, S))

generators QuotientRing := opts -> (S) -> (
     if opts.CoefficientRing === S then {}
     else apply(generators(ambient S,opts), m -> promote(m,S)))
char QuotientRing := (stashValue symbol char) ((S) -> (
     p := char ambient S;
     if p == 1 then return 1;
     if isPrime p or member(QQ,S.baseRings) then return if S == 0 then 1 else p;
     relns := presentation S;
     if relns == 0 then return char ring relns;
     if coefficientRing S =!= ZZ then notImplemented();
     g := generators gb relns;
     if g == 0 then return char ring g;
     m := g_(0,0);
     if liftable(m,ZZ) then lift(m,ZZ) else 0))
singularLocus(Ring) := QuotientRing => (R) -> (
     f := presentation R;
     A := ring f;
     A / (ideal f + minors(codim(R,Generic=>true), jacobian presentation R)))

singularLocus(Ideal) := QuotientRing => (I) -> singularLocus(ring I / I)

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
nextPrime Number := n -> (
   n0 := ceiling n;
   if n0 <= 2 then return 2;
   if even n0 then n0=n0+1;
   while not isPrime n0 do n0=n0+2;
   n0
   )


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
