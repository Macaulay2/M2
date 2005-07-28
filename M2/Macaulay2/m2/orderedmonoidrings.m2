--		Copyright 1993-2002 by Daniel R. Grayson

-----------------------------------------------------------------------------

Monoid = new Type of Type
Monoid.synonym = "monoid"

options Monoid := x -> null

baseName Symbol := identity

OrderedMonoid = new Type of Monoid
OrderedMonoid.synonym = "ordered monoid"
degreeLength OrderedMonoid := M -> M.degreeLength

-----------------------------------------------------------------------------

terms := symbol terms
PolynomialRing = new Type of EngineRing
PolynomialRing.synonym = "polynomial ring"

isPolynomialRing = method(TypicalValue => Boolean)
isPolynomialRing Thing := x -> false
isPolynomialRing PolynomialRing := (R) -> true

exponents RingElement := (f) -> listForm f / ( (monom,coeff) -> monom )

describe PolynomialRing := R -> net expression R
expression PolynomialRing := R -> (expression last R.baseRings) (expression monoid R)

tex PolynomialRing := R -> "$" | texMath R | "$"	    -- silly!

texMath PolynomialRing := R -> (
     if R.?tex then R.tex
     else if ReverseDictionary#?R then "\\text{" | toString ReverseDictionary#R  | "}"
     else (texMath last R.baseRings)|(texMath expression monoid R)
     )

net PolynomialRing := R -> (
     if ReverseDictionary#?R then toString ReverseDictionary#R
     else net expression R)
toString PolynomialRing := R -> (
     if ReverseDictionary#?R then toString ReverseDictionary#R
     else toString expression R)
toExternalString PolynomialRing := R -> toString expression R

degreeLength PolynomialRing := (RM) -> degreeLength monoid RM

degreesRing ZZ := PolynomialRing => memoize(
     (n) -> (
	  local ZZn;
	  local Zn;
	  if n == 0 then (
	       ZZn = new PolynomialRing from rawPolynomialRing();
	       PrintNames#ZZn = "ZZ[]";
	       ZZn.basering = ZZ;
	       ZZn.flatmonoid = ZZn.monoid = monoid[];
	       ZZn.baseRings = {ZZ};
	       ZZn)
	  else (
	       Zn = degreesMonoid n;
	       ZZn = ZZ Zn;
	       ZZn.baseRings = {ZZ};
	       PrintNames#ZZn = "ZZ[" | PrintNames#Zn | "]";
	       ZZn)))

degreesRing PolynomialRing := PolynomialRing => R -> (
     if R.?degreesRing then R.degreesRing
     else degreesRing degreeLength R)

degreesRing Ring := R -> error "no degreesRing for this ring"

generators PolynomialRing := R -> R.generators
coefficientRing PolynomialRing := Ring => R -> last R.baseRings
allGenerators PolynomialRing := R -> join(generators R, apply(allGenerators coefficientRing R, a -> a * 1_R))
isHomogeneous PolynomialRing := R -> (
     k := coefficientRing R;
     isField k or isHomogeneous k)

standardForm RingElement := (f) -> (
     R := ring f;
     k := coefficientRing R;
     (cc,mm) := rawPairs(raw k, raw f);
     new HashTable from toList apply(cc, mm, (c,m) -> (standardForm m, new k from c)))

-- this way turns out to be much slower by a factor of 10
-- standardForm RingElement := (f) -> (
--      R := ring f;
--      k := coefficientRing R;
--      (mm,cc) := coefficients f;
--      new HashTable from apply(
-- 	  flatten entries mm / leadMonomial / raw / standardForm,
-- 	  flatten entries lift(cc, k),
-- 	  identity))

listForm = method()
listForm RingElement := (f) -> (
     R := ring f;
     n := numgens R;
     k := coefficientRing R;
     (cc,mm) := rawPairs(raw k, raw f);
     toList apply(cc, mm, (c,m) -> (exponents(n,m), new k from c)))

-- this way turns out to be much slower by a factor of 10
-- listForm RingElement := (f) -> (
--      R := ring f;
--      k := coefficientRing R;
--      (mm,cc) := coefficients f;
--      reverse apply(
-- 	  flatten entries mm / leadMonomial / exponents,
-- 	  flatten entries lift(cc, k),
-- 	  identity))

lcm2 := (x,y) -> x*y//gcd(x,y)
lcm := args -> (
     n := 1;
     scan(args, i -> n = lcm2(n,i));
     n)
commden := (f) -> lcm apply( first \ entries lift((coefficients f)#1, QQ), denominator)

indices := (M,vars) -> apply(vars, x -> if class x === ZZ then x else (
	  x = baseName x;
	  if M.index#?x then M.index#x
	  else error "expected a variable of the ring or an integer"))

Ring OrderedMonoid := PolynomialRing => (			  -- no memoize
     (R,M) -> (
	  if not M.?RawMonoid then error "expected ordered monoid handled by the engine";
	  if not R.?RawRing then error "expected coefficient ring handled by the engine";
     	  num := numgens M;
	  (basering,flatmonoid) := (
	       if R.?isBasic then (R,M)
	       else if R.?basering and R.?flatmonoid then (R.basering, M ** R.flatmonoid)
	       else if instance(R,FractionField) then (R,M)
	       else error "internal error: expected coefficient ring to have a base ring and a flat monoid"
	       );
	  local basering; local flatmonoid;
	  quotfix := rawRM -> if class R === QuotientRing and class ultimate(ambient,R) === PolynomialRing then rawQuotientRing(rawRM, raw R) else rawRM;
	  Weyl := M.Options.WeylAlgebra =!= {};
	  skews := M.Options.SkewCommutative;
	  degRing := if degreeLength M != 0 then degreesRing degreeLength M else ZZ;
     	  local RM;
	  if Weyl or R.?diffs0 then (
	       diffs := M.Options.WeylAlgebra;
	       if class diffs === Option 
	       then diffs = {diffs}
	       else if class diffs =!= List 
	       then error "expected list as WeylAlgebra option";
	       diffs = apply(diffs, x -> if class x === Option then toList x else x);
	       h    := select(diffs, x -> class x =!= List);
	       if #h > 1 then error "WeylAlgebra: expected at most one homogenizing variable";
	       h = indices(M,h);
	       if #h === 1 then h = h#0 else h = -1;
     	       if R.?h then (
		    if h == -1
		    then h = R.h + num
		    else (
		    	 if R.h + num =!= h then error "expected the same homogenizing variable";
			 )
		    )
	       else (
		    if R.?diffs0 and h != -1 then error "coefficient Weyl algebra has no homogenizing variable";
		    );
	       diffs = select(diffs, x -> class x === List);
	       diffs = apply(diffs, x -> (
			 if #x =!= 2 then error "WeylAlgebra: expected x=>dx, {x,dx}";
			 if class x#0 === Sequence and class x#1 === Sequence
			 then (
			      if #(x#0) =!= #(x#1) then error "expected sequences of the same length";
			      mingle x
			      )
			 else toList x
			 ));
	       diffs = flatten diffs;
	       local diffs0; local diffs1;
	       diffs = pack(2,diffs);
	       diffs0 = indices(M,first\diffs);
	       diffs1 = indices(M,last\diffs);
	       if R.?diffs0 and R.?diffs1 then (
		    diffs0 = join(diffs0, apply(R.diffs0, i -> i + num));
		    diffs1 = join(diffs1, apply(R.diffs1, i -> i + num));
		    );
	       scan(diffs0,diffs1,(x,dx) -> if not x<dx then error "expected differentiation variables to occur to the right of their variables");
	       if R.?SkewCommutative then error "coefficient ring has skew commuting variables";
	       RM = new PolynomialRing from quotfix rawWeylAlgebra(rawPolynomialRing(raw basering, raw flatmonoid),diffs0,diffs1,h);
	       RM.diffs0 = diffs0;
	       RM.diffs1 = diffs1;
     	       if h != -1 then RM.h = h;
	       )
	  else if skews =!= false or R.?SkewCommutative then (
	       if R.?diffs0 then error "coefficient ring is a Weyl algebra";
	       skews = (
		    if skews === false then {}
		    else if skews === true then toList (0 .. num - 1)
		    else if class skews === List and all(skews, i -> class i === ZZ or class i === Symbol or instance(i,RingElement))
		    then (
			 skews = skews / (i -> if instance(i,RingElement) then baseName i else i);
			 indices(M,skews)
			 )
		    else error "expected SkewCommutative option to be true, false, or a list of variables or integers"
		    );
	       if R.?SkewCommutative then skews = join(skews, apply(R.SkewCommutative, i -> i + num));
	       RM = new PolynomialRing from quotfix rawSkewPolynomialRing(rawPolynomialRing(raw basering, raw flatmonoid),skews);
	       if #skews > 0 then RM.SkewCommutative = skews;
	       )
	  else (
	       RM = new PolynomialRing from quotfix rawPolynomialRing(raw basering, raw flatmonoid);
	       );
	  RM.basering = basering;
	  RM.flatmonoid = flatmonoid;
	  RM.baseRings = append(R.baseRings,R);
	  RM.monoid = M;
	  RM.Adjust = (options M).Adjust;
	  RM.Repair = (options M).Repair;
	  RM.degreesRing = degRing;
	  RM.isCommutative = not Weyl and not RM.?SkewCommutative;
     	  ONE := RM#1;
	  if R.?char then RM.char = R.char;
	  RM ? RM := (f,g) -> raw f ? raw g;
	  R * M := (r,m) -> new RM from rawTerm(RM.RawRing,raw r,m.RawMonomial);
	  M * R := (m,r) -> new RM from rawTerm(RM.RawRing,raw r,m.RawMonomial);
	  RM * M := (p,m) -> p * (R#1 * m);
	  M * RM := (m,p) -> (R#1 * m) * p;
	  M / RM := (m,f) -> (m * ONE) / f;
	  M / R := (m,r) -> (m * ONE) / (r * ONE);
	  RM / M := (f,m) -> f / (m * ONE);
	  R / M := (r,m) -> (r * ONE) / (m * ONE);
	  M % RM := (m,f) -> (m * ONE) % f;
	  M % R := (m,r) -> (m * ONE) % (r * ONE);
	  RM % M := (f,m) -> f % (m * ONE);
	  R % M := (r,m) -> (r * ONE) % (m * ONE);
	  R + M := (r,m) -> r * M#1 + R#1 * m;
	  M + R := (m,r) -> r * M#1 + R#1 * m;
	  RM + M := (p,m) -> p + R#1 * m;
	  M + RM := (m,p) -> p + R#1 * m;
	  R - M := (r,m) -> r * M#1 - R#1 * m;
	  M - R := (m,r) -> R#1 * m - r * M#1;
	  RM - M := (p,m) -> p - R#1 * m;
	  M - RM := (m,p) -> R#1 * m - p;
	  RM _ M := (f,m) -> new R from rawCoefficient(R.RawRing, f.RawRingElement, m.RawMonomial);
	  expression RM := f -> (
	       (coeffs,monoms) -> (
		    if #coeffs === 0
		    then expression 0
		    else sum(coeffs,monoms, (a,m) -> expression (if a == 1 then 1 else new R from a) * expression (if m == 1 then 1 else new M from m))
		    )
	       ) rawPairs(raw R, raw f);
	  toString RM := toExternalString RM := x -> toString expression x;
	  factor RM := options -> f -> (
	       c := 1;
	       if R === QQ then (
	       	    d := commden f;
		    c = 1/d;
		    f = d * f;
		    );
	       (facs,exps) := rawFactor raw f;	-- example value: ((11, x+1, x-1, 2x+3), (1, 1, 1, 1)); constant term is first, if there is one
     	       facs = apply(facs, p -> new RM from p);
	       if degree facs#0 === {0} then (
	       	    assert(exps#0 == 1);
		    c = c * facs#0;
		    facs = drop(facs,1);
		    exps = drop(exps,1);
		    );
	       if c != 1 then (
		    facs = append(facs,c);
		    exps = append(exps,1);
		    );
	       new Product from apply(facs,exps,(p,n) -> new Power from {p,n}));
	  isPrime RM := f -> (
	       v := factor f;				    -- constant term last
	       #v === 1 and last v#0 === 1 and not isConstant first v#0
	       or
	       #v === 2 and v#0#1 === 1 and isConstant first v#0 and v#1#1 === 1
	       );
	  RM.generatorSymbols = M.generatorSymbols;
	  RM.generatorExpressions = M.generatorExpressions;
	  RM.generators = apply(# M.generators, i -> RM#(toString M.generators#i) = RM_i);
	  gt := apply(RM.generatorSymbols, RM.generators, (v,x) -> v => x);
	  RM.generatorsTable = (
	       if R.?generatorsTable 
	       then hashTable join(
		    apply(pairs M.generatorsTable, (v,x) -> v => new RM from rawTerm(RM.RawRing, (1_R).RawRingElement, x.RawMonomial)),
		    gt)
	       else hashTable gt
	       );
	  scan(keys R, k -> if class k === String then RM#k = new RM from rawPromote(raw RM, raw R#k));
	  RM.use = x -> (
	       M + M := (m,n) -> R#1 * m + R#1 * n;
	       M - M := (m,n) -> R#1 * m - R#1 * n;
	       - M := (m,n) -> - R#1 * n;
	       scan(RM.baseRings, A -> (
		    if A =!= R then (
		    	 A * M := (i,m) -> (i * R#1) * m;
		    	 M * A := (m,i) -> m * (i * R#1);
			 );
		    A + M := (i,m) -> i * ONE + m * ONE;
		    M + A := (m,i) -> m * ONE + i * ONE;
		    A - M := (i,m) -> i * ONE - m * ONE;
		    M - A := (m,i) -> m * ONE - i * ONE;
		    M / A := (m,r) -> (m * ONE) / (r * ONE);
		    A / M := (r,m) -> (r * ONE) / (m * ONE);
		    M % A := (m,r) -> (m * ONE) % (r * ONE);
		    A % M := (r,m) -> (r * ONE) % (m * ONE);
		    ));
	       RM);
	  RM
	  )
     )

samering := (f,g) -> (
     if ring f =!= ring g then error "expected elements from the same ring";
     )

Ring Array := PolynomialRing => (R,variables) -> use R monoid variables
PolynomialRing _ List := (R,v) -> product ( #v , i -> R_i^(v#i) )
Ring _ List := RingElement => (R,w) -> product(#w, i -> (R_i)^(w_i))
dim PolynomialRing := R -> dim coefficientRing R + # generators R - if R.?SkewCommutative then #R.SkewCommutative else 0
char PolynomialRing := (R) -> char coefficientRing R
numgens PolynomialRing := R -> numgens monoid R

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
