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
     else if R.?name then (
	  if 1 === #R.name then R.name
	  else "\\text{" | R.name | "}"
	  )
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
	       ZZn.name = "ZZ[]";
	       ZZn)
	  else (
	       Zn = degreesMonoid n;
	       ZZn = ZZ Zn;
	       ZZn.name = "ZZ[" | Zn.name | "]";
     	       use ZZn;
	       ZZn)))

degreesRing PolynomialRing := PolynomialRing => R -> (
     if R.?degreesRing then R.degreesRing
     else degreesRing degreeLength R)

generators PolynomialRing := R -> R.generators
coefficientRing PolynomialRing := Ring => R -> last R.baseRings
allGenerators PolynomialRing := R -> join(
     apply(allGenerators coefficientRing R, a -> a * 1_R),
     generators R)
isHomogeneous PolynomialRing := R -> (
     k := coefficientRing R;
     isField k or isHomogeneous k)

standardForm RingElement := (f) -> (
     R := ring f;
     k := coefficientRing R;
     (cc,mm) := rawPairs raw f;
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
     (cc,mm) := rawPairs raw f;
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
commden := (f) -> (
     lcm apply(
	  first entries lift((coefficients f)#1, QQ),
	  denominator))

indices := (M,vars) -> apply(vars, x -> (
	  x = baseName x;
	  if M.index#?x then M.index#x
	  else error "expected a variable of the ring"))

Ring OrderedMonoid := PolynomialRing => (			  -- no memoize
     (R,M) -> (
	  if not M.?RawMonoid then error "expected ordered monoid handled by the engine";
	  if not R.?RawRing then error "expected coefficient ring handled by the engine";
	  Weyl := M.Options.WeylAlgebra =!= {};
	  Skew := M.Options.SkewCommutative;
	  degRing := if degreeLength M != 0 then degreesRing degreeLength M else ZZ;
	  RM := if Weyl then (
	       diffs := M.Options.WeylAlgebra;
	       if class diffs === Option 
	       then diffs = {diffs}
	       else if class diffs =!= List 
	       then error "expected list as WeylAlgebra option";
	       diffs = apply(diffs, x -> if class x === Option then toList x else x);
	       h    := select(diffs, x -> class x =!= List);
	       if #h > 1 then error "WeylAlgebra: expected at most one homogenizing element";
	       h = indices(M,h);
	       if #h === 1 then h = h#0 else h = -1;
	       diffs = select(diffs, x -> class x === List);
	       diffs = apply(diffs, x -> (
			 if #x =!= 2 then error "WeylAlgebra: expected x=>dx, {x,dx}";
			 if class x#0 === Sequence and class x#1 === Sequence
			 then (
			      if #(x#0) =!= #(x#1) then error "WeylAlgebra: expected sequences of the same length";
			      mingle x
			      )
			 else toList x
			 ));
	       diffs = flatten diffs;
	       local diffs0;
	       local diffs1;
	       diffs = pack(2,diffs);
	       diffs0 = indices(M,first\diffs);
	       diffs1 = indices(M,last\diffs);
	       scan(diffs0,diffs1,(x,dx) -> 
		    if not x<dx
		    then error "WeylAlgebra: expected differentiation variables to occur to the right of their variables"
		    );
	       new PolynomialRing from rawWeylAlgebra(rawPolynomialRing(R.RawRing,M.RawMonoid),diffs0,diffs1,h)
	       )
	  else if Skew then (
	       skews := (
		    if M.Options.SkewCommutative === true
		    then toList (0 .. numgens M - 1)
		    else if class M.Options.SkewCommutative === List
		    then indices(M,M.Options.SkewCommutative)
		    else error "expected SkewCommutative option to be 'true' or a list of variables"
		    );
	       new PolynomialRing from rawSkewPolynomialRing(rawPolynomialRing(R.RawRing,M.RawMonoid),skews)
	       )
	  else (
	       rawRM := rawPolynomialRing(raw R, raw M);
	       if class R === QuotientRing and class ultimate(ambient,R) === PolynomialRing then rawRM = rawQuotientRing(rawRM, raw R);
	       new PolynomialRing from rawRM
	       );
	  RM.baseRings = append(R.baseRings,R);
	  RM.monoid = M;
	  RM.Adjust = (options M).Adjust;
	  RM.Repair = (options M).Repair;
	  RM.degreesRing = degRing;
	  RM.isCommutative = not Weyl and M.Options.SkewCommutative === false;
     	  ONE := RM#1;
	  if R.?char then RM.char = R.char;
	  RM ? RM := (f,g) -> rawCompareMonomial(raw M, rawLeadMonomial raw f, rawLeadMonomial raw g); -- this is wrong, as the monoid doesn't know the ordering in the ring!  Mike.
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
	  RM _ M := (f,m) -> new R from rawCoefficient(f.RawRingElement, m.RawMonomial);
	  expression RM := f -> (
	       (coeffs,monoms) -> sum(coeffs,monoms, (a,m) -> expression (if a == 1 then 1 else new R from a) * expression (if m == 1 then 1 else new M from m))
	       ) rawPairs f.RawRingElement;
	  toString RM := toExternalString RM := x -> toString expression x;
	  fac := options -> f -> (
	       facs := rawFactor raw f;
	       new Product from apply(facs#0,facs#1,(p,n) -> new Power from {new RM from p,n})
	       );
	  factor RM := if R === QQ then (
	       -- for factoring over QQ we find a commond denominator ourselves and reduce the problem to factoring over ZZ
	       options -> f -> (
	       	    d := commden f;
		    f = d * f;
		    s := (fac options) f;
		    if d === 1 then s
		    else (
			 if degree first last s == {0}
			 then s = append(drop(s,-1), new Power from { (1/d) * first last s, 1 })
			 else s = append(s,          new Power from { (1/d) * 1_RM        , 1 })
			 )
		    )
	       )
	  else fac;
	  isPrime RM := f -> (
	       v := factor f;				    -- constant term last
	       #v === 1 and last v#0 === 1 and not isConstant first v#0
	       or
	       #v === 2 and v#0#1 === 1 and isConstant first v#0
	       );
	  RM.generatorSymbols = M.generatorSymbols;
	  RM.generatorExpressions = M.generatorExpressions;
	  RM.generators = apply(# M.generators, i -> RM#(toString M.generators#i) = RM_i);
	  gt := apply(RM.generatorSymbols, RM.generators, (v,x) -> v => x);
	  RM.generatorsTable = (
	       if R.?generatorsTable 
	       then hashTable join(
		    apply(pairs M.generatorsTable,
			 (v,x) -> v => new RM from rawTerm(RM.RawRing, (0_R).RawRingElement, x.RawMonomial)),
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
dim PolynomialRing := R -> (
     if (options R).SkewCommutative then dim coefficientRing R
     else dim coefficientRing R + # generators R
     )
char PolynomialRing := (R) -> char coefficientRing R
numgens PolynomialRing := R -> numgens monoid R

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
