--		Copyright 1993-1999 by Daniel R. Grayson

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

expression PolynomialRing := R -> (
     if R.?name
     then hold R.name
     else (expression last R.baseRings) (expression monoid R)
     )

tex PolynomialRing := R -> "$" | texMath R | "$"	    -- silly!

texMath PolynomialRing := R -> (
     if R.?tex then R.tex
     else if R.?name then (
	  if 1 === #R.name then R.name
	  else "\\text{" | R.name | "}"
	  )
     else (texMath last R.baseRings)|(texMath expression monoid R)
     )

net PolynomialRing := R -> net expression R
toString PolynomialRing := toExternalString PolynomialRing := R -> toString expression R

degreeLength PolynomialRing := (RM) -> degreeLength monoid RM

degreesRing2 := memoize(
     (n) -> (
	  Zn := degreesMonoid n;
	  ZZn := ZZ Zn;
	  ZZn.name = "ZZ[" | Zn.name | "]";
     	  use ZZn
	  ))

degreesRing ZZ := PolynomialRing => n -> degreesRing2 n

newDegreesRing2 := memoize(
     (n) -> (
	  Zn := newDegreesMonoid n;
	  ZZn := ZZZ Zn;
	  ZZn.name = "ZZ[" | Zn.name | "]";
	  use ZZn
	  )
     )

newDegreesRing ZZ := n -> newDegreesRing2 n

degreesRing PolynomialRing := PolynomialRing => R -> (
     if R.?degreesRing then R.degreesRing
     else degreesRing degreeLength R)

generators PolynomialRing := R -> R.generators
isHomogeneous PolynomialRing := R -> true
coefficientRing PolynomialRing := Ring => R -> last R.baseRings

standardForm = method()
standardForm RingElement := (f) -> (
     RM := ring f;
     if not isPolynomialRing RM then error "expected a polynomial";
     -- was: convert(RM.standardForm, sendgg(ggPush f, ggtonet))
     c := coefficients f;
     new HashTable from apply(
	  first entries c#0 / leadMonomial / standardForm,
	  first entries lift(c#1, coefficientRing RM),
	  identity))

listForm = method()
listForm RingElement := (f) -> (
     RM := ring f;
     if not isPolynomialRing RM then error "expected a polynomial";
     -- was: convert(RM.listForm, sendgg(ggPush f, ggtonet))
     c := coefficients f;
     apply(
	  first entries c#0 / leadMonomial / exponents,
	  first entries lift(c#1, coefficientRing RM),
	  identity))

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
	  if not (M.?Engine and M.Engine) 
	  then error "expected ordered monoid handled by the engine";
	  if not (R.?Engine and R.Engine) 
	  then error "expected coefficient ring handled by the engine";
	  if R.?newEngine != M.?newEngine
	  then error "expected both ring and monoid to be handled by new engine routines";
	  Weyl := M.Options.WeylAlgebra =!= {};
	  Skew := M.Options.SkewCommutative =!= false;
	  degRing := (
	       if M.?newEngine or R.?newEngine
	       then if degreeLength M != 0 then newDegreesRing degreeLength M else ZZZ
	       else if degreeLength M != 0 then degreesRing degreeLength M else ZZ
	       );
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
	       if R.?newEngine then (
		    diffs = pack(2,diffs);
		    diffs0 = indices(M,first\diffs);
		    diffs1 = indices(M,last\diffs);
		    scan(diffs0,diffs1,(x,dx) -> 
			 if not x<dx
			 then error "WeylAlgebra: expected differentiation variables to occur to the right of their variables"
			 );
	       	    new PolynomialRing from (
		    	 ggPush degRing, 
		    	 ggPush flatten (options M).Degrees, 
			 ggPush R, ggPush M, 
			 ggPush diffs0,
			 ggPush diffs1,
			 ggPush h,
			 ggweylalgebra)
		    )
	       else (
		    diffs = pack(2,diffs);
		    diffs0 = indices(M,first\diffs);
		    diffs1 = indices(M,last\diffs);
		    scan(diffs0,diffs1,(x,dx) -> 
			 if not x<dx
			 then error "WeylAlgebra: expected differentiation variables to occur to the right of their variables"
			 );
	       	    new PolynomialRing from (
			 ggPush R, ggPush M, 
			 ggPush diffs0,
			 ggPush diffs1,
			 ggPush h,
			 ggweylalgebra)
--	       	    diffs = indices(M,diffs);
--	       	    new PolynomialRing from (ggPush R, ggPush M, ggPush diffs, ggweylalgebra)
		    )
	       )
	  else if Skew then (
	       if M.?newEngine
	       then (
		    skews := (
		    	 if M.Options.SkewCommutative === true
			 then toList (0 .. numgens M - 1)
			 else if class M.Options.SkewCommutative === List
			 then indices(M,M.Options.SkewCommutative)
			 else error "expected SkewCommutative option to be 'true' or a list of variables"
			 );
		    new PolynomialRing from (
		    	 ggPush degRing, 
		    	 ggPush flatten (options M).Degrees, 
		    	 ggPush R, 
		    	 ggPush M,
		    	 ggPush skews,
		    	 ggskewpolyring
		    ))
	       else new PolynomialRing from (ggPush R, ggPush M, ggpolyring)
	       )
	  else (
	       if M.?newEngine
	       then new PolynomialRing from (
		    ggPush degRing, 
		    ggPush flatten (options M).Degrees, 
		    ggPush R, 
		    ggPush M, 
		    ggpolyring
		    )
	       else new PolynomialRing from (ggPush R, ggPush M, ggpolyring)
	       );
	  RM.baseRings = append(R.baseRings,R);
	  if M.?newEngine then RM.newEngine = true;
	  RM.monoid = M;
	  RM.degreesRing = degRing;
	  RM.isCommutative = not Weyl and M.Options.SkewCommutative === false;
     	  ONE := RM#1;
	  if R.?char then RM.char = R.char;
	  RM ? RM := (f,g) -> (
	       sendgg(ggPush f, 
		    ggleadmonom, 
		    ggPush g, 
		    ggleadmonom, 
		    ggcompare);
	       ret := ZZ.pop();
	       if ret === 1 then symbol >
	       else if ret === 0 then symbol ==
	       else symbol <
	       );
	  R * M := (r,m) -> (
	       sendgg(ggPush RM, ggPush r, ggPush m, ggterm);
	       new RM);
	  M * R := (m,r) -> (
	       sendgg(ggPush RM, ggPush r, ggPush m, ggterm);
	       new RM);
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
	  RM _ M := (f,m) -> (
	       sendgg(ggPush f, ggPush m, gggetcoeff);
	       R.pop()
	       );
	  if RM.?newEngine then (
	       RM.ConvertToExpression = ConvertApply(
		    args -> if #args === 1 then args#0 else new Sum from toList args,
		    ConvertRepeat ConvertApply ( 
			 (m,r) -> r * m,
			 ConvertJoin(M.ConvertToExpression, R.ConvertToExpression)));
	       )
	  else (
	       RM.ConvertToExpression = ConvertApply(
		    args -> if #args === 1 then args#0 else new Sum from toList args,
		    ConvertRepeat ConvertApply ( 
			 (m,r) -> r * m,
			 ConvertJoin(M.ConvertToExpression, R.ConvertToExpression)));
	       );
	  expression RM := f -> convert( RM.ConvertToExpression, sendgg(ggPush f, ggtonet) );
	  toString RM := toExternalString RM := x -> toString expression x;
	  net RM := x -> net expression x;
	  fac := options -> f -> (
	       sendgg(ggPush f, ggfactor);
	       new Product from 
	       apply(eePopInt(), i -> ( exp := eePopInt(); new Power from {RM.pop(),exp})));
	  factor RM := if R === QQ then (
	       options -> f -> (
		    error "factorization over QQ not implemented yet";
	       	    d := commden f;
		    f = d * f;
		    new Divide from { (fac options)(f), d }
		    )
	       )
	  else fac;
	  isPrime RM := f -> (
	       v := factor f;		  -- constant term always last
	       #v === 2 and v#1#1 === 1 and v#0#1 === 1
	       );
	  RM.generatorSymbols = M.generatorSymbols;
	  RM.generatorExpressions = M.generatorExpressions;
	  RM.generators = apply(# M.generators, i -> RM#(toString M.generators#i) = RM_i);
	  scan(keys R, k -> if class k === String then RM#k = promote(R#k,RM));
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
     if options.SkewCommutative then dim coefficientRing R
     else dim coefficientRing R + # generators R
     )
char PolynomialRing := (R) -> char coefficientRing R
numgens PolynomialRing := R -> numgens monoid R
