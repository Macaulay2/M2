--		Copyright 1996-2002 by Daniel R. Grayson and Michael E. Stillman


QuotientRing = new Type of EngineRing
QuotientRing.synonym = "quotient ring"

ideal QuotientRing := R -> R.ideal

isQuotientRing = method(TypicalValue => Boolean)
isQuotientRing Ring := R -> false
isQuotientRing QuotientRing := R -> true
coefficientRing QuotientRing := R -> coefficientRing last R.baseRings
options QuotientRing := R -> options last R.baseRings

isQuotientOf = method(TypicalValue => Boolean)
isQuotientOf(Ring,Ring) := (S,R) -> S === R
isQuotientOf(QuotientRing,Ring) := (S,R) -> (
     S === R or isQuotientOf(last S.baseRings,R)
     )

degreeLength QuotientRing := S -> degreeLength last S.baseRings
vars QuotientRing := S -> (
     if S.?vars 
     then S.vars 
     else S.vars = matrix table (1, numgens S, (i,j) -> S_j)
     )
numgens QuotientRing := S -> numgens last S.baseRings

pretty := relns -> (
     s := toSequence flatten entries relns;
     if #s === 1 then s = first s;
     s)

toString QuotientRing := S -> if S.?name then S.name else (
     concatenate( toString last S.baseRings, "/", toString pretty S.relations)
     )

random QuotientRing := S -> (
     if S.baseRings === {ZZ} then (random char S)_S
     else notImplemented())

expression QuotientRing := S -> (
     if S.?name then hold S.name
     else new Divide from { 
	  expression last S.baseRings,  
	  expression pretty S.relations 
	  }
     )

tex QuotientRing := S -> "$" | texMath S | "$"
texMath QuotientRing := S -> texMath new Divide from { last S.baseRings, pretty S.relations }

net QuotientRing := S -> net expression S

ambient PolynomialRing := R -> R
ambient QuotientRing := Ring => R -> last R.baseRings

isHomogeneous QuotientRing := R -> (
     if R.?isHomogeneous then R.isHomogeneous 
     else R.isHomogeneous = (
	  degreeLength R == 0
	  or
	  isHomogeneous ambient R and isHomogeneous R.relations
	  )
     )

Ring / Module := QuotientRing => (R,I) -> (
     if ambient I != R^1 or I.?relations
     then error ("expected ", toString I, " to be an ideal of ", toString R);
     R / ideal I)

savedQuotients := new MutableHashTable
savedEQuotients := new MutableHashTable

ZZquotient := (R,I) -> (
     if ring I.generators =!= ZZ then error "expected an ideal of ZZ";
     gensI := I.generators;
     gensgbI := generators gb gensI;
     n := if gensgbI == 0 then 0 else gensgbI_(0,0);
     if n < 0 then n = -n;
     if n === 0 then ZZ
     else if savedQuotients#?n 
     then savedQuotients#n
     else (
	  if n > 32767 then error "large characteristics not implemented yet";
	  if n > 1 and not isPrime n
	  then error "ZZ/n not implemented yet for composite n";
	  G := degreesMonoid 0;
	  sendgg(ggPush n, ggPush G, ggcharp);
	  S := new QuotientRing from newHandle();
	  S.ideal = I;
	  S.baseRings = {R};
	  S.relations = gensI;
	  S.isCommutative = R.isCommutative;
	  S.presentation = gensgbI;
	  S.order = S.char = n;
	  if n === 1 then S.dim = -1 else if n === 0 then S.dim = 1 else S.dim = 0;
	  expression S := x -> convert(
	       S.ConvertToExpression, sendgg(ggPush x, ggtonet)
	       );
	  S.frac = S;		  -- ZZ/n with n PRIME!
	  savedQuotients#n = S;
	  S))

Ring / Ideal := QuotientRing => (R,I) -> (
     if ring I =!= R then error "expected ideal of the same ring";
     if I == 0 then return R;
     if R === ZZ then return ZZquotient(R,I);
     error "can't form quotient of this ring";
     )

predecessors := method()
predecessors Ring := R -> {R}
predecessors QuotientRing := R -> append(predecessors last R.baseRings, R)

EngineRing / Ideal := (R,I) -> (
     if ring I =!= R then error "expected ideal of the same ring";
     if I == 0 then return R;
     -- recall that ZZ is NOT an engine ring.
     A := R;
     while class A === QuotientRing do A = last A.baseRings;
     gensI := I.generators ** R;
     gensgbI := generators gb gensI;
     sendgg(ggPush gensgbI, ggqring);
     S := new QuotientRing from newHandle();
     if R.?Adjust then S.Adjust = R.Adjust;
     if R.?Repair then S.Repair = R.Repair;
     S.ideal = I;
     S.baseRings = append(R.baseRings,R);
     S.relations = gensI;
     S.isCommutative = R.isCommutative;
     if R.?generatorSymbols then S.generatorSymbols = R.generatorSymbols;
     if R.?generatorExpressions then (
	  S.generatorExpressions = R.generatorExpressions;
	  scan(R.generatorExpressions, x -> (
		    a := if class x === Symbol then toString x else toString x;
		    S#a = promote(R#a,S);
		    ));
	  );
     S.use = x -> (
	  try monoid S;
	  if S.?monoid then (
	       M := S.monoid;
	       M + M := (m,n) -> S#1 * m + S#1 * n;
	       M - M := (m,n) -> S#1 * m - S#1 * n;
	       - M := m -> (- S#1) * m;
	       scan(S.baseRings, A -> (
		    A + M := (i,m) -> promote(i, S) + m;
		    M + A := (m,i) -> m + promote(i, S);
		    A - M := (i,m) -> promote(i, S) - m;
		    M - A := (m,i) -> m - promote(i, S);
		    A * M := (i,m) -> promote(i, S) * m;
		    M * A := (m,i) -> m * promote(i, S);
		    ));
	       );
	  );
     S)

Ring / ZZ := (R,f) -> R / ideal f_R

Ring / RingElement := Ring / List := Ring / Sequence := QuotientRing => (R,f) -> R / ideal f

presentation QuotientRing := Matrix => R -> (
     if R.?presentation then R.presentation else R.presentation = (
	  S := ambient R;
	  f := gens ideal R;
	  while class S === QuotientRing do (		    -- untested code
	       f = lift(f,ambient S) | gens ideal S;
	       S = ambient S;
	       );
	  f
	  )
     )

presentation PolynomialRing := R -> map(R^1,R^0,0)

presentation(QuotientRing,QuotientRing) := 
presentation(PolynomialRing,QuotientRing) := 
presentation(QuotientRing,PolynomialRing) := 
presentation(PolynomialRing,PolynomialRing) := (R,S) -> (
     if not isQuotientOf(S,R) then error "expected ring and a quotient ring of it";
     v := map(R^1,R^0,0);
     while S =!= R do (
	  v = v | lift(S.relations,R);
	  S = last S.baseRings;
	  );
     v)

codim PolynomialRing := R -> 0
codim QuotientRing := (R) -> codim cokernel presentation R
dim QuotientRing := (R) -> (
     if isField R then 0
     else if (options R).SkewCommutative then dim coefficientRing R
     else dim ultimate(ambient,R) - codim R
     )

hilbertSeries QuotientRing := options -> (S) -> (
     hilbertSeries(coker presentation S,options)
     )

monoid QuotientRing := (S) -> (
     if S.?monoid then S.monoid
     else (
	  R := ultimate(ambient,S);
	  S.monoid = monoid R
	  )
     )

degreesRing QuotientRing := (S) -> (
     if S.?degreesRing then S.degreesRing 
     else (
	  R := ultimate(ambient,S);
	  S.degreesRing = degreesRing R
	  )
     )

QuotientRing_String := (S,s) -> (
     if S#?s then S#s
     else (
	  R := ultimate(ambient, S);
	  S#s = promote(R_s, S)
	  )
     )

generators QuotientRing := (S) -> (
     if S.?generators 
     then S.generators
     else S.generators = (
	  A := ultimate(ambient,S);
	  S.generators = apply(generators A, m -> promote(m,S))))

char QuotientRing := (S) -> (
     if S.?char then S.char
     else S.char = (
	  R := ultimate(ambient,S);
	  -- eventually we'll have to compute it correctly
	  char R))

singularLocus(Ring) := QuotientRing => (R) -> (
     if not isAffineRing(R) then error "expected an affine ring";
     f := presentation R;
     A := ring f;
     A / (ideal f + minors(codim R, jacobian presentation R))
     )

singularLocus(Ideal) := QuotientRing => (I) -> singularLocus(ring I / I)

toField = R -> (
     R.frac = R;
     R.isField = true;
     if R.?Engine and R.Engine then (
	  sendgg(ggPush R, ggdeclarefield);
	  R / R := (x,y) -> (
	       try (
		    sendgg(ggPush x, ggPush y, ggdiv);
		    R.pop())
	       else (
		    if y != 0 then R.nonUnit = y;
		    error "division by non-invertible element attempted";
		    )
	       );
	  );
     R)

getNonUnit = R -> (
     if R.?nonUnit then R.nonUnit
     else if R.?Engine and R.Engine then (
     	  sendgg(ggPush R, gggetzerodivisor);
     	  r := R.pop();
	  if r != 0 then r
	  )
     )

getZeroDivisor = R -> (
     if R.?Engine and R.Engine then (
     	  sendgg(ggPush R, gggetzerodivisor);
     	  r := R.pop();
	  if r != 0 then r
	  )
     )
