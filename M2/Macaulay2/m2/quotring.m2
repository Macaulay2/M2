--		Copyright 1996 by Daniel R. Grayson and Michael E. Stillman


QuotientRing = new Type of EngineRing

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
net QuotientRing := S -> net expression S
dim QuotientRing := S -> if S.?dim then S.dim else S.dim = dim S^1

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

ZZZquotient := (R,I) -> (
     if ring I.generators =!= ZZZ then error "expected an ideal of ZZZ";
     EgensI := I.generators;
     EgensgbI := if rank source EgensI === 1 then EgensI else generators gb EgensI;
     En := if EgensgbI == 0 then 0 else EgensgbI_(0,0);
     En = lift(En,ZZ);
     if En < 0 then En = -En;
     if En === 0 then ZZ
     else if savedEQuotients#?En 
     then savedEQuotients#En
     else (
	  if En > 32767 then error "large characteristics not implemented yet";
	  if En > 1 and not isPrime En
	  then error "ZZZ/n not implemented yet for composite n";
	  ES := new QuotientRing from newHandle(ggPush En, ggEcharp);
	  ES.baseRings = append(R.baseRings,R);
	  ES.newEngine = true;
	  ES.relations = EgensI;
	  ES.ConvertToExpression = R.ConvertToExpression;
	  ES.isCommutative = R.isCommutative;
	  ES.presentation = EgensgbI;
	  ES.char = En;
	  if En === 1 then ES.dim = -1 else if En === 0 then ES.dim = 1 else ES.dim = 0;
	  ES.ConvertToExpression = ConvertApply(expression, ConvertInteger);
	  expression ES := x -> convert(
	       ES.ConvertToExpression, sendgg(ggPush x, ggtonet)
	       );
	  ES.frac = ES;		  -- ZZ/n with n PRIME!
	  savedEQuotients#En = ES;
	  ES))

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
	  S.baseRings = {R};
	  S.relations = gensI;
	  S.ConvertToExpression = R.ConvertToExpression;
	  S.isCommutative = R.isCommutative;
	  S.presentation = gensgbI;
	  S.char = n;
	  if n === 1 then S.dim = -1 else if n === 0 then S.dim = 1 else S.dim = 0;
	  S.ConvertToExpression = ConvertApply(expression, ConvertInteger);
	  expression S := x -> convert(
	       S.ConvertToExpression, sendgg(ggPush x, ggtonet)
	       );
	  S.frac = S;		  -- ZZ/n with n PRIME!
	  savedQuotients#n = S;
	  S))

Ring / Ideal := QuotientRing => (R,I) -> if I == 0 then R else (
     if R === ZZZ then ZZZquotient(R,I)
     else if R === ZZ then ZZquotient(R,I)
     else (
	  error "can't form quotient of this ring"
	  )
     )

predecessors := method()
predecessors Ring := R -> {R}
predecessors QuotientRing := R -> append(predecessors last R.baseRings, R)

EngineRing / Ideal := (R,I) -> if I == 0 then R else if R === ZZZ then ZZZquotient(R,I) else (
     -- recall that ZZ is NOT an engine ring.
     A := R;
     while class A === QuotientRing do A = last A.baseRings;
     gensI := I.generators ** R;
     gensgbI := generators gb gensI;
     sendgg(ggPush gensgbI, ggqring);
     S := new QuotientRing from newHandle();
     if R.?newEngine then S.newEngine = true;
     S.baseRings = append(R.baseRings,R);
     S.relations = gensI;
     S.ConvertToExpression = R.ConvertToExpression;
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
	  sendgg (ggPush R, gggetideal);
	  getMatrix ultimate(ambient,R)
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
     R / minors(codim R, jacobian presentation R))

singularLocus(Ideal) := QuotientRing => (I) -> singularLocus(ring I / I)
