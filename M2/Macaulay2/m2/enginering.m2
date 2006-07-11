--		Copyright 1995-2002 by Daniel R. Grayson
RingElement = new Type of HashTable
RingElement.synonym = "ring element"
value RingElement := identity
raw RingElement := f -> f.RawRingElement
RingElement == RawRingElement := (x,y) -> raw x === y
RawRingElement == RingElement := (x,y) -> x === raw y
ring RingElement := r -> class r
factor RingElement := r -> error "factor: unimplemented for this ring"

promote'(Number,RingElement) := (n,R) -> new R from rawFromNumber(R,n)

-- RingElement.directSum = v -> directSum apply(v, a -> matrix{{a}})

EngineRing = new Type of Ring
EngineRing.synonym = "engine ring"
raw EngineRing := R -> R.RawRing
raw Ring := R -> if R.?RawRing then R.RawRing else error "no raw engine ring associated with this ring"
isField EngineRing := R -> rawIsField raw R
-----------------------------------------------------------------------------
--- new lift and promote, version 3
basicLift = (r,B) -> new B from rawLift(raw B, raw r)
multipleBasicLift = (r,v) -> ( r = raw r; scan(v, B -> r = rawLift(raw B, raw r)); new last v from r )

basicLiftMatrix = (m,F) -> map(F,, rawLift(raw F, raw m))
multipleBasicLiftMatrix = (m,v) -> (m = raw m; scan(v, F -> m = rawLift(raw F, m)); map(last v,,m) )

basicPromote = (r,B) -> new B from rawPromote(raw B, raw r)
multipleBasicPromote = (r,v) -> ( r = raw r; scan(v, B -> r = rawPromote(raw B, raw r)); new last v from r )

basicPromoteMatrix = (m,F) -> map(F,, rawPromote(raw F, raw m))
multipleBasicPromoteMatrix = (m,v) -> (m = raw m; scan(v, F -> m = rawPromote(raw F, m)) map(last v,,m) )

-- if debugLevel > 0 then (
-- 
-- basicLiftMatrix = (m,F) -> ( R := ring F; map(F,,applyTable(entries m, r -> lift(r, R)))); -- temporary
-- multipleBasicLiftMatrix = (m,v) -> (m = entries m; scan(v, F -> m = applyTable(m, r -> lift(r, ring F))); map(last v,,m) ); -- temporary
-- 
-- basicPromoteMatrix = (m,F) -> ( R := ring F; map(F,,applyTable(entries m, r -> promote(r, R)))); -- temporary
-- multipleBasicPromoteMatrix = (m,v) -> (m = entries m; scan(v, F -> m = applyTable(m, r -> promote(r, ring F))); map(last v,,m) ); -- temporary
-- 
-- )

commonEngineRingInitializations = (F) -> (
     F ? F := (f,g) -> raw f ? raw g;
     promote'(F,F) := (f,F) -> f;
     baserings := F.baseRings;
     n := # baserings;
     scan(n, i -> (
	       A := baserings#i;
	       if not ancestor(A, Number) then (
		    if i == n-1 then (
			 promote'(A,F) := basicPromote;
			 )
		    else (
			 v := append(take(baserings, -(n-i-1)), F);
			 promote'(A,F) := (a,F) -> multipleBasicPromote(a, v);
			 ))));
     )

-----------------------------------------------------------------------------
reduce := (r,s) -> (
     z := syz( matrix{{r,s}}, SyzygyLimit => 1 );
     a := z_(1,0);
     b := - z_(0,0);
     if isField coefficientRing ring b then (
     	  c := leadCoefficient b;
	  a = a//c;
	  b = b//c;
	  );
     (a,b))

toString EngineRing := R -> if ReverseDictionary#?R then toString ReverseDictionary#R else toString R.RawRing

ZZ _ EngineRing := 
RR _ EngineRing := 
promote(ZZ,EngineRing) := RingElement => (i,R) -> new R from i_(R.RawRing)

promote(Number,RingElement) := RingElement => (i,R) -> new R from rawFromNumber(raw R, i)

new RingElement from RawRingElement := (R, f) -> new R from { symbol RawRingElement => f };

new EngineRing from RawRing := (EngineRing,R) -> (
     S := new EngineRing of RingElement;
     S.RawRing = R;
     S#1 = 1_S;
     S#0 = 0_S;
     S)

promote(RR,EngineRing) := RingElement => (i,R) -> i_R

-----------------------------------------------------------------------------
                FractionField = new Type of EngineRing
		FractionField.synonym = "fraction field"

frac = method(TypicalValue => FractionField)
frac Ring := R -> (
     if R.?frac then R.frac 
     else error "no method found"
     )

           frac FractionField := identity
coefficientRing FractionField := F -> coefficientRing last F.baseRings
   degreeLength FractionField := F -> degreeLength last F.baseRings
       toString FractionField := F -> if ReverseDictionary#?F then toString ReverseDictionary#F else "frac(" | toString last F.baseRings | ")"
        numgens FractionField := F -> numgens last F.baseRings
  allGenerators FractionField := F -> allGenerators last F.baseRings
	isField FractionField := F -> true
           char FractionField := F -> char last F.baseRings
	    dim FractionField := F -> 0
            net FractionField := F -> if ReverseDictionary#?F then toString ReverseDictionary#F else net new FunctionApplication from { frac, last F.baseRings }
     expression FractionField := F -> if ReverseDictionary#?F then hold toString ReverseDictionary#F else (expression frac) (expression last F.baseRings)

-- freduce := (f) -> (numerator f)/(denominator f)

isHomogeneous FractionField := (F) -> isHomogeneous last F.baseRings

frac EngineRing := R -> (
     if R.?frac then R.frac
     else (
	  o := options R;
	  if o.Inverses then error "not implemented : fraction fields of rings with inverses";
	  if o.WeylAlgebra =!= {} or R.?SkewCommutative
	  then error "fraction field of non-commutative ring requested";
	  R.frac = F := new FractionField from rawFractionRing R.RawRing;
	  F.baseRings = append(R.baseRings,R);
	  factor F := options -> f -> factor numerator f / factor denominator f;
	  toString F := x -> toString expression x;
	  net F := x -> net expression x;
	  baseName F := (f) -> (
	       if denominator f != 1 
	       then error "expected a generator"
	       else baseName numerator f);
	  expression F := (f) -> expression numerator f / expression denominator f;
	  numerator F := (f) -> new R from rawNumerator f.RawRingElement;
	  denominator F := (f) -> new R from rawDenominator f.RawRingElement;
	  F.generators = apply(generators R, m -> promote(m,F));
	  fraction(F,F) := F / F := (x,y) -> x//y;
	  fraction(R,R) := (r,s) -> new F from rawFraction(F.RawRing,r.RawRingElement,s.RawRingElement);
	  commonEngineRingInitializations F;
	  if R.?generatorSymbols then F.generatorSymbols = R.generatorSymbols;
	  if R.?generatorExpressions then F.generatorExpressions = R.generatorExpressions;
	  if R.?generators then F.generators = apply(R.generators, r -> promote(r,F));
	  if R.?indexSymbols then F.indexSymbols = applyValues(R.indexSymbols, r -> promote(r,F));
	  if R.?indexStrings then F.indexStrings = applyValues(R.indexStrings, r -> promote(r,F));
	  F))

-- methods for all ring elements

degreeLength Ring := R -> R.degreeLength

use Ring := R -> (
     if R.?ring then use R.ring;			    -- I'm not sure what this is for.  Which rings have this key?
     generators R;
     if R.?generators and R.?generatorSymbols then scan(R.generatorSymbols,R.generators,(sym,val) -> sym <- val);
     if R.?use then R.use R;
     R)

numgens EngineRing := R -> #R.generators

generators EngineRing := R -> R.generators

part(ZZ,RingElement) := RingElement => (d,f) -> (
     u := select(terms f, t -> d === sum degree t);
     if #u === 0 then 0_(ring f)
     else sum u
     )

part(List,RingElement) := RingElement => (d,f) -> (
     if degreeLength ring f =!= #d
     then error ("degree length of ring element doesn't match specified degree");
     u := select(terms f, t -> d === degree t);
     if #u === 0 then 0_(ring f)
     else sum u
     )

Ring _ ZZ := RingElement => (R,i) -> (generators R)#i

protect numallvars

EngineRing _ ZZ := (R,i) -> (
     if i < 0 or R.?numallvars and i >= R.numallvars then error("index ", toString i, " out of bounds 0 .. ", toString (R.numallvars-1));
     new R from R.RawRing_i
     )

size RingElement := f -> rawTermCount(numgens ring f, f.RawRingElement)

isHomogeneous RingElement := f -> rawIsHomogeneous f.RawRingElement

- RingElement := RingElement => x -> new ring x from -x.RawRingElement

RingElement ? ZZ := (x,n) -> x ? n_(class x)
ZZ ? RingElement := (m,y) -> m_(class y) ? y

RingElement ^ ZZ := RingElement => (x,i) -> new ring x from x.RawRingElement^i

toString RingElement := x -> toString expression x

net RingElement := x -> net expression x

someTerms(RingElement,ZZ,ZZ) := RingElement => (f,i,n) -> new ring f from rawGetTerms(numgens ring f,raw f,i,n+i-1)

baseName RingElement := x -> (
     R := class x;
     i := rawIndexIfVariable raw x;
     if i === null then error "expected a generator";
     S := R;
     while i >= length generators S do (
	  i = i - length generators S;
	  try S = coefficientRing S else error "internal error: raw index too large";
	  );
     S.generatorSymbols#i
     )

leadCoefficient RingElement := RingElement => (f) -> (
     k := coefficientRing ring f;
     new k from rawLeadCoefficient(k.RawRing, f.RawRingElement))

degree RingElement := f -> if f == 0 then -infinity else (
     d := rawMultiDegree f.RawRingElement;
     R := ring f;
     if R.?Repair then R.Repair d else d
     )

-- delayed installation of certain methods

expression RingElement := (r) -> (
     R := class r;
     if class R === QuotientRing then (
	  A := ultimate(ambient,R);
	  expression R := lookup(expression,A);
	  expression r)
     else error ("no method found for element of ring ", toString R)
     )

leadTerm RingElement := RingElement => (f) -> someTerms(f,0,1)

RingElement % RingElement := RingElement => 
QQ % RingElement := RingElement % QQ :=
ZZ % RingElement := RingElement % ZZ :=
(f,g) -> (
     R := class f;
     S := class g;
     R % S := (
	  if R === S then (
	       (x,y) -> new R from x.RawRingElement % y.RawRingElement
	       )
	  else if member(R,S.baseRings) then (
	       (x,y) -> promote(x,S) % y
	       )
	  else if member(S,R.baseRings) then (
	       (x,y) -> x % promote(y,R)
	       )
	  else error "expected pair to have a method for '%'"
	  );
     f % g
     )

RingElement // RingElement := RingElement => 
RR // RingElement := RingElement // RR :=
QQ // RingElement := RingElement // QQ :=
ZZ // RingElement := RingElement // ZZ := 
(f,g) -> (
     R := class f;
     S := class g;
     R // S := (
	  if R === S then (
	       (x,y) -> new R from x.RawRingElement // y.RawRingElement
	       )
	  else if member(R,S.baseRings) then (
	       (x,y) -> promote(x,S) // y
	       )
	  else if member(S,R.baseRings) then (
	       (x,y) -> x // promote(y,R)
	       )
	  else error "expected pair to have a method for '//'"
	  );
     f // g
     )

QQ - RingElement := (f,g) -> (
     R := class g;
     QQ - R := (
	  (r,f) -> promote(r,R) - f
	  );
     f - g)

RingElement - QQ := (f,g) -> (
     R := class f;
     R - QQ := (
	  (f,r) -> f - promote(r,R)
	  );
     f - g)

RingElement - RingElement := RingElement =>
RR - RingElement := RingElement - RR := 
ZZ - RingElement := RingElement - ZZ := 
(f,g) -> (
     R := class f;
     S := class g;
     R - S := (
	  if R === S then (
	       (x,y) -> new R from x.RawRingElement - y.RawRingElement
	       )
	  else if member(R,S.baseRings) then (
	       (x,y) -> promote(x,S) - y
	       )
	  else if member(S,R.baseRings) then (
	       (x,y) -> x - promote(y,R)
	       )
	  else error "expected pair to have a method for '-'"
	  );
     f - g
     )

QQ * RingElement := (f,g) -> (
     R := class g;
     QQ * R := (
	  (r,f) -> promote(r,R) * f
	  );
     f * g)

RingElement * QQ := (f,g) -> (
     R := class f;
     R * QQ := (
	  (f,r) -> f * promote(r,R)
	  );
     f * g)

RingElement * RingElement := RingElement =>
RR * RingElement := RingElement * RR :=
ZZ * RingElement := RingElement * ZZ :=
(f,g) -> (
     R := class f;
     S := class g;
     R * S := (
	  if R === S then (
	       (x,y) -> new R from x.RawRingElement * y.RawRingElement
	       )
	  else if member(R,S.baseRings) then (
	       (x,y) -> promote(x,S) * y
	       )
	  else if member(S,R.baseRings) then (
	       (x,y) -> x * promote(y,R)
	       )
	  else error "expected pair to have a method for '*'"
	  );
     f * g
     )

QQ + RingElement := (f,g) -> (
     R := class g;
     QQ + R := (
	  (r,f) -> promote(r,R) + f
	  );
     f + g)

RingElement + QQ := (f,g) -> (
     R := class f;
     R + QQ := (
	  (f,r) -> f + promote(r,R)
	  );
     f + g)

RingElement + RingElement := RingElement =>
RR + RingElement := RingElement + RR := 
ZZ + RingElement := RingElement + ZZ := 
(f,g) -> (
     R := class f;
     S := class g;
     R + S := (
	  if R === S then (
	       (x,y) -> new R from x.RawRingElement + y.RawRingElement
	       )
	  else if member(R,S.baseRings) then (
	       (x,y) -> promote(x,S) + y
	       )
	  else if member(S,R.baseRings) then (
	       (x,y) -> x + promote(y,R)
	       )
	  else error "expected pair to have a method for '+'"
	  );
     f + g
     )

ZZ == RingElement := (i,x) -> i == x.RawRingElement
RingElement == ZZ := (x,i) -> x.RawRingElement == i
RingElement == RingElement := (f,g) -> (
     R := class f;
     S := class g;
     R == S := (
	  if R === S then (
	       (x,y) -> x.RawRingElement === y.RawRingElement
	       )
	  else if member(R,S.baseRings) then (
	       (x,y) -> promote(x,S) == y
	       )
	  else if member(S,R.baseRings) then (
	       (x,y) -> x == promote(y,R)
	       )
	  else error "expected pair to have a method for '=='"
	  );
     f == g
     )

QQ == RingElement := (r,x) -> promote(r,ring x) == x
RingElement == QQ := (x,r) -> promote(r,ring x) == x

QQ / RingElement := (f,g) -> (
     R := class g;
     QQ / R := (
	  (r,f) -> promote(r,R) / f
	  );
     f / g)

RingElement / QQ := (f,g) -> (
     R := class f;
     R / QQ := (
	  (f,r) -> f / promote(r,R)
	  );
     f / g)

fraction(RingElement,RingElement) := (r,s) -> (
     R := ring r;
     S := ring s;
     if R === S then (
	  frac R;
	  fraction(r,s))
     else error "numerator and denominator not in the same ring"
     )

RingElement / RingElement := RingElement =>
RR / RingElement := RingElement / RR :=
ZZ / RingElement := RingElement / ZZ :=
(f,g) -> (
     R := class f;
     S := class g;
     R / S := (
	  if R === S then (
	       frac R; 
	       (r,s) -> fraction (r,s)
	       )
	  else if member(R,S.baseRings) then (
	       (x,y) -> promote(x,S) / y
	       )
	  else if member(S,R.baseRings) then (
	       (x,y) -> x / promote(y,R)
	       )
	  else error "expected pair to have a method for '/'"
	  );
     f / g
     )

-----------------------------------------------------------------------------

-- new lift and promote, version 2

liftChain := (R,A) -> (
     -- how to lift from R to A, assuming A is a precursor of R
     if R === A then ()
     else (
	  S := R;
	  while S =!= A and class S === QuotientRing do S = ambient S;
	  if S === A then 1 : S
	  else (
	       if S.?baseRings then S = last S.baseRings;
	       if S === A then 1 : S
	       else if R === S then error "no lifting possible for these rings"
	       else prepend(S, liftChain(S, A)))))

promoteChain := (A,R) -> (
     -- how to promote from A to R, assuming A is a precursor of R
     if R === A then ()
     else append((
	       S := R;
	       while S =!= A and class S === QuotientRing do S = ambient S;
	       if S === A then ()
	       else (
	       	    if class S === PolynomialRing 
	       	    or class S === GaloisField
	       	    or class S === FractionField
		    then S = last S.baseRings;
		    if S === A then ()
		    else if R === S then error "no promotion possible for these rings"
		    else promoteChain(A, S))),
	  R))

eeLift := (B,r) -> new B from rawLift(raw B, raw r)

lift(RingElement, RingElement) := RingElement =>
lift(RingElement, ZZ) :=
lift(RingElement, QQ) := (r,o) -> (
     R := class r;
     A := class o;
     if R === A then (
	  lift(R,A) := (r,o) -> r
	  )
     else (
	  c := liftChain(R,A);
	  lift(R,A) := (r,o) -> (
	       scan(c, B -> r = eeLift(B,r));
	       r)
	  );
     lift(r,o))

lift(RingElement,Ring) := RingElement => 
lift(ZZ,Ring) :=
lift(QQ,Ring) := (r,A) -> lift(r,A#0)

promote(QQ, RingElement) := RingElement => (r,o) -> (
     S := class o;
     if member(QQ,S.baseRings) then (
	  c := promoteChain(QQ,S);
	  promote(QQ,S) := (f,o) -> (
	       f = raw f;
	       scan(c, S -> f = rawPromote(S.RawRing,f));
	       new S from f)
	  )
     else (
	  promote(QQ,S) := (r,S) -> (
	       a := promote(numerator r,S);
	       b := promote(denominator r,S);
	       if isField class S then (
		    a/b
		    )
	       else (
	       	    if a % b == 0 then a // b
	       	    else error "division not possible"
		    )
	       );
	  );
     promote(r,S))

promote(RingElement, RingElement) := RingElement => (r,o) -> (
     R := class r;
     S := class o;
     if R === S then (
	  promote(R,S) := (r,o) -> r
	  )
     else (
	  c := promoteChain(R,S);
	  promote(R,S) := (f,o) -> (
	       f = raw f;
	       scan(c, S -> f = rawPromote(S.RawRing,f));
	       new S from f)
	  );
     promote(r,S))

promote(ZZ,RingElement) := (i,o) -> promote(i, ring o)

promote(RingElement,Ring) := RingElement => (r,S) -> promote(r,S#0)
promote(ZZ,Ring) := (r,S) -> promote(r,S#0)

liftable(RingElement,Ring) := Boolean => 
liftable(ZZ,Ring) := 
liftable(QQ,Ring) := (f,R) -> try (lift(f,R);true) else false

isUnit(RingElement) := (f) -> 1 % gb matrix {{f}} == 0

Ring _ String := RingElement => (x,s) -> x.indexStrings#s
Ring _ Symbol := RingElement => (x,s) -> x.indexSymbols#s

random Ring := RingElement => opts -> (R) -> (
     if R.?random then R.random()
     else error "no method found for item of class Ring"
     )

ZZ _ Ring := RingElement => (i,R) -> (
     if i === 1 then R#1
     else if i === 0 then R#0
     else i * R#1
     )

isConstant RingElement := r -> r == 0 or all(degree r, i -> i === 0)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
