--		Copyright 1995 by Daniel R. Grayson
lift = method()
liftable = method()
promote = method()
RingElement = new Type of MutableHashTable
EngineRing = new Type of Ring
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

toString EngineRing := R -> if R.?name then R.name else sendgg(ggPush R, ggsee, ggpop)

net EngineRing := toString

new EngineRing from Handle := (EngineRing, h) -> new EngineRing from ggPush h

new EngineRing from Ring := (EngineRing, R) -> new EngineRing from handle R

new EngineRing from List :=
new EngineRing from Sequence :=
new EngineRing from String := (EngineRing, ggcmds) -> (
     R := new EngineRing of RingElement;
     R.Engine = true;
     R.handle = newHandle ggcmds;
     new R from Handle := (R,h) -> new R from { (quote handle, h) };
     new R := R -> newClass( R, hashTable { (
		    quote handle, 
		    toHandle convert(ConvertInteger, sendgg (ggaddress,ggtonet))
		    ) } );
     R#1 = 1_R;
     R#0 = 0_R;
     R.pop = () -> new R;
     R)

ZZ _ EngineRing := 
promote(ZZ,EngineRing) := (i,R) -> (
     new R from {( quote handle, newHandle (ggPush R, ggINT, gg i, ggfromint) )}
     )

-----------------------------------------------------------------------------

ZZZ = new EngineRing of RingElement
ZZZ.generators = {}
ZZZ.pop = () -> new ZZZ
ZZZ.handle = newHandle ggEZZ
new ZZZ from Handle := (ZZZ,h) -> new ZZZ from { (quote handle, h) };
new ZZZ := ZZZ -> newClass( ZZZ, hashTable { (
	       quote handle, 
	       toHandle convert(ConvertInteger, sendgg (ggaddress,ggtonet))
	       ) } );
ZZZ.newEngine = true
ZZZ#1 = 1_ZZZ
ZZZ#0 = 0_ZZZ
ZZZ.isCommutative = true
ZZZ.char = 0
ZZZ.ConversionFormat = ConvertInteger
ZZZ.Engine = true
ZZZ.baseRings = {ZZ}
ZZZ.ConvertToExpression = ConvertInteger
ZZZ.degreeLength = 0
net ZZZ := toString ZZZ := see
expression ZZZ := n -> expression lift(n,ZZ)

-----------------------------------------------------------------------------
                FractionField = new Type of EngineRing
           frac FractionField := identity
coefficientRing FractionField := F -> coefficientRing F.baseRings#-1
   degreeLength FractionField := F -> degreeLength F.baseRings#-1
       toString FractionField := F -> if F.?name then F.name else "frac(" | toString F.baseRings#-1 | ")"
	isField FractionField := F -> true
            net FractionField := F -> (
		 if F.?name then F.name
		 else net new FunctionApplication from { frac, F.baseRings#-1 }
		 )

frac Ring := R -> (
     if R.?frac then R.frac 
     else error "no method found"
     )

freduce := (f) -> (numerator f)/(denominator f)

isHomogeneous FractionField := (F) -> isHomogeneous F.baseRings#-1

frac EngineRing := R -> (
     if R.?frac then R.frac
     else (
	  R.frac = F := new FractionField from (ggPush R, ggfractionfield);
	  F.baseRings = append(R.baseRings,R);
     	  if R.?newEngine then F.newEngine = true;
	  F.ConvertToExpression = ConvertApply(
	       (x,y) -> x/y, R.ConvertToExpression, R.ConvertToExpression
	       );
	  factor F := options -> f -> factor numerator f / factor denominator f;
	  toString F := x -> toString expression x;
	  net F := x -> net expression x;
	  baseName F := (f) -> (
	       if denominator f != 1 
	       then error "expected a generator"
	       else baseName numerator f);
	  expression F := (f) -> convert(
	       F.ConvertToExpression,
	       sendgg(ggPush f, ggtonet)
	       );
	  numerator F := (f) -> (
	       sendgg( ggPush f, ggnumerator );
	       R.pop());
	  denominator F := (f) -> (
	       sendgg( ggPush f, ggdenominator );
	       R.pop());
	  isHomogeneous F := (f) -> (
	       isHomogeneous numerator f and isHomogeneous denominator f);
	  degree F := (f) -> degree numerator f - degree denominator f;
	  F + F := (x,y) -> (
	       sendgg(ggPush x, ggPush y, ggadd);
	       freduce new F);
	  F - F := (x,y) -> (
	       sendgg ( ggPush x, ggPush y, ggsubtract );
	       freduce new F);
	  F * F := (x,y) -> (
	       sendgg ( ggPush x, ggPush y, ggmult);
	       freduce new F);
	  F / F := (x,y) -> (
	       sendgg ( ggPush x, ggPush y, ggdiv);
	       freduce new F);
	  fraction(R,R) := (r,s) -> (
	       -- need a better method
	       r = promote(r,F);
	       s = promote(s,F);
	       sendgg ( ggPush r, ggPush s, ggdiv);
	       new F);
	  if R.?generatorSymbols then F.generatorSymbols = R.generatorSymbols;
	  if R.?generatorExpressions then F.generatorExpressions = R.generatorExpressions;
	  if R.?generators then F.generators = apply(R.generators, r -> promote(r,F));
	  scan(keys R,k -> if class k === String then F#k = promote(R#k,F));
	  F))

-- methods for all ring elements

degreeLength Ring := R -> R.degreeLength

use Ring := R -> (
     if R.?ring then use R.ring;
     try generators R;
     if R.?generators and R.?generatorSymbols then scan(R.generatorSymbols,R.generators,assign);
     if R.?use then R.use R;
     R)

numgens EngineRing := R -> #R.generators

generators EngineRing := R -> R.generators

RingElement _ RingElement := (f,m) -> (
     R := ring f;
     if R =!= ring m then error "expected monomial in the same ring";
     R _ R := (f,m) -> (
     	  if size m === 1
     	  and leadCoefficient m == 1
     	  then f_(leadMonomial m)
     	  else error("expected ", toString m, " to be a monomial")
	  );
     f _ m)

RingElement _ ZZ := (f,m) -> (
     R := ring f;
     R _ ZZ := (f,m) -> (
     	  if m =!= 1 then error "expected index to be 1 or a monomial";
     	  f_(1_(monoid R))
	  );
     f _ m)

Ring _ ZZ := (R,i) -> (
     if R.?generators 
     then R.generators#i
     else error "ring has no generators"
     )

EngineRing _ ZZ := (R,i) -> (
     if R.?generators 
     then R.generators#i
     else (
     	  sendgg(ggPush i, ggPush 1, ggPush R, ggvar);
     	  new R)
     )

size RingElement := f -> (
     sendgg(ggPush f, gglength); 
     eePopInt())

isHomogeneous RingElement := f -> (
     sendgg(ggPush f, ggishomogeneous);
     eePopBool())

- RingElement := x -> (
     sendgg(ggPush x, ggnegate); 
     new ring x)

RingElement ? RingElement := (x,y) -> (
     if ring x === ring y and x == y then quote == else handle x ? handle y
     );

net RingElement := x -> stack lines sendgg(ggPush x, ggsee, ggpop);

RingElement ^ ZZ := (x,i) -> (
     R := ring x;
     if i === 0
     then R#1
     else (
	  sendgg(ggPush x, ggPush i, ggpower);
	  new R));

toString RingElement := x -> toString expression x

net RingElement := x -> net expression x

someTerms(RingElement,ZZ,ZZ) := (f,i,n) -> (
     S := ring f;
     if n <= 0
     then 0_S
     else (
	  sendgg(ggPush f, ggPush i, ggPush (i + n - 1), gggetterms);
	  new S))

baseName RingElement := x -> (
     if size x === 1 and leadCoefficient x == 1
     then (
     	  R := ring x;
     	  if R.?newEngine then (
	       sendgg(ggPush x, ggleadmonom);
	       v := eePopIntarray();
	       if #v === 2 and v#1 === 1 then R.generatorSymbols#(v#0)
	       else error "expected a generator"
	       )
     	  else baseName leadMonomial x
	  )
     else error "expected a generator"
     )

leadCoefficient RingElement := (f) -> (
     if f == 0 then error "lead coefficient of 0 requested";
     R := ring f;
     k := coefficientRing R;
     leadCoefficient R := (f) -> (
     	  sendgg(ggPush f, ggleadcoeff);
     	  k.pop());
     leadCoefficient f)

leadMonomial RingElement := (f) -> (
     R := ring f;
     leadMonomial R := if R.?newEngine then (
	  f -> (
	       sendgg(ggPush R, ggPush ((coefficientRing R)#1), ggPush f, ggleadmonom, ggterm);
	       R.pop()
	       )
	  )
     else (
     	  M := monoid R;
	  f -> (
	       sendgg(ggPush f, ggleadmonom);
	       M.pop()
	       )
	  );
     leadMonomial f)

degree RingElement := f -> if f == 0 then -infinity else (
     sendgg(ggPush f, ggdegree);
     eePopIntarray())

-- delayed installation of methods

expression RingElement := (r) -> (
     R := class r;
     if class R === QuotientRing then (
	  A := ultimate(ambient,R);
	  expression R := lookup(expression,A);
	  expression r)
     else error ("no method found for element of ring ", toString R)
     )

leadTermSetup := (R) -> (
     leadTerm R := (f) -> (
     	  if f == 0 then error "encountered a zero polynomial";
     	  sendgg(ggPush f, ggPush 0, ggPush 0, gggetterms);
     	  new R);
     )

leadTerm RingElement := (f) -> (
     leadTermSetup ring f;
     leadTerm f)

QQ % RingElement := RingElement % QQ :=
ZZ % RingElement := RingElement % ZZ :=
RingElement % RingElement := (f,g) -> (
     R := class f;
     S := class g;
     R % S := (
	  if R === S then (
	       (x,y) -> (
	       	    sendgg ( ggPush x, ggPush y, ggmod);
	       	    new R)
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

QQ // RingElement := RingElement // QQ :=
ZZ // RingElement := RingElement // ZZ :=

RingElement // RingElement := (f,g) -> (
     R := class f;
     S := class g;
     R // S := (
	  if R === S then (x,y) -> (
	       try (
	       	    sendgg ( ggPush x, ggPush y, ggdiv);
	       	    new R)
	       else first first entries (matrix {{f}} // g))
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

ZZ - RingElement := RingElement - ZZ :=
RingElement - RingElement := (f,g) -> (
     R := class f;
     S := class g;
     R - S := (
	  if R === S then (
	       (x,y) -> (
	       	    sendgg ( ggPush x, ggPush y, ggsubtract);
	       	    new R)
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

ZZ * RingElement := RingElement * ZZ :=
RingElement * RingElement := (f,g) -> (
     R := class f;
     S := class g;
     R * S := (
	  if R === S then (
	       (x,y) -> (
	       	    sendgg ( ggPush x, ggPush y, ggmult);
	       	    new R)
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

ZZ + RingElement := RingElement + ZZ := RingElement + RingElement := (f,g) -> (
     R := class f;
     S := class g;
     R + S := (
	  if R === S then (
	       (x,y) -> (
		    sendgg ( ggPush x, ggPush y, ggadd);
		    new R)
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

ZZ == RingElement := (i,x) -> (
     R := class x;
     ZZ == R := (i,x) -> (
	  if i === 0
	  then (
	       sendgg(ggPush x, ggiszero);
	       eePopBool())
	  else (
	       sendgg(ggPush x, ggPush i, ggisequal);
	       eePopBool()));
     i == x)

RingElement == ZZ := (x,i) -> (
     R := ring x;
     R == ZZ := (x,i) -> (
	  if i === 0
	  then (
	       sendgg(ggPush x, ggiszero);
	       eePopBool())
	  else (
	       sendgg(ggPush x, ggPush i, ggisequal);
	       eePopBool()));
     x == i)

RingElement == RingElement := (f,g) -> (
     R := class f;
     S := class g;
     R == S := (
	  if R === S then (
	       (x,y) -> (
	       	    sendgg ( ggPush x, ggPush y, ggisequal);
	       	    eePopBool())
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

ZZ / RingElement := RingElement / ZZ :=
RingElement / RingElement := (f,g) -> (
     R := class f;
     S := class g;
     R / S := (
	  if R === S then (
	       F := frac R; 
	       (r,s) -> ( 
	  	    ((r,s) -> fraction(r,s)) reduce (r,s)
	  	    )
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
	  if S === A then singleton S
	  else (
	       if class S === PolynomialRing 
	       or class S === GaloisField
	       or class S === FractionField
	       then S = S.baseRings#-1;
	       if S === A then singleton S
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
		    then S = S.baseRings#-1;
		    if S === A then ()
		    else if R === S then error "no promotion possible for these rings"
		    else promoteChain(A, S))),
	  R))

lift(RingElement, ZZ) :=
lift(RingElement, QQ) :=
lift(RingElement, RingElement) := (r,o) -> (
     R := class r;
     A := class o;
     if R === A then (
	  lift(R,A) := (r,o) -> r
	  )
     else (
	  c := liftChain(R,A);
	  lift(R,A) := (r,o) -> (
	       scan(c, B -> r = eeLift(r,B));
	       r)
	  );
     lift(r,o))

lift(ZZZ,ZZ) := (r,o) -> convert(ConvertInteger, callgg(ggtonet, r))
lift(ZZ,Ring) :=
lift(QQ,Ring) :=
lift(RingElement,Ring) := (r,A) -> lift(r,A#0)

promote(QQ, RingElement) := (r,o) -> (
     S := class o;
     if member(QQ,S.baseRings) then (
	  c := promoteChain(QQ,S);
	  promote(QQ,S) := (f,o) -> (
	       scan(c, S -> f = eePromote(f,S));
	       f)
	  )
     else (
	  promote(QQ,S) := (r,S) -> (
	       a := promote(numerator r,S);
	       b := promote(denominator r,S);
	       if a % b == 0 then a // b
	       else error "division not possible"
	       );
	  );
     promote(r,S))

promote(RingElement, RingElement) := (r,o) -> (
     R := class r;
     S := class o;
     if R === S then (
	  promote(R,S) := (r,o) -> r
	  )
     else (
	  c := promoteChain(R,S);
	  promote(R,S) := (f,o) -> (
	       scan(c, S -> f = eePromote(f,S));
	       f)
	  );
     promote(r,S))

--     ggPushS := concatenate ggPush S;
--     ggpromotenewh := concatenate (ggpromote,ggaddress,ggtonet);
--     ggpush := lookup(ggPush,R);
--     promote(R,S) := (f,S) -> newClass( S, hashTable { (
--		    quote handle, 
--		    toHandle convert(ConvertInteger,
--			 sendgg (ggPushS, ggpush f, ggpromotenewh)))});


promote(ZZ,RingElement) := (i,o) -> promote(i, ring o)

promote(ZZ,Ring) := 
promote(QQ,Ring) := 
promote(RingElement,Ring) := (r,S) -> promote(r,S#0)

promote(ZZ,QQ) := (i,o) -> i/1
promote(ZZ,ZZ) := (i,o) -> i
promote(QQ,QQ) := (i,o) -> i

liftable(ZZ,Ring) := 
liftable(QQ,Ring) := 
liftable(RingElement,Ring) := (f,R) -> try (lift(f,R);true) else false

isUnit(RingElement) := (f) -> 1 % ideal f == 0
