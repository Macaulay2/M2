--		Copyright 1995 by Daniel R. Grayson

document { quote baseRings,
     TT "baseRings", " -- a symbol used as a key in a ring ", TT "R", " under which is
     stored a list of base rings for ", TT "R", ".",
     PARA,
     "A base ring ", TT "A", " of ", TT "R", " is one of the rings involved in the
     construction of ", TT "R", ".  The natural ring homomorphism from ", TT "A", "
     to ", TT "R", " is implemented with ", TO "promote", ".",
     PARA,
     "The base rings are presented in chronological order."
     }

lift = method()

document { quote lift,
     TT "lift(f,R)", " -- promotes a ring element ", TT "f", " to 
     the ring ", TT "R", ".",
     PARA,
     "The ring ", TT "R", " should be one of the base rings associated with the
     ring of ", TT "f", ".",
     SEEALSO "baseRings"
     }

liftable = method()

document { quote liftable,
     TT "lift(f,R)", " -- tells whether a ring element ", TT "f", " can be
     lifted to the ring ", TT "R", ".",
     PARA,
     "The ring ", TT "R", " should be one of the base rings associated with the
     ring of ", TT "f", ".",
     SEEALSO "baseRings"
     }

promote = method()

document { quote promote,
     TT "promote(f,R)", " -- promotes a ring element ", TT "f", " to 
     the ring ", TT "R", ".",
     PARA,
     "The element ", TT "f", " should be an element of some base ring of ", TT "R", ".",
     PARA,
     "A special feature is that if ", TT "f", " is rational, and ", TT "R", " is not
     an algebra over ", TT "QQ", ", then an element of ", TT "R", " is provided
     by attempting the evident division.",
     SEEALSO "baseRings"
     }

RingElement = new Type of MutableHashTable

document { quote RingElement,
     TT "RingElement", " -- the class of all ring elements handled by the 
     ", TO "engine", ".",
     PARA,
     SEEALSO "PolynomialRing"
     }

EngineRing = new Type of Ring
document { quote EngineRing,
     TT "EngineRing", " -- denotes the class of all special-purpose engine
     rings, such as finite fields.",
     PARA,
     "The command ", TT "new Engine from x", " is not meant for general 
     users, and provides the developers with a way to create top-level 
     rings corresponding to rings implemented in the engine.  Here ", TT "x", "
     may be:",
     MENU {
	  "commands for the engine, as a string, or a sequence or list
	  of strings, which cause a ring to be placed on the top of the
	  engine's stack.",
	  "a ring, in which case another top-level ring is formed as
	  an interface to the same underlying engine ring.",
	  "the handle of on engine ring"
	  },
     "Types of EngineRing:",
     MENU {
	  TO "FractionField",
	  TO "GaloisField",
	  TO "PolynomialRing",
	  TO "QuotientRing",
	  TO "SchurRing"
	  }
     }

-----------------------------------------------------------------------------

reduce := (r,s) -> (
     z := syz( matrix{{r,s}}, SyzygyLimit => 1 );
     a := z_(1,0);
     b := - z_(0,0);
     c := leadCoefficient b;
     ((a//c),(b//c))
     )

name EngineRing := R -> sendgg(ggPush R, ggsee, ggpop)

net EngineRing := name

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
     R.pop = () -> new R;
     R#0 = 0_R;
     R#1 = 1_R;
     R)

TEST "
    -- test of lift/promote of an ideal
    A = ZZ/101[a..d]
    A = QQ[a..d]
    A = GF(5,2)[a..d]
    B = A/(a^2-d^2)
    I = ideal(a,b)
    assert(ring I === A)
    I1 = I*B
    I2 = lift(I1,A)
    assert(trim I2 == ideal(a,b,d^2))
    C = B/(b^3-c^3)
    I3 = I2*C
    I3a = I*C
    assert(I3 == I3a)
    I4 = lift(I3,B)
    I5 = trim lift(I3,A)
    assert(I5 == ideal(a,b,c^3,d^2))
"
-----------------------------------------------------------------------------
                FractionField = new Type of EngineRing
           frac FractionField := identity
coefficientRing FractionField := F -> coefficientRing F.baseRings#-1
       degreeLength FractionField := F -> degreeLength F.baseRings#-1
           name FractionField := F -> "frac(" | name F.baseRings#-1 | ")"
	isField FractionField := F -> true
            net FractionField := F -> (
		 if F.?name
		 then net F.name
		 else net new FunctionApplication from { frac, F.baseRings#-1 }
		 )

frac Ring := R -> (
     if R.?frac then R.frac 
     else error "no method found"
     )

freduce := (f) -> (numerator f)/(denominator f)

frac EngineRing := R -> (
     if R.?frac then R.frac
     else (
	  R.frac = F := new FractionField from (ggPush R, ggfractionfield);
	  F.ConvertToExpression = ConvertApply(
	       (x,y) -> x/y, R.ConvertToExpression, R.ConvertToExpression
	       );
	  factor F := (f,options) -> factor numerator f / factor denominator f;
	  name F := x -> name expression x;
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
	  F.baseRings = append(R.baseRings,R);
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
	  if R.?syms then F.syms = R.syms;
	  if R.?generators then F.generators = apply(R.generators, r -> promote(r,F));
	  scan(keys R,k -> if class k === String then F#k = promote(R#k,F));
	  if not R.?name then use F;
	  F))

document { quote fraction,
     TT "fraction(f,g)", " -- manufactures the fraction f/g in the fraction
     ring of the ring containing f and g without reducing it to lowest terms."
     }

TEST "
frac(QQ[a,b])
assert ( a == denominator(b/a) )
assert ( b == numerator(b/a) )
assert ( 1 == numerator(b/b) )
"

document { quote FractionField,
     TT "FractionField", " -- the class of all fraction fields.",
     PARA,
     "Functions:",
     MENU {
	  (TO "frac", "     -- constructing a fraction field"),
	  (TO "fraction", " -- constructing a fraction")
	  }
     }

document { quote frac,
     TT "frac R", " -- construct the fraction field of the ring ", TT "R", ".",
     PARA,
     "If ", TT "R", " has no name yet, then the names for its symbols will
     be usurped as names for the corresponding elements of ", TT "R", ".",
     PARA,
     EXAMPLE "F = frac (ZZ/101[x,y])",
     EXAMPLE "1/x + 1/y + 1/2",
     SEEALSO "FractionField"
     }

-- methods for all ring elements

degreeLength Ring := R -> R.degreeLength

use Ring := R -> (
     if R.?ring then use R.ring;
     try generators R;
     if R.?generators and R.?syms then scan(R.syms,R.generators,assign);
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
     	  else error("expected ", name m, " to be a monomial")
	  );
     f _ m)

EngineRing _ ZZ := (R,i) -> (
     if R.?generators 
     then R.generators#i
     else (
     	  sendgg(ggPush R, ggPush i, ggPush 1, ggvar);
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

net RingElement := x -> verticalJoin lines sendgg(ggPush x, ggsee, ggpop);

RingElement ^ ZZ := (x,i) -> (
     R := ring x;
     if i === 0
     then R#1
     else (
	  sendgg(ggPush x, ggPush i, ggpower);
	  new R));

name RingElement := x -> name expression x

net RingElement := x -> net expression x

someTerms(RingElement,ZZ,ZZ) := (f,i,n) -> (
     S := ring S;
     if n <= 0
     then 0_S
     else (
	  sendgg(ggPush f, ggPush i, ggPush (i + n - 1), gggetterms);
	  new S))

baseName RingElement := x -> (
     if size x === 1
     and leadCoefficient x == 1
     then baseName leadMonomial x
     )

leadCoefficient RingElement := (f) -> (
     R := ring f;
     k := coefficientRing R;
     leadCoefficient R := (f) -> (
     	  sendgg(ggPush f, ggleadcoeff);
     	  k.pop());
     leadCoefficient f)

leadMonomial RingElement := (f) -> (
     R := ring f;
     M := monoid R;
     leadMonomial R := f -> (
	  sendgg(ggPush f, ggleadmonom);
	  M.pop());
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
     else error ("no method found for element of ring", name R)
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
	  if R === S then (
	       (x,y) -> (
	       	    sendgg ( ggPush x, ggPush y, ggdiv);
	       	    new R)
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

QQ + RingElement := { RingElement,
     (f,g) -> (
     	  R := class g;
     	  QQ + R := (
	       (r,f) -> promote(r,R) + f
	       );
     	  f + g)
     }

RingElement + QQ := { RingElement,
     (f,g) -> (
	  R := class f;
	  R + QQ := (
	       (f,r) -> f + promote(r,R)
	       );
	  f + g)
     }

ZZ + RingElement := RingElement + ZZ := RingElement + RingElement := { RingElement,
     (f,g) -> (
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
     }

ZZ == RingElement := (i,x) -> (
     R := class x;
     ZZ == R := (i,x) -> (
	  if i === 0
	  then (
	       sendgg(ggPush x, ggiszero);
	       eePopBool())
	  else (
	       sendgg(ggPush i, ggPush x, ggisequal);
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
	       sendgg(ggPush i, ggPush x, ggisequal);
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

liftChain = (R,A) -> (
     -- how to lift from R to A, assuming A is a precursor of R
     if R === A then ()
     else (
	  S := R;
	  while S =!= A and class S === QuotientRing do S = ambient S;
	  if S === A then seq S
	  else (
	       if class S === PolynomialRing 
	       or class S === GaloisField
	       or class S === FractionField
	       then S = S.baseRings#-1;
	       if S === A then seq S
	       else if R === S then error "no lifting possible for these rings"
	       else prepend(S, liftChain(S, A)))))

promoteChain = (A,R) -> (
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


ZZ _ EngineRing := 
promote(ZZ,EngineRing) := (i,R) -> new R from {(
	  quote handle, 
	  newHandle (ggPush R, ggINT, gg i, ggfromint))}

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
