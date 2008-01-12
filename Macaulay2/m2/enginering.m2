--		Copyright 1995-2002 by Daniel R. Grayson
RingElement = new Type of BasicList
RingElement.synonym = "ring element"
value RingElement := identity
raw RingElement := f -> f#0
RingElement == RawRingElement := (x,y) -> raw x === y
RawRingElement == RingElement := (x,y) -> x === raw y
ring RingElement := r -> class r
factor RingElement := r -> error "factor: unimplemented for this ring"
precision RingElement := precision @@ ring

-- RingElement.directSum = v -> directSum apply(v, a -> matrix{{a}})

EngineRing = new Type of Ring
EngineRing.synonym = "engine ring"
raw EngineRing := R -> R.RawRing
raw Ring := R -> if R.?RawRing then R.RawRing else error "no raw engine ring associated with this ring"
isField EngineRing := R -> rawIsField raw R
-----------------------------------------------------------------------------
-- rational promotion to any engine ring
promote(QQ,RingElement) := (r,S) -> (
     a := promote(numerator r,S);
     b := promote(denominator r,S);
     if a % b == 0 then a // b
     else error ("promotion of this rational number to the ring ", toString S, " not possible"))

-- some remnants from lift and promote, version 2
liftable(RingElement,RingElement) := 
liftable(Number,RingElement) := 
liftable(RingElement,Number) := 
liftable(Number,Number) := (f,R) -> null =!= lift(f,R,Verify=>false)

--- new lift and promote, version 3
basicLift = opts -> (r,B) -> (
     s := rawLift(raw B, raw r);
     if s =!= null then new B from s
     else if opts.Verify then error "cannot lift given ring element")
multipleBasicLift = opts -> (r,v) -> ( 
     r = raw r; 
     scan(v, B -> (
	       r = rawLift(raw B, r);
	       if r === null then break;
	       ));
     if r =!= null then new v#-1 from r
     else if opts.Verify then error "cannot lift given ring element")
basicPromote = (r,B) -> new B from rawPromote(raw B, raw r)
multipleBasicPromote = (r,v) -> (
     r = raw r;
     scan(v, B -> r = rawPromote(raw B, r));
     new v#-1 from r)

protect promoteDegree
protect liftDegree
rk := v -> if instance(v,ZZ) then v else #v
makepromoter = memoize (
     degreerank -> (
     	       zr := toList ( degreerank : 0 );
	       v -> toList (rk v : zr)))

basicPromoteMatrix = (m,R,p) -> (
     dF := p minus degrees target m;
     dG := p minus degrees source m;
     F := R^dF;
     G := R^dG;
     map(F,G, rawPromote(raw F, raw m)))

multipleBasicPromoteMatrix = (m,v) -> (
     dF := - degrees target m;
     dG := - degrees source m;
     m = raw m;
     local S;
     scan(v, (R,p) -> ( S = R; dF = p dF; dG = p dG; m = rawPromote((raw R)^dF, m)));
     map(S^dF,S^dG,m))

basicLiftMatrix = opts -> (m,R,p) -> (
     dF := p minus degrees target m;
     dG := p minus degrees source m;
     F := R^dF;
     G := R^dG;
     n := rawLift(raw F, raw m);
     if n =!= null then map(F,G,n)
     else if opts.Verify then error "cannot lift given matrix")
multipleBasicLiftMatrix = opts -> (m,v) -> (
     dF := - degrees target m;
     dG := - degrees source m;
     m = raw m;
     local S;
     scan(v, (R,p) -> ( 
	       S = R; 
	       dF = p dF; 
	       dG = p dG; 
	       m = rawLift((raw R)^dF, m); 
	       if m === null then break));
     if m =!= null then map(S^dF,S^dG,m)
     else if opts.Verify then error "cannot lift given matrix")

multipleBasicLiftDegrees = multipleBasicPromoteDegrees = (dF,v) -> ( scan(v, p -> dF = p dF); dF )

promote(ZZ,RingElement) := (n,R) -> new R from rawFromNumber(R,n)

commonRingInitializations = (F) -> (
     lift(F,F) := opts -> (f,F) -> f;
     promote(F,F) := (f,F) -> f;
     promote(List,F,F) := (m,F,G) -> m;
     promote(Matrix,F,F) := (m,F,G) -> m;
     lift(List,F,F) := opts -> (m,F,G) -> m;
     lift(Matrix,F,F) := opts -> (m,F,G) -> m;
     )

commonEngineRingInitializations = (F) -> (
     F.Engine = true;
     if debugLevel > 5 then (
	  registerFinalizer(F,"ring");
	  );
     commonRingInitializations F;
     F ? F := (f,g) -> raw f ? raw g;
     baserings := F.baseRings;
     n := # baserings;
     baserings = append(baserings, F);
     promoters:= apply(baserings, R -> if R.?promoteDegree then R.promoteDegree else identity);
     lifters  := apply(baserings, R -> if R.?liftDegree then R.liftDegree else identity);
     scan(n, i -> (
	       A := baserings#i;
	       Aclass := class 0_A;			    -- A might be RR_200 and Aclass would then be RR
	       if i == n-1 then (
		    promoter := promoters#n;
		    lifter := lifters#n;
	       	    promote(Aclass,F) := (
			 if ancestor(Number, A) 
		    	 then (n,R) -> new R from rawFromNumber(raw R,n)
	       	    	 else basicPromote);
		    lift(F,A) := opts -> (F,A) -> (basicLift opts)(F,A);
		    promote(Matrix,A,F) := (m,A,F) -> basicPromoteMatrix(m,F,promoter);
		    lift(Matrix,F,A) := opts -> (m,F,A) -> (basicLiftMatrix opts)(m,A,lifter);
		    promote(List,A,F) := (m,A,F) -> promoter m;
		    lift(List,F,A) := opts -> (m,F,A) -> lifter m;
		    )
	       else (
		    promoteChain := take(baserings, {i+1,n});
		    liftChain    := reverse take(baserings, {i,n-1});
		    promoteDegreesChain := take(promoters, {i+1,n});
		    liftDegreesChain    := reverse take(lifters, {i+1,n});
		    promoteMatrixChain := apply(promoteChain, promoteDegreesChain, identity);
		    liftMatrixChain    := apply(liftChain   , liftDegreesChain   , identity);
	       	    promote(Aclass,F) := (
			 if ancestor(Number, A)
		    	 then (n,R) -> new R from rawFromNumber(raw R,n)
	       	    	 else (a,F) -> multipleBasicPromote(a, promoteChain));
		    lift(F,A) := opts -> (f,A) -> (multipleBasicLift opts)(f, liftChain);
		    promote(Matrix,A,F) := (m,A,F) -> multipleBasicPromoteMatrix(m,promoteMatrixChain);
		    lift   (Matrix,F,A) := opts -> (m,F,A) -> (multipleBasicLiftMatrix opts)(m,liftMatrixChain);
		    promote(List,A,F) := (m,A,F) -> multipleBasicPromoteDegrees(m,promoteDegreesChain);
		    lift   (List,F,A) := (m,F,A) -> multipleBasicLiftDegrees(m,liftDegreesChain);
		    )));
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
RR _ EngineRing := RingElement => (i,R) -> new R from i_(R.RawRing)

new RingElement from RawRingElement := (R, f) -> (
     -- this might take too much time:
     -- if R.RawRing =!= rawRing f then error "internal error: raw ring mismatch encountered";
     new R from {f})

new EngineRing from RawRing := (EngineRing,R) -> (
     S := new EngineRing of RingElement;
     S.RawRing = R;
     S#1 = 1_S;
     S#0 = 0_S;
     S)

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
     generators FractionField := opts -> F -> if opts.CoefficientRing === F then {} else generators(last F.baseRings, opts) / (r -> promote(r,F))
           char FractionField := F -> char last F.baseRings
	    dim FractionField := F -> 0
            net FractionField := F -> if ReverseDictionary#?F then toString ReverseDictionary#F else net new FunctionApplication from { frac, last F.baseRings }
     expression FractionField := F -> (expression frac) (expression last F.baseRings)
       describe FractionField := F -> net expression F

-- freduce := (f) -> (numerator f)/(denominator f)

isHomogeneous FractionField := (F) -> isHomogeneous last F.baseRings

frac EngineRing := R -> if isField R then R else if R.?frac then R.frac else (
     o := options R;
     if o.Inverses then error "not implemented : fraction fields of rings with inverses";
     if o.WeylAlgebra =!= {} or R.?SkewCommutative
     then error "fraction field of non-commutative ring requested";
     R.frac = F := new FractionField from rawFractionRing R.RawRing;
     F.frac = F;
     F.baseRings = append(R.baseRings,R);
     -- F.promoteDegree = makepromoter 0;
     -- F.liftDegree = makepromoter degreeLength R;
     commonEngineRingInitializations F;
     factor F := options -> f -> factor numerator f / factor denominator f;
     toString F := x -> toString expression x;
     net F := x -> net expression x;
     baseName F := (f) -> (
	  if denominator f != 1 
	  then error "expected a generator"
	  else baseName numerator f);
     expression F := (f) -> expression numerator f / expression denominator f;
     numerator F := (f) -> new R from rawNumerator raw f;
     denominator F := (f) -> new R from rawDenominator raw f;
     fraction(F,F) := F / F := (x,y) -> if y != 0 then x//y else error "division by 0";
     fraction(R,R) := (r,s) -> new F from rawFraction(F.RawRing,raw r,raw s);
     F % F := (x,y) -> if y == 0 then x else 0_F;	    -- not implemented in the engine, for some reason
     F.generators = apply(generators R, m -> promote(m,F));
     if R.?generatorSymbols then F.generatorSymbols = R.generatorSymbols;
     if R.?generatorExpressions then F.generatorExpressions = R.generatorExpressions;
     if R.?generators then F.generators = apply(R.generators, r -> promote(r,F));
     if R.?indexSymbols then F.indexSymbols = applyValues(R.indexSymbols, r -> promote(r,F));
     if R.?indexStrings then F.indexStrings = applyValues(R.indexStrings, r -> promote(r,F));
     F)

-- methods for all ring elements

clean(RR,RingElement) := (epsilon,f) -> new RR from clean(epsilon,raw f)
norm(RR,RingElement) := (p,f) -> new RR from norm(p,raw f)
norm(RingElement) := (f) -> norm(numeric(precision f,infinity), f)

degreeLength Ring := R -> R.degreeLength

use Ring := R -> (
     if R.?ring then use R.ring;			    -- I'm not sure what this is for.  Which rings have this key?
     generators R;
     if R.?generators and R.?generatorSymbols then scan(R.generatorSymbols,R.generators,(sym,val) -> sym <- val);
     if R.?use then R.use R;
     R)

numgens EngineRing := R -> #R.generators

generators EngineRing := opts -> R -> if opts.CoefficientRing === null then R.generators else if opts.CoefficientRing === R then {} else errorGenCoeff()

part(ZZ,RingElement) := RingElement => (d,f) -> new ring f from rawGetPart(first \ degrees (ring f).flatmonoid, raw f, d, d)

part(List,RingElement) := RingElement => (d,f) -> (
     if degreeLength ring f =!= #d
     then error ("degree length of ring element doesn't match specified degree");
     u := select(terms f, t -> d === degree t);
     if #u === 0 then 0_(ring f)
     else sum u
     )
part(Sequence,RingElement) := Sequence => (s,f) -> apply(s,d->part(d,f))

Ring _ ZZ := RingElement => (R,i) -> (generators R)#i

protect numallvars

EngineRing _ ZZ := (R,i) -> (
     if i < 0 or R.?numallvars and i >= R.numallvars then error("index ", toString i, " out of bounds 0 .. ", toString (R.numallvars-1));
     new R from R.RawRing_i
     )

size RingElement := f -> rawTermCount(numgens ring f, raw f)

isHomogeneous RingElement := f -> rawIsHomogeneous raw f

- RingElement := RingElement => x -> new ring x from -raw x

RingElement ? ZZ := (x,n) -> x ? n_(class x)
ZZ ? RingElement := (m,y) -> m_(class y) ? y

RingElement ^ ZZ := RingElement => (x,i) -> new ring x from (raw x)^i

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
     new k from rawLeadCoefficient(raw k, raw f))

degree RingElement := f -> if f == 0 then -infinity else rawMultiDegree raw f

leadTerm RingElement := RingElement => (f) -> someTerms(f,0,1)

Number % RingElement := (f,g) -> (try promote(f,class g) else error "can't promote number to ring") % g
RingElement % Number := (f,g) -> f % (try promote(g,class f) else error "can't promote number to ring")
RingElement % RingElement := RingElement => (f,g) -> (
     R := class f;
     S := class g;
     R % S := (
	  if R === S then (
	       (x,y) -> new R from raw x % raw y
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

Number // RingElement := (f,g) -> (try promote(f,class g) else error "can't promote number to ring") // g
RingElement // Number := (f,g) -> f // (try promote(g,class f) else error "can't promote number to ring")
RingElement // RingElement := RingElement => (f,g) -> (
     R := class f;
     S := class g;
     R // S := (
	  if R === S then (
	       (x,y) -> new R from raw x // raw y
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

Number - RingElement := (f,g) -> (try promote(f,class g) else error "can't promote number to ring") - g
RingElement - Number := (f,g) -> f - (try promote(g,class f) else error "can't promote number to ring")
RingElement - RingElement := RingElement => (f,g) -> (
     R := class f;
     S := class g;
     R - S := (
	  if R === S then (
	       (x,y) -> new R from raw x - raw y
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

Number * RingElement := (f,g) -> (try promote(f,class g) else error "can't promote number to ring") * g
RingElement * Number := (f,g) -> f * (try promote(g,class f) else error "can't promote number to ring")
RingElement * RingElement := RingElement => (f,g) -> (
     R := class f;
     S := class g;
     R * S := (
	  if R === S then (
	       (x,y) -> new R from raw x * raw y
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

Number + RingElement := (f,g) -> (try promote(f,class g) else error "can't promote number to ring") + g
RingElement + Number := (f,g) -> f + (try promote(g,class f) else error "can't promote number to ring")
RingElement + RingElement := RingElement => (f,g) -> (
     R := class f;
     S := class g;
     R + S := (
	  if R === S then (
	       (x,y) -> new R from raw x + raw y
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

ZZ == RingElement := (i,x) -> i == raw x
RingElement == ZZ := (x,i) -> raw x == i

Number == RingElement := (f,g) -> (try promote(f,class g) else error "can't promote number to ring") == g
RingElement == Number := (f,g) -> f == (try promote(g,class f) else error "can't promote number to ring")
RingElement == RingElement := (f,g) -> (
     R := class f;
     S := class g;
     R == S := (
	  if R === S then (
	       (x,y) -> raw x === raw y
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


fraction(RingElement,RingElement) := (r,s) -> (
     R := ring r;
     S := ring s;
     if R === S then (
	  frac R;
	  fraction(r,s))
     else error "numerator and denominator not in the same ring"
     )

Number / RingElement := (f,g) -> (try promote(f,class g) else error "can't promote number to ring") / g
RingElement / Number := (f,g) -> f / (try promote(g,class f) else error "can't promote number to ring")
RingElement / RingElement := RingElement => (f,g) -> (
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

isUnit(RingElement) := (f) -> 1 % ideal f == 0

Ring _ String := RingElement => (x,s) -> x.indexStrings#s
Ring _ Symbol := RingElement => (x,s) -> x.indexSymbols#s

ZZ _ Ring := RingElement => (i,R) -> (
     if i === 1 then R#1
     else if i === 0 then R#0
     else i * R#1
     )

isConstant RingElement := r -> liftable(r, coefficientRing ring r)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
