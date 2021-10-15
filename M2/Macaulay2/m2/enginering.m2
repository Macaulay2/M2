--		Copyright 1995-2002 by Daniel R. Grayson

-- TODO: some functions seems to depend on other files
needs "expressions.m2"
needs "remember.m2"
needs "rings.m2"

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
hasEngineLinearAlgebra Ring := (R) -> instance(R, InexactField) or R#?"EngineLinearAlgebra" -- used to decide which algorithm to use
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
basicLift = opts -> (r,Brawring,Bclass) -> (
     s := rawLift(Brawring, raw r);
     if s =!= null then new Bclass from s
     else if opts.Verify then error "cannot lift given ring element")
multipleLift = opts -> (r,v) -> ( 
     r = raw r; 
     scan(v, B -> (
	       r = rawLift(raw B, r);
	       if r === null then break;
	       ));
     if r =!= null then new v#-1 from r
     else if opts.Verify then error "cannot lift given ring element")
basicPromote = (r,B) -> new B from rawPromote(raw B, raw r)
multiplePromote = (r,v) -> (
     r = raw r;
     scan(v, B -> r = rawPromote(raw B, r));
     new v#-1 from r)

protect promoteDegree
protect liftDegree
rk := v -> if instance(v,ZZ) then v else #v
makepromoter = memoize (
     degreerank -> (
     	       zr := toList ( degreerank : 0 );
	       deg -> (
		    assert( instance(deg,List) );
		    toList (rk deg : zr)			    -- the naive degree map, that sends all degrees to zero
		    )
	       ))

basicPromoteModule = (M,R,p) -> (
     N := R^(p minus degrees M);
     if M.cache.?components then N.cache.components = apply(M.cache.components, M -> basicPromoteModule(M,R,p)); -- lift the direct summands, too
     N)

basicLiftModule = (M,R,p) -> (
     N := R^(p minus degrees M);
     if M.cache.?components then N.cache.components = apply(M.cache.components, M -> basicLiftModule(M,R,p)); -- lift the direct summands, too
     N)

multipleLiftModule = (M,v) -> (
     dM := - degrees M;
     local S;
     scan(v, (R,p) -> (S,dM) = (R,p dM));
     N := S^dM;
     if M.cache.?components then N.cache.components = apply(M.cache.components, M -> multipleLiftModule(M,v)); -- lift the direct summands, too
     N)

multiplePromoteModule = (M,v) -> (
     dM := - degrees M;
     local S;
     scan(v, (R,p) -> (S,dM) = (R,p dM));
     N := S^dM;
     if M.cache.?components then N.cache.components = apply(M.cache.components, M -> multiplePromoteModule(M,v)); -- lift the direct summands, too
     N)

basicPromoteMatrix = (m,R,p) -> (
     F := basicPromoteModule(target m,R,p);
     G := basicPromoteModule(source m,R,p);
     map(F, G, rawPromote(raw F, raw m)))

multiplePromoteMatrix = (m,v) -> (
     F := target m;
     dF := - degrees F;
     G := source m;
     dG := - degrees G;
     m = raw m;
     local S;
     scan(v, (R,p) -> ( S = R; dF = p dF; dG = p dG; m = rawPromote((raw R)^dF, m)));
     map(multiplePromoteModule(F,v),multiplePromoteModule(G,v),m))

basicLiftMatrix = opts -> (m,R,p) -> (
     F := basicLiftModule(target m, R, p);
     G := basicLiftModule(source m, R, p);
     n := rawLift(raw F, raw m);
     if n =!= null then map(F,G,n)
     else if opts.Verify then error "cannot lift given matrix")

multipleLiftMatrix = opts -> (m,v) -> (
     F := target m;
     G := source m;
     dF := - degrees F;
     dG := - degrees G;
     m = raw m;
     local S;
     scan(v, (R,p) -> ( 
	       S = R; 
	       dF = p dF; 
	       m = rawLift((raw R)^dF, m);		    -- lift as raw matrices to avoid making many intermediate free modules
	       if m === null then break;
	       dG = p dG; 
	       ));
     if m =!= null then map(multipleLiftModule(F,v),multipleLiftModule(G,v),m)
     else if opts.Verify then error "cannot lift given matrix")

basicPromoteMutableMatrix = (m,R) -> map(R, rawPromote(raw R, raw m))
multiplePromoteMutableMatrix = (m,v) -> (
     m = raw m;
     scan(v, B -> m = rawPromote(raw B, raw m));
     map(v#-1, m))

basicLiftMutableMatrix = opts -> (m,R) -> (
     n := rawLift(raw R, raw m);
     if n =!= null then map(R,n)
     else if opts.Verify then error "cannot lift given matrix")
multipleLiftMutableMatrix = opts -> (m,v) -> ( 
     m = raw m; 
     scan(v, B -> (
	       m = rawLift(raw B, m);
	       if m === null then break;
	       ));
     if m =!= null then map(v#-1, m)
     else if opts.Verify then error "cannot lift given mutable matrix")

-------------
multipleLiftDegrees = multiplePromoteDegrees = (dF,v) -> ( scan(v, p -> dF = p dF); dF )

promote(ZZ,RingElement) := (n,R) -> new R from rawFromNumber(raw R,n)

commonRingInitializations = (F) -> (
     lift(F,F) := opts -> (f,F) -> f;
     promote(F,F) := (f,F) -> f;
     promote(List,F,F) := (m,F,G) -> m;
     promote(Matrix,F,F) := (m,F,G) -> m;
     lift(List,F,F) := opts -> (m,F,G) -> m;
     lift(Matrix,F,F) := opts -> (m,F,G) -> m;
     promote(Module,F,F) := (M,F,G) -> M;
     lift(Module,F,F) := opts -> (M,F,G) -> M;
     lift(MutableMatrix,F,F) := opts -> (m,F,G) -> m;
     promote(MutableMatrix,F,F) := (m,F,G) -> m;
     )

commonEngineRingInitializations = (F) -> (
     F.Engine = true;
     commonRingInitializations F;
     F ? F := (f,g) -> raw f ? raw g;
     baserings := F.baseRings;
     n := # baserings;
     baserings = append(baserings, F);
     promoters:= apply(baserings, R -> if R.?promoteDegree then R.promoteDegree else identity);
     lifters  := apply(baserings, R -> if R.?liftDegree then R.liftDegree else identity);
     scan(n, i -> (
	       A := baserings#i;
	       Aclass := class 0_A;			    -- A might be RR_200 and Aclass would then be RR; sometimes A is RR, too (?)
	       rawA := raw ring 0_A;
	       if i == n-1 then (
		    promoter := promoters#n;
		    lifter := lifters#n;
	       	    promote(Aclass,F) := (
			 if ancestor(Number, A) 
		    	 then (n,R) -> new R from rawFromNumber(raw R,n)
	       	    	 else basicPromote);
		    lift(F,A) := opts -> (F,A) -> (basicLift opts)(F,rawA,Aclass);
     	       	    promote(Module,A,F) := (M,A,F) -> basicPromoteModule(M,F,promoter);
     	       	    lift(Module,F,A) := opts -> (M,F,A) -> basicLiftModule(M,A,lifter);
		    promote(Matrix,A,F) := (m,A,F) -> basicPromoteMatrix(m,F,promoter);
		    lift(Matrix,F,A) := opts -> (m,F,A) -> (basicLiftMatrix opts)(m,A,lifter);
		    promote(MutableMatrix,A,F) := (m,A,F) -> basicPromoteMutableMatrix(m,F);
		    lift(MutableMatrix,F,A) := opts -> (m,F,A) -> (basicLiftMutableMatrix opts)(m,A);
		    promote(List,A,F) := (m,A,F) -> promoter m;
		    lift(List,F,A) := opts -> (m,F,A) -> lifter m;
		    )
	       else (
		    promoteChain := take(baserings, {i+1,n});
		    liftChain    := reverse take(baserings, {i,n-1});
		    promoteDegreesChain := take(promoters, {i+1,n});
		    liftDegreesChain    := reverse take(lifters, {i+1,n});
		    promoteRingsAndDegreesChain := apply(promoteChain, promoteDegreesChain, identity);
		    liftRingsAndDegreesChain    := apply(liftChain   , liftDegreesChain   , identity);
	       	    promote(Aclass,F) := (
			 if ancestor(Number, A)
		    	 then (n,R) -> new R from rawFromNumber(raw R,n)
	       	    	 else (a,F) -> multiplePromote(a, promoteChain));
		    lift(F,A) := opts -> (f,A) -> (multipleLift opts)(f, liftChain);
		    promote(Module,A,F) := (M,A,F) -> multiplePromoteModule(M,liftRingsAndDegreesChain);
		    lift   (Module,F,A) := opts -> (M,F,A) -> multipleLiftModule(M,liftRingsAndDegreesChain);
		    promote(Matrix,A,F) := (m,A,F) -> multiplePromoteMatrix(m,promoteRingsAndDegreesChain);
		    lift   (Matrix,F,A) := opts -> (m,F,A) -> (multipleLiftMatrix opts)(m,liftRingsAndDegreesChain);
		    promote(MutableMatrix,A,F) := (m,A,F) -> multiplePromoteMutableMatrix(m,promoteChain);
		    lift   (MutableMatrix,F,A) := opts -> (m,F,A) -> (multipleLiftMutableMatrix opts)(m,liftChain);
		    promote(List,A,F) := (m,A,F) -> multiplePromoteDegrees(m,promoteDegreesChain);
		    lift   (List,F,A) := (m,F,A) -> multipleLiftDegrees(m,liftDegreesChain);
		    )));
     if debugLevel > 25 then (
	  registerFinalizer(F,"ring");
	  );
     -*
     if debugLevel > 50 then (
	  registerFinalizer(F#0,"ring 0");
	  registerFinalizer(F#1,"ring 1");
	  );
     *-
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

expression EngineRing := R -> if hasAttribute(R,ReverseDictionary) then expression getAttribute(R,ReverseDictionary) else expression toString R.RawRing -- should never be used
texMath EngineRing := R -> texMath expression R
toString EngineRing := toString @@ expression
net EngineRing := net @@ expression

ZZ _ EngineRing := 
RR _ EngineRing :=
RRi _ EngineRing := RingElement => (i,R) -> new R from i_(R.RawRing)

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
        degrees FractionField := F -> degrees last F.baseRings
      precision FractionField := F -> precision last F.baseRings
        numgens FractionField := F -> numgens last F.baseRings
     generators FractionField := opts -> F -> if opts.CoefficientRing === F then {} else generators(last F.baseRings, opts) / (r -> promote(r,F))
           char FractionField := F -> char last F.baseRings
	    dim FractionField := F -> 0
     expression FractionField := F -> if hasAttribute(F,ReverseDictionary) then expression getAttribute(F,ReverseDictionary) else (expression frac) (expression last F.baseRings)
     describe FractionField := F -> Describe (expression frac) (describe last F.baseRings)
     toExternalString FractionField := F -> toString describe F

-- freduce := (f) -> (numerator f)/(denominator f)

factoryAlmostGood = R -> (
     k := coefficientRing R;
     k === QQ or
     k === ZZ or
     instance(k,QuotientRing) and ambient k === ZZ and isPrime char k or
     instance(k,GaloisField))
factoryGood = R -> factoryAlmostGood R and not (options R).Inverses

frac EngineRing := R -> if isField R then R else if R.?frac then R.frac else (
     o := options R;
     if o.Inverses then error "not implemented : fraction fields of rings with inverses";
     if o.WeylAlgebra =!= {} or R.?SkewCommutative
     then error "fraction field of non-commutative ring requested";
     if not factoryGood R then error "not implemented yet: fraction fields of polynomial rings over rings other than ZZ, QQ, or a finite field";
     R.frac = F := new FractionField from rawFractionRing R.RawRing;
     F.frac = F;
     F.isCommutative = true;
     F.baseRings = append(R.baseRings,R);
     F.isHomogeneous = isHomogeneous R and all (degrees R, deg -> all (deg, i -> i === 0));
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
     if R.?generators then F.generators = apply(R.generators, r -> promote(r,F));
     if R.?generatorExpressions then F.generatorExpressions = (
	  R.generatorExpressions
	  -- apply(R.generatorExpressions,F.generators,(e,x)->new Holder2 from {e#0,x})
	  );
     if R.?indexSymbols then F.indexSymbols = applyValues(R.indexSymbols, r -> promote(r,F));
     if R.?indexStrings then F.indexStrings = applyValues(R.indexStrings, r -> promote(r,F));
     if R.?numallvars then F.numallvars=R.numallvars;
     F)

-- methods for all ring elements

clean(RR,Number) := (epsilon,f) -> if abs f < epsilon then f-f else f
clean(RR,RingElement) := (epsilon,f) -> new ring f from clean(epsilon,raw f)

norm(List) := z -> max(abs\z)
norm(RR,Number) := (p,z) -> p-p+abs z
norm(RR,RingElement) := (p,f) -> new RR from norm(p,raw f)
norm(InfiniteNumber,Number) := 
norm(InfiniteNumber,RingElement) := (p,f) -> norm(numeric(precision f, p), f)
norm(RingElement) := (f) -> norm(numeric(precision f,infinity), f)
norm Number := abs

degreeLength Ring := R -> R.degreeLength

use Ring := R -> (
     if R.?ring then use R.ring;			    -- I'm not sure what this is for.  Which rings have this key?
     generators R;
     if R.?generators and R.?generatorSymbols then scan(R.generatorSymbols,R.generators,(sym,val) -> sym <- val);
     if R.?use then R.use R;
     R)

numgens EngineRing := R -> #R.generators

generators EngineRing := opts -> R -> if opts.CoefficientRing === null then R.generators else if opts.CoefficientRing === R then {} else errorGenCoeff()

prepwts = wts -> (
     wts = spliceInside toList wts;
     if not isListOfIntegers wts then error "part: expected a list of integers";
     wts)

part(Nothing,Nothing,VisibleList,RingElement) := 
part(Nothing,ZZ,VisibleList,RingElement) := 
part(ZZ,Nothing,VisibleList,RingElement) := 
part(ZZ,ZZ,VisibleList,RingElement) := RingElement => (d,e,wts,f) -> new ring f from rawGetPart(prepwts wts, raw f, d, e)

part(InfiniteNumber,ZZ,VisibleList,RingElement) := RingElement => (d,e,wts,f) -> (
     if d < 0 then part(,e,wts,f) else 0_(ring f))
part(ZZ,InfiniteNumber,VisibleList,RingElement) := RingElement => (d,e,wts,f) -> (
     if e > 0 then part(d,,wts,f) else 0_(ring f))
part(InfiniteNumber,InfiniteNumber,VisibleList,RingElement) := RingElement => (d,e,wts,f) -> (
     prepwts wts;				    -- just for the error messages
     if d < e then f else 0_(ring f))

defaultWeight := f -> (
     if degreeLength ring f =!= 1 then error "part: expected a singly graded ring";
     flatten degrees (ring f).FlatMonoid)

part(Nothing,Nothing,RingElement) :=
part(InfiniteNumber,InfiniteNumber,RingElement) := 
part(InfiniteNumber,ZZ,RingElement) := 
part(ZZ,InfiniteNumber,RingElement) :=
part(Nothing,ZZ,RingElement) := 
part(ZZ,Nothing,RingElement) :=
part(ZZ,ZZ,RingElement) := RingElement => (d,e,f) -> part(d,e,defaultWeight f,f)

part(ZZ,RingElement) := RingElement => (d,f) -> part(d,d,f)
part(ZZ,VisibleList,RingElement) := RingElement => (d,wts,f) -> part(d,d,wts,f)

part(InfiniteNumber,InfiniteNumber,Number) := 
part(ZZ,InfiniteNumber,Number) := 
part(InfiniteNumber,ZZ,Number) := 
part(ZZ,ZZ,Number) := (d,e,r) -> if d <= 0 and e >= 0 then r else 0_(ring r)

part(ZZ,Number) := (d,r) -> part(d,d,r)

part(List,RingElement) := RingElement => (d,f) -> (
     if degreeLength ring f =!= #d then error ("degree length of ring element doesn't match specified degree");
     u := select(terms f, t -> d === degree t);		    -- this is slow!
     if #u === 0 then 0_(ring f)
     else sum u
     )

Ring _ ZZ := RingElement => (R,i) -> (generators R)#i

protect numallvars

EngineRing _ ZZ := (R,i) -> (
     if i < 0 or R.?numallvars and i >= R.numallvars then error("index ", toString i, " out of bounds 0 .. ", toString (R.numallvars-1));
     new R from R.RawRing_i
     )

size RingElement := f -> rawTermCount(numgens ring f, raw f)

isHomogeneous RingElement := f -> isHomogeneous ring f and rawIsHomogeneous raw f

+ RingElement := identity
- RingElement := RingElement => x -> new ring x from -raw x

RingElement ? ZZ := (x,n) -> x ? n_(class x)
ZZ ? RingElement := (m,y) -> m_(class y) ? y

RingElement ^ ZZ := RingElement => (x,i) -> new ring x from (raw x)^i

toString RingElement := x -> toString expression x
toExternalString RingElement := x -> toExternalFormat expression x
net RingElement := x -> net expression x
texMath RingElement := x -> texMath expression x

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
     promote(rawLeadCoefficient(raw k, raw f),k))

degree RingElement := f -> if f == 0 then -infinity else rawMultiDegree raw f

leadTerm RingElement := RingElement => (f) -> someTerms(f,0,1)

oops := () -> error "can't promote number to ring"
promoteleftinexact = (f,g) -> (
     p := precision ring g;
     if p === infinity then oops();
     f = numeric(p,f);
     f = try promote(f,class g) else oops();
     (f,g))
promoteleftexact = (f,g) -> (
     f = try promote(f,class g) else oops();
     (f,g))
exchange = (x,y) -> (y,x)
promoterightexact   = exchange @@ promoteleftexact   @@ exchange
promoterightinexact = exchange @@ promoteleftinexact @@ exchange

divmod := R -> (f,g) -> (
     (q,r) := rawDivMod(raw f, raw g);
     (new R from q, new R from r))
quotientRemainder(RingElement,RingElement) := (f,g) -> (
     R := ring f;
     S := ring g;
     m := quotientRemainder(R,S) := (
	  if R === S then divmod R
	  else if member(R,S.baseRings) then (
	       (x,y) -> divmod(promote(x,S), y)
	       )
	  else if member(S,R.baseRings) then (
	       (x,y) -> divmod(x, promote(y,R))
	       )
	  else error "expected pair to have a method for quotientRemainder"
	  );
     m(f,g))
quotientRemainder(Number,RingElement) := quotientRemainder @@ promoteleftexact
quotientRemainder(RingElement,Number) := quotientRemainder @@ promoterightexact
quotientRemainder(InexactNumber,RingElement) := quotientRemainder @@ promoteleftinexact
quotientRemainder(RingElement,InexactNumber) := quotientRemainder @@ promoterightinexact

rem0 := (f,g) -> f % g
Number % RingElement := rem0 @@ promoteleftexact
RingElement % Number := rem0 @@ promoterightexact
InexactNumber % RingElement := rem0 @@ promoteleftinexact
RingElement % InexactNumber := rem0 @@ promoterightinexact
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

quot0 = (f,g) -> f // g
Number // RingElement := quot0 @@ promoteleftexact
RingElement // Number := quot0 @@ promoterightexact
InexactNumber // RingElement := quot0 @@ promoteleftinexact
RingElement // InexactNumber := quot0 @@ promoterightinexact
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
     f // g)

Number - RingElement := difference @@ promoteleftexact
RingElement - Number := difference @@ promoterightexact
InexactNumber - RingElement := difference @@ promoteleftinexact
RingElement - InexactNumber := difference @@ promoterightinexact
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
     f - g)

Number * RingElement := times @@ promoteleftexact
RingElement * Number := times @@ promoterightexact
InexactNumber * RingElement := times @@ promoteleftinexact
RingElement * InexactNumber := times @@ promoterightinexact
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
     f * g)

Number + RingElement := plus @@ promoteleftexact
RingElement + Number := plus @@ promoterightexact
InexactNumber + RingElement := plus @@ promoteleftinexact
RingElement + InexactNumber := plus @@ promoterightinexact
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
     f + g)

ZZ == RingElement := (i,x) -> i == raw x
RingElement == ZZ := (x,i) -> raw x == i

eq0 = (f,g) -> f == g
Number == RingElement := eq0 @@ promoteleftexact
RingElement == Number := eq0 @@ promoterightexact
InexactNumber == RingElement := eq0 @@ promoteleftinexact
RingElement == InexactNumber := eq0 @@ promoterightinexact
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
     f / g)
frac0 = (f,g) -> f/g

Number / RingElement := frac0 @@ promoteleftexact
RingElement / Number := (f,g) -> (1/g) * f

InexactNumber / RingElement := frac0 @@ promoteleftinexact
RingElement / InexactNumber := (f,g) -> (1/g) * f

fraction(RingElement,RingElement) := (r,s) -> (
     R := ring r;
     S := ring s;
     if R =!= S then error "numerator and denominator not in the same ring";
     frac R;
     fraction(r,s))
-----------------------------------------------------------------------------

isUnit(RingElement) := (f) -> (
    if (options ring f).?Inverses and (options ring f).Inverses then 
      size f === 1 and isUnit leadCoefficient f
    else
      1 % ideal f == 0
    )

Ring _ String := RingElement => (x,s) -> x.indexStrings#s
Ring _ Symbol := RingElement => (x,s) -> x.indexSymbols#s

isConstant RingElement := r -> liftable(r, coefficientRing ring r)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
