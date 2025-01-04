--		Copyright 1996-2002 by Daniel R. Grayson

needs "enginering.m2"
needs "quotring.m2"
needs "polyrings.m2"

GaloisField = new Type of EngineRing
GaloisField.synonym = "Galois field"

isField GaloisField := F -> true
isFinitePrimeField GaloisField := F -> F.degree == 1

expression GaloisField := F -> if hasAttribute(F,ReverseDictionary) then expression getAttribute(F,ReverseDictionary) else (expression GF) (expression F.order)
describe GaloisField := F -> Describe (expression GF) (expression F.order)

precision GaloisField := F -> infinity

generators GaloisField := opts -> (F) -> (
     if opts.CoefficientRing === null then F.generators
     else if opts.CoefficientRing === F then {}
     else apply(generators(ambient F,opts), m -> promote(m,F)))

ambient GaloisField := Ring => R -> last R.baseRings

---- only polynomial rings and quotients of them should have coefficientRings.
---- a Galois field hides that info
-- coefficientRing GaloisField := Ring => R -> coefficientRing last R.baseRings

GF = method (
     Options => { 
	  PrimitiveElement => FindOne,
	  Variable => null,
	  SizeLimit => 10000,
	  Strategy => null 
	    -- other Strategy options:
	    --   "Givaro":  use Givaro representation, but Conway polynomial, if one exists
	    --      gives an error if size is over the SizeLimit
	    --   "CompleteGivaro", uses Givaro representation, and also its choice of polynomial
	  }
     )

lastp := 2

unpack := (S) -> (			  -- a quotient ring
     if class S =!= QuotientRing
     then error("expected ",toString S," to be a quotient ring");
     R := ultimate(ambient, S);			  -- original poly ring
     if R === ZZ then (
	  if isField S then (
	       R = S[local t];
	       S = R/t;
	       )
     	  else error("expected ",toString S," to be a field")
	  );
     if class R =!= PolynomialRing
     or numgens R =!= 1
     then error("expected ",toString R," to be a polynomial ring in one variable");
     if degreeLength R =!= 1
     then error("expected ",toString R," to be a simply graded polynomial ring");
     p := char R;
     if coefficientRing R =!= ZZ/p or (p =!= lastp and not isPrime p)
     then error("expected ",toString R," to be a polynomial ring over a finite prime field");
     lastp = p;
     I := presentation S;			  -- a Matrix, sigh
     if rank source I =!= 1
     then error("expected ",toString S," to be a quotient ring by a principal ideal");
     f := I_(0,0);
     n := first degree f;
     (R,p,n,f))

isPrimitive = (g) -> g != 0 and (
     (R,p,n,f) := unpack ring g;
     q := p^n;
     all(factor (q-1), v -> 1 != g ^ ((q-1)//v#0)))

findPrimitive = (S) -> (
     -- S should be a polynomial ring of the form (ZZ/p)[x]/(f)
     -- 
     t := S_0;
     if isPrimitive t then t
     else (
	  n := S.degree;
	  p := char S;
	  local g;
	  while ( 
	       g = sum(n, i -> random p * t^i); 
	       not isPrimitive g
	  ) do ();
     	  g
	  )
     )

findGalois = method (Options => options GF)

findGalois(ZZ,ZZ) := RingElement => opts -> (p,n) -> (
     if not isPrime p then error "expected a prime number as base";
     if n <= 0 then error "expected positive exponent";
     x := opts.Variable;
     x = if x === null then getSymbol "a" else baseName x;
     R := (ZZ/p) (monoid [x]);
     t := R_0;
     local kk;
     local f;
     if opts.Strategy =!= "CompleteGivaro" then (
     	  f = runHooks(GaloisField,FindOne,(p,n,t));
     	  if f =!= null then (
	       kk = R/f;
	       kk.char = p;
	       kk.degree = n;
	       kk.order = p^n;
	       kk_0)
     	  else (
	       while ( f = t^n + sum(n, i-> random p * t^i); not isPrime f) do ();
	       kk = R/f;
	       kk.char = p;
	       kk.degree = n;
	       kk.order = p^n;
	       findPrimitive kk)
	  )
     else error (opts.Strategy | " is not a valid argument for' Strategy' option")
     )

GF(ZZ,ZZ) := GaloisField => opts -> (p,n) -> (
     if not isPrime p then error "expected a prime number as base";
     if n <= 0 then error "expected positive exponent";
     if n == 1 and isMember(opts.Strategy, {"Old", "Aring"})
       then return ZZp(p, Strategy => opts.Strategy);
     x := opts.Variable;
     primelem := findGalois(p,n,opts);
     GF(ring primelem, PrimitiveElement=>primelem, Strategy=>opts.Strategy, 
         SizeLimit=>opts.SizeLimit, Variable=>opts.Variable)
     )

--     x = if x === null then getSymbol "a" else baseName x;
--     R := (ZZ/p) (monoid [x]);
--     t := R_0;
--     f := runHooks(GaloisField,FindOne,(p,n,t));
--     if f =!= null then (
--	  k := R/f;
--	  GF(k,Variable => x, PrimitiveElement => k_0))
--     else (
--	  while ( f = t^n + sum(n, i-> random p * t^i); not isPrime f) do ();
--	  GF(R/f,Variable => x)))

GF(ZZ) := GaloisField => opts -> (q) -> (
     factors := factor q;
     if #factors =!= 1 or factors#0#0 === -1
     then error "expected a power of a prime";
     GF(factors#0#0,factors#0#1,opts))

GF(Ring) := GaloisField => opts -> (S) -> (
     (R,p,n,f) := unpack S;
     if not isPrime f
     then error("expected ",toString S," to be a quotient ring by an irreducible polynomial");
     primitiveElement := opts.PrimitiveElement;
     if primitiveElement === FindOne then (
	  t := S_0;
	  if isPrimitive t then primitiveElement = t
	  else while ( 
	       primitiveElement = sum(n, i -> random p * t^i); 
	       not isPrimitive primitiveElement
	       ) do ())
     else (
	  if ring primitiveElement =!= S then error "expected primitive element in the right ring";
     	  if not isPrimitive primitiveElement then error "expected ring element to be primitive";
     	  );
     var := opts.Variable;
     if var =!= null then var = baseName var
     else (
	  var = S.generatorSymbols#0;
	  );
     d := p^n-1;
     typ := opts.Strategy;
     if d >= opts.SizeLimit or primitiveElement != S_0 then (
         typ = "FlintBig";
         primitiveElement = S_0; -- Possibly NOT the primitive element in this case!!  We don't need it, and we don't want to compute it yet.
           -- Note: we used to have Galois fields always encoded by powers of the primitive element.  Ring maps would use this
           -- (in ringmap.m2) to help tell the engine where the primitive element goes.
           -- But: for FlintBig, this isn't being used.  We should perhaps consider changing the code in ringmap.m2.
           -- For now, setting primitiveElement will do.
         )
     else if typ === null then typ = "Flint";
     --if d < opts.SizeLimit or opts.Strategy === "FlintBig"
     --then (
	  -- three cases: call rawGaloisField, rawARingGaloisField, rawARingGaloisField1
	  rawF := if typ === "Old" then 
                       rawGaloisField raw primitiveElement
		  else if typ === "New" then
		      rawARingGaloisField1 raw primitiveElement
                  else if typ === "FlintBig" then
                       rawARingGaloisFieldFlintBig raw S_0 -- we do not pass a primitive element in this case
                  else if typ === "Flint" then
                       rawARingGaloisFieldFlintZech raw primitiveElement
                  else error(///unknown type of Galois Field requested:///|opts.Strategy|///Possible values include "Flint", "FlintBig", "Old", "New"///);
	  F := new GaloisField from rawF;
     	  F.degreeLength = 0;
	  F.rawGaloisField = true;
	--  )
     F.degreeLength = 0;
     F.PrimitiveElement = primitiveElement;		    -- notice the primitive element is not in F
     F.isBasic = true;
     toString F := h -> toString expression h;
     net F := h -> net expression h;
     F.baseRings = append(S.baseRings,S);
     F.promoteDegree = makepromoter 0;			    -- do this before commonEngineRingInitializations
     F.liftDegree = makepromoter degreeLength S;	    -- do this before commonEngineRingInitializations
     commonEngineRingInitializations F;
     F.isCommutative = true;
     expression F := t -> expression lift(t, S);
     F.char = p;
     F.degree = n;
     F.order = p^n;
     F.frac = F;
     F.generators = apply(generators S, m -> promote(m,F)); -- this will be wrong if S is a tower
     if S.?generatorSymbols then F.generatorSymbols = S.generatorSymbols;
     if S.?generatorExpressions then F.generatorExpressions = (
	  S.generatorExpressions
	  -- apply(S.generatorExpressions,F.generators,(e,x) -> new Holder2 from {e#0,x})
	  );
     if S.?generators then F.generators = apply(S.generators, r -> promote(r,F));
     if S.?indexSymbols then F.indexSymbols = applyValues(S.indexSymbols, r -> promote(r,F));
     if S.?indexStrings then F.indexStrings = applyValues(S.indexStrings, r -> promote(r,F));
     baseName F := y -> (
	  if F_0 == y then var else error "expected a generator"
	  );
     F.use = F -> var <- F_0;
     F.use F;
     F.cache = new CacheTable;
     F / F := (x,y) -> if y == 0 then error "division by zero" else x // y;
     F % F := (x,y) -> if y == 0 then x else 0_F;
     F)

random GaloisField := opts -> F -> (
     p := char F;
     t := F_0;
     sum(F.degree, i -> random p * t^i)
     )

dim GaloisField := ZZ => R -> 0

isField Ring := Boolean => R -> R.?isField and R.isField

isAffineRing = method(TypicalValue => Boolean)
isAffineRing Ring := isField
isAffineRing PolynomialRing := R -> isCommutative R and not (options R).Inverses and isAffineRing coefficientRing R
isAffineRing QuotientRing := R -> isField R or isAffineRing ambient R

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
