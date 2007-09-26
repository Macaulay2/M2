--		Copyright 1993-2002 by Daniel R. Grayson

-----------------------------------------------------------------------------

Monoid = new Type of Type
Monoid.synonym = "monoid"
use Monoid := x -> ( if x.?use then x.use x; x)

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

degrees PolynomialRing := R -> degrees monoid R

exponents RingElement := (f) -> listForm f / ( (monom,coeff) -> monom )

expression PolynomialRing := R -> (
     if ReverseDictionary#?R then return expression ReverseDictionary#R;
     k := last R.baseRings;
     T := if (options R).Local === true then List else Array;
     (expression if ReverseDictionary#?k then ReverseDictionary#k else k) (new T from (monoid R).generatorExpressions)
     )

describe PolynomialRing := R -> (
     k := last R.baseRings;
     net ((expression if ReverseDictionary#?k then ReverseDictionary#k else k) (expression monoid R)))
toExternalString PolynomialRing := R -> (
     k := last R.baseRings;
     toString ((expression if ReverseDictionary#?k then ReverseDictionary#k else k) (expression monoid R)))

tex PolynomialRing := R -> "$" | texMath R | "$"	    -- silly!

texMath PolynomialRing := R -> (
     if R.?tex then R.tex
     else if ReverseDictionary#?R then "\\text{" | toString ReverseDictionary#R  | "}"
     else (texMath last R.baseRings)|(texMath expression monoid R)
     )

net PolynomialRing := R -> (
     if ReverseDictionary#?R then toString ReverseDictionary#R
     else net expression R)
toString PolynomialRing := R -> (
     if ReverseDictionary#?R then toString ReverseDictionary#R
     else toString expression R)

degreeLength PolynomialRing := (RM) -> degreeLength monoid RM

protect basering
protect flatmonoid

degreesRing ZZ := PolynomialRing => memoize(
     (n) -> (
	  local ZZn;
	  local Zn;
	  if n == 0 then (
	       ZZn = new PolynomialRing from rawPolynomialRing();
	       ZZn.basering = ZZ;
	       ZZn.flatmonoid = ZZn.monoid = monoid[];
	       ZZn.numallvars = 0;
	       ZZn.baseRings = {ZZ};
	       ZZn.degreesRing = ZZn;
	       ZZn.isCommutative = true;
	       ZZn.generatorSymbols = {};
	       ZZn.generatorExpressions = {};
	       ZZn.generators = {};
	       ZZn.indexSymbols = new HashTable;
	       ZZn.indexStrings = new HashTable;
	       ZZn)
	  else ZZ degreesMonoid n))

degreesRing PolynomialRing := PolynomialRing => R -> (
     if R.?degreesRing then R.degreesRing
     else degreesRing degreeLength R)

degreesRing Ring := R -> error "no degreesRing for this ring"

generators PolynomialRing := opts -> R -> (
     if opts.CoefficientRing === null then R.generators
     else if opts.CoefficientRing === R then {}
     else join(R.generators, generators(coefficientRing R, opts) / (r -> promote(r,R))))
coefficientRing PolynomialRing := Ring => R -> last R.baseRings
isHomogeneous PolynomialRing := R -> (
     k := coefficientRing R;
     isField k or isHomogeneous k)

standardForm RingElement := (f) -> (
     R := ring f;
     k := coefficientRing R;
     (cc,mm) := rawPairs(raw k, raw f);
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
     (cc,mm) := rawPairs(raw k, raw f);
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

protect diffs0						    -- private keys for storing info about indices of WeylAlgebra variables
protect diffs1

protect indexStrings
protect generatorSymbols
protect generatorExpressions
protect indexSymbols

Ring OrderedMonoid := PolynomialRing => (			  -- no memoize
     (R,M) -> (
	  if not M.?RawMonoid then error "expected ordered monoid handled by the engine";
	  if not R.?RawRing then error "expected coefficient ring handled by the engine";
     	  num := numgens M;
	  (basering,flatmonoid,numallvars) := (
	       if R.?isBasic then (R,M,num)
	       else if R.?basering and R.?flatmonoid 
	       then ( 
		    R.basering, 
		    tensor(M, R.flatmonoid,
			 Degrees => (
			      degrees M
			      |
			      if M.Options.ConstantCoefficients
			      then toList ( numgens R.flatmonoid : toList (degreeLength M : 0))
			      else (
				   if degreeLength R.flatmonoid =!= degreeLength M then error "expected coefficient ring to have the same degree length";
				   degrees R.flatmonoid
				   )
			      ),
			 Heft => M.Options.Heft
			 ),
		    num + R.numallvars)
	       else if instance(R,FractionField) then (R,M,num)
	       else error "internal error: expected coefficient ring to have a base ring and a flat monoid"
	       );
     	  local RM;
	  quotfix := rawRM -> if isQuotientOf(PolynomialRing,R) then rawQuotientRing(rawRM, raw R) else rawRM;
	  Weyl := M.Options.WeylAlgebra =!= {};
	  skews := monoidIndices(M,M.Options.SkewCommutative);
	  degRing := if degreeLength M != 0 then degreesRing degreeLength M else ZZ;
	  coeffOptions := options R;
	  coeffWeyl := coeffOptions =!= null and coeffOptions.WeylAlgebra =!= {};
	  coeffSkew := coeffOptions =!= null and coeffOptions.SkewCommutative =!= {};
	  if Weyl or coeffWeyl then (
	       if Weyl and R.?SkewCommutative then error "coefficient ring has skew commuting variables";
	       if Weyl and skews =!= {} then error "skew commutative Weyl algebra requested";
	       diffs := M.Options.WeylAlgebra;
	       if class diffs === Option then diffs = {diffs}
	       else if class diffs =!= List then error "expected list as WeylAlgebra option";
	       diffs = apply(diffs, x -> if class x === Option then toList x else x);
	       h    := select(diffs, x -> class x =!= List);
	       if #h > 1 then error "WeylAlgebra: expected at most one homogenizing variable";
	       h = monoidIndices(M,h);
	       if #h === 1 then h = h#0 else h = -1;
     	       if R.?homogenize then (
		    if h == -1 then h = R.homogenize + num
		    else if R.homogenize + num =!= h then error "expected the same homogenizing variable";
		    )
	       else if coeffWeyl and h != -1 then error "coefficient Weyl algebra has no homogenizing variable";
	       diffs = select(diffs, x -> class x === List);
	       diffs = apply(diffs, x -> (
			 if #x =!= 2 then error "WeylAlgebra: expected x=>dx, {x,dx}";
			 if class x#0 === Sequence and class x#1 === Sequence
			 then (
			      if #(x#0) =!= #(x#1) then error "expected sequences of the same length";
			      mingle x
			      )
			 else toList x
			 ));
	       diffs = flatten diffs;
	       local diffs0; local diffs1;
	       diffs = pack(2,diffs);
	       diffs0 = monoidIndices(M,first\diffs);
	       diffs1 = monoidIndices(M,last\diffs);
	       if any(values tally join(diffs0,diffs1), n -> n > 1) then error "WeylAlgebra option: a variable specified more than once";
	       if coeffWeyl then (
		    diffs0 = join(diffs0, apply(R.diffs0, i -> i + num));
		    diffs1 = join(diffs1, apply(R.diffs1, i -> i + num));
		    );
	       scan(diffs0,diffs1,(x,dx) -> if not x<dx then error "expected differentiation variables to occur to the right of their variables");
	       RM = new PolynomialRing from quotfix rawWeylAlgebra(rawPolynomialRing(raw basering, raw flatmonoid),diffs0,diffs1,h);
	       RM.diffs0 = diffs0;
	       RM.diffs1 = diffs1;
     	       addHook(RM, QuotientRingHook, S -> (S.diffs0 = diffs0; S.diffs1 = diffs1));
     	       if h != -1 then RM.homogenize = h;
	       )
	  else if skews =!= {} or R.?SkewCommutative then (
	       if R.?diffs0 then error "coefficient ring is a Weyl algebra";
	       if R.?SkewCommutative then skews = join(skews, apply(R.SkewCommutative, i -> i + num));
	       RM = new PolynomialRing from quotfix rawSkewPolynomialRing(rawPolynomialRing(raw basering, raw flatmonoid),skews);
	       RM.SkewCommutative = skews;
	       )
	  else (
	       log := FunctionApplication {rawPolynomialRing, (raw basering, raw flatmonoid)};
	       RM = new PolynomialRing from quotfix value log;
	       RM#"raw creation log" = Bag {log};
	       );
	  RM.basering = basering;
	  RM.flatmonoid = flatmonoid;
	  RM.numallvars = numallvars;
	  RM.promoteDegree = makepromoter degreeLength M;
	  RM.liftDegree = makepromoter degreeLength R;
	  RM.baseRings = append(R.baseRings,R);
	  commonEngineRingInitializations RM;
	  RM.monoid = M;
	  RM.degreesRing = degRing;
	  RM.isCommutative = not Weyl and not RM.?SkewCommutative;
     	  ONE := RM#1;
	  if R.?char then RM.char = R.char;
	  RM _ M := (f,m) -> new R from rawCoefficient(R.RawRing, raw f, raw m);
	  expression RM := f -> (
	       (coeffs,monoms) -> (
		    if #coeffs === 0
		    then expression 0
		    else sum(coeffs,monoms, (a,m) -> expression (if a == 1 then 1 else new R from a) * expression (if m == 1 then 1 else new M from m))
		    )
	       ) rawPairs(raw R, raw f);
	  toString RM := toExternalString RM := x -> toString expression x;
	  factor RM := opts -> f -> (
	       c := 1;
	       (facs,exps) := rawFactor raw f;	-- example value: ((11, x+1, x-1, 2x+3), (1, 1, 1, 1)); constant term is first, if there is one
     	       facs = apply(facs, p -> new RM from p);
	       if liftable(facs#0,R) then (
		    -- factory returns the possible constant factor in front
	       	    assert(exps#0 == 1);
		    c = facs#0;
		    facs = drop(facs,1);
		    exps = drop(exps,1);
		    );
	       if #facs != 0 then (facs,exps) = toSequence transpose sort transpose {toList facs, toList exps};
	       if c != 1 then (
		    -- we put the possible constant factor at the end
		    facs = append(facs,c);
		    exps = append(exps,1);
		    );
	       new Product from apply(facs,exps,(p,n) -> new Power from {p,n}));
	  isPrime RM := f -> (
	       v := factor f;				    -- constant term last
	       #v === 1 and last v#0 === 1 and not isConstant first v#0
	       or
	       #v === 2 and v#0#1 === 1 and isConstant first v#0 and v#1#1 === 1
	       );
	  RM.generatorSymbols = M.generatorSymbols;
	  RM.generatorExpressions = M.generatorExpressions;
	  RM.generators = apply(num, i -> RM_i);
	  RM.indexSymbols = new HashTable from join(
	       if R.?indexSymbols then apply(pairs R.indexSymbols, (nm,x) -> nm => new RM from rawPromote(raw RM,raw x)) else {},
	       apply(num, i -> M.generatorSymbols#i => RM_i)
	       );
     	  RM.indexStrings = applyKeys(RM.indexSymbols, toString);
	  RM))

samering := (f,g) -> (
     if ring f =!= ring g then error "expected elements from the same ring";
     )

Ring Array := PolynomialRing => (R,variables) -> use R monoid variables
Ring List := PolynomialRing => (R,variables) -> use R monoid (variables,Local => true)
PolynomialRing _ List := (R,v) -> product ( #v , i -> R_i^(v#i) )
Ring _ List := RingElement => (R,w) -> product(#w, i -> (R_i)^(w_i))
dim PolynomialRing := R -> dim coefficientRing R + # generators R - if R.?SkewCommutative then #R.SkewCommutative else 0
char PolynomialRing := (R) -> char coefficientRing R
numgens PolynomialRing := R -> numgens monoid R
isSkewCommutative PolynomialRing := R -> isSkewCommutative coefficientRing R or 0 < #(options R).SkewCommutative
weightRange = method()
weightRange(List,RingElement) := (w,f) -> rawWeightRange(w,raw f)
weightRange RingElement := f -> weightRange(first \ degrees ring f, f)
parts = method()
parts RingElement := f -> sum(select(apply(
	       ((i,j) -> i .. j) weightRange(first \ degrees (ring f).flatmonoid, f),
	       n -> part_n f), p -> p != 0), p -> new Parenthesize from {p})
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
