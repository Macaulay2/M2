--		Copyright 1993-2002 by Daniel R. Grayson

-- TODO: seems to need quotring.m2 for isQuotientOf
needs "methods.m2"
needs "enginering.m2"
needs "monoids.m2"
needs "tables.m2"
needs "indeterminates.m2" -- runLengthEncode

-----------------------------------------------------------------------------
-- PolynomialRing type declaration and basic methods
-----------------------------------------------------------------------------

PolynomialRing = new Type of EngineRing
PolynomialRing.synonym = "polynomial ring"
PolynomialRing#{Standard,AfterPrint} = R -> (
    << endl << concatenate(interpreterDepth:"o") << lineNumber << " : "; -- standard template
    << "PolynomialRing";
    if #R.monoid.Options.WeylAlgebra > 0
    then << ", " << #R.monoid.Options.WeylAlgebra << " differential variables";
    if #R.monoid.Options.SkewCommutative > 0
    then << ", " << #R.monoid.Options.SkewCommutative << " skew commutative variables";
    << endl;
    )

isPolynomialRing = method(TypicalValue => Boolean)
isPolynomialRing Thing := x -> false
isPolynomialRing PolynomialRing := R -> true

isHomogeneous PolynomialRing := R -> true
isWeylAlgebra PolynomialRing := R -> isWeylAlgebra coefficientRing R or ( o := options R;
    o.?WeylAlgebra     and 0 < #o.WeylAlgebra)
isSkewCommutative PolynomialRing := R -> isSkewCommutative coefficientRing R or (
    R.?SkewCommutative and 0 < #R.SkewCommutative)

-- TODO: is the second one needed?
Ring _ List :=
PolynomialRing _ List := RingElement => (R, v) -> if #v === 0 then 1_R else product ( #v , i -> R_i^(v#i) )

coefficientRing PolynomialRing := R -> last R.baseRings
monoid PolynomialRing := o -> R -> R.monoid
monoid FractionField  := o -> monoid @@ baseRing
monoid Ring           := o -> degreesMonoid @@ degreeLength

generators PolynomialRing := opts -> R -> (
    if opts.CoefficientRing === null then R.generators else
    if opts.CoefficientRing === R then {}
    else join(R.generators, generators(coefficientRing R, opts) / (r -> promote(r, R))))

char      PolynomialRing :=      char @@ coefficientRing
precision PolynomialRing := precision @@ coefficientRing
numgens   PolynomialRing := numgens @@ monoid
options   PolynomialRing := options @@ monoid
dim       PolynomialRing := R -> dim coefficientRing R + #generators R - (
    if R.?SkewCommutative then #R.SkewCommutative else 0)

-- printing helpers
expressionPolynomialRing = R -> (
    T := if (options R).Local === true then List else Array;
    new T from toSequence runLengthEncode R.generatorExpressions)

describe   PolynomialRing := R -> Describe (expression last R.baseRings) expressionMonoid monoid R
expression PolynomialRing := R -> (
    if hasAttribute(R, ReverseDictionary)
    then expression getAttribute(R, ReverseDictionary)
    else(expression last R.baseRings) expressionPolynomialRing R)

toExternalString PolynomialRing := toString @@ describe
-- the rest are inherited from EngineRing

-----------------------------------------------------------------------------
-- degreesRing, etc.
-----------------------------------------------------------------------------

protect basering
protect FlatMonoid

degreesRing = method(TypicalValue => PolynomialRing)
degreesRing ZZ   := memoize( n -> if n == 0 then degreesRing {} else ZZ degreesMonoid n )
degreesRing List := memoize(
     hft -> if #hft === 0 then (
	       S := new PolynomialRing from rawPolynomialRing();
	       S.basering = ZZ;
	       S.FlatMonoid = monoid[DegreeRank => 0, Inverses => true, Global => false];
	       S.numallvars = 0;
	       S.baseRings = {ZZ};
	       S.degreesRing = S;
	       S.degreesMonoid = S.monoid = S.FlatMonoid;
	       S.isCommutative = true;
	       S.generatorSymbols = S.generatorExpressions = S.generators = {};
	       S.indexSymbols = S.indexStrings = new HashTable;
	       S)
	  else ZZ degreesMonoid hft)
degreesRing Ring   :=
degreesRing Monoid :=
degreesRing PolynomialRing   := R -> if R.?degreesRing   then R.degreesRing   else error "no degrees ring present"
degreesMonoid PolynomialRing := R -> if R.?degreesMonoid then R.degreesMonoid else error "no degrees monoid present"
degreeLength  PolynomialRing := R -> degreeLength R.FlatMonoid

-----------------------------------------------------------------------------
-- Main polynomial ring constructor
-----------------------------------------------------------------------------

protect diffs0						    -- private keys for storing info about indices of WeylAlgebra variables
protect diffs1

Ring List   := PolynomialRing => (R, M) -> use R monoid(M, Local => true)
Ring Array  := PolynomialRing => (R, M) -> use R monoid M
Ring Monoid := PolynomialRing => (R, M) -> (
    if not M.?RawMonoid then error "expected monoid handled by the engine";
	  if not R.?RawRing then error "expected coefficient ring handled by the engine";
     	  num := numgens M;
	  (basering,flatmonoid,numallvars) := (
	       if R.?isBasic then (R,M,num)
	       else if R.?basering and R.?FlatMonoid 
	       then ( R.basering, tensor(M, R.FlatMonoid), num + R.numallvars)
	       else if instance(R,FractionField) then (R,M,num)
	       else error "internal error: expected coefficient ring to have a base ring and a flat monoid"
	       );
	  -----------------------------------------------------------------------------
     	  local RM;
	  Weyl := M.Options.WeylAlgebra =!= {};
	  skews := monoidIndices(M,M.Options.SkewCommutative);
	  coeffOptions := options R;
	  coeffWeyl := coeffOptions =!= null and coeffOptions.WeylAlgebra =!= {};
	  coeffSkew := coeffOptions =!= null and coeffOptions.SkewCommutative =!= {};
	  coeffConstants := coeffOptions =!= null and coeffOptions.Constants;
	  constants := false;
	  -----------------------------------------------------------------------------
	  if M.Options.Constants or coeffConstants then (
	       constants = true;
	       RM = new PolynomialRing from rawTowerRing(char R, flatmonoid.generatorSymbols / toString // toSequence);
	       )
	  -----------------------------------------------------------------------------
	  else if Weyl or coeffWeyl then (
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
	       RM = new PolynomialRing from rawWeylAlgebra(rawPolynomialRing(raw basering, raw flatmonoid),diffs0,diffs1,h);
	       RM.diffs0 = diffs0;
	       RM.diffs1 = diffs1;
     	       addHook(RM, QuotientRingHook, S -> (S.diffs0 = diffs0; S.diffs1 = diffs1));
     	       if h != -1 then RM.homogenize = h;
	       )
	  -----------------------------------------------------------------------------
	  else if skews =!= {} or R.?SkewCommutative then (
	       if R.?diffs0 then error "coefficient ring is a Weyl algebra";
	       if R.?SkewCommutative then skews = join(skews, apply(R.SkewCommutative, i -> i + num));
	       RM = new PolynomialRing from rawSkewPolynomialRing(rawPolynomialRing(raw basering, raw flatmonoid),skews);
	       RM.SkewCommutative = skews;
	       )
	  -----------------------------------------------------------------------------
	  else (
	       log := FunctionApplication {rawPolynomialRing, (raw basering, raw flatmonoid)};
	       RM = new PolynomialRing from value log;
	       RM#"raw creation log" = Bag {log};
	       );
	  -----------------------------------------------------------------------------
	  if R#?"has quotient elements" or isQuotientOf(PolynomialRing,R) then (
	       RM.RawRing = rawQuotientRing(RM.RawRing, R.RawRing);
	       RM#"has quotient elements" = true;
	       );
	  RM.basering = basering;
	  RM.FlatMonoid = flatmonoid;
	  RM.numallvars = numallvars;
	  RM.promoteDegree = (
	       if flatmonoid.Options.DegreeMap === null
	       then makepromoter degreeLength RM	    -- means the degree map is zero
	       else (
	       	    dm := flatmonoid.Options.DegreeMap;
	       	    nd := flatmonoid.Options.DegreeRank;
	       	    degs -> apply(degs,deg -> degreePad(nd,dm deg))));
	  RM.liftDegree = (
	       if flatmonoid.Options.DegreeLift === null
	       then makepromoter degreeLength R		    -- lifing the zero degree map
	       else (
		    lm := flatmonoid.Options.DegreeLift;
		    degs -> apply(degs,lm)
		    ));
	  RM.baseRings = append(R.baseRings,R);
	  -- see enginering.m2
	  commonEngineRingInitializations RM;
	  RM.monoid = M;
	  if flatmonoid.?degreesRing   then RM.degreesRing   = flatmonoid.degreesRing;
	  if flatmonoid.?degreesMonoid then RM.degreesMonoid = flatmonoid.degreesMonoid;
	  RM.isCommutative = not Weyl and not RM.?SkewCommutative;
     	  ONE := RM#1;
	  if R.?char then RM.char = R.char;
	  -- TODO: what is this?
	  RM _ M := (f,m) -> new R from rawCoefficient(R.RawRing, raw f, raw m);
	  processMons := (coeffs, monoms) -> if #coeffs === 0 then expression 0 else sum(coeffs, monoms,
	      (c, m) -> expression(if c == 1 then 1 else promote(c, R)) * expression(if m == 1 then 1 else new M from m));
	  -- TODO: put in something prettier when there are constants
	  expression RM := if constants then f -> toString raw f else f -> processMons rawPairs(raw R, raw f);
     	  if M.Options.Inverses === true then (
	       denominator RM := f -> RM_( - min \ apply(transpose exponents f,x->x|{0}) );
	       numerator RM := f -> f * denominator f;
	       );
	  -----------------------------------------------------------------------------
	  factor RM := opts -> f -> (
	       c := 1_R; 
	       if (options RM).Inverses then (
        	   minexps:=min\transpose apply(toList (rawPairs(raw RM.basering,raw f))#1,m->exponents(RM.numallvars,m));
		   f=f*RM_(-minexps); -- get rid of monomial in factor if f Laurent polynomial
		   c=RM_minexps;
		   );
	       isSimpleNumberField := F -> isField F and instance(baseRing F, QuotientRing) and coefficientRing baseRing F === QQ and numgens baseRing F == 1 and numgens ideal baseRing F == 1; 
	       (facs,exps) := if isSimpleNumberField R then (
		   (RM', toRM') := flattenRing(RM, CoefficientRing=>QQ);
		   minp := (ideal RM')_0;
		   ((fs,es) -> (for f in fs list raw (map(RM, RM', generators RM|{R_0})) new RM' from f, es)) (
		       rawFactor(raw toRM' f, raw minp)) -- apply rawFactor, but the factors need to be converted back to RM
	       ) else if instance(R, FractionField) then (
        	   denom := lcm \\ (t -> denominator t_1) \ listForm f;
        	   baseRM := (baseRing R)(RM.monoid);
		   f = (map(baseRM, RM, generators baseRM)) (denom * f);
		   ((fs,es) -> (for i in (0..<#fs) list raw (((map(RM, baseRM, generators RM)) new baseRM from fs_i) * if i==0 then 1/denom else 1), es)) (
		       rawFactor raw f) -- similar: convert back to RM, and put denom back into the leadCoefficient
	       ) else rawFactor raw f;	-- example value: ((11, x+1, x-1, 2x+3), (1, 1, 1, 1)); constant term is first, if there is one
	       leadCoeff := x->( -- iterated leadCoefficient
		   R:=ring x;
		   if class R === PolynomialRing then leadCoeff leadCoefficient x else
		   if class R === QuotientRing or class R === GaloisField then leadCoeff lift(x,ambient R) else
    	    	   x);
     	       facs = apply(#facs, i -> (
		       p:=new RM from facs#i;
		       if leadCoeff p >= 0 then p else (if odd(exps#i) then c=-c; -p)
		       ));
    	       if liftable(facs#0,RM.basering) then (
		    -- factory returns the possible constant factor in front
	       	    assert(exps#0 == 1);
		    c = c*(facs#0);
		    facs = drop(facs,1);
		    exps = drop(exps,1);
		    );
	       if #facs != 0 then (facs,exps) = toSequence transpose sort transpose {toList facs, toList exps};
	       if c != 1 then (
		    -- we put the possible constant (and monomial for Laurent polynomials) at the end
		    facs = append(facs,c);
		    exps = append(exps,1);
		    );
	       new Product from apply(facs,exps,(p,n) -> new Power from {p,n}));
	  -----------------------------------------------------------------------------
	  isPrime RM := {} >> o -> f -> (
	      v := factor f;
	      cnt := 0; -- counts number of factors
	      scan(v, x -> ( if not isUnit(x#0) then cnt=cnt+x#1 ));
	      cnt == 1 -- cnt=0 is invertible element; cnt>1 is composite element; cnt=1 is prime element
	       );
	  -----------------------------------------------------------------------------
	  RM.generatorExpressions = M.generatorExpressions;
	  RM.generatorSymbols = M.generatorSymbols;
	  RM.generators = apply(num, i -> RM_i);
	  RM.indexSymbols = new HashTable from join(
	       if R.?indexSymbols then apply(pairs R.indexSymbols, (nm,x) -> nm => new RM from rawPromote(raw RM,raw x)) else {},
	       apply(num, i -> M.generatorSymbols#i => RM_i)
	       );
     	  RM.indexStrings = hashTable apply(pairs RM.indexSymbols, (k,v) -> (toString k, v));
	  RM)
-- e.g RR[x] or CC[x]
InexactFieldFamily List   :=
InexactFieldFamily Array  :=
InexactFieldFamily Monoid := PolynomialRing => (T, M) -> (default T) M

-----------------------------------------------------------------------------

weightRange = method()
weightRange(List,RingElement) := (w,f) -> rawWeightRange(w,raw f)
weightRange RingElement := f -> (
     if degreeLength ring f === 1
     then weightRange(first \ degrees ring f, f)
     else error "weightRange: expected a singly graded ring")

parts = method()
parts RingElement := f -> (
     if degreeLength ring f === 1
     then sum(select(apply(
	       ((i,j) -> i .. j) weightRange(first \ degrees (ring f).FlatMonoid, f),
	       n -> part_n f), p -> p != 0), p -> new Parenthesize from {p})
     else error "parts: expected a singly graded ring")

-----------------------------------------------------------------------------

off := 0
pw := (v,wts) -> (
     for i in v list if i<off then continue else if i>=off+#wts then break else wts#(i-off))
pg := (v,wts) -> first(pw(v,wts), off = off + #wts)
pn := (v,nw) -> (
     off = off + nw;
     n:=0;
     for i in v do if i<off then continue else if i>=off+nw then break else n=n+1;
     n)
selop = new HashTable from { GRevLex => pg, Weights => pw, Lex => pn, RevLex => pn, GroupLex => pn, GroupRevLex => pn, NCLex => pn }
selmo = (v,mo) -> ( off = 0; apply(mo, x -> if instance(x,Option) and selop#?(x#0) then x#0 => selop#(x#0)(v,x#1) else x))
ord := (v,nv) -> (
     n := -1;
     for i in v do (
	  if not instance(i,ZZ) or i < 0 or i >= nv then error("selectVariables: expected an increasing list of numbers in the range 0..",toString(nv-1));
	  if i <= n then error "selectVariables: expected a strictly increasing list";
	  n = i;
	  ))     
selectVariables = method()
selectVariables(List,PolynomialRing) := (v,R) -> (
     v = splice v;
     ord(v,numgens R);
     o := new MutableHashTable from options R;
     o.MonomialOrder = selmo(v,o.MonomialOrder);
     o.Variables = o.Variables_v;
     o.Degrees = o.Degrees_v;
     o = new OptionTable from o;
     (S := (coefficientRing R)(monoid [o]),map(R,S,(generators R)_v)))

-----------------------------------------------------------------------------

antipode = method();
antipode RingElement := (f) -> new ring f from rawAntipode raw f;

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
