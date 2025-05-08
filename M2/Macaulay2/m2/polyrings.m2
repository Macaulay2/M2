--		Copyright 1993-2002 by Daniel R. Grayson

-- TODO: seems to need quotring.m2 for isQuotientOf
needs "methods.m2"
needs "enginering.m2"
needs "monoids.m2"
needs "indeterminates.m2" -- runLengthEncode

-----------------------------------------------------------------------------
-- PolynomialRing type declaration and basic methods
-----------------------------------------------------------------------------

PolynomialRing = new Type of EngineRing
PolynomialRing.synonym = "polynomial ring"
PolynomialRing#AfterPrint = R -> (
    class R,
    if #R.monoid.Options.SkewCommutative > 0
    then (", ", #R.monoid.Options.SkewCommutative, " skew commutative variable(s)"),
    if #R.monoid.Options.WeylAlgebra > 0
    then (", ", #R.monoid.Options.WeylAlgebra, " differential variable(s)"),
    if R.?homogenize then (" and one homogenizing variable")
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
ambient PolynomialRing := identity
monoid PolynomialRing := o -> R -> R.monoid
monoid FractionField := o -> monoid @@ baseRing
monoid Ring := o -> degreesMonoid @@ degreeLength

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
-- the rest are inherited from EngineRing

-----------------------------------------------------------------------------
-- degreesRing, etc.
-----------------------------------------------------------------------------

protect BaseRing
protect FlatMonoid

degreesRing = method(TypicalValue => PolynomialRing)
degreesRing ZZ   := memoize( n -> if n == 0 then degreesRing {} else ZZ degreesMonoid n )
degreesRing List := memoize(
     hft -> if #hft === 0 then (
	       S := new PolynomialRing from rawPolynomialRing();
	       S.BaseRing = ZZ;
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
degreeGroup   PolynomialRing := R -> degreeGroup  R.FlatMonoid
degreeGroup   FractionField  := degreeGroup @@ baseRing

-----------------------------------------------------------------------------
-- Helpers for polynomial ring
-----------------------------------------------------------------------------

newSkewPolyRing = (S, R, RSkew, M, MSkew, num) -> (
    if RSkew =!= {} then MSkew = shiftAndJoin_num (MSkew, RSkew);
    RM := new PolynomialRing from rawSkewPolynomialRing(S, MSkew);
    RM.SkewCommutative = MSkew;
    RM)

newWeylAlgebra = (S, R, RWeyl, M, MWeyl, num) -> (
    -- identify the homogenizing variable
    hvar := if MWeyl#?-1 and instance(MWeyl#-1, ZZ)
    then first(MWeyl#-1, MWeyl = drop(MWeyl, -1)) else -1;
    if R.?homogenize then (
	if hvar == -1 then hvar = R.homogenize + num else
	if hvar =!= R.homogenize + num then error "WeylAlgebra: expected the same homogenizing variable as the base ring")
    else if RWeyl =!= {} and hvar != -1 then error "WeylAlgebra: coefficient Weyl algebra has no homogenizing variable";
    --
    xvars := shiftAndJoin_num (first \ MWeyl, if R.?xvars then R.xvars);
    dvars := shiftAndJoin_num (last  \ MWeyl, if R.?dvars then R.dvars);
    if any(xvars, dvars, (x, dx) -> dx < x) then error "WeylAlgebra: expected differential variables to occur to the right of their variables";
    --
    RM := new PolynomialRing from rawWeylAlgebra(S, xvars, dvars, hvar);
    RM.xvars = xvars; RM.dvars = dvars;
    if hvar != -1 then RM.homogenize = hvar;
    addHook(RM, QuotientRingHook, S -> (S.xvars = xvars; S.dvars = dvars));
    RM.WeylAlgebra = pack_2 mingle(xvars, dvars);
    RM)

-----------------------------------------------------------------------------
-- Main polynomial ring constructor
-----------------------------------------------------------------------------

-- private keys for storing info about indices of WeylAlgebra variables
protect xvars
protect dvars

Ring List   := PolynomialRing => (R, M) -> use R monoid(M, Local => true)
Ring Array  := PolynomialRing => (R, M) -> use R monoid M
Ring Monoid := PolynomialRing => (R, M) -> (
    if not M.?RawMonoid then error "expected monoid handled by the engine";
    if not R.?RawRing then error "expected coefficient ring handled by the engine";
    nvars := numgens M;
    (K, F, numallvars) := (
	if R.?isBasic or instance(R, FractionField) then (R, M, nvars) else
	if R.?BaseRing and R.?FlatMonoid            then (R.BaseRing, tensor(M, R.FlatMonoid), nvars + R.numallvars)
	else error "internal error: expected coefficient ring to have a base ring and a flat monoid");
    -----------------------------------------------------------------------------
    -- TODO: why not use K and F here?
    MOpts := options M;
    ROpts := options R;
    RCons := if ROpts.?Constants       then ROpts.Constants else false;
    MCons := if MOpts.?Constants       then MOpts.Constants else false;
    RWeyl := if ROpts.?WeylAlgebra     then ROpts.WeylAlgebra else {};
    MWeyl := if MOpts.?WeylAlgebra     then MOpts.WeylAlgebra else {};
    RSkew := if ROpts.?SkewCommutative then ROpts.SkewCommutative else {};
    MSkew := if MOpts.?SkewCommutative then MOpts.SkewCommutative else {};
    -- FIXME: remove once Weyl variables are stored as indices in the monoid
    RWeyl = monoidIndices_R RWeyl;
    MWeyl = monoidIndices_M MWeyl;
    if (MWeyl =!= {} or RWeyl =!= {}) and (MSkew =!= {} or RSkew =!= {})
    then error "rings with both skew commuting and differential variables are not yet implemented";
    -----------------------------------------------------------------------------
    S := if (constants := RCons or MCons)
    then rawTowerRing(char R, F.generatorSymbols / toString // toSequence) -- TODO: document this
    else rawPolynomialRing(raw K, raw F);
    -----------------------------------------------------------------------------
    local RM;
    if MWeyl =!= {} or RWeyl =!= {} then RM =  newWeylAlgebra(S, R, RWeyl, M, MWeyl, nvars) else
    if MSkew =!= {} or RSkew =!= {} then RM = newSkewPolyRing(S, R, RSkew, M, MSkew, nvars)
    else RM = new PolynomialRing from S;
    -----------------------------------------------------------------------------
    if R#?"has quotient elements" or isQuotientOf(PolynomialRing, R) then (
	RM.RawRing = rawQuotientRing(RM.RawRing, R.RawRing);
	RM#"has quotient elements" = true);
    --
    RM.monoid     = M;
    RM.BaseRing   = K;
    RM.FlatMonoid = F;
    RM.numallvars = numallvars;
    RM.baseRings  = append(R.baseRings, R);
    RM.cache      = new CacheTable;
    RM.promoteDegree = (
	if F.Options.DegreeMap === null
	then makepromoter degreeLength RM -- means the degree map is zero
	else (
	    dm := F.Options.DegreeMap;
	    nd := F.Options.DegreeRank;
	    degs -> apply(degs, deg -> degreePad(nd, dm deg))));
    RM.liftDegree = (
	if F.Options.DegreeLift === null
	then makepromoter degreeLength R -- lifing the zero degree map
	else (
	    lm := F.Options.DegreeLift;
	    degs -> apply(degs, lm)));
    --
    if R.?char          then RM.char          = R.char; -- TODO: what ring doesn't have .char?
    if F.?degreesRing   then RM.degreesRing   = F.degreesRing;
    if F.?degreesMonoid then RM.degreesMonoid = F.degreesMonoid;
    RM.isCommutative = RWeyl === {} and MWeyl === {} and not RM.?SkewCommutative;
    -- see enginering.m2
    commonEngineRingInitializations RM;
    -- TODO: what is this?
    RM _ M := (f,m) -> new R from rawCoefficient(R.RawRing, raw f, raw m);
    -- printing
    processMons := (coeffs, monoms) -> if #coeffs === 0 then expression 0 else sum(coeffs, monoms,
	(c, m) -> expression(if c == 1 then 1 else promote(c, R)) * expression(new M from m));
    -- TODO: put in something prettier when there are constants
    expression RM := if constants then f -> toString raw f else f -> processMons rawPairs(raw R, raw f);
    --
    if MOpts.Inverses === true then (
	denominator RM := f -> RM_( - min \ apply(transpose exponents f,x->x|{0}) );
	numerator   RM := f -> f * denominator f);
    -----------------------------------------------------------------------------
    RM.generators           = apply(nvars, i -> RM_i);
    RM.generatorSymbols     = M.generatorSymbols;
    RM.generatorExpressions = M.generatorExpressions;
    --
    RM.index        = hashTable apply(RM.generatorSymbols, 0 ..< nvars,  identity);
    RM.indexSymbols = hashTable join(
	-- FIXME: switching the order of the following two reveals a bug in Schubert2
	apply(if R.?indexSymbols then pairs R.indexSymbols else {},
	    (sym, x) -> sym => new RM from rawPromote(raw RM, raw x)),
	apply(RM.generatorSymbols, RM.generators, identity)
	);
    try RM.indexStrings = applyKeys(RM.indexSymbols, toString); -- no error, because this is often harmless
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
     S := (coefficientRing R)(monoid [o]);
     f := map(R,S,(generators R)_v);
     g := map(S,R,apply(generators R, v->substitute(v,S)));
     setupPromote f;
     setupLift g;
     (S,f))

-----------------------------------------------------------------------------

antipode = method();
antipode RingElement := (f) -> new ring f from rawAntipode raw f;

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
