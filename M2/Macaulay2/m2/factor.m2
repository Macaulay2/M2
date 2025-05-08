--		Copyright 1995-2002 by Daniel R. Grayson

needs "integers.m2"
needs "lists.m2"
needs "matrix1.m2"

-----------------------------------------------------------------------------

monic := t -> (
     c := leadCoefficient t;
     c' := 1 // c;
     if c * c' == 1 then t = t * c';
     t)

gcd(ZZ,RingElement) := (r,s) -> gcd(promote(r,ring s),s)
gcd(RingElement,ZZ) := (r,s) -> gcd(promote(s,ring r),r)

gcd(RingElement,RingElement) := RingElement => (r,s) -> (
     R := ring r;
     if ring s =!= R then error "gcd: expected elements in the same ring";
     if instance(R,QuotientRing) then error "gcd: unimplemented for this ring";
     if isField R then if r == 0 and s == 0 then 0_R else 1_R
     else if factoryAlmostGood R then (
	  if (options R).Inverses then (r,s) = (numerator r, numerator s);
 	  new ring r from rawGCD(raw r, raw s))
     else (
	 (R1,f,g) := flattenRing(R,Result=>3);
	 if factoryAlmostGood R1 then g gcd(f r,f s)
     else if instance(R,PolynomialRing) and numgens R == 1 and isField coefficientRing R then monic (
	  -- does this depend on the monomial order in R, too?
	  -- would this code work for more than one variable?
	  if gbTrace >= 3 then << "gcd via syzygies" << endl;
	  if r == 0 then s
	  else if s == 0 then r
	  else (
	       a := (syz( matrix{{r,s}}, SyzygyLimit => 1 ))_(0,0);
	       if s%a != 0 then error "can't find gcd in this ring";
	       s // a))
     else notImplemented()))
gcd RingElement := identity

gcdCoefficients(ZZ, RingElement) := (r, s) -> gcdCoefficients(r_(ring s), s)
gcdCoefficients(RingElement, ZZ) := (r, s) -> gcdCoefficients(r, s_(ring r))
gcdCoefficients(RingElement,RingElement) := (f,g) -> (	    -- ??
     R := ring f;
     if R =!= ring g then error "expected elements of the same ring";
     if not isPolynomialRing R then error "expected a polynomial ring";
     if not isField coefficientRing R then error "expected a polynomial ring over a field";
     if numgens R > 1 then error "expected a polynomial ring in at most one variable";
     apply(rawExtendedGCD(raw f, raw g), r -> new R from r))

lcm(ZZ,RingElement) := (r,s) -> lcm(promote(abs r,ring s),s)
lcm(RingElement,ZZ) := (r,s) -> lcm(promote(abs s,ring r),r)
lcm(RingElement,RingElement) := (f,g) -> (
    d := gcd(f, g);
    if d == 0 then d
    else f * (g // d))
lcm RingElement := identity

-----------------------------------------------------------------------------

isSimpleNumberField := F -> ( isField F
    and instance(R := baseRing F, QuotientRing)
    and coefficientRing R === QQ
    and numgens ideal R == 1
    and numgens R == 1 )

leadCoeff := x -> ( R := ring x; -- iterated leadCoefficient
    if instance(R, PolynomialRing) then leadCoeff leadCoefficient x  else
    if instance(R, QuotientRing)
    or instance(R, GaloisField)    then leadCoeff lift(x, ambient R) else x )

isUnit RingElement := f -> if (o := options ring f).?Inverses and o.Inverses then (
    size f === 1 and isUnit leadCoefficient f) else 1 % ideal f == 0

isPrime RingElement := {} >> o -> f -> (
    -- =0 means invertible element; =1 prime element; >1 composite element.
    1 == sum(toList factor f, x -> if isUnit x#0 then 0 else x#1))

factor RingElement := opts -> f -> (
    RM := ring f;
    R := coefficientRing RM;
    c := 1_R;
    -- get rid of monomial in factor if f Laurent polynomial
    if (options RM).Inverses then (
	minexps := toList last rawPairs(raw RM.BaseRing, raw f);
	minexps  = min \ transpose apply(minexps, exponents_(RM.numallvars));
	f = RM_(-minexps) * f;
	c = RM_minexps);
    -- the actual computation occurs here
    (facs, exps) := if (ret := runHooks((factor, RingElement), (opts, f))) =!= null
    then ret else error "factor: no method implemented for this type of element";
    -- TODO: simplify this
    facs = apply(facs, exps, (f, e) -> if leadCoeff(p := new RM from f) >= 0 then p else (if odd e then c = -c; -p));
    if liftable(facs#0, RM.BaseRing) then (
	-- factory returns the possible constant factor in front
	assert(exps#0 == 1);
	c = c * facs#0;
	facs = drop(facs, 1);
	exps = drop(exps, 1);
	);
    if 0 < #facs then (facs, exps) = toSequence transpose sort transpose {toList facs, toList exps};
    if c != 1 then (
	-- we put the possible constant (and monomial for Laurent polynomials) at the end
	facs = append(facs, c);
	exps = append(exps, 1);
	);
    --
    new Product from apply(facs, exps, (f, e) -> new Power from {f, e}))

-- This is a map from method keys to strategy hash tables
algorithms := new MutableHashTable from {}
algorithms#(factor, RingElement) = new MutableHashTable from {
    -- example value: ((11, x+1, x-1, 2x+3), (1, 1, 1, 1)); constant term is first, if there is one
    Default => (opts, f) -> rawFactor raw f,

    "NumberField" => (opts, f) -> (
	R := coefficientRing(RM := ring f);
	if not isSimpleNumberField R
	then return null;
	(RM', toRM') := flattenRing(RM, CoefficientRing => QQ);
	toRM := map(RM, RM', generators RM | {R_0});
	minp := (ideal RM')_0; -- minimal polynomial of the number field
	func := (fs, es) -> (for f in fs list raw toRM new RM' from f, es);
	func rawFactor(raw toRM' f, raw minp)), -- apply rawFactor, but the factors need to be converted back to RM

    FractionField => (opts, f) -> (
	R := coefficientRing(RM := ring f);
	if not instance(R, FractionField)
	then return null;
	RM' := (baseRing R) RM.monoid;
	toRM := map(RM, RM', generators RM);
	toRM' := map(RM', RM, generators RM');
	denom := lcm apply(listForm f, t -> denominator t_1);
	func := (fs, es) -> (for i to #fs - 1 list raw((toRM new RM' from fs_i) * (if i == 0 then 1/denom else 1)), es);
	func rawFactor raw toRM'(denom * f)), -- similar: convert back to RM, and put denom back into the leadCoefficient
    }

-- Installing hooks for factor RingElement
scan({Default, FractionField, "NumberField"}, strategy ->
    addHook(key := (factor, RingElement), algorithms#key#strategy, Strategy => strategy))

-----------------------------------------------------------------------------

pseudoRemainder = method()
pseudoRemainder(RingElement,RingElement) := RingElement => (f,g) -> (
     R := ring f;
     if R =!= ring g then error "expected elements of the same ring";
     new R from rawPseudoRemainder(raw f, raw g));

-----------------------------------------------------------------------------

-- We mimic the procedure for finding a finite field addition table used in the routine gf_get_table
-- for building the file name in "gffilename", in the file BUILD_DIR/libraries/factory/build/factory/gfops.cc .
-- Reminder: the contents of currentLayout are determined by the file ../d/startup.m2.in .
gfdirs = ( if isAbsolutePath currentLayout#"factory gftables"
           then {                  currentLayout#"factory gftables"}
	   else {prefixDirectory | currentLayout#"factory gftables"} )
i := position(gfdirs, gfdir -> fileExists(gfdir | "gftables/961")) -- 961==31^2
if i === null
then error ("sample Factory finite field addition table file missing, needed for factorization: ", concatenate between_", " gfdirs)
setFactoryGFtableDirectory gfdirs_i

irreducibleCharacteristicSeries = method()
irreducibleCharacteristicSeries Ideal := I -> (		    -- rawCharSeries
     R := ring I;
     if not factoryGood R then error "not implemented for this type of ring";
     m := generators I;
     m = compress m;					    -- avoid a bug in Messollen's code for rawCharSeries when a generator is zero
     (S,RtoS) := flattenRing R;
     StoR := RtoS^-1;
     m = RtoS m;
     re := rawIdealReorder raw m; -- we need to substitute S_(re#i) => T_i beforehand and let the user undo it afterward, if desired
     re' := inversePermutation re; -- so the two substitutions are S_j => T_(re'_j) and T_i => S_(re_i)
     k := coefficientRing R;
     T := k ( monoid [ Variables => S.generatorSymbols_re, Degrees => (degrees S)_re, MonomialOrder => Lex, Heft => heft S ] );
     StoT := map(T,S,(generators T)_re');
     TtoS := map(S,T,(generators S)_re );
     RtoT := StoT * RtoS;
     TtoR := StoR * TtoS;
     RtoT.cache.inverse = TtoR;
     TtoR.cache.inverse = RtoT;
     (apply(rawCharSeries raw StoT m, rawmat -> map(T,rawmat)),TtoR))

-----------------------------------------------------------------------------

factor ZZ := opts -> n -> (
    if n === 0 then Product { Power{0,1} }
    else (
	r := rawZZfactor n; -- format is: (sign, factor1, exponent1, factor2, exponent2, ...)
        nfactors := #r//2;
        assert(#r == 2*nfactors + 1);
        r#0 * Product for i from 1 to nfactors list Power{r#(2*i-1), r#(2*i)}
	)
    )
 
factor QQ := opts -> (r) -> factor numerator r / factor denominator r

-----------------------------------------------------------------------------

topCoefficients = method()
topCoefficients Matrix := f -> (
     R := ring f;
     (monoms, coeffs) := rawTopCoefficients f.RawMatrix;
     (map(R,monoms), map(R,coeffs)))
topCoefficients RingElement := f -> (
     if f == 0 then (1_(ring f), f)
     else (
     	  (monoms,coeffs) := topCoefficients matrix{{f}};
     	  (monoms_(0,0), coeffs_(0,0))))

roots = method(Options => true);
roots RingElement := {Precision => -1, Unique => false} >> o -> p ->
  toList apply(rawRoots(raw p, o.Precision, o.Unique), r -> new CC from r)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
