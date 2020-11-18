--		Copyright 1995-2002 by Daniel R. Grayson

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
     if isField R then if r == 0 and s == 0 then 0_R else 1_R
     else if factoryAlmostGood R then (
	  if (options R).Inverses then (r,s) = (numerator r, numerator s);
 	  new ring r from rawGCD(raw r, raw s))
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
     else notImplemented())

gcdCoefficients(RingElement,RingElement) := (f,g) -> (	    -- ??
     R := ring f;
     if R =!= ring g then error "expected elements of the same ring";
     if not isPolynomialRing R then error "expected a polynomial ring";
     if not isField coefficientRing R then error "expected a polynomial ring over a field";
     if numgens R > 1 then error "expected a polynomial ring in at most one variable";
     toList apply(rawExtendedGCD(raw f, raw g), r -> new R from r))

lcm(ZZ,RingElement) := (r,s) -> lcm(promote(abs r,ring s),s)
lcm(RingElement,ZZ) := (r,s) -> lcm(promote(abs s,ring r),r)
lcm(RingElement,RingElement) := (f,g) -> f * (g // gcd(f,g))

pseudoRemainder = method()
pseudoRemainder(RingElement,RingElement) := RingElement => (f,g) -> (
     R := ring f;
     if R =!= ring g then error "expected elements of the same ring";
     new R from rawPseudoRemainder(raw f, raw g));

inversePermutation = v -> ( w := new MutableList from #v:null; scan(#v, i -> w#(v#i)=i); toList w)

-- We mimic the procedure for finding a finite field addition table used in the routine gf_get_table
-- for building the file name in "gffilename", in the file BUILD_DIR/libraries/factory/build/factory/gfops.cc .
-- Reminder: the contents of currentLayout are determined by the file ../d/startup.m2.in .
gfdirs = ( if isAbsolutePath currentLayout#"factory gftables"
           then {                  currentLayout#"factory gftables"}
	   else {prefixDirectory | currentLayout#"factory gftables"} )
i := position(gfdirs, gfdir -> fileExists(gfdir | "gftables/961")) -- 961==31^2
if i === null
then error ("sample Factory finite field addition table file missing, needed for factorization: ",
    prefixDirectory, currentLayout#"factory gftables")
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
