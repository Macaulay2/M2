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
gfdirs = {prefixDirectory | currentLayout#"factory gftables",
	  currentLayout#"factory gftables"}
i := position(gfdirs, gfdir -> fileExists(gfdir | "gftables/961")) -- 961==31^2
if i === null
then error ("sample Factory finite field addition table file missing, needed for factorization: ",
    prefixDirectory, currentLayout#"factory gftables",
    " or ",
    currentLayout#"factory gftables")
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
     --error "debug me iredCharSeries";
     (apply(rawCharSeries raw StoT m, rawmat -> map(T,rawmat)),TtoR))

factor ZZ := ( f -> opts -> f ) (
     if instance(Pari$factorint, Function)
     then n -> (
	  if n === 0 then Product { Power{0,1} }
	  else (
	       r := Pari$factorint n;
	       Product apply(r#0,r#1,(p,i)-> Power{p,i})
	       )
	  )
     else n -> Product apply(sort pairs factorInteger n, (p,i)-> Power{p,i})
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

minimalPrimes Ideal := decompose Ideal := (cacheValue symbol minimalPrimes) (
     (I) -> (
	  R := ring I;
	  (I',F) := flattenRing I; -- F is not needed
	  A := ring I';
	  G := map(R, A, generators(R, CoefficientRing => coefficientRing A));
     	  --I = trim I';
	  I = I';
	  if not isPolynomialRing A then error "expected ideal in a polynomial ring or a quotient of one";
	  if not isCommutative A then
	    error "expected commutative polynomial ring";
	  kk := coefficientRing A;
	  if kk =!= QQ and not instance(kk,QuotientRing) then
	    error "expected base field to be QQ or ZZ/p";
	  if I == 0 then return {if A === R then I else ideal map(R^1,R^0,0)};
	  if debugLevel > 0 then homog := isHomogeneous I;
	  ics := irreducibleCharacteristicSeries I;
	  if debugLevel > 0 and homog then (
	       if not all(ics#0, isHomogeneous) then error "minimalPrimes: irreducibleCharacteristicSeries destroyed homogeneity";
	       );
	  -- remove any elements which have numgens > numgens I (Krull's Hauptidealsatz)
	  ngens := numgens I;
	  ics0 := select(ics#0, CS -> numgens source CS <= ngens);
	  Psi := apply(ics0, CS -> (
		    chk := topCoefficients CS;
		    chk = chk#1;  -- just keep the coefficients
		    chk = first entries chk;
		    iniCS := select(chk, i -> # support i > 0); -- this is bad if degrees are 0: degree i =!= {0});
		    if gbTrace >= 1 then << "saturating with " << iniCS << endl;
		    CS = ideal CS;
		    --<< "saturating " << CS << " with respect to " << iniCS << endl;
		    -- warning: over ZZ saturate does unexpected things.
		    scan(iniCS, a -> CS = saturate(CS, a, Strategy=>Eliminate));
     --	       scan(iniCS, a -> CS = saturate(CS, a));
		    --<< "result is " << CS << endl;
		    CS));
	  Psi = select(Psi, I -> I != 1);
	  Psi = new MutableList from Psi;
	  p := #Psi;
	  scan(0 .. p-1, i -> if Psi#i =!= null then
	       scan(i+1 .. p-1, j ->
		    if Psi#i =!= null and Psi#j =!= null then
		    if isSubset(Psi#i, Psi#j) then Psi#j = null else
		    if isSubset(Psi#j, Psi#i) then Psi#i = null));
	  Psi = toList select(Psi,i -> i =!= null);
	  components := apply(Psi, p -> ics#1 p);
	  if A =!= R then (
	       components = apply(components, P -> trim(G P));
	       );
	  --error "debug me";
	  components
	  ))

isPrime Ideal := J -> (C := minimalPrimes J; #C === 1 and C#0 == J)

roots = method(Options => true);
roots RingElement := {Precision => -1, Unique => false} >> o -> p ->
  toList apply(rawRoots(raw p, o.Precision, o.Unique), r -> new CC from r)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
