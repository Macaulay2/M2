--		Copyright 1995-2002 by Daniel R. Grayson

good := k -> (
     k === QQ or
     k === ZZ or
     instance(k,QuotientRing) and ambient k === ZZ and isPrime char k or
     instance(k,GaloisField) and char k === k.order
     )

monic := t -> (
     c := leadCoefficient t;
     c' := 1 // c;
     if c * c' == 1 then t = t * c';
     t)

gcd(RingElement,RingElement) := RingElement => (r,s) -> (
     R := ring r;
     if ring s =!= R then error "gcd: expected elements in the same ring";
     if isField R then if r == 0 and s == 0 then 0_R else 1_R
     else if instance(R,PolynomialRing) and good coefficientRing R then (
	  -- use factory for this
     	  new ring r from rawGCD(raw r, raw s)
	  )
     else if instance(R,PolynomialRing) and numgens R == 1 and isField coefficientRing R then monic (
	  -- does this depend on the monomial order in R, too?
	  -- would this code work for more than one variable?
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

pseudoRemainder = method()
pseudoRemainder(RingElement,RingElement) := RingElement => (f,g) -> (
     R := ring f;
     if R =!= ring g then error "expected elements of the same ring";
     new R from rawPseudoRemainder(raw f, raw g));

reorder := I -> (					    -- rawIdealReorder
     f := generators I;
     R := ring I;
     v := rawIdealReorder raw f;
     assert( #v == numgens R );
     v)

-- lcm2 := (x,y) -> x*y//gcd(x,y)
-- lcm := args -> (
--      n := 1;
--      scan(args, i -> n = lcm2(n,i));
--      n)
-- commden := (f) -> lcm apply( last \ listForm f, denominator)

irreducibleCharacteristicSeries = method()
irreducibleCharacteristicSeries Ideal := I -> (		    -- rawCharSeries
     f := generators I;
     f = compress f;					    -- avoid a bug in Messollen's code for rawCharSeries when a generator is zero
     R := ring I;
     if not isPolynomialRing R 
     then error "expected ideal in a polynomial ring";
     k := coefficientRing R;
     if not isField k then error "factorization not implemented for coefficient rings that are not fields";
     -- if k === QQ then f = matrix { first entries f / (r -> r * commden r) };
     re := reorder I;
     n := #re;
     f = substitute(f,apply(n,i -> R_(re#i) => R_i));
     ics := rawCharSeries raw f;
     ics = apply(ics, m -> map(R,m));
     phi := map(R,R,apply(n,i->R_(re#i)));
     {ics,phi}
     )

factor ZZ := options -> (n) -> Product apply(sort pairs factorInteger n, (p,i)-> Power{p,i} )
factor QQ := options -> (r) -> factor numerator r / factor denominator r
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

minimalPrimes Ideal := (cacheValue symbol minimalPrimes) (
     (I) -> (
	  R := ring I;
	  if isQuotientRing R then (
	       A := ultimate(ambient, R);
	       I = lift(I,A);
	       )
	  else A = R;
	  if not isPolynomialRing A then error "expected ideal in a polynomial ring or a quotient of one";
	  if I == 0 then return {if A === R then I else ideal 0_R};
	  ics := irreducibleCharacteristicSeries I;
	  -- remove any elements which have numgens > numgens I (Krull's Hauptidealsatz)
	  ngens := numgens I;
	  ics0 := select(ics#0, CS -> numgens source CS <= ngens);
	  Psi := apply(ics0, CS -> (
		    chk := topCoefficients CS;
		    chk = chk#1;  -- just keep the coefficients
		    chk = first entries chk;
		    iniCS := select(chk, i -> degree i =!= {0});
		    if gbTrace >= 1 then << "saturating with " << iniCS << endl;
		    CS = ideal CS;
		    --<< "saturating " << CS << " with respect to " << iniCS << endl;
		    -- warning: over ZZ saturate does unexpected things.
		    scan(iniCS, a -> CS = saturate(CS, a, Strategy=>Eliminate));
     --	       scan(iniCS, a -> CS = saturate(CS, a));
		    --<< "result is " << CS << endl;
		    CS));
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
	       components = apply(components, I -> ideal(generators I ** R));
	       );
	  components
	  ))

isPrime Ideal := J -> (C := minimalPrimes J; #C === 1 and C#0 == J)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
