-- This code is based on/taken from the paper
-- "Direct methods for primary decomposition" by 
-- Eisenbud, Huneke, and Vasconcelos that appeared in
-- Inventiones Math. 110, 207-235 (1992)

--
--
-- This first section of the file defines the assassinator.
-- There are two strategies defined first
--
--

ass1 = method(Options => options associatedPrimes)
ass1 Ideal := List => o -> (I) -> (
     assassinator := {};
     RI := ring I;
     polyRing := ring presentation RI;
     I1 := lift(I, polyRing);
     i := codim I1;
     d := dim polyRing;
     if o.CodimensionLimit >= 0 then d = min(d, o.CodimensionLimit);
     local currentext;
     C := resolution(cokernel generators I, LengthLimit => d+1);
     --here we look at the associated primes of the i-th
     --ext and pick out the height i components ala EisenbudHunekeVasconcelos
     while i <= d do ( 
      if debugLevel >= 2 then (<< "  associatedPrimes loop " << i << endl);
      currentext = Ext^i(polyRing^1/I1,polyRing);
      if codim currentext == i then (
           firstlist := minimalPrimes ann currentext;
           scan(firstlist, P -> (if codim P == i then (
                  if debugLevel >= 1 then << "    " << P  << endl << endl ;
                  assassinator = append(assassinator,P)))
            ) );
      i=i+1;);
     assassinator = apply( assassinator, P -> (trim substitute(P, RI)) );
     assassinator
     )

-- WARNING: this code below does NOT compute the associated primes of I.
-- Why not? (MES, 3 Jul 2006).
ass2 = (I) -> (
     assassinator := {};
     local newcomponents;
     RI := ring I;
     polyRing := ring presentation RI;
     currentI := lift(I,polyRing);
     topcurrentI := topComponents currentI;
     while not isSubset(topcurrentI, currentI) do(
    if debugLevel >= 2 then printerr "  beginning new associatedPrimes loop";
        newcomponents = flatten minimalPrimes topcurrentI;
    if debugLevel >=1 then (scan(newcomponents, P -> ( << endl << "    " << P << endl)));
    assassinator = append(assassinator, newcomponents);
      currentI = currentI:topcurrentI;
      topcurrentI = topComponents currentI;
      );
     newcomponents = flatten minimalPrimes topcurrentI;
     if debugLevel >=1 then (scan(newcomponents, P -> ( << endl << "    " << P << endl)));
     assassinator = append(assassinator, newcomponents);
     assassinator = flatten assassinator;
     assassinator = apply(
      assassinator, P -> (trim substitute(P,RI))
      );
     assassinator 
     )

--In localize, P is an associated prime.
--This routine computes the localization of I at P.
--For this version, we have to know associatedPrimes I

SYlocalize = (assroutine) -> (I,P) ->(
     local ret;
     RI := ring I;
     polyRing := ring presentation RI;
     I1 := lift(I,polyRing);
     P1 := lift(P,polyRing);
     IntersectionOfPrimes := ideal (1_polyRing);
     if debugLevel >= 1 then (<< endl << "Finding the assassinator of " << I1 << endl);
     Assasin := assroutine I1;
     if debugLevel >= 2 then (<< endl << "It equals " << Assasin << endl);
     RP := polyRing/P1;
     scan(Assasin, Q -> if not isSubset(Q,P) 
      then (IntersectionOfPrimes = 
           intersect(IntersectionOfPrimes,Q))
      );
     IOPrp := substitute(IntersectionOfPrimes,RP);
     if IntersectionOfPrimes == ideal(1_RI) 
     then ret = I
     else (
      if debugLevel >= 1 then (<< endl << "Finding a separator polynomial" << endl);
      -- f := lift((flatten entries generators gb IOPrp)#0, polyRing);
      f := first select(flatten entries generators gb IntersectionOfPrimes, g -> g % P1 != 0);
      if debugLevel >= 2 then (<< endl << "It equals " << f << endl);
      f = substitute (f,RI);
      if debugLevel >= 1 then (<< endl << "Saturating with respect to the separator polynomial" << endl);
      ret = saturate(I,f);
      );
     ret
     ) 

-- alternate localize: you only need to know I and the prime
-- this only works if it's homogeneous and equidimensional
-- and radical. 

EHVlocalize = (I,P) ->(
     polyRing := ring presentation ring I;
     I0 := lift(I,polyRing);
     P0 := lift(P,polyRing);
     d := degree I0;
     if debugLevel >= 1 then (<< endl << "Finding the " << d*(d+1) << " power of the prime ideal" << endl);
     P0Power := P0^(d*(d+1));
     if debugLevel >= 2 then << endl << "It equals " << P0Power << endl;
     if debugLevel >= 1 then (<< endl << "Finding the top of " << I0 + P0Power << endl);
     I1 := topComponents (I0 + P0Power);
     if debugLevel >= 2 then(<< endl << "It equals " << endl << I1 << endl);
     gensI1 := flatten entries mingens I1;
     gensI1 = apply(gensI1, g -> 
      if first degree g <= d then g else 0);
     I2 := ideal gensI1;
     if debugLevel >= 1 then (<< endl << "Finding the final top" << endl);
     I3 := topComponents I2;
     if debugLevel >= 2 then (<< endl << "It equals " << endl << I3 << endl);
     if debugLevel >= 1 then (<< endl << "Finding the radical" << endl);
     --radical I3;
     trim substitute(I3,ring I)
     )

--This part should compute the/a primary component of I
--corresponding to P.
--A BIG issue here is how to increment m.  this could make
--a big speed difference.

-- Of course in this next thing, if we know associatedPrimes, we can use
-- localize, but if we just know I and the prime, we might 
-- rather use EHVlocalize.  It's hard to know.  
primarycomponent = (localizeroutine) ->
     (I,P,inc) -> (
      polyRing := ring presentation ring I;
      I0 := lift(I,polyRing);
      P0 := lift(P,polyRing);
      ret := null;
      m := 1;
      I0P0 := localizeroutine (I0,P0);
      ImportantColonIdeal := saturate (I0P0,P0);
      while ret === null do (
           if debugLevel >= 1 then (<< endl << "primaryComponent checking power " << m << endl);
           if debugLevel >= 2 then (<< endl << "Computing topComponents of " << I0 << "+ the power of " << P0 << 
            endl);
           Q := topComponents (I0 + P0^m);
           if debugLevel >= 2 then (<< endl << "It equals " << Q << endl); 
           IIntersection := intersect(Q,ImportantColonIdeal);
           if isSubset(IIntersection,I0P0) 
           then ret = Q 
           else m = m+inc;
           );
      trim substitute(ideal mingens ret,ring I)
      )

--This computes a primary decomposition of an ideal.
EHVprimaryDecomposition = (I) -> (
    Assasin := associatedPrimes I;
    nprimes := #Assasin;
    counter := 0;
    ListofPrimaries := {};
    while counter < nprimes do (
     newcomponent := primaryComponent(I, Assasin#counter);
     ListofPrimaries = append(ListofPrimaries, newcomponent);
         counter = counter + 1;
     );
     ListofPrimaries
     )

--This computes a primary decomposition of an ideal.
HprimaryDecomposition = (I,assstrategy,localizestrategy) -> (
     assroutine := (
      if assstrategy === 1 then ass1
      else ass2);
     localizeroutine := (
      if localizestrategy === 1 then EHVlocalize
      else SYlocalize assroutine);
     primarycomponentroutine := primarycomponent(localizeroutine);
     Assasin := assroutine I;
     nprimes := #Assasin;
     counter := 0;
     ListofPrimaries := {};
     while counter < nprimes do (
      newcomponent := primarycomponentroutine(I, Assasin#counter, 1);
      ListofPrimaries = append(ListofPrimaries, newcomponent);
          counter = counter + 1;
      );
     ListofPrimaries
     )

-- Additional functions useful for primary decomposition

bracketPower = (I, n) -> ideal apply(I_*, f -> f^n)

getEmbeddedComponent = method(Options => { Strategy => null })
getEmbeddedComponent (Module, Ideal, Function) := o -> (M, p, checkFunction) -> (
     foundValidComponent := false;
     j := if ann M == 0 then 2 else max(2, ceiling(max((ann M)_*/degree/sum) / min(p_*/degree/sum)));
     while not foundValidComponent do (
          if debugLevel > 0 then printerr("Trying bracket power " | toString(j) | " for candidate embedded component...");
          N := bracketPower(p, j)*M;
          Q := M/N;
	  storeAssociatedPrimesComputation(Q, {p}, codim p); -- used later in kernelOfLocalization
          strat := toString o.Strategy;
          C := if codim p == dim ring Q then (
               if debugLevel > 0 then printerr("Embedded prime is maximal!");
               0*Q
          ) else (
               if debugLevel > 0 then printerr("Using strategy " | if strat == "null" then "Sat" else strat);
               if strat == "Res" then topComponents(Q, codim p)
               else if strat == "Hom" then equidimHull(Q, codim p)
               else kernelOfLocalization(Q, p) -- this uses the cached associated primes computation
          );
          C = trim subquotient(generators C | generators N, relations M);
          foundValidComponent = checkFunction C;
          j = ceiling(3/2*j); -- 2*j;
     );
     C
)

kernelOfLocalization = method()
kernelOfLocalization (Module, Ideal) := Module => (M, P) -> (
     AP := associatedPrimes(M, CodimensionLimit => infinity);
     f := product(AP, p -> ( i := position(p_*, g -> g % P != 0); if i === null then 1 else p_i ));
     if debugLevel > 0 then printerr("Computing saturation...");
     if f == 1 then 0*M else saturate(0*M, f)
)

equidimHull = method() -- equidimensional hull of 0 in a module M
equidimHull (Module, ZZ) := Module => (M, c) -> (
     R := ring M;
     if debugLevel > 0 then printerr("Finding maximal regular sequence...");
     S := comodule regSeqInIdeal(annihilator M, c, c, 2);
     if debugLevel > 0 then printerr("Computing Ext...");
     if numColumns mingens M == 1 then (
          if debugLevel > 0 then printerr("Using case for cyclic module...");
          if debugLevel > 1 then printerr(toString M);
          if debugLevel > 1 then printerr(toString S);
          E := Hom(M, S); -- = Ext^c(M, R)
          if debugLevel > 0 then printerr("Getting annihilator...");
          return subquotient(generators annihilator E, relations M);
     );
     -- the following uses code from doubleDualMap in AnalyzeSheafOnP1
     h := coverMap M;
     ddh := Hom(Hom(h, S), S); -- = Ext^c(Ext^c(h, R), R)
     if debugLevel > 0 then printerr("Getting hull as kernel of " | toString(numRows matrix ddh) | " by " | toString(numColumns matrix ddh) | " matrix...");
     kernel map(target ddh, M, matrix ddh)
)
equidimHull Module := Module => M -> equidimHull(M, codim M)

regSeqInIdeal = method(Options => {Strategy => "Quick"})
regSeqInIdeal (Ideal, ZZ, ZZ, ZZ) := Ideal => opts -> (I, n, c, initialTime) -> ( -- attempts to find sparse maximal regular sequence contained in an ideal (in a CM ring)
     -- if debugLevel > 1 then printerr(toString I, n, c, initialTime);
     G := sort(flatten entries mingens I, f -> (sum degree f, #terms f));
     if c == #G then return ideal G;
     k := coefficientRing ring I;
     J := ideal(G#0);
     searchList := sort(subsets(1..<#G, 2), sum);
     for pair in searchList do (
          (i, j) := (pair#0, pair#1);
          coeffList := {0_k, 1_k};
          if debugLevel > 1 then printerr("Trying generators " | toString(i, j));
          for a in coeffList do (
               cand := G#i + a*G#j;
               K := J + ideal cand;
               if debugLevel > 1 then printerr("Testing regular sequence...");
               m := if opts.Strategy == "Quick" then ( 
                    try ( alarm initialTime; r := codim K; alarm 0; r ) else -1
               ) else codim K;
               if m == 1 + #J_* then (
                    J = K;
                    if debugLevel > 1 then printerr "Found nonzerodivisor!";
                    break;
               ) else if m == -1 and debugLevel > 1 then printerr "Exceeded time for codim check";
          );
          if #J_* == n then ( if debugLevel > 1 then printerr("Found regular sequence!"); return J );
     );
     if debugLevel > 1 then printerr "Could not find sparse regular sequence. Trying denser elements...";
     G1 := matrix{select(G, g -> g % J != 0)};
     J1 := J + ideal apply(n-#J_*, j -> G1*random(k^(numcols G1),k^1));
     if codim J1 == #J1_* then J1 else error "Could not find regular sequence. Try again with Strategy => \"Full\""
)
regSeqInIdeal (Ideal, ZZ) := Ideal => opts -> (I, n) -> (
     if debugLevel > 1 then printerr "Timing initial codim check...";
     t := timing codim I;
     t0 := 2 + ceiling first t;
     if debugLevel > 1 then printerr("Setting time limit of " | toString t0 | " seconds for each check...");
     c := last t;
     n = min(n, c);
     if c == infinity then ideal take(gens ring I, min(n, dim ring I)) else regSeqInIdeal(I, n, c, t0)
)
regSeqInIdeal Ideal := Ideal => opts -> I -> regSeqInIdeal(I, dim ring I + 1)

sort (List, Function) := opts -> (L, f) -> (
    H := hashTable(identity, apply(L, l -> f(l) => l));
    deepSplice join apply(sort keys H, k -> H#k))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PrimaryDecomposition.installed "
-- End:
