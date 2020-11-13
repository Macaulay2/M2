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
     if I.cache#?"AssociatedPrimes" and o.CodimensionLimit < 0 then return I.cache#"AssociatedPrimes";
     I.cache#"AssociatedPrimes" = (
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
     ))


-- WARNING: this code below does NOT compute the associated primes of I.
-- Why not? (MES, 3 Jul 2006).
ass2 := (I) -> (
     if I.cache#?"AssociatedPrimes" then I.cache#"AssociatedPrimes" else I.cache#"AssociatedPrimes" = (
     assassinator := {};
     local newcomponents;
     RI := ring I;
     polyRing := ring presentation RI;
     currentI := lift(I,polyRing);
     topcurrentI := topComponents currentI;
     while not isSubset(topcurrentI, currentI) do(
    if debugLevel >= 2 then print "  beginning new associatedPrimes loop";
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
     ))


associatedPrimes Ideal := List => o -> (I) -> ass1(I, o)

TEST ///
-- This last little code is to check if two lists are the
-- same up to permutation.

isSameList = (L1,L2) ->(
     ret := null;
     ret1 := true;
     counter1 := 0;
     counter2 := 0;
     if #L1 =!= #L2 then ret = false else(
          while counter1 < #L1 and ret1 == true do (
         ret2 := false;
         counter2 = 0;
         while counter2 < #L2 and ret2 == false do(
            if L1#counter1 == L2#counter2 
            then ret2 = true;
            counter2 = counter2 + 1;
            );
           if ret2 == false then ret1 = false;
           counter1 = counter1 + 1;);
      ret = ret1;
      );
     ret	       
     )     	    

R=ZZ/(101)[x,y,z];
I=ideal (x^2,x*y);
assoutcome = associatedPrimes(I,Strategy=>1)
trueass = {ideal(x),ideal(x,y)};
assert(isSameList(assoutcome,trueass))

I=ideal (x^2,x*y);
assoutcome = associatedPrimes(I,Strategy=>2)
isSameList(assoutcome, trueass)
S=R/I;
J=ideal(0_S);	
assoutcome = associatedPrimes J
trueass = {ideal(x),ideal(x,y)};
assert(isSameList(assoutcome, trueass))

R=ZZ/31991[x,y,z]
I1 = ideal(x^3,x*y,z^2);
I2 = ideal(x,y+z);
I3 = ideal(x^5*z,y^3*z^2);
K=intersect(I1,I2,I3)
assoutcome = associatedPrimes K
trueass = {ideal(z), ideal(y,x), ideal(z,x), ideal(y+z,x)}     
assert(isSameList(assoutcome, trueass))
///


--In localize, P is an associated prime.
--This routine computes the localization of I at P.
--For this version, we have to know associatedPrimes I

SYlocalize := (assroutine) -> (I,P) ->(
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
      f := lift((flatten entries generators gb IOPrp)#0, polyRing);
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

EHVlocalize := (I,P) ->(
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

localize = method(Options =>{ Strategy => 1})

localize (Ideal,Ideal) := Ideal => o -> (I,P) -> (
     if o.Strategy === 0 then (
      if debugLevel >=1 then print "localize:  Using Strategy 0";
      EHVlocalize(I,P))
     else if o.Strategy === 1 then (
      if debugLevel >=1 then print "localize:  Using Strategy 1";
      (SYlocalize ass1)(I,P))
     else (
      if debugLevel >=1 then print "localize:  Using Strategy 2";
      (SYlocalize ass2)(I,P))
     )

TEST ///
R = ZZ/(101)[x,y];
I = ideal (x^2,x*y);
P1 = ideal (x);
outcome = localize(I,P1)
outcome == P1
P2 = ideal (x,y);
outcome = localize(I,P2)
outcome == I

R = ZZ/(31991)[x,y,z];
I = ideal(x^2,x*z,y*z);
P1 = ideal(x,y);
outcome = localize(I,P1)
outcome == P1
P2 = ideal(x,z);
outcome = localize(I,P2)
trueanswer = ideal(x^2,z);
outcome == trueanswer
///

--This part should compute the/a primary component of I
--corresponding to P.
--A BIG issue here is how to increment m.  this could make
--a big speed difference.

-- Of course in this next thing, if we know associatedPrimes, we can use
-- localize, but if we just know I and the prime, we might 
-- rather use EHVlocalize.  It's hard to know.  
primarycomponent := (localizeroutine) -> 
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

primaryComponent = method( Options => { Strategy => 2, Increment =>1 })

primaryComponent(Ideal,Ideal) := Ideal => o -> (I,P) -> (
     localizefcn := if o.Strategy === 1 then
      SYlocalize ass1
     else if o.Strategy === 2 then
      SYlocalize ass2
     else EHVlocalize;
     (primarycomponent localizefcn)(I,P,o.Increment))

TEST ///
R = ZZ/(101)[x,y];
I = ideal (x^2,x*y);
P1 = ideal (x);
primaryComponent(I,P1)
P2 = ideal (x,y);
primaryComponent(I,P2)

R = ZZ/(31991)[x,y,z];
I = ideal(x^2,x*z,y*z);
P1 = ideal(x,y);
primaryComponent(I,P1)
P2 = ideal(x,z);
primaryComponent(I,P2)
///

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


TEST ///
R=ZZ/(101)[x,y,z];
I=ideal (x^2,x*y);
associatedPrimes(I,Strategy=>1)
associatedPrimes(I,Strategy=>2)
S=R/I;
J=ideal(0_S);
primaryDecomposition(J,Strategy=>EisenbudHunekeVasconcelos)
associatedPrimes J
P=ideal(x);

R = ZZ/31991[x,y,z]
J=ideal (x*y^2,x*z^2);
P2=ideal(y,z);
associatedPrimes(J)
localize (J,P2)
primaryComponent(J,P2)
primaryDecomposition(J,Strategy=>EisenbudHunekeVasconcelos)

R = ZZ/101[a..d]
S = R/(a*b-c^2)
T = S/(a^3,b^3)
J = ideal(c)
ring presentation T
J1 = lift(J,ring presentation T)
J1 = J1 + ideal(R_3^5)
trim substitute(J1,T)
///


-- Primary decomposition code for modules

associatedPrimes Module := List => o -> M -> ( -- modified code in ass1 for modules
     if not M.cache#?"AssociatedPrimes" then M.cache#"AssociatedPrimes" = {};
     k := if o.CodimensionLimit < 0 then infinity else o.CodimensionLimit;
     p := if M.cache#?"associatedPrimesCodimLimit" then M.cache#"associatedPrimesCodimLimit" else -2;
     if p >= o.CodimensionLimit then return select(M.cache#"AssociatedPrimes", P -> codim P <= k);
     rel := presentation ring M;
     S := ring rel; -- S is the ambient polynomial ring which ring M is a quotient of
     M1 := if ring M === S then M else (
          liftRel := id_(lift(target relations M, S)) ** rel;
          trim subquotient(lift(gens M, S), lift(relations M, S) | liftRel)
     );
     mapback := I -> trim((map(ring M, S, vars ring M)) I);
     (A, c, d, C) := (trim ann M1, codim M1, dim S, null);
     for i from max(1+p, c) to min(d, k) do (
          if debugLevel > 0 then print("Extracting associated primes of codim " | toString i);
          newPrimes := if i == d and ((isHomogeneous M and (c == d or pdim M1 == d)) or (c == d and all(gens S, v -> radicalContainment(v, A)))) then {ideal gens S} else (
               if i > c then ( -- computes ann Ext^i(M1, S)
                    if C === null then C = res(M1, LengthLimit => k+1);
                    if length C < i then ( k = infinity; break; );
                    A = trim ann minPres(ker transpose C.dd_(i+1) / image transpose C.dd_i);
               );
               if codim A > i then {} else minimalPrimes(A, CodimensionLimit => i)
          );
          M.cache#"AssociatedPrimes" = M.cache#"AssociatedPrimes" | newPrimes/mapback;
          M.cache#"associatedPrimesCodimLimit" = i;
     );
     if k >= dim S then M.cache#"associatedPrimesCodimLimit" = infinity;
     M.cache#"AssociatedPrimes"
)
associatedPrimes Ring := List => o -> R -> associatedPrimes(comodule ideal R, o)

-- Returns a primary decomposition of 0 in M. Assumes all embedded primes appear after all primes they contain, i.e. isSubset(AP#i, AP#j) => i \le j (equivalently, the ordering of associated primes is a linear extension of the partial order by inclusion). This is the case for associatedPrimes(Module), which returns associated primes ordered by codimension
primaryDecomposition Module := List => o -> M -> (
     if not M.cache#?"primaryComponents" then M.cache#"primaryComponents" = new MutableHashTable;
     AP := associatedPrimes(M, CodimensionLimit => infinity);
     if #values(M.cache#"primaryComponents") != #AP then (
          H := hashTable apply(AP, p -> p => select(#AP, i -> isSubset(AP#i, p)));
          for i to #AP - 1 do (
               if debugLevel > 0 then print("Prime: " | toString(i+1) | "/" | toString(#AP));
               p := AP#i;
               if M.cache#"primaryComponents"#?p then continue;
               f := product(AP - set AP_(H#p), q -> q_(position(q_*, g -> g % p != 0)));
               isolComp := if f == 1 then 0*M else saturate(0*M, f);
               M.cache#"primaryComponents"#p = if #(H#p) > 1 then (
                    B := intersect apply(H#p - set{i}, k -> M.cache#"primaryComponents"#(AP#k));
                    getEmbeddedComponent(M, p, C -> isSubset(intersect(B, C), isolComp), o)
               ) else isolComp;
          );
     );
     apply(AP, p -> M.cache#"primaryComponents"#p)
)
primaryDecomposition Ring := List => o -> R -> primaryDecomposition(comodule ideal R, o)

-- Additional functions useful for primary decomposition

bracketPower = (I, n) -> ideal apply(I_*, f -> f^n)

getEmbeddedComponent = method(Options => options primaryDecomposition)
getEmbeddedComponent (Module, Ideal, Function) := o -> (M, p, checkFunction) -> (
     foundValidComponent := false;
     j := max(2, ceiling(max((ann M)_*/degree/sum) / min(p_*/degree/sum)));
     while not foundValidComponent do (
          if debugLevel > 0 then print("Trying bracket power " | toString(j) | " for candidate embedded component...");
          N := bracketPower(p, j)*M;
          Q := M/N;
          Q.cache#"AssociatedPrimes" = {p};
          Q.cache#"associatedPrimesCodimLimit" = codim p;
          strat := toString o.Strategy;
          C := if codim p == dim ring Q then (
               if debugLevel > 0 then print("Embedded prime is maximal!");
               0*Q
          ) else (
               if debugLevel > 0 then print("Using strategy " | if strat == "null" then "Sat" else strat);
               if strat == "Res" then topComponents(Q, codim p)
               else if strat == "Hom" then equidimHull(Q, codim p)
               else kernelOfLocalization(Q, p)
          );
          C = trim subquotient(generators C | generators N, relations M);
          foundValidComponent = checkFunction C;
          j = 2*j;
     );
     C
)

kernelOfLocalization = method()
kernelOfLocalization (Module, Ideal) := Module => (M, P) -> (
     AP := associatedPrimes(M, CodimensionLimit => infinity);
     f := product(AP, p -> ( i := position(p_*, g -> g % P != 0); if i === null then 1 else p_i ));
     if debugLevel > 0 then print("Computing saturation...");
     if f == 1 then 0*M else saturate(0*M, f)
)

topComponents (Module, ZZ) := Module => (M, e) -> (
     S := ring M;
     N := 0*M;
     f := pdim M;  -- calls minimalPresentation, will compute a resolution if needed...
     while f > e do (
          E := Ext^f(M,S);
          if codim E == f then (
               if debugLevel > 0 then print("Getting annihilator of Ext...");
               I := annihilator E;
               if debugLevel > 0 then print("Removing components of codim " | toString(f));
               N = N : I;
          );
          f = f-1;
     );
     N
)

equidimHull = method() -- equidimensional hull of 0 in a module M
equidimHull (Module, ZZ) := Module => (M, c) -> (
     R := ring M;
     if debugLevel > 0 then print("Finding maximal regular sequence...");
     S := comodule regSeqInIdeal(annihilator M, c, c, 2);
     if debugLevel > 0 then print("Computing Ext...");
     if numColumns mingens M == 1 then (
          if debugLevel > 0 then print("Using case for cyclic module...");
          if debugLevel > 1 then print(toString M);
          if debugLevel > 1 then print(toString S);
          E := Hom(M, S); -- = Ext^c(M, R)
          if debugLevel > 0 then print("Getting annihilator...");
          return subquotient(generators annihilator E, relations M);
     );
     -- the following uses code from doubleDualMap in AnalyzeSheafOnP1
     h := coverMap M;
     ddh := Hom(Hom(h, S), S); -- = Ext^c(Ext^c(h, R), R)
     if debugLevel > 0 then print("Getting hull as kernel of " | toString(numRows matrix ddh) | " by " | toString(numColumns matrix ddh) | " matrix...");
     kernel map(target ddh, M, matrix ddh)
)
equidimHull Module := Module => M -> equidimHull(M, codim M)

regSeqInIdeal = method(Options => {Strategy => "Quick"})
regSeqInIdeal (Ideal, ZZ, ZZ, ZZ) := Ideal => opts -> (I, n, c, initialTime) -> ( -- attempts to find sparse maximal regular sequence contained in an ideal (in a CM ring)
     if debugLevel > 1 then print(toString I, n, c, initialTime);
     G := sort(flatten entries mingens I, f -> (sum degree f, #terms f));
     if c == #G then return ideal G;
     k := coefficientRing ring I;
     J := ideal(G#0);
     searchList := sort(subsets(1..<#G, 2), sum);
     for pair in searchList do (
          (i, j) := (pair#0, pair#1);
          coeffList := {0_k, 1_k};
          if debugLevel > 1 then print("Trying generators " | toString(i, j));
          for a in coeffList do (
               cand := G#i + a*G#j;
               K := J + ideal cand;
               if debugLevel > 1 then print("Testing regular sequence...");
               m := if opts.Strategy == "Quick" then ( 
                    try ( alarm initialTime; r := codim K; alarm 0; r ) else -1
               ) else codim K;
               if m == 1 + #J_* then (
                    J = K;
                    if debugLevel > 1 then print "Found nonzerodivisor!";
                    break;
               ) else if m == -1 and debugLevel > 1 then print "Exceeded time for codim check";
          );
          if #J_* == n then ( if debugLevel > 1 then print("Found regular sequence!"); return J );
     );
     if debugLevel > 1 then print "Could not find sparse regular sequence. Trying denser elements...";
     G1 := matrix{select(G, g -> g % J != 0)};
     J1 := J + ideal apply(n-#J_*, j -> G1*random(k^(numcols G1),k^1));
     if codim J1 == #J1_* then J1 else error "Could not find regular sequence. Try again with Strategy => \"Full\""
)
regSeqInIdeal (Ideal, ZZ) := Ideal => opts -> (I, n) -> (
     if debugLevel > 1 then print "Timing initial codim check...";
     t := timing codim I;
     t0 := 2 + ceiling first t;
     if debugLevel > 1 then print("Setting time limit of " | toString t0 | " seconds for each check...");
     c := last t;
     n = min(n, c);
     if c == infinity then ideal take(gens ring I, min(n, dim ring I)) else regSeqInIdeal(I, n, c, t0)
)
regSeqInIdeal Ideal := Ideal => opts -> I -> regSeqInIdeal(I, dim ring I + 1)

sort (List, Function) := opts -> (L, f) -> (
     H := hashTable(identity, apply(L, l -> f(l) => l));
     deepSplice join apply(sort keys H, k -> H#k)
)

TEST /// -- non-cyclic modules
R = QQ[x_0..x_3]
I = monomialCurveIdeal(R,{1,2,3})
J = monomialCurveIdeal(R,{1,3,4})
K = monomialCurveIdeal(R,{1,4,5})
M = comodule I ++ comodule J ++ comodule K -- direct sum
AP = associatedPrimes M
assert(set associatedPrimes M === set{I,J,K})
comps = primaryDecomposition M
assert(intersect comps == 0 and all(comps, isPrimary_M))
N = coker map(M, R^1, transpose matrix{{1_R,1,1}}) -- coker of diagonal map
assert(numcols mingens N == 2 and #associatedPrimes N == 5)
comps = primaryDecomposition N
assert(intersect comps == 0 and all(comps, isPrimary_N))
///

TEST /// -- modules over iterated quotient rings
R = QQ[x,y,z,w,u]/(u^4 - x*y*z*w)
S = R/(x^3 - y^2*z)
T = S/(y*w^2 - z*x^2)
N1 = coker gens ideal (T_0^4 - 1, T_1^4)
N2 = comodule ideal (T_2^6, T_3^7)
N3 = comodule ideal (T_4^3*T_0, T_1^2*T_2^2)
M = N1 ++ N2 ++ N3
assert(#associatedPrimes M == 4)
comps = primaryDecomposition M
assert(intersect comps == 0 and all(comps, isPrimary_M))
N = coker map(M, (ring M)^1, transpose matrix{{1_(ring M),1,1}})
assert(#associatedPrimes N == 2)
elapsedTime comps = primaryDecomposition N
assert(intersect comps == 0 and all(comps, isPrimary_N))
///

TEST /// -- multiply embedded prime
R = QQ[x_0..x_3]
I = intersect((ideal(x_0..x_3))^5, (ideal(x_0..x_2))^4, (ideal(x_0..x_1))^3)
M = comodule I
AP = associatedPrimes M
comps = primaryDecomposition M
assert(intersect comps == 0 and all(comps, isPrimary_M))
///

TEST /// -- tough example for old primaryDecomposition, good on new code for modules
-- Example 4.4 in https://arxiv.org/pdf/2006.13881.pdf
R = QQ[x_0..x_5]
P = minors(2, matrix{{x_0,x_1,x_3,x_4},{x_1,x_2,x_4,x_5}}) -- surface scroll S(2,2) in P^5
L = P^2_*; I = ideal (L_0 + L_9, L_0 + L_12, L_13 + L_20)
M = comodule I
assert(#associatedPrimes M == 5)
comps = primaryDecomposition M
assert(sum(comps, Q -> degree(I + ideal gens Q)) == degree I)
///

TEST /// -- cf. https://groups.google.com/g/macaulay2/c/dFPzfS3tR2E
R = ZZ/2[Z_1..Z_9]
I = ideal(Z_6*Z_8+Z_5*Z_9,Z_3*Z_8+Z_2*Z_9,Z_6*Z_7+Z_4*Z_9,Z_4^3+Z_5^3+Z_6^3,Z_1*Z_2^2+Z_4*Z_5^2+Z_7*Z_8^2,Z_1^3+Z_5^3+Z_6^3+Z_8^3+Z_9^3,Z_1*Z_2*Z_4^2*Z_5*Z_9+Z_2^2*Z_5^3*Z_9+Z_2^2*Z_6^3*Z_9+Z_1^2*Z_7*Z_8^2*Z_9+Z_2^2*Z_8^3*Z_9+Z_2^2*Z_9^4)
M = comodule I
elapsedTime associatedPrimes M; -- ~ 5-15 seconds (?)
elapsedTime primaryDecomposition M; -- ~ 5 seconds
assert(all(primaryDecomposition M, isPrimary_M))
assert(intersect apply(primaryDecomposition M, Q -> I + ideal gens Q) == I)
-- note: primaryDecomposition I takes ~ 60 seconds, and if interrupted, gives missed components error on resume
///

TEST /// -- [associatedPrimes, CodimensionLimit] test
R = QQ[x_0..x_5]
exps = {6,7}
supps = {ideal(R_0,R_1,R_2), ideal(R_0,R_3,R_4,R_5)}
elapsedTime I = intersect apply(#supps, i -> (supps#i)^(exps#i));
M = comodule I;
elapsedTime assert(associatedPrimes(M, CodimensionLimit => 2) == {})
elapsedTime AP = associatedPrimes(M, CodimensionLimit => 4) -- ~ 4 seconds
-- elapsedTime associatedPrimes(M, CodimensionLimit => infinity) -- > 40 seconds (computing unnecessary Ext)
assert(all(AP, P -> any(supps, Q -> Q == P)) and all(supps, P -> any(AP, Q -> Q == P)))
M.cache#"associatedPrimesCodimLimit" = infinity
elapsedTime comps = primaryDecomposition M; -- ~ 4 seconds
assert(intersect comps == 0 and all(comps, isPrimary_M))
///

TEST /// -- Optimizing cases for associatedPrimes without computing res
R = QQ[x_1..x_5]
I = intersect apply(10, i -> ideal apply(gens R, v -> v - random QQ)); -- 10 points in A^5
M = comodule I;
elapsedTime AP = associatedPrimes M;
elapsedTime comps = primaryDecomposition M;
assert(intersect comps == 0 and all(comps, isPrimary_M))

I = intersect apply(10, i -> ideal apply(delete(first random gens R, gens R), v -> v - random QQ)); -- 10 lines in A^5
M = comodule I;
elapsedTime AP = associatedPrimes(M, CodimensionLimit => codim M) -- < 2 seconds
M.cache#"associatedPrimesCodimLimit" = infinity
elapsedTime comps = primaryDecomposition M; -- ~ 6 seconds
assert(intersect comps == 0 and all(comps, isPrimary_M))
-- note: primaryDecomposition I is very slow
///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PrimaryDecomposition.installed "
-- End:
