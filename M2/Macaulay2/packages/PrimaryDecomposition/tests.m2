TEST ///
  -- trivial cases:
  R = QQ[x,y,z]
  I = monomialIdeal(1_R)
  assert(primaryDecomposition I == {})
  assert not isPrimary I
  assert not isPrime I
  assert(minimalPrimes I == {})

  I = ideal(1_R)
  assert(primaryDecomposition I == {})
  assert not isPrimary I
  assert not isPrime I
  assert(minimalPrimes I == {})

  I = ideal(0_R)
  assert(primaryDecomposition I == {trim ideal(0_R)})
  assert isPrimary I
  assert isPrime I
  assert(minimalPrimes I == {ideal(0_R)})

  I = trim ideal(0_R)
  assert(primaryDecomposition I == {trim ideal(0_R)})
  assert isPrimary I
  assert isPrime I
  assert(minimalPrimes I == {ideal(0_R)})

  I = monomialIdeal(0_R)
  assert(primaryDecomposition I == {monomialIdeal(0_R)})
  assert isPrimary I
  assert isPrime I
  assert(minimalPrimes I == {ideal(0_R)})
  assert(all(minimalPrimes I, f -> class f === MonomialIdeal))
  
  M = comodule ideal(0_R)
  assert(associatedPrimes M == {ideal(0_R)})
  assert(primaryDecomposition M == {0})
///

TEST get(currentFileDirectory | "associatedPrimes-test.m2")
TEST get(currentFileDirectory | "associatedPrimes2-test.m2")
TEST get(currentFileDirectory | "primaryDecomposition-test.m2")

testResult = method()
testResult(Ideal,List) := (I,L) -> (
    assert(#L > 0);
    scan(L, J -> assert(isIdeal J and ring J === ring I));
    assert(I == intersect L);
    if #L > 1 then scan(#L, i -> (
	    L2 := L_(select(toList(0 .. (#L-1)), j -> j != i));
	    assert(I != intersect L2)));
    L3 := associatedPrimes I;
    assert(#L == #L3);
    -- print(radical \ L, L3);
    scan(#L, i -> (
	    J := L_i;
	    P := radical J;
	    -- This assertion is important, because the order of
	    -- associated primes and primary components should match
	    assert(P == L3_i);
	    if isPrimary(J,P) then () else (
		print(ring I);
		print I;
		print L;
		print J;
		assert false);
	    ));
    )

TEST /// -- testing strategies
  importFrom_PrimaryDecomposition {"testResult"}
  w,x,y,z
  scan({  QQ, ZZ/3, ZZ/2, ZZ/101, ZZ/32003}, k -> (
	  Q := k[w,x,y,z];
	  scan({  ideal(x*y,y^2),
		  ideal(x^4*y^5),
		  ideal(w*x, y*z, w*y+x*z),
		  intersect((ideal(w,x,y-1))^2, ideal(y,z,w-1))}, I -> (
		  sl := {"Comodule", ShimoyamaYokoyama, EisenbudHunekeVasconcelos};
		  if isMonomialIdeal I then sl = {Monomial} | sl;
		  scan(sl, s -> testResult(I, primaryDecomposition(ideal I_*, Strategy => s)))));
	  scan({  {1,1}, {1,2}, {2,1}, {2,2}},
	      s -> testResult(ideal(x^4*y^5),
		  primaryDecomposition(ideal(x^4*y^5), Strategy => Hybrid s)));
	  )
      )
///

-- This last little code is to check if two lists are the same up to permutation.
-- caveat: {1,1,2} and {1,2,2} are equal with this code
isSameList = (L1, L2) -> #L1 == #L2 and all(L1, I -> any(L2, J -> I == J)) and all(L2, I -> any(L1, J -> I == J))
-*
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
*-

TEST /// -- tests for associatedPrimes
  importFrom_PrimaryDecomposition { "isSameList" }

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

TEST /// -- tests for localize
  R = ZZ/(101)[x,y];
  I = ideal (x^2,x*y);
  P1 = ideal (x);
  outcome = localize(I,P1)
  outcome == P1
  P2 = ideal (x,y);
  outcome = localize(I,P2)
  assert(outcome == I)

  R = ZZ/(31991)[x,y,z];
  I = ideal(x^2,x*z,y*z);
  P1 = ideal(x,y);
  outcome = localize(I,P1)
  outcome == P1
  P2 = ideal(x,z);
  outcome = localize(I,P2)
  trueanswer = ideal(x^2,z);
  assert(outcome == trueanswer)
///

TEST /// -- another test for localize, cf. https://github.com/Macaulay2/M2/issues/923
  R = QQ[x]
  P = ideal(x)
  I = ideal(x^3+x^2)
  IP = localize(I, P)
  assert(IP == first select(primaryDecomposition I, Q -> radical Q == P))
///

TEST /// -- tests for primaryComponent
  R = ZZ/(101)[x,y];
  I = ideal (x^2,x*y);
  P1 = ideal (x);
  Q1 = primaryComponent(I,P1)
  assert(any(primaryDecomposition I, Q -> Q == Q1))
  P2 = ideal (x,y);
  Q2 = primaryComponent(I,P2)
  assert(I == intersect(Q1, Q2)) -- embedded components are not unique!

  R = ZZ/(31991)[x,y,z];
  I = ideal(x^2,x*z,y*z);
  P1 = ideal(x,y);
  Q1 = primaryComponent(I,P1)
  assert(any(primaryDecomposition I, Q -> Q == Q1))
  P2 = ideal(x,z);
  Q2 = primaryComponent(I,P2)
  assert(any(primaryDecomposition I, Q -> Q == Q2))
///

TEST /// -- tests for EisenbudHunekeVasconcelos (no asserts?!)
  R = ZZ/(101)[x,y,z];
  I = ideal(x^2, x*y);
  associatedPrimes(ideal I_*, Strategy => 1)
  associatedPrimes(ideal I_*, Strategy => 2)
  S = R/I;
  J = ideal(0_S);
  primaryDecomposition(J, Strategy => EisenbudHunekeVasconcelos)
  associatedPrimes J
  P = ideal x;

  R = ZZ/31991[x,y,z]
  J = ideal(x*y^2, x*z^2);
  P2 = ideal(y,z);
  associatedPrimes J
  localize(J, P2)
  primaryComponent(J, P2)
  primaryDecomposition(J, Strategy => EisenbudHunekeVasconcelos)

  R = ZZ/101[a..d]
  S = R/(a*b-c^2)
  T = S/(a^3, b^3)
  J = ideal c
  ring presentation T
  J1 = lift(J, ring presentation T)
  J1 = J1 + ideal(R_3^5)
  trim substitute(J1, T)
///

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

TEST /// -- modules over quotient rings, II
R = QQ[x,y,z]/(x^2 - y*z)
P = ideal(x, z)
associatedPrimes comodule P^2
M = comodule P^2
associatedPrimes(M, CodimensionLimit => 2)
assert(associatedPrimes(M, CodimensionLimit => 2) == {P, ideal gens R})
elapsedTime comps = primaryDecomposition M
assert(intersect comps == 0 and all(comps, isPrimary_M))
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

TEST /// -- testing Strategy => Hybrid
-- Has 2 embedded primes
-- One embedded prime will need to increase bracket power at least once
-- On bracket power increase, minimalPresentation Q (quotient by candidate) is slow
  R = QQ[h,l,s,z,y,x]
  I = ideal(h*l-l^2-4*l*s+h*y,h^2*s-6*l*s^3+h^2*z,x*h^2-l^2*s-h^3)
  M = comodule I
  elapsedTime AP = associatedPrimes M
  elapsedTime comps = primaryDecomposition(M, Strategy => Hybrid{"Hom"});
  assert(intersect comps == 0 and all(comps, isPrimary_M))
///

TEST /// -- cf. https://groups.google.com/g/macaulay2/c/dFPzfS3tR2E
  R = ZZ/2[Z_1..Z_9];
  I = ideal(Z_6*Z_8+Z_5*Z_9,Z_3*Z_8+Z_2*Z_9,Z_6*Z_7+Z_4*Z_9,Z_4^3+Z_5^3+Z_6^3,Z_1*Z_2^2+Z_4*Z_5^2+Z_7*Z_8^2,Z_1^3+Z_5^3+Z_6^3+Z_8^3+Z_9^3,Z_1*Z_2*Z_4^2*Z_5*Z_9+Z_2^2*Z_5^3*Z_9+Z_2^2*Z_6^3*Z_9+Z_1^2*Z_7*Z_8^2*Z_9+Z_2^2*Z_8^3*Z_9+Z_2^2*Z_9^4);
  M = comodule I;
  elapsedTime assert(22 == #associatedPrimes M); -- ~ 5 seconds
  elapsedTime assert(22 == #primaryDecomposition M) -- ~ 5 seconds
  assert(all(primaryDecomposition M, isPrimary_M));
  assert(intersect apply(primaryDecomposition M, Q -> I + ideal gens Q) == I);
  -- TODO: other primaryDecomposition I strategies take ~ 60 seconds,
  -- and if interrupted, give missed components error on resume
  elapsedTime assert(22 == #primaryDecomposition I) -- should be instantaneous from cache
///

TEST /// -- [associatedPrimes, CodimensionLimit] test
  importFrom_PrimaryDecomposition {"AssociatedPrimesOptions"}
  R = QQ[x_0..x_5]
  exps = {6,7}
  supps = {ideal(R_0,R_1,R_2), ideal(R_0,R_3,R_4,R_5)}
  elapsedTime I = intersect apply(#supps, i -> (supps#i)^(exps#i));
  M = comodule I;
  elapsedTime assert(associatedPrimes(M, CodimensionLimit => 2) == {})
  elapsedTime AP = associatedPrimes(M, CodimensionLimit => 4) -- ~ 3 seconds
  -- elapsedTime associatedPrimes(M, CodimensionLimit => infinity) -- > 40 seconds (computing unnecessary Ext)
  assert(all(AP, P -> any(supps, Q -> Q == P)) and all(supps, P -> any(AP, Q -> Q == P)))
  M.cache#(AssociatedPrimesOptions{}).CodimensionLimit = infinity
  elapsedTime comps = primaryDecomposition M; -- ~ 4 seconds
  assert(intersect comps == 0 and all(comps, isPrimary_M))
///

TEST /// -- Optimizing cases for associatedPrimes without computing res
  importFrom_PrimaryDecomposition {"AssociatedPrimesOptions"}
  R = QQ[x_1..x_5]
  I = intersect apply(10, i -> ideal apply(gens R, v -> v - random QQ)); -- 10 points in A^5
  M = comodule I;
  elapsedTime AP = associatedPrimes M;
  elapsedTime comps = primaryDecomposition M;
  assert(intersect comps == 0 and all(comps, isPrimary_M))

  I = intersect apply(10, i -> ideal apply(delete(first random gens R, gens R), v -> v - random QQ)); -- 10 lines in A^5
  M = comodule I;
  elapsedTime AP = associatedPrimes(M, CodimensionLimit => codim M) -- < 2 seconds
  M.cache#(AssociatedPrimesOptions{}).CodimensionLimit = infinity
  elapsedTime comps = primaryDecomposition M; -- ~ 3 seconds
  assert(intersect comps == 0 and all(comps, isPrimary_M))
///

TEST /// -- testing monomial ideal inputs
  R = QQ[a..g]
  I = ideal(a*b*c^3, a^3*d*f^2, a*b*c*d*e, b*c^4*d^3, e*f^5)
  C = primaryDecomposition I
  A = associatedPrimes I
  scan(#C, i -> radical(monomialIdeal C_i) == monomialIdeal A_i)
  -- radical of a monomial ideal should immediately call the monomial ideal cdoe too
  radical C_1
  I = ideal(a^2,a*b,b^2)
  C = primaryDecomposition I
  irreducibleDecomposition monomialIdeal I

  I = intersect(ideal(a^2,a*b,b^2), ideal(b,c,d^10))
  C = primaryDecomposition I
  associatedPrimes I
///

TEST /// -- testing Shimoyama-Yokoyama
  -- Simple examples
  -- Example 1.
  R = ZZ/32003[a..d]
  I = ideal(a*b, c*d, a*c+b*d)
  time primaryDecomposition I
  -- 3 components
  -- (a,d), (b,c), ((c,d)^2,(a,b)^2,ac+bd)

  -- Example 2.
  R = ZZ/32003[a,b,c]
  I = ideal(a^2, a*b)
  time primaryDecomposition I
  -- two components: (a), (a2, b)

  -- Example 3.
  R = ZZ/32003[a..d]
  I = ideal(a,b,c-1)
  J = ideal(c,d,a-1)
  L = intersect(I^2, J)
  time primaryDecomposition L

  -- By hand:
  --C1 = PD ideal flatten entries generators L
  --next C1
  --donode C1
  --peek C1
///

TEST ///
  R = ZZ/101[symbol a..symbol d]
  I = monomialCurveIdeal(R,{1,2,3})
  I^2
  removeLowestDimension(I^2)
  assert(I == radical(I^2))
  assert(I == radical(I^2, Unmixed => true))
  assert(topComponents (I^2) == I^2)
  S = R/(a^3, b^3)
  I = ideal(0_S)
  J = I^2
  J1 = topComponents J
  J1 == J
  time (radical I)

  -- 3 by 3 nilpotent matrices
  R = ZZ/101[vars(0..8)]
  M = genericMatrix(R,a,3,3)
  I = ideal (M^3)
  I1 = ideal(I_0,I_1,I_2)
  codim I1
  radical(I, CompleteIntersection=>I1)
  -- radical(I,Unmixed=>true)
  -- I1 = removeLowestDimension I
  -- I2 = removeLowestDimension I1
///

TEST /// -- test of removeLowestDimension
  R = ZZ/32003[a,b,c]
  I = ideal(a^2,b^2)
  J = ideal(a^3,b^3,c^3)
  I = intersect(I,J)
  assert same(removeLowestDimension I, topComponents I, ideal"b2,a2")
  assert(radical I == ideal(a,b))

  R = ZZ/101[a..d]
  I = intersect(ideal(a^2,b^2,c), ideal(a,d^4), ideal(b^2,c^2,d^2))
  assert same(topComponents I, removeLowestDimension I, ideal"a,d4")
  assert(radical I == ideal(a*b, a*c, a*d, b*d, c*d))
///
