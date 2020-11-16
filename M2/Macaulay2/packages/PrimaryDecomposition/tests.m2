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
///

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
    scan(#L, i -> (
	    J := L_i;
	    P := radical J;
	    assert(P == L3_i);
	    if isPrimary(J,P) then () else (
		print(ring I);
		print I;
		print L;
		print J;
		assert false);
	    ));
    )

TEST ///
  debug PrimaryDecomposition
  w,x,y,z
  scan({  QQ, ZZ/3, ZZ/2, ZZ/101, ZZ/32003}, k -> (
	  Q := k[w,x,y,z];
	  scan({  ideal(x*y,y^2),
		  ideal(x^4*y^5),
		  ideal(w*x, y*z, w*y+x*z),
		  intersect((ideal(w,x,y-1))^2, ideal(y,z,w-1))}, I -> (
		  sl := {
		      ShimoyamaYokoyama,
		      EisenbudHunekeVasconcelos,
		      new Hybrid from (1,2),
		      new Hybrid from (2,2)};
		  if isMonomialIdeal I then sl = {Monomial} | sl;
		  scan(sl, s -> testResult(I, primaryDecomposition(I, Strategy => s)))));
	  scan({  new Hybrid from (1,1),
		  new Hybrid from (2,1)}, s -> (
		  testResult(ideal(x^4*y^5), primaryDecomposition(ideal(x^4*y^5), Strategy => s))));
	  )
      )
///
