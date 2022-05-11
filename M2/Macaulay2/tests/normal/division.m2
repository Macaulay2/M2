-- As of M2 1.20, division in rings with non-global orders
-- gives an error instead of silently returning non-useful answers
-- (i.e. if something is clearly divisible, it sometimes returned 0 instead of the quotient).
-- Since division for such polynomials is not defined, we give an error now if 
-- we cannot compute it.

R = ZZ[t..u, Degrees => {2:1}, MonomialOrder => { MonomialSize => 32, GroupRevLex => 2, Position => Up}, Inverses => true]
assert( (quotientRemainder(1-t^3,1-t)) === (1+t+t^2,0_R) )
assert( (quotientRemainder(1-t^3,1-t)) === (1+t+t^2,0_R) )
assert( (quotientRemainder(1-t^3,1-t)) === (1+t+t^2,0_R) )
assert( (quotientRemainder(1-t^3,1-t)) === (1+t+t^2,0_R) )
assert( try (quotientRemainder(1-u^3,1-u) === (0_R,1-u^3); false) else true )
assert( try (quotientRemainder(1-u^3,1-u) === (0_R,1-u^3); false) else true )
assert( try (quotientRemainder(1-u^3,1-u) === (0_R,1-u^3); false) else true )
assert( try (quotientRemainder(1-u^3,1-u) === (0_R,1-u^3); false) else true )
assert( try (quotientRemainder(1-u^3,1-u) === (0_R,1-u^3); false) else true )

R = ZZ[t..u, Degrees => {2:1}, MonomialOrder => { MonomialSize => 32, Weights => {0,-1}, GroupRevLex => 2, Position => Up}, Inverses => true]
assert( try((quotientRemainder(1-t^3,1-t)) === (0_R,1-t^3); false) else true ) -- this is now disallowed
assert( try((quotientRemainder(1-t^3,1-t)) === (0_R,1-t^3); false) else true ) -- this is now disallowed
assert( try((quotientRemainder(1-t^3,1-t)) === (0_R,1-t^3); false) else true ) -- this is now disallowed
assert( try((quotientRemainder(1-t^3,1-t)) === (0_R,1-t^3); false) else true ) -- this is now disallowed
assert( try((quotientRemainder(1-t^3,1-t)) === (0_R,1-t^3); false) else true ) -- this is now disallowed
assert( (quotientRemainder(1-u^3,1-u)) === (1+u+u^2,0_R) )
assert( (quotientRemainder(1-u^3,1-u)) === (1+u+u^2,0_R) )
assert( (quotientRemainder(1-u^3,1-u)) === (1+u+u^2,0_R) )
assert( (quotientRemainder(1-u^3,1-u)) === (1+u+u^2,0_R) )
assert( (quotientRemainder(1-u^3,1-u)) === (1+u+u^2,0_R) )

R = ZZ[t..u, Degrees => {2:1}, MonomialOrder => { MonomialSize => 32, Weights => {-1,-1}, GroupRevLex => 2, Position => Up}, Inverses => true]
assert( (quotientRemainder(1-t^3,1-t)) === (1+t+t^2,0_R) )
assert( (quotientRemainder(1-t^3,1-t)) === (1+t+t^2,0_R) )
assert( (quotientRemainder(1-t^3,1-t)) === (1+t+t^2,0_R) )
assert( (quotientRemainder(1-t^3,1-t)) === (1+t+t^2,0_R) )
assert( (quotientRemainder(1-t^3,1-t)) === (1+t+t^2,0_R) )
assert( (quotientRemainder(1-u^3,1-u)) === (1+u+u^2,0_R) )
assert( (quotientRemainder(1-u^3,1-u)) === (1+u+u^2,0_R) )
assert( (quotientRemainder(1-u^3,1-u)) === (1+u+u^2,0_R) )
assert( (quotientRemainder(1-u^3,1-u)) === (1+u+u^2,0_R) )
assert( (quotientRemainder(1-u^3,1-u)) === (1+u+u^2,0_R) )

-- git issue 2374.  Divide by 0 crash. FIXED May 2022.
  F=frac(ZZ[q,t])
  m=matrix{{(q^2-2)/q^2,0},{(q^2-1)/2/q^2,1/2}}
  assert try (sub(m, q => 0); false) else true  -- crash!! FIXED (gives error, as it should)
  
  F=frac(ZZ[q,t])
  matrix{{1/q,1/2}}
  assert try (sub(oo, q => 0); false) else true -- crash! FIXED (gives error, as it should)

-- git issue 2346. FIXED May 2022.
  k=4;
  u = symbol u
  R=ZZ[u_0..u_(k-1),Inverses=>true,MonomialOrder=>Lex];
  M = matrix table(k,k,(i,j)->u_i^j)
  assert(det M != 0) -- OK now.  (used to) fail... (gave wrong answer, using Bareiss currently)
  d1 = det(M, Strategy => Cofactor) -- Another case where Bareiss (used to) fail...
  d2 = det(M, Strategy => Bareiss) -- Now works... -- now fast too
  assert(d1 == d2)
  
  -- a simpler version of the error:
  -- division was failing in each of these rings:
  R = ZZ[a, b, c, Inverses => true, MonomialOrder => Lex]
  f = a * (b-c)
  assert(f // (b-c) == a)
  assert(f % (b-c) == 0)

  R = ZZ[a, b, c, Inverses => true, Global => false, Inverses => true, MonomialOrder => {GroupLex => 3}]
  f = a * (b-c)
  assert(f // (b-c) == a)
  assert(f % (b-c) == 0)

  R = ZZ[a, b, c, Global => false, Inverses => true, MonomialOrder => {GroupLex => 3}]
  f = a * (b-c)
  assert(f // (b-c) == a)
  assert(f % (b-c) == 0)

  R = ZZ[a, b, c, Inverses => true, MonomialOrder => {GroupLex => 3}]
  f = a * (b-c)
  assert(f // (b-c) == a)
  assert(f % (b-c) == 0)

  R = ZZ[a, b, c, Inverses => true, MonomialOrder => {Lex => 3}]
  f = a * (b-c)
  assert(f // (b-c) == a)
  assert(f % (b-c) == 0)

  R = ZZ[a, b, c, MonomialOrder => {GroupLex => 2, Lex => 1}]
  f = a * (b-c)
  assert(f // (b-c) == a)
  assert(f % (b-c) == 0)

  f = (a^-1 * b^-1) * a * (b-c)
  assert(f // (b^-1*(b-c)) == 1)
  assert(f % (b-c) == 0)

-- However, division in the following cases, which used to basically return 0, now gives an error
-- if it can't figure out how to do the division in that ring.

  -- R = ZZ[a, b, c, Inverses => true, Global => false, Inverses => true, MonomialOrder => {Weights => {-1,-1,-1}, GroupLex => 3}]
  -- f = a * (b-c)
  -- assert(f // (b-c) == a) -- should be true
  -- assert(f % (b-c) == 0) -- should be true.


  -- R = ZZ[a, b, c, Inverses => true, MonomialOrder => {RevLex => 3}]
  -- f = a * (b-c)
  -- assert(f // (b-c) == a) -- should be true
  -- assert(f % (b-c) == 0) -- should be true.

  -- R = ZZ[a, b, c, MonomialOrder => {RevLex => 3}, Global => false]
  -- f = a * (b-c)
  -- assert(f // (b-c) == a) -- should be true
  -- assert(f % (b-c) == 0) -- should be true.

  -- R = QQ[a, b, c, MonomialOrder => {RevLex => 3}, Global => false]
  -- f = a * (b-c)
  -- assert(f // (b-c) == a) -- should be true
  -- assert(f % (b-c) == 0) -- should be true.

-- git issue #2215.  Fixed May 2022
  R=QQ[x,Inverses=>true,MonomialOrder=>Lex];
  f=map(QQ,R,{-17});
  assert(-1/17 == f (x^-1)) -- was crash, now appears fixed.

  -- These were fixed in a recent pull request.
  assert((-2)^(-2) == 1/4)
  assert((-2)^(-4) == 1/16)
  assert((-2)^(-3) == -1/8)
  assert((-17)^(-1) == -1/17)

-- git issue 2177, 2178 FIXED
  K=frac(QQ[t]) 
  R=K[y,x, MonomialOrder=>GLex]; 
  use R; 
  M=matrix{{x*(1/t^3),x*(1/(2*t))}} 
  M=substitute(M, x=>1) 
  assert try (substitute(M, t=>0); false) else true
  M=t*M 
  assert try (substitute(M, t=>0); false) else true  -- crash! NOW FIXED (gives an error)

-- git issue 1199 FIXED (although there are related issues here to audit).
  needsPackage "LocalRings"
  x = symbol x;
  y = symbol y;
  R = ZZ/32003[x_1,x_2,x_3,y_1..y_3, Degrees => {3:{1,0}, 3:{0,1}}]
  phi = matrix {{x_1,0,x_3}, {x_2,x_1,0}, {0,x_2,x_1}, {0,0,x_2}}

  RP = localRing(R, ideal(x_1,x_2))
  psi = sub(phi, RP)

  print "first issue"
  assert try (minors(3, psi); false) else true
  -- previous behavior:
  -- incorrect and there are warnings that should be errors.
    -- TODO (NOT DONE YET):
    --       (1) change the default order for this ring to be Cofactor.
    --       (2) the ring should perhaps allow division by a non-unit!
  -- these no longer occur:
  --error message bumped: attempt to divide by non-unit
  --error message bumped: attempt to divide by non-unit
  --error message bumped: attempt to divide by non-unit
  --o20 = ideal ()
  --o20 : Ideal of RP

  print "second issue" 
  m1 = minors(3, phi) -- crash! NO CRASH anymore
  m2 = minors(3, phi, Strategy => Cofactor) -- works!
  assert(m1 == m2)
  m3 = minors(3, psi, Strategy => Cofactor) -- works!
  assert(sub(m1, RP) == m3)

-- git issue 2249 FIXED (move these to AssociativeAlgebras?)
  needsPackage "AssociativeAlgebras"
  R = frac(QQ[t])
  S = R<|T|>
  f = map(S,S)
  a = t^(-1)*T
  assert(f a == a)
  f

  needsPackage "AssociativeAlgebras"
  R = frac(QQ[t])
  S = R<|T,X|>/(X*T-T*X)
  f = map(S,S)
  a = t^(-1)*(T*X-X)
  assert(f a == a)
  f

-- git issue #595 FIXED
  R = QQ{t}
  assert(1//t == 0_R)
  assert((1//t)*1 == 0_R) -- used to monomial overflow...
  assert(1//t == 0) -- used to not be a valid element in this ring! 
  assert(1_R % t == 1)
  assert(1_R == (1//t) * t + (1%t))

end--
generateAssertions ///
R = ZZ[t..u, Degrees => {2:1}, MonomialOrder => { MonomialSize => 32, GroupRevLex => 2, Position => Up}, Inverses => true]
quotientRemainder(1-t^3,1-t)
quotientRemainder(1-t^3,1-t)
quotientRemainder(1-t^3,1-t)
quotientRemainder(1-t^3,1-t)
quotientRemainder(1-u^3,1-u)
quotientRemainder(1-u^3,1-u)
quotientRemainder(1-u^3,1-u)
quotientRemainder(1-u^3,1-u)
quotientRemainder(1-u^3,1-u)
R = ZZ[t..u, Degrees => {2:1}, MonomialOrder => { MonomialSize => 32, Weights => {0,-1}, GroupRevLex => 2, Position => Up}, Inverses => true]
quotientRemainder(1-t^3,1-t)
quotientRemainder(1-t^3,1-t)
quotientRemainder(1-t^3,1-t)
quotientRemainder(1-t^3,1-t)
quotientRemainder(1-u^3,1-u)
quotientRemainder(1-u^3,1-u)
quotientRemainder(1-u^3,1-u)
quotientRemainder(1-u^3,1-u)
quotientRemainder(1-u^3,1-u)
///
