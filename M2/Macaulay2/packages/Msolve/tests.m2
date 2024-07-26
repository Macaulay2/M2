TEST ///
  K = ZZ/1073741827
  R = K[x_(0,0), x_(0,1)]
  I = ideal(2*x_(0,0)+3*x_(0,1))
  assert(I == ideal msolveSaturate(I, x_(0,0)))
  assert(I == saturate(I, x_(0,0), Strategy => Msolve))

  -- c.f. https://github.com/algebraic-solving/msolve/issues/165
  K = ZZ/11
  R = K[x_(0,0), x_(0,1)]
  I = ideal(2*x_(0,0)+3*x_(0,1))
  assert(try (ideal msolveSaturate(I, x_(0,0));         false) else true)
  assert(try (saturate(I, x_(0,0), Strategy => Msolve); false) else true)
  assert(I == saturate(I, x_(0,0)))
///

TEST ///
  A = QQ[x,y,z]
  B = A[u,v,w]
  I = minors_2 matrix {{x,y,z}, {u,v,w}}
  assert(I == ideal msolveGB I)
  I = minors_2 matrix {{x,y^2,z^3}, {u^4,v^5,w^6}}
  assert(I == ideal msolveGB I)
///

TEST ///
  R = QQ[x,a,b,c,d]
  f = 7*x^2+a*x+b
  g = 2*x^2+c*x+d
  I = eliminate(x, ideal(f,g))
  J = msolveEliminate(x, ideal(f,g))
  assert(sub(I, ring J) == J)

  R = ZZ/11[x,a,b,c,d]
  f = 7*x^2+a*x+b
  g = 2*x^2+c*x+d
  I = eliminate(x, ideal(f,g))
  J = msolveEliminate(x, ideal(f,g))
  assert(sub(I, ring J) == ideal selectInSubring(1, gens J))
///
