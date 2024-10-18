TEST ///
  -- c.f. https://github.com/algebraic-solving/msolve/issues/165
  K = ZZ/65537 -- > 2^16
  R = K[x_(0,0), x_(0,1)]
  I = ideal(2*x_(0,0)+3*x_(0,1))
  assert(I == ideal msolveSaturate(I, x_(0,0)))
  assert(I == saturate(I, x_(0,0), Strategy => Msolve))

  K = ZZ/1073741827 -- > 2^30
  R = K[x_(0,0), x_(0,1)]
  I = ideal(2*x_(0,0)+3*x_(0,1))
  assert(I == ideal msolveSaturate(I, x_(0,0)))
  assert(I == saturate(I, x_(0,0), Strategy => Msolve))

  -- F4SAT doesn't work with small primes
  K = ZZ/32771 -- < 2^16
  R = K[x_(0,0), x_(0,1)]
  I = ideal(2*x_(0,0)+3*x_(0,1))
  assert(try (ideal msolveSaturate(I, x_(0,0));         false) else true)
  assert(try (saturate(I, x_(0,0), Strategy => Msolve); false) else true)
  assert(I == saturate(I, x_(0,0)))
///

TEST ///
  A = ZZ/1073741827[x,y,z]
  B = A[u,v,w]
  I = minors_2 matrix {{x,y,z}, {u,v,w}}
  -- TODO: loses homogeneity
  assert(I == ideal msolveGB I)
  assert(I == ideal msolveSaturate(I, B_0))
  assert(I == ideal msolveSaturate(I, B_3))
  -- TODO: eliminate doesn't work over tower rings
  msolveEliminate(I, B_0) -- == eliminate(I, B_0)
  -- TODO: msolveEliminate can't eliminate variables in the base ring:
  -- msolveEliminate(I, B_3)
  -- TODO: msolveLeadMonomials doesn't work for tower rings yet
  -- msolveLeadMonomials I
  --
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

TEST ///
  R = ZZ/1073741827[z_1..z_3]
  I = ideal(7*z_1*z_2+5*z_2*z_3+z_3^2+z_1+5*z_3+10,8*z_1^2+13*z_1*z_3+10*z_3^2+z_2+z_1)
  assert(leadTerm msolveGB I == matrix{{z_1*z_2, z_1^2, z_1*z_3^2, z_2^2*z_3^2}})

  R = QQ[z_1..z_3]
  I = ideal(7*z_1*z_2+5*z_2*z_3+z_3^2+z_1+5*z_3+10,8*z_1^2+13*z_1*z_3+10*z_3^2+z_2+z_1)
  assert(ideal msolveGB I == ideal groebnerBasis I)
  assert(leadTerm msolveGB I == matrix{{7*z_1*z_2, 8*z_1^2, 56*z_1*z_3^2, 235*z_2^2*z_3^2}})
///
	      
TEST ///
  R = QQ[x,y];
  I = ideal ((x-3)*(x^2+1),y-1);
  assert(msolveRealSolutions I == {{3.0, 1.0}})
  assert(msolveRealSolutions I === msolveRealSolutions(I, QQi))
  scan({QQ, QQi, RR, RR_53, RRi, RR_53},
      F -> assert({{3.0, 1.0}} == msolveRealSolutions(I, F)))
///

TEST ///
  S = ZZ/1073741827[t12,t13,t14,t23,t24,t34];
  I = ideal(
      (t13*t12-t23)*(1-t14)+(t14*t12-t24)*(1-t13) - (t12+1)*(1-t13)*(1-t14),
      (t23*t12-t13)*(1-t24)+(t24*t12-t14)*(1-t23) - (t12+1)*(1-t23)*(1-t24),
      (t14*t34-t13)*(1-t24)+(t24*t34-t23)*(1-t14) - (t34+1)*(1-t14)*(1-t24));
  sat = (1-t24)
  elapsedTime J1 = saturate(ideal I_*, sat, Strategy => Eliminate);
  elapsedTime J2 = ideal msolveSaturate(ideal I_*, sat);
  assert(J1 == J2)
///

TEST ///
  R = QQ[x,a,b,c,d];
  f = x^2+a*x+b;
  g = x^2+c*x+d;
  I = ideal(f, g)
  eM2 = eliminate(x, I);
  eMsolve = msolveEliminate(x, I);
  assert(eM2 == sub(eMsolve, ring eM2))
///

end

restart
needsPackage "Msolve"
check Msolve
