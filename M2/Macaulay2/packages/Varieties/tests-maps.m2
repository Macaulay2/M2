TEST ///
  S = QQ[x_1..x_3];
  X = Proj S;
  phi1 = vars S
  G = sheaf target phi1
  F = sheaf source phi1
  shphi = map(G,F,phi1)
  peek shphi
  assert(source shphi === OO_X^3(-1))
  assert(target shphi === OO_X^1)
  assert(degree shphi ===1)
  phi = truncate(3,phi1);
  shphi2 = map(G,F,phi,3)
  assert(source shphi2 === OO_X^3(-1))
  assert(target shphi2 === OO_X^1)
  assert(degree shphi2 === 3)
  -- FIXME: the assertion for dual fails!
  shphi3 = map(sheaf target phi,F,phi,3)
  assert isWellDefined shphi3
  --assert isWellDefined dual shphi3
  assert(degree shphi3 === 3)
  assert(degree dual shphi3 === 0)
  -- FIXME: first assertion fails (unless degree 3 is specified)
  shphi4 = map(G, sheaf source phi, phi)
  --assert isWellDefined shphi4
  assert isWellDefined dual shphi4
  assert(degree shphi4 === 3)
  assert(degree dual shphi4 === 0)
///

TEST ///
  S = QQ[x_1..x_3];
  X = Proj S;
  phi = vars S;
  psi = (transpose phi)**S^{1:-2}
  shphi = sheafMap(phi)
  assert(matrix entries shphi.map == matrix {{x_1, x_2, x_3}})
  shpsi = sheafMap(psi)
  shphi*shpsi
  shphi3 = sheafMap(phi,3)
  shphi3*shpsi
///

TEST /// -- tests for cached saturation map M --> Gamma_* sheaf M
  S = QQ[x_1..x_4];
  X = Proj S
  -- zero sheaf
  N = sheaf coker vars S
  assert(N == 0)
  -- structure sheaf
  F = sheaf truncate(4,S^1)
  assert(F == OO_X^1)
  G = prune F
  h = G.cache.pruningMap
  assert(inverse h * h === id_F)
  --assert(h * inverse h === id_G) -- FIXME
  assert isIsomorphism h
  assert isIsomorphism inverse h
  assert(prune h === id_G)
  assert(prune inverse h === id_G)
  -- cotangent bundle
  Omega = sheaf ker vars S;
  pOmega = prune Omega
  pMap = pOmega.cache.pruningMap
  assert(prune inverse pMap === id_pOmega)
  assert(source pMap === Omega)
  assert(target pMap === pOmega)
  sMap = Omega.cache.SaturationMap
  assert(source sMap === module Omega)
  assert(target sMap === module pOmega)

  F = sheaf comodule intersect(ideal(x_1,x_2,x_3), ideal(x_3,x_4))
  pF = prune F
  sMap = F.cache.SaturationMap
  assert(source sMap === module F)
  assert(target sMap === module pF)
///

TEST ///
  S = QQ[x_1..x_4];
  X = Proj S;
  -- TODO: add assertions
  F = sheaf comodule intersect(ideal(x_1,x_2), ideal(x_3,x_4))
  pF = prune F
  F = sheaf S^{5} / intersect(ideal(x_1,x_2), ideal(x_3,x_4))
  pF = prune F
  F = sheaf S^{5} / intersect(ideal(x_1,x_2,x_3), ideal(x_3,x_4))
  pF = prune F
  sMap = F.cache.SaturationMap
  shsMap = sheafMap sMap
  lift shsMap
  F = sheaf comodule intersect(ideal(x_1,x_2), ideal(x_3,x_4))
  pF = prune F
  sMap = F.cache.SaturationMap
  pF.module.cache.pruningMap
  peek F.module.cache
  shsMap = sheafMap(sMap,4)
  lift shsMap
///

TEST ///
  S = QQ[x_0..x_2];
  X = Proj S
  --TODO: check if things have already been pruned
  f = sheafMap(truncate(2, vars S))
  assert(f == prune f)
  assert all(4, i -> HH^i(f) == Ext^i(OO_X^1, f))
  assert all(4, i -> HH^i(f(1)) == Ext^i(OO_X^1(-1), f))

  F = sheaf coker (vars S)_{0,2}
  assert(Ext^2(F, f) == matrix(QQ, {{0, 1, 0}}))
  assert(source Ext^2(F, f) == Ext^2(F, source f))
  assert(target Ext^2(F, f) == Ext^2(F, target f))

  f = sheafMap vars S ** OO_X(1)
  assert(HH^0 f == id_(QQ^3))
  f = sheafMap vars S ** OO_X(-2)
  assert(source HH^2 f == HH^2 source f)
  assert(target HH^2 f == HH^2 target f)
  f = sheafMap vars S ** OO_X(-3)
  assert(source HH^2 f == HH^2 source f)
  assert(target HH^2 f == HH^2 target f)

  kk = ZZ/101
  S = kk[x_0..x_3]
  X = Proj S
  K = dual ker vars S
  f = prune sheafMap dual wedgeProduct(1,1,K)
  assert(f == prune f)
  --needsPackage "BGG"
  --cohomologyTable(source f, -5,5)
  assert(source HH^2 f == HH^2 source f)
  assert(target HH^2 f == HH^2 target f)
  assert(HH^2 f == matrix(kk, {{2}}))
  assert(HH^3 f == 0)

  kk = ZZ/2
  S = kk[x_0..x_2]
  X = Proj S
  K = ker vars S
  f = sheafMap wedgeProduct(1,1,K)
  assert(f == prune f)
  assert(HH^2 f == 1)
  assert(HH^3 f == 0)

  S = QQ[x_0..x_3]
  R = S/ideal(x_0*x_1 - x_2*x_3)
  X = Proj R
  f = sheafMap vars R ** OO_X(2)
  HH^0 f
///

TEST ///
  kk = ZZ/17
  S = kk[x_0..x_3]
  j = inducedMap (ambient ker vars S, ker vars S)
  I = ideal random(3, S)
  R = quotient I; X = Proj R;
  F = sheaf coker (vars R)_{1,3} -- skyscraper sheaf
  phi = jacobian R;
  psi = sheafMap( phi // (j**R))
  OmegaX = coker psi;
  OmegaPX = target psi;
  a = inducedMap(OmegaX, OmegaPX)
  b = dual a
  HH^1(b)
  rank HH^2(psi)
  apply(3, i -> Ext^i(F, psi))
  Ext^2(F, psi)
  HH^2(psi)
///

/// -- TODO: re-enable
  S = kk[x,y,z]
  R = S/ideal(x)
  f = vars R
  f' = flattenMorphism f
  target f' == flattenModule target f
  source f' == flattenModule source f
///

TEST ///
  -- testing homology(SheafMap, SheafMap)
  S = QQ[x,y,z]
  g = sheafMap(koszul_2 vars S, 4)
  g == 0 -- FIXME: this doesn't work with something I did
  f = sheafMap koszul_3 vars S
  assert(0 == prune homology(g, f))

  g = sheafMap koszul_1 vars S
  f = sheafMap koszul_3 vars S * g(-3)
  assert(0 != prune homology(g(-1), f))

  S = QQ[x,y]
  g = sheafMap koszul_1 vars S
  f = sheafMap koszul_2 vars S * g(-2)
  assert(0 == prune homology(g, f))
///

TEST ///
  -- testing SheafMap == ZZ
  S = QQ[x,y]
  f = map(coker matrix{{x^2,y^2}}, , {{x}});
  assert(f != 0)
  g = sheafMap f;
  assert(g == 0)
///

TEST ///
  -- testing homomorphism and homomorphism'
  S = QQ[x,y,z]
  X = Proj S
  --
  F = sheaf module truncate(3, S)
  G = F(1)
  H = Hom(F, G)
  v = random(H, QQ^1)
  h = homomorphism v
  assert(source h === F)
  assert(target h === G)
  assert(homomorphism' h === v)
  assert(sub(last coefficients matrix prune h, QQ) === v)
  --
  F = OO_X^1
  G = sheaf truncate(0, S^{1})
  H = Hom(F, G)
  v = random(H, QQ^1)
  h = homomorphism v
  assert(source h === F)
  assert(target h === G)
  -- assert(homomorphism' h === v) -- FIXME: why is the matrix reversed?
///

TEST ///
  S = QQ[a..d];
  X = Proj S;
  I = ideal(a,b^3*c,b^4);
  F = sheaf module I;
  assert isWellDefined F
  H = Hom(F, OO_X^1/F);
  assert(QQ^15 === H)
  checkHoms = i -> (
      h := homomorphism H_{i};
      assert isWellDefined h;
      assert(source h === F);
      assert(target h === OO_X^1/F);
      -- FIXME: fails due to the basis/truncate bug below
      --assert(homomorphism' h === H_{i});
      -- TODO: remove this when the above works:
      assert(target homomorphism' h === H);
      assert(source homomorphism' h === QQ^1);
      )
  scan(15, checkHoms)
  --
  H = prune sheafHom(F, OO_X^1/F)
  f = H.cache.pruningMap
  g = f^-1
  -- FIXME:
  prune(g * f) === prune id_(target g)
  prune(f * g) === prune id_(target f)
///

TEST ///
  -- this may be the core of the bug mentioned above:
  S = QQ[x,y,z]
  A = S^{1}
  B = truncate(0, A, MinimalGenerators => false)
  M = image basis(0, A) -- image {-1} | x y z |
  N = image basis(0, B) -- image {-1} | z y x |
  assert(id_M === inducedMap(M, N))
///

TEST ///
  -- testing inverse
  S = QQ[x,y,z]
  X = Proj S
  f = map(OO_X^1, OO_X^1, truncate(2, id_(S^1)), 2)
  assert isWellDefined inverse f
  assert isIsomorphism(f * inverse f)
  assert isIsomorphism(inverse f * f)
  assert(prune(inverse f * f) === id_(OO_X^1)) 
  assert(prune(f * inverse f) === id_(OO_X^1))
  --
  f = map(sheaf truncate(2, S^1), OO_X^1, truncate(2, id_(S^1)), 2)
  assert isWellDefined f -- FIXME
  assert isWellDefined inverse f -- FIXME
  assert isIsomorphism(f * inverse f)
  assert isIsomorphism(inverse f * f)
  -- assert(prune(inverse f * f) === id_(OO_X^1)) -- FIXME
  assert(prune(f * inverse f) === id_(OO_X^1))
  --
  f = map(OO_X^1, sheaf truncate(2, S^1), truncate(2, id_(S^1)), 2)
  assert isWellDefined inverse f
  assert isIsomorphism(f * inverse f)
  assert isIsomorphism(inverse f * f)
  assert(prune(inverse f * f) === id_(OO_X^1))
  assert(prune(f * inverse f) === id_(OO_X^1))
  --
  f = map(sheaf truncate(2, S^1), sheaf truncate(2, S^1), truncate(2, id_(S^1)), 2)
  assert isWellDefined inverse f
  assert isIsomorphism(f * inverse f)
  assert isIsomorphism(inverse f * f)
  assert(prune(inverse f * f) === id_(OO_X^1))
  assert(prune(f * inverse f) === id_(OO_X^1))
///

TEST ///
  -- testing yonedaSheafExtension
  S = QQ[x,y]
  X = Proj S
  d = 1
  F = OO_X(2)
  G = OO_X(0)
  E = Ext^d(F, G)
  f = E_{0}
  (p, i) = toSequence yonedaSheafExtension f
  assert(source i === G)
  assert(target i === source p)
  assert(target p == F) -- FIXME
  assert(prune p === map(OO_X^1(2),OO_X^2(1), map(S^{2}, , {{x, -y}})))
  assert(prune i === map(OO_X^2(1),OO_X^1, map(S^{2:1}, , {{y}, {-x}})))
  assert(coker i == F)
  assert(image i == ker p)
  assert(ker p == G)
  assert(0 == p * i)
  assert(0 == homology(p, i))
  -- FIXME: somehow the generators are changed
  -- assert(0 == homology(prune \ (p, i)))
  assert(0 == ker i)
  assert(0 == coker p)
  --
  S = QQ[x,y,z]
  X = Proj S
  d = 1
  F = tangentSheaf X
  G = OO_X^1
  E = Ext^d(F, G)
  f = E_{0}
  -- 0 <-- T_X <-- O_X(1)^3 <-- O_X <-- 0
  (p, i) = toSequence yonedaSheafExtension f
  assert(source i === G)
  assert(target i === source p)
  assert(source p == OO_X^{3:1})
  assert(target p === F)
  assert(0 == p * i)
  assert(0 == homology(p, i))
  assert(0 == homology(prune \ (p, i)))
  assert(0 == ker i)
  assert(0 == coker p)
///

TEST ///
  S = QQ[x,y,z]
  M = S^1 ++ S^1
  assert(prune pullback(M_[0], M_[1]) == 0)
  assert(prune pushout(M^[0], M^[1]) == 0)

  X = Proj S
  F = sheaf(X, M)
  assert(prune pullback(F_[0], F_[1]) == 0)
  assert(prune pushout(F^[0], F^[1]) == 0)
///

TEST ///
  S = QQ[x_0..x_13]
  X = Proj S
  M = truncate(1, S^1)
  F = sheaf M
  elapsedTime assert first isIsomorphic(M, module F); -- 3.6s
  elapsedTime assert first isIsomorphic(OO_X^1, F)    -- 8.4s -> 3.1s
///


TEST ///
  -- tests for consistent truncations
  S = QQ[x,y,z]
  X = Proj S
  M = image vars S
  F = OO_X^1
  G = sheaf M
  f = id_M
  assert(sheafMap f === id_G)
  assert isWellDefined sheafMap(f, 1)
  assert isWellDefined sheafMap(f, 2)
  assert(prune sheafMap(f,  4) === id_F)
  assert(prune sheafMap(f, -4) === id_F)
  assert(prune sheafMap(f,  6) === id_F)

  m = inducedMap(S^1, image vars S)
  f = sheafMap m
  g = inverse f
  assert(f * g  == id_F)
  assert(g * f === id_G)

  f = sheafMap(m, 4)
  g = inverse f
  assert(prune g      === id_F)
  assert(prune(f * g) === id_F)
  assert(prune(g * f) === id_F)
///


TEST ///
  S = (ZZ/11)[x,y,z,t]
  P = Proj S
  J = ker map((ZZ/11)[u,v],S,{u^4,u^3*v,u*v^3,v^4})
  p = sheafMap inducedMap(S^1/J,S^1)
  assert(rank connectingExtMap (0,OO_P^1,p**OO_P(1)) == 1)

///

TEST ///
  R = ZZ/3[x,y,z]/x
  X = Proj R
  connectingExtMap(0, OO_X^1, sheafMap sub(vars prune R,R),LengthLimit => 5)
///

