TEST ///
  S = QQ[x_1..x_3];
  X = Proj S;
  phi1 = vars S
  G = sheaf target phi1
  F = sheaf source phi1
  --
  shphi = map(G,F,phi1)
  assert(source shphi === OO_X^3(-1))
  assert(target shphi === OO_X^1)
  assert(shphi.degree === 1)
  --
  phi = truncate(3,phi1);
  shphi2 = map(G,F,phi,3)
  assert(source shphi2 === OO_X^3(-1))
  assert(target shphi2 === OO_X^1)
  assert(shphi2.degree === 3)
  -- tests for dual
  shphi3 = map(sheaf target phi,F,phi,3)
  assert isWellDefined shphi3
  assert isWellDefined dual shphi3
  assert(shphi3.degree === 3)
  assert((dual shphi3).degree === 0)
  --
  shphi4 = map(G, sheaf source phi, phi)
  assert isWellDefined shphi4
  assert isWellDefined dual shphi4
  assert(shphi4.degree === 3)
  assert((dual shphi4).degree === 0)
  --
  isWellDefined map(OO_X^1, OO_X^0, 0)
///

TEST ///
  S = QQ[x_1..x_3];
  X = Proj S;
  phi = vars S;
  psi = (transpose phi)**S^{1:-2}
  shphi = sheaf phi
  assert(matrix entries shphi.map == matrix {{x_1, x_2, x_3}})
  shpsi = sheaf psi
  shphi*shpsi
  shphi3 = sheaf(phi, 3)
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
  assert isIsomorphic(F, G)
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
  shsMap = sheaf sMap
  lift shsMap
  F = sheaf comodule intersect(ideal(x_1,x_2), ideal(x_3,x_4))
  pF = prune F
  sMap = F.cache.SaturationMap
  pF.module.cache.pruningMap
  peek F.module.cache
  shsMap = sheaf(sMap,4)
  lift shsMap
///

TEST ///
  S = QQ[x_0..x_2];
  X = Proj S
  --TODO: check if things have already been pruned
  f = sheaf truncate(2, vars S)
  assert(f == prune f)
  assert all(4, i -> HH^i(f) == Ext^i(OO_X^1, f))
  assert all(4, i -> HH^i(f(1)) == Ext^i(OO_X^1(-1), f))

  F = sheaf coker (vars S)_{0,2}
  assert(Ext^2(F, f) == matrix(QQ, {{0, 1, 0}}))
  assert(source Ext^2(F, f) == Ext^2(F, source f))
  assert(target Ext^2(F, f) == Ext^2(F, target f))

  f = sheaf vars S ** OO_X(1)
  assert(HH^0 f == id_(QQ^3))
  f = sheaf vars S ** OO_X(-2)
  assert(source HH^2 f == HH^2 source f)
  assert(target HH^2 f == HH^2 target f)
  f = sheaf vars S ** OO_X(-3)
  assert(source HH^2 f == HH^2 source f)
  assert(target HH^2 f == HH^2 target f)

  kk = ZZ/101
  S = kk[x_0..x_3]
  X = Proj S
  K = dual ker vars S
  f = prune sheaf dual wedgeProduct(1,1,K)
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
  f = sheaf wedgeProduct(1,1,K)
  assert(f == prune f)
  assert(HH^2 f == 1)
  assert(HH^3 f == 0)

  S = QQ[x_0..x_3]
  R = S/ideal(x_0*x_1 - x_2*x_3)
  X = Proj R
  f = sheaf vars R ** OO_X(2)
  HH^0 f
///

TEST ///
  -- tests for concatenation
  debug Varieties
  f = sheaf vars(QQ[x,y,z])
  assert(SheafMap.concatRows(3:f) == f || f || f)
  assert(SheafMap.concatCols(3:f) == f |  f |  f)
  isWellDefined SheafMap.concatBlocks(3:5:f)
  isWellDefined matrix {{f,f},{f,f}}
///

TEST ///
  -- tests for tensor
  S = ZZ/11[x_0,x_1]
  X = Proj S
  -- f = sheaf truncate(1, vars S)
  m = random(S^3, S^{-1,-1})
  f = sheaf m
  f' = sheaf truncate(2, m)
  f'' = sheaf(m, 2)
  g = f ** f;
  assert isWellDefined g
  assert(f ** f == f' ** f')
  assert(f ** f == f'' ** f'')
///

TEST ///
  kk = ZZ/17
  S = kk[x_0..x_3]
  j = inducedMap (ambient ker vars S, ker vars S)
  I = ideal random(3, S)
  R = quotient I; X = Proj R;
  F = sheaf coker (vars R)_{1,3} -- skyscraper sheaf
  phi = jacobian R;
  psi = sheaf(phi // (j**R))
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
  X = Proj S
  g = sheaf(koszul_2 vars S, 4)
  g == 0 -- FIXME: this doesn't work with something I did
  f = sheaf koszul_3 vars S
  assert(0 == prune homology(g, f))

  g = sheaf koszul_1 vars S
  f = sheaf koszul_3 vars S * g(-3)
  assert(0 != prune homology(g(-1), f))

  S = QQ[x,y]
  X = Proj S
  g = sheaf koszul_1 vars S
  f = sheaf koszul_2 vars S * g(-2)
  assert(0 == prune homology(g, f))
///

TEST ///
  -- testing SheafMap == ZZ
  S = QQ[x,y]
  X = Proj S
  f = map(coker matrix{{x^2,y^2}}, , {{x}});
  assert(f != 0)
  g = sheaf f;
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
  assert isWellDefined f
  assert isWellDefined inverse f
  assert isIsomorphism(f * inverse f)
  assert isIsomorphism(inverse f * f)
  assert(prune(inverse f * f) === id_(OO_X^1))
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
  -- tests for isIsomorphic
  S = QQ[x_0..x_13]
  X = Proj S
  M = truncate(1, S^1)
  F = sheaf M
  --elapsedTime assert isIsomorphic(M, module F); -- 3.6s --NOW DOES NOT FINISH
  --elapsedTime assert isIsomorphic(OO_X^1, F)    -- 8.4s -> 3.1s --NOW DOES NOT FINISH
  elapsedTime assert isIsomorphic(OO_X^1, prune F)
///


TEST ///
  -- tests for consistent truncations
  S = QQ[x,y,z]
  X = Proj S
  M = image vars S
  F = OO_X^1
  G = sheaf M
  f = id_M
  assert(sheaf f === id_G)
  assert isWellDefined sheaf(f, 1)
  assert isWellDefined sheaf(f, 2)
  assert(prune sheaf(f,  4) === id_F)
  assert(prune sheaf(f, -4) === id_F)
  assert(prune sheaf(f,  6) === id_F)

  m = inducedMap(S^1, image vars S)
  f = sheaf m
  g = inverse f
  assert(f * g  == id_F)
  assert(g * f === id_G)

  f = sheaf(m, 4)
  g = inverse f
  assert(prune g      === id_F)
  assert(prune(f * g) === id_F)
  assert(prune(g * f) === id_F)
///

TEST ///
  -- tests for inducedMap
  K = ZZ/11
  S = K[x,y,z,t]
  P = Proj S
  F = sheaf cokernel map(S^{1},S^{-1, 3:-2}, {{y*z-x*t, z^3-y*t^2, x*z^2-y^2*t, y^3-x^2*z}})
  f = map(F, OO_P^1(1), map(module F, , {{1}}))
  assert isWellDefined f
  g = inducedMap(source f, ker f)
  assert isWellDefined g
  assert(g === sheaf inducedMap(module source f, module ker f))
///

///
  -- tests for connectingExtMap
  K = ZZ/11
  S = K[x,y,z,t]
  P = Proj S
  J = ker map(K[u,v], S, {u^4, u^3*v, u*v^3, v^4})
  p = sheaf inducedMap(S^1/J, S^1)
  assert(1 == rank connectingExtMap(0, OO_P^1, p ** OO_P(1)))

  R = K[x,y,z]/x
  X = Proj R
  f = sheaf sub(vars prune R, R)
  -- TODO: this LengthLimit should really be unnecessary
  assert(1 == rank connectingExtMap(0, OO_X^1, f, LengthLimit => 5))
///

TEST ///
  -- tests for factoring
  X = Proj QQ[x,y,z]
  S = ring X
  K = koszul vars S
  -- given f: A-->C and g: A-->B, then find (f//g): B-->C such that (f//g) o g = f
  f = sheaf K.dd_2
  g = sheaf inducedMap(coker K.dd_3, K_2)
  h = g \\ f
  -- TODO: tests this more thoroughly, since in general == may be best we can hope for
  assert(h * g === f)
  -- given f: A-->C and g: B-->C, then find (f//g): A-->B such that g o (f//g) = f
  g = sheaf inducedMap(K_1, ker K.dd_1)
  h = f // g
  assert(g * h === f)
///

///
  -- tests for cotangentSurjection
  S = QQ[x,y,z,w]
  P = Proj S
  I = ideal(x*w-y*z)
  R = S/I
  X = Proj R
  f = cotangentSurjection X
  assert isWellDefined f
  assert(prune ker f == OO_X(-2))

  use S
  J = minors(2,matrix{{x,y,z},{y,z,w}})
  R = S/J
  X = Proj R
  f = cotangentSurjection X
  assert isWellDefined f
  assert isIsomorphic(prune ker f, sheaf(J/J^2**R))
///


///
  -- tests for embeddedToAbstract
  S = (ZZ/17)[x,y,z]
  I = ideal(x^5+y^5+z^5)
  X = Proj(S/I)
  assert not isSurjective embeddedToAbstract(X)
///
///
  -- Slow test! Use this as benchmark to speed things up or only run it occasionally
  S = (ZZ/17)[x,y,z,w]
  I = ideal(x^4+y^4+z^4+w^4)
  X = Proj(S/I)
  assert(rank embeddedToAbstract(X) == 19)
///

TEST ///
   S = (ZZ/3)[x,y,z]
   I = ideal(x^3*y+y^3*z+z^3*x)
   R = S/I
   X = Proj R
   L = sheaf module ideal(x,y)
   L2 = L^**2
   assert(Hom(OO_X, prune L2) ==  Hom(OO_X, L2))
///

TEST ///
  S = (ZZ/2)[x,y,z,w]
  I = ideal(x^3+x^2*y+y^3+x*y*z+z^3+w^3)
  R = S/I
  X = Proj R
  B = sheaf coker map(R^{21:{-1}, {-2}}, ,
      matrix {{y+z, x+z, 0, y, x+y, x, x, w^2, w^2, 0, x*y+y*z, x*z+z^2, x^2+y*z+z^2, w^2, 0, 0, w^2, w^2, 0, x*w, y*w+z*w, y*w},
	  {z, y+z, w, x+y, x, x+y+z, 0, w^2, 0, w^2, x*y+x*z+y*z, x^2+x*y+y^2+x*z+z^2, x*y+x*z+z^2, 0, 0, w^2, w^2, w^2, 0, x*w+y*w, x*w+z*w, z*w},
	  {y+z, y, w, y, y, y, x+y, w^2, w^2, 0, y^2, y*z, y^2+z^2, w^2, 0, w^2, w^2, w^2, w^2, y*w, y*w+z*w, 0},
	  {w, 0, 0, 0, w, 0, w, x*y+y^2+x*z+z^2, x*y+y*z+z^2, x^2+x*z+y*z, y*w, x*w, 0, x*y+y^2+x*z, x*y+y^2+x*z, x*y+y^2+x*z, x*y+y*z+z^2, x*y+y^2, z^2, w^2, w^2, 0},
	  {0, 0, 0, w, 0, 0, w, x*y+y*z+z^2, x*y+y*z+z^2, x*y+y^2+x*z, x*w+y*w, y*w, z*w, x^2+x*y+y^2+y*z, x*y+y^2+x*z, x*y+y*z, y^2+x*z+y*z, x*y+x*z+y*z+z^2, x*y+y^2, 0, w^2, 0},
	  {y+z, y, 0, z, x, y+z, z, 0, w^2, w^2, y^2, y*z, 0, 0, 0, 0, w^2, 0, w^2, y*w, 0, z*w},
	  {0, w, 0, w, 0, w, 0, 0, x*y+y^2+x*z+z^2, 0, x*w+y*w, x*w, x*w+y*w, x*y+y^2+x*z, x^2+x*z+y*z, x*z, x*y+y*z+z^2, z^2, y^2+x*z+y*z+z^2, 0, 0, w^2},
	  {x+z, x, 0, x+z, y, y+z, y, w^2, 0, 0, x^2+x*y+x*z+y*z, x*y+y*z+z^2, y^2+x*z+y*z+z^2, w^2, 0, 0, 0, 0, w^2, y*w, x*w+y*w+z*w, y*w+z*w},
	  {z, y, w, x+y, z, y+z, y+z, w^2, 0, w^2, y^2, y*z, y*z, w^2, 0, w^2, w^2, 0, w^2, z*w, z*w, z*w},
	  {w, w, 0, 0, 0, 0, 0, x^2+x*y+y^2+y*z, 0, x*y+z^2, 0, y*w, x*w+y*w, x*y+y^2+x*z, 0, y^2+x*z+y*z+z^2, x*y+y*z+z^2, x*y+z^2, x*z, w^2, 0, 0},
	  {0, 0, 0, 0, 0, w, 0, 0, x*y+y*z+z^2, 0, x*w+y*w, x*w, z*w, y^2+x*z+z^2, x*y+y^2+x*z, x*y+x*z+y*z+z^2, x^2+y^2+z^2, x*y+y^2+x*z+y*z, x*y+z^2, w^2, 0, 0},
	  {x+y, z, w, y, z, y, z, 0, 0, w^2, y*z, z^2, y^2, w^2, 0, 0, w^2, w^2, 0, 0, 0, z*w},
	  {w, 0, 0, 0, 0, w, 0, x*y+y*z+z^2, x^2+y^2+z^2, x*y+y^2+x*z, x*w+y*w+z*w, y*w+z*w, x*w+y*w, x*y+y^2+x*z, y^2+x*z+z^2, y*z, x*y+y*z+z^2, y^2+y*z+z^2, x*y+y*z+z^2, w^2, w^2, 0},
	  {y+z, x, 0, z, y+z, y, y, 0, w^2, w^2, z^2, y^2+z^2, y^2+y*z, 0, 0, w^2, 0, 0, 0, 0, z*w, z*w},
	  {y+z, z, 0, y, 0, x+y, y+z, w^2, w^2, 0, y*z, z^2, y*z+z^2, w^2, 0, w^2, 0, w^2, 0, z*w, z*w, y*w},
	  {0, 0, x, 0, 0, 0, 0, y*w+z*w, x*w+y*w+z*w, x*w+y*w+z*w, 0, w^2, w^2, x*w+z*w, 0, z*w, y*w, x*w, 0, z^2, x^2+y^2+z^2, x*y+x*z},
	  {0, 0, x+z, 0, 0, 0, 0, x*w+y*w, x*w, z*w, 0, 0, w^2, x*w+y*w, 0, x*w+y*w, x*w+y*w+z*w, x*w, y*w+z*w, x^2+x*y+y^2+z^2, x*y+x*z+y*z, y^2+y*z+z^2},
	  {0, w, 0, 0, w, w, w, 0, x*y+y*z+z^2, 0, x*w, x*w+y*w+z*w, x*w+y*w, x*y+y^2+x*z, x*y+y^2+x*z, x^2+y^2+x*z+z^2, x*y+y*z+z^2, y^2+x*z+y*z+z^2, x*y+y^2+x*z, 0, 0, 0},
	  {0, 0, x+y, 0, 0, 0, 0, y*w, y*w+z*w, y*w+z*w, w^2, w^2, w^2, y*w, z*w, y*w, y*w, y*w, y*w, y^2+y*z+z^2, y*z, y^2+y*z+z^2},
	  {0, 0, 0, w, w, w, 0, x*y+y*z+z^2, x*y+y*z+z^2, x*y+y^2+x*z, x*w+y*w+z*w, x*w+y*w+z*w, x*w+z*w, x*y+y^2+x*z, x*y+y^2+x*z, y^2+x*z+y*z+z^2, x*y+y*z+z^2, x^2+x*y+z^2, y^2+x*z+y*z+z^2, 0, 0, 0},
	  {0, 0, 0, 0, 0, 0, w, 0, 0, 0, y*w, y*w, x*w, x*y+y^2+x*z, 0, x*y+y*z+z^2, x*y+y*z+z^2, x*y+y^2+x*z, x^2+x*y+y^2+x*z+y*z, w^2, 0, 0},
	  {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, w, w, w, w, w, y, y+z, x}})
  assert(rank Hom(B, OO_X) == 15)
///
