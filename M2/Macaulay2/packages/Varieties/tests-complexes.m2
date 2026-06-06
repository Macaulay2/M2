TEST ///
  -- tests for Euler sequence
  S = QQ[x,y,z]
  R = S/(x^2-y*z)
  X = Proj S
  Y = Proj R
  C = eulerSequence Proj R
  assert all(3, i -> instance(C_i, CoherentSheaf))
  assert(C.dd_1 * C.dd_2 == 0)
  assert(homology(C.dd_1, C.dd_2) == 0)
  M = sheaf(module cotangentSheaf X ** R)
  assert isIsomorphic(M, C_2)

  -- testing tensor of complexes of sheaves
  D = complex OO_Y^{-1}
  E = D ** C
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
  C = yonedaSheafExtension f
  (p, i) = (C.dd_1, C.dd_2)
  assert(source i === G)
  assert(target i === source p)
  assert(target p == F)
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
///

TEST ///
  -- testing yonedaSheafExtension
  S = QQ[x,y,z]
  X = Proj S
  d = 1
  F = tangentSheaf X
  G = OO_X^1
  E = Ext^d(F, G)
  f = E_{0}
  -- 0 <-- T_X <-- O_X(1)^3 <-- O_X <-- 0
  C = yonedaSheafExtension f
  (p, i) = (C.dd_1, C.dd_2)
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
  -- testing yonedaSheafExtension
  S = (ZZ/17)[x,y,z]
  I = ideal(x^3+y^3+z^3)
  X = Proj(S/I)
  E = Ext^1(OO_X, OO_X)
  C = yonedaSheafExtension matrix E_{0}
  -- TODO: once C is a complex, simply check homology
  assert(0 == prune coker C.dd_1)
  assert(0 == prune homology(C.dd_1, C.dd_2))
  assert(0 == prune ker C.dd_2)
  elapsedTime assert(target C.dd_1 == OO_X^1) -- ~0.15s
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
  Q = QQ[x_1..x_3];
  X = Proj Q;
  K = koszulComplex vars Q;
  sK = sheaf K
  assert isWellDefined sK
  assert(module sK == K)
  assert(length sK == 3)
  assert(sK_1 == OO_X^3(-1))
  assert(sK^2 == 0)
  --
  f = id_sK
  assert isWellDefined f
  f | f
  f_1
  f^10
  assert(f - f == 0)
  assert(f == 1)
  x_1 * f
  f * f
  f + f
  ComplexMap.directSum {f, f, f}
  G = sheaf freeResolution ideal(x_1^2+x_2^2)
  RHom(OO_X^1, sK)
  RHom(OO_X^1, G)
  S := coker G.dd_1
  HH^0 S
  RHom(G, sK)
  Ext^2(OO_X^1, sK(-3))
  Ext^0 (OO_X^1, G(3))
  RHom(OO_X^1, G(2))
  RHom(OO_X^1, G, 3)
  RHom(sK, G, 0)
  RHom(sK, OO_X^1,0)
  RHom(S, OO_X^1(3))
  --all of the following are running just fine with the current complexes code
  ssK = sK++sK
  id_sK
  idssK = id_sK ++ id_sK
  isCommutative idssK
  assert(idssK == id_ssK)
  components(sK ++ sK)
  components(id_sK ++ id_sK)
  ssK^[0]
  ker ssK^[0]
  ssK_[1]
  ssK_[1]
  idssK^[0]
  ssK[1]
  H = sheaf truncate(2, K)
  Hp = prune H
  g = Hp.cache.pruningMap
  assert(g * g^(-1) == id_(target g) and g^(-1) * g == id_(source g))
  naiveTruncation(g, (1,2), (2,3))
  -- CanonicalTruncation does not work
  -- canonicalMap does not work
  -- cone does not work if not all modules are free
  -- cylinder doesn't work, because it is constructing maps between modules instead of sheaves
  phi = idssK^[0]
  indpsi = inducedMap(source phi, ker phi)
  assert isWellDefined indpsi
  nu = idssK_[0]
  indnu = inducedMap(coker nu, target nu)
  -- above induced maps produce errors upon pruning
///

end--
restart
loadPackage("Truncations", FileName => currentDirectory() | "Truncations.m2", Reload => true)
loadPackage("Complexes",   FileName => currentDirectory() | "Complexes.m2",   Reload => true)
debug loadPackage("Varieties",   FileName => currentDirectory() | "Varieties.m2",   Reload => true)

check Varieties
