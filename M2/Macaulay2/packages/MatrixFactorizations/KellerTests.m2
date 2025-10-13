TEST ///
--direct sum testing
Q = QQ[x_1..x_3];
F = ZZdfactorization {x_1,x_2,x_3}
assert((id_F++id_F)==1)
assert((isdFactorization F)_0)
G = ZZdfactorization {Q^1,Q^2,Q^3}
assert((isdFactorization G)_0)
D = G++F
assert(isWellDefined D)
assert(not (isdFactorization D)_0)
assert(id_F++id_G == 1)
assert(id_G++id_F == 1)
assert(isWellDefined D^[1])
assert(isCommutative D^[1])
assert(kernel D_[1] == 0)
assert(prune coker D_[1] == G)
///


TEST ///
--playing around with 0 factorizations
Q = ZZ/101[x_1..x_3]
F = ZZdfactorization {Q^0,Q^0}
assert(F==0)
assert(0==F)
assert(id_F == 1)
assert(id_F == 0)
assert(ring F === Q)
assert(period F == 2)
assert(id_F == x_1)
G = ZZdfactorization toList(5:Q^0)
assert(G == 0)
assert(not(F==G))
///

TEST ///
--playing around with folding and unfolding
Q = QQ[x_1..x_3]
K = koszulComplex vars Q
KF = Fold(K,2)
assert(isWellDefined KF)
assert(isZZdComplex KF)
assert(isWellDefined unfold(KF,4))
f = x_1^2+x_2^2+x_3^2; 
Kt = koszulMF({x_1,x_2,x_3},f)
assert(isWellDefined Kt)
assert(isZZdComplex (Kt**Q/(f)))
assert(isWellDefined unfold(Kt**Q/(f),4))
assert(not isWellDefined unfold(Kt,4))
assert(source K.dd == K)
assert(target K.dd == K)
assert(degree K.dd == -1)
E = End(Kt)
assert(isWellDefined E)
assert(isZZdComplex E)
H = prune HH E
assert(H_0 == H_1)

KF = Fold(K,4)
assert(K == unfold(KF,3))

idFold = Fold(id_K,2)
assert(isWellDefined idFold)
assert(coker idFold == 0)
assert(ker idFold == 0)
assert(image idFold == Fold(K,2))

///

TEST ///
--playing around with applying ring maps/tensoring with modules/rings
Q=ZZ/101[x_1..x_3]
S = ZZ/101[s,t]
phi = map(S,Q,{s^2,s*t,t^2})
f = x_1^2+x_2^2+x_3^2; 
Kt = koszulMF({x_1,x_2,x_3},f)
phiKt = phi(Kt)
assert(isWellDefined Kt)
assert(phiKt**phiKt == phi(Kt**Kt))
assert(End(phiKt)==phi(End(Kt)))
assert(phi**Kt==Kt**phi)
F = ZZdfactorization {Q^0,Q^0}
assert(isWellDefined phi(F))
assert(phi(F)==0)
assert(ring phi(F) === S)

Q=ZZ/101[x_1,x_2]
f = x_1^2-x_2^2
phi = map(S,Q,{s*t,s*t})
K = koszulMF({x_1,x_2},f)
phiK = phi(K)
assert(not isZZdComplex K)
assert(isZZdComplex phi(K))
H = prune HH phi(K)
assert(H_0 == H_1)
///


TEST ///
--playing around with pruning maps/cached data of pruning maps (pruning 0 maps)
S = ZZ/101[x,y,z,w];
F = koszulMF({x,y,z,w}, y*w - x*z);
E = Hom(F,F)
assert(potential E == 0)
assert(E.dd^2 == 0)
Ed = dual E
C = HH E
D = prune C
assert(isWellDefined D)
assert((isdFactorization D)_0)
assert(D.cache.?pruningMap)
g = D.cache.pruningMap
assert isWellDefined g
assert isFactorizationMorphism g
assert isWellDefined prune g
assert(prune g == id_D)
assert (target g == C)
assert (source g == D)
g^-1
assert(isWellDefined g^(-1))
assert(g*g^-1 == 1 and g^-1*g == 1)
///

TEST ///
--playing around with adjoining roots of unity
Q = QQ[x_1..x_3];
d = 3;
Qt = adjoinRoot(d, Q, t)
assert(Qt.?rootOfUnity) --stores the root of unity in the ring
{t, t^2, t^3}
Dt = ZZdfactorization {x_1*1_Qt,x_2*1_Qt,x_3*1_Qt}
assert(Dt.cache.?rootOfUnity)
S = QQ[x,y,z];
C = ZZdfactorization {x,y,z}
assert(not C.cache.?rootOfUnity)
Ct = adjoinRoot(C, t)
assert(Ct.cache.?rootOfUnity)
assert(Ct.cache.rootOfUnity^3 == 1)
St = adjoinRoot(3, S, t);
Ct' = C**St
assert(Ct'.cache.?rootOfUnity)
R = ZZ/3[x,y,z]
R.rootOfUnity = 1_R
Cx = ZZdfactorization {x,x,x}
Cy = ZZdfactorization {y,y,y}
Cz = ZZdfactorization {z,z,z}
Cxyz = Cx**Cy**Cz
assert(isWellDefined Cxyz)
assert(potential Cxyz == x^3+y^3+z^3)
Cxyz.dd
assert(Cxyz.dd^3 == (x^3+y^3+z^3)*id_(Cxyz))
///

TEST ///
--playing around with shifting
Q=ZZ/101[x_1..x_3]
f = x_1^2+x_2^2+x_3^2; 
Kt = koszulMF({x_1,x_2,x_3},f)
assert(potential Kt == f)
assert(isWellDefined (Kt[1]))
assert(potential(Kt[1]) == f)
assert(Kt[2]==Kt)
assert(isWellDefined dual Kt)
assert(potential dual Kt == -f)
assert(isWellDefined (Kt++Kt[1]))

///

--restart
--debug needsPackage "MatrixFactorizations"

TEST ///
-- cone f.  Trivial and strange cases
  f1 = map(ZZdfactorization {ZZ^1,ZZ^1}, ZZdfactorization {ZZ^0,ZZ^0}, 0)
  f2 = map(ZZdfactorization {ZZ^0,ZZ^0}, ZZdfactorization {ZZ^1,ZZ^1}, 0)
  cone f1
  assert(isWellDefined cone f1)
  assert(isZZdComplex cone f1)
  assert(HH cone f1 == cone f1)
  cone f2
  prune cone f1
  prune cone f2
///


TEST ///
--tests for composing maps (including the 0 maps!!)
  f1 = map(ZZdfactorization {ZZ^1,ZZ^1}, ZZdfactorization {ZZ^0,ZZ^0}, 0)
  f2 = map(ZZdfactorization {ZZ^0,ZZ^0}, ZZdfactorization {ZZ^1,ZZ^1}, 0)
  assert(f1*f2 == 0)
  assert(f2*f1 == 0)
  
  
///

TEST ///
--playing around with taking kernels/cokernels/images/etc
Q=ZZ/101[x_1..x_3]
F = ZZdfactorization {x_1,x_2}
assert(potential cone(x_1*id_F) == x_1*x_2)
assert(prune kernel (x_1*id_F) == 0)
f = x_1^2
Ker = prune(kernel (x_1*id_F**(Q/(f))))
assert(isWellDefined Ker)
assert(isZZdComplex Ker)

Shift = suspension(F)
pMap = Shift.cache.pruningMap
assert(isCommutative pMap)
assert(kernel pMap == 0)
assert(cokernel pMap == 0)
assert(isWellDefined pMap^(-1))
///


TEST ///
--tests for constructing random MF maps between random factorizations
needsPackage "Complexes"
  -- Hom(C,D) --> f : C --> D
  S = ZZ/101[a..e]
  I = ideal(a*b,c*d,a*e)
  J = ideal(a*b,c*d)
  D = freeResolution I
  C = freeResolution J
  E = Hom(C,D)

  KE = ker dd^E_0
  g = a^2**KE_{0} + b**KE_{1}
  assert isHomogeneous g
  f = homomorphism(0, g, E)
  assert isWellDefined f
  assert isComplexMorphism f
  assert isCommutative f
  assert isHomogeneous f
  assert(source f === C)
  assert(target f === D)

  f = randomComplexMap(D,C)
  assert isWellDefined f
  assert isHomogeneous f
  assert(degree f === 0)

  f = randomComplexMap(D,C,Degree=>-2)
  assert isWellDefined f
  assert isHomogeneous f
  assert(degree f === -2)

  f = randomComplexMap(D,C,Degree=>2)
  assert isWellDefined f
  assert isHomogeneous f
  assert(degree f === 2)
  assert(f == 0)

  f = randomComplexMap(D,C, Cycle=>true)
  assert isWellDefined f
  assert isComplexMorphism f
  assert isCommutative f
  assert isHomogeneous f

  f = randomComplexMap(D,C,InternalDegree=>-1)
  assert isWellDefined f
  assert isHomogeneous f

  f = randomComplexMap(D,C ** S^{-1})
  assert isWellDefined f
  assert isHomogeneous f

  f = randomComplexMap(D, C ** S^{-1}, Cycle=>true)
  assert isWellDefined f
  assert isComplexMorphism f
  assert isHomogeneous f

  f = randomComplexMap(D, C ** S^{-1}, Cycle=>true, InternalDegree=>1)
  assert isWellDefined f
  assert isComplexMorphism f
  assert isHomogeneous f

  f = randomComplexMap(D, C, Cycle=>true, InternalDegree=>1)
  assert isWellDefined f
  assert isComplexMorphism f
  assert isHomogeneous f

  C1 = C ** S^{-1}
  f = randomComplexMap(D, C1, Cycle=>true, Degree=>-1)
  assert isWellDefined f
  assert isCommutative f
  assert isHomogeneous f
  degree f
  f * dd^C1 + dd^D * f

  C1 = C ** S^{-1}
  f = randomComplexMap(D, C1, Cycle=>true, InternalDegree=>1, Degree=>-1)
  assert isWellDefined f
  assert isCommutative f
  assert isHomogeneous f
  degree f
  f * dd^C1 + dd^D * f
  assert(degree f_1 === {1})
  assert(degree f === -1)

  C1 = C ** S^{-1}
  f = randomComplexMap(D, C1, Boundary=>true)
  assert isNullHomotopic f
  h = nullHomotopy f
  assert isNullHomotopyOf(h,f)
  assert isWellDefined f
  assert isWellDefined h
  
  f2 = randomComplexMap(D, C1, Cycle=>true)
  assert not isNullHomotopic f2
  h2 = nullHomotopy f2
  assert not isNullHomotopyOf(h2,f2)
  assert isWellDefined f2
  assert isWellDefined h2
  assert not isCommutative h2

  E = Hom(C ** S^{-1}, D)
  B = basis(0,ker dd^E_0)
  mors = for i from 0 to numColumns B-1 list homomorphism(0, B_{i}, E)
  -- maps which are null-homotopic:
  bd = basis(0, image dd^E_(-1))
  bd = image dd^E_1
  -- I want the map: bd -->E_0, so I can compose: 
  map(E_0, bd, gens bd)
  bds = for i from 0 to numgens bd-1 list homomorphism(0, map(E_0, bd, gens bd) * bd_{i}, E)
  for f in mors do assert(isComplexMorphism f)
  for f in bds do assert(isComplexMorphism f)

  h = nullHomotopy bds_0
  isNullHomotopyOf(h, bds_0)

  isNullHomotopic bds_0

  for f in bds do (
      h := nullHomotopy f;
      assert(f == h * dd^(source h) + dd^(target h) * h)
      );
  for f in bds list (
      h := nullHomotopy f;
      assert isNullHomotopyOf(h, f)
      )
  
  assert(homomorphism(0, B_{0} + B_{5} + B_{6} + B_{7}, E) == mors_0 + mors_5 + mors_6 + mors_7)
  
  prune HH_0(E)
///

TEST ///
--using homomorphism and homomorphism' commands
-- Hom(C,D) --> f : C --> D
  S = ZZ/101[a..e]
  I = ideal(a*b,c*d,a*e)
  J = ideal(a*b,c*d)
  D = freeResolution I
  C = freeResolution J
  E = Hom(C,D)
  f = homomorphism(1,E_1_{2},E)
  assert isWellDefined f
  assert isWellDefined homomorphism(0, (E_0)_{7}, E)
  assert isWellDefined homomorphism(1, a * (E_1)_{6}, E)
  assert isWellDefined homomorphism(-1, a * (E_-1)_{1}, E)
  assert isHomogeneous f
  assert isHomogeneous homomorphism(0, (E_0)_{7}, E)
  --assert isHomogeneous homomorphism(1, a ** (E_1)_{6}, E)
  --assert isHomogeneous homomorphism(-1, a ** (E_-1)_{1}, E)

  fh = homomorphism' f
  isWellDefined fh

  -- ZZZ
  -- to do: 
  -- (1) cache tensor, check the signs in tensor products
  -- (2) TEST homomorphism'
  -- (3) now use this code to check signs for Hom
  h = E_1_{2}
  g = map(E, (complex source h)[-1], hashTable {1 => h})
  f1 = homomorphism g -- this should give a homomorphism f : C --> D of degree 1
  assert(f1 == f)

  assert(HH f1 == 0)
  assert isWellDefined HH f1
  prune HH f1 -- not yet
  
  E = Hom(C,D)
  -- the next test makes sure that Hom is being cached in the youngest complex (here that is C).
  homs = select(keys C.cache, x -> instance(x, Sequence) and first x === Hom)
  assert(#homs === 1 and homs#0 === (Hom, C, D))

  -- f|g, f||g
  f = homomorphism(1,E_1_{2},E)
  g = homomorphism(1,E_1_{3},E)
  target f === target g
  source f === source g

  h1 = f || g  
  assert not h1.cache.?isCommutative
  assert isWellDefined h1
  assert not isCommutative h1
  assert h1.cache.?isCommutative
  assert not h1.cache.isCommutative
  assert(source h1 === C)
  assert(target h1 == D ++ D)
  h1.cache.isCommutative = true;
  debugLevel = 1
  assert not isWellDefined h1
  debugLevel = 0
  h1.cache.isCommutative = false; -- set it back to be the correct value
  
  h2 = f | g
  assert isWellDefined h2
  assert not isCommutative h2
  assert(target h2 === D)
  assert(source h2 === C ++ C)

  KE = ker dd^E_0
  g = a^2**KE_{0} + b**KE_{1}
  assert isHomogeneous g
  f = homomorphism(0, g, E)
  assert isWellDefined f
  assert isComplexMorphism f
  assert isCommutative f
  assert isHomogeneous f
  assert(source f === C)
  assert(target f === D)
  f.cache.isCommutative = false
  debugLevel = 1
  assert not isWellDefined f
  debugLevel = 0
  f.cache.isCommutative = true
  assert isWellDefined HH f
  assert(HH f != 0)

  -- test map(Complex,Complex,Function)
  assert(map(target f, source f, i -> f_i)  == f)
  -- test of prune ComplexMap
  assert(prune f == f)
  g = canonicalMap(target f, image f)
  g' = prune g
  assert isWellDefined g'
  source g' == source f
  assert(target g' == target f)

  f1 = f | f
  assert isWellDefined f1
  assert isComplexMorphism f1
  assert isCommutative f1
  --assert isHomogeneous f1 -- fails, see github issue #607.
  assert(source f1 === C ++ C)
  assert(target f1 === D)
  degrees source f1_1
  degrees source f_1
  degrees target f1_1
  degrees target f_1
  degree f_1

  dual target f1 
  dual source f1
  Hom(f1, complex ring f1)
  f2 = Hom(f1, S^1)
  assert isWellDefined f2
  isWellDefined HH f2

  H' = Hom(dual D, dual C)
  f = homomorphism(1,H'_0_{7},H')
  isCommutative f  
  assert try (HH f; false) else true
  KH' = ker dd^H'_0
  g = homomorphism(0, c**KH'_{0},  H')
  assert(isWellDefined HH g)
  assert(HH g != 0)

  -- Test of tensor product with a ring  
  C
  R = S/(a*b)
  CR = C ** R
  assert isCommutative g
  assert isWellDefined CR
  gR = g ** R
  assert isWellDefined gR
  assert isCommutative gR
  
  -- now do ring maps
  phi = map(R,S)
  CR1 = phi C
  assert(isWellDefined CR1)
  assert(CR1 == CR)
  gR1 = phi g 
  assert isWellDefined gR1
  assert(gR1 == gR)
  
  -- ZZZ, should more tests in
///

TEST ///
--from tensor doc
S = ZZ/101[a..c]
      Ca = ZZdfactorization {a,a}
      Cb = ZZdfactorization {b,b}
      Cc = ZZdfactorization {c,c}
      Cab = Cb ** Ca
      diffs = dd^Cab
      diffs^2
      potential Ca
      potential Cb
      potential Cab
      Cabc = Cc ** Cab
      Cc ** Cb ** Ca
      diffs2 = dd^Cabc
      diffs2^2
      St = adjoinRoot(3,S,t)
      t^3
      St.rootOfUnity
      D1 = (ZZdfactorization {a,a,a})**St
      D2 = (ZZdfactorization {b,b,b})**St
      D12 = D1**D2
      diffs = D12.dd
      diffs^3
      potential D1
      potential D2
      potential D12
      indices Cabc_1
      components Cabc_1
      Cabc_1_[{1,0}]
      indices Cabc_2
      components Cabc_2
      indices D12_0
      indices D12_1
      indices D12_2
      K = koszulComplex vars S;
      KCa = K**Ca
      assert isWellDefined KCa
      assert(potential KCa == potential Ca)
///


TEST ///
--tests from Hom documentation
S = ZZ/101[a..c];
      f = a^3 + b^3 + c^3
      C = randomTailMF(f)
      D = Hom(C,C)
      assert(isZZdComplex D)
      Q = ZZ/101[x,y];
      F = randomLinearMF(2,Q)
      E = Hom(F,F)
      diffs = E.dd
      diffs^2
      E = Hom(C, S^2)
      assert( (isdFactorization E)_0 )
      S = ZZ/101[a..c];
      f = a^3 + b^3 + c^3
      C = randomTailMF(f)
      K = koszulComplex vars S
      D = Hom(K,C)
      assert(isWellDefined D)
      assert(potential D == -potential C)
///


TEST ///
--tests from dual documentation
      S = ZZ/101[a..d];
      f = a^2+b^2+c^2+d^2;
      R = S/(f)
      C1 = tailMF ideal vars R 
      C2 = dual C1
      assert( (isdFactorization C2)_0 )
      assert(potential C1 == - potential C2)
      Q = ZZ/101[a..c];
      f = a^3+b^3+c^3
      F = linearMF(f,t) --this syntax automatically adjoins a root
      Fd = dual F
      assert isWellDefined Fd.dd
      assert (potential F == - potential dual F)
      F' = ZZdfactorization {a,a,a} --no root of unity in this case
      Ft = adjoinRoot(F',t)
      assert isWellDefined dual Ft
      Fd' = dual(F', t) --this syntax adjoins the root for the user
      assert isWellDefined Fd'.dd
///


TEST ///
--tests arising from ringmap documentation
            R = QQ[x,y,z]
            S = QQ[s,t]
            phi = map(S, R, {s, s+t, t})
            Rf = R/(x^3+y^3+z^3)
            C = tailMF ideal vars Rf
            D = phi C
	    assert( (isdFactorization D)_0 )
	    assert(phi(potential C) == potential D)
            assert isWellDefined dd^D
	    R = ZZ/101[a..d]
            S = ZZ/101[s,t]
            phi = map(S, R, {s^4, s^3*t, s*t^3, t^4}, DegreeMap => i -> 4*i)
            C = koszulMF({a,b,c,d}, a^2+b^2+c^2+d^2)
            D = phi C
            assert( (isdFactorization D)_0 )
	    assert(potential D == phi(potential C))
///


TEST ///
--test arising from tensoring with ring map
            R = QQ[x,y,z]
            S = QQ[s,t]
            phi = map(S, R, {s, s+t, t})
            Rf = R/(x^3+y^3+z^3)
            C = tailMF ideal vars Rf
            D = phi**C
	    assert( (isdFactorization D)_0)
	    assert(phi(potential C) == potential D)
            assert isWellDefined dd^D
	    use R;
            A = R/(x^2+y^2+z^2);
            C ** A
            assert(map(A,R) ** C == C ** A)
	    assert(D == C ** phi)
            assert(C ** A == A ** C)
	    use R
            C' = C ** coker matrix{{x^2+y^2+z^2}} 
            D1 = phi C
            assert( (isdFactorization D1)_0 )
            D2 = phi ** C'
            assert( (isdFactorization D2)_0 )
///


TEST ///
--tests arising from pruning documentation
            S = ZZ/101[x,y,z,w];
            F = koszulMF({x,y,z,w}, y*w - x*z);
	    E = Hom(F,F)
            Ed = dual E
            C = HH E
            D = prune C
            g = D.cache.pruningMap
            assert isWellDefined g
            assert isFactorizationMorphism g
            assert (target g == C)
            assert (source g == D)
            g^-1
            assert(g*g^-1 == 1 and g^-1*g == 1)
///


TEST ///
----tests arising from map documentation
            S = ZZ/101[a,b,c]
            R = S/(a^2+b^2+c^2);
	    m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
	    use S
            H = hashTable { 2 => map(C_0, D_0, matrix {{0, 0, -c, -b, 0, -c, 0, 0}, {0, 0, 0, 0, -c, 0, 0, 0}, {0, 0,
                     0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, b, 0, 0, 0}}),
                1 => map(C_1, D_1, matrix {{0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, 0, -c, b, c, 0, 0}, {-c, 0, 0,
                     0, 0, 0, 0, 0}, {b, 0, 0, 0, 0, 0, 0, 0}})
                }
            f = map(C, D, H)
            assert isWellDefined f
            assert isHomogeneous f
            assert(degree f == 0)
            assert isFactorizationMorphism f
	    E = D[1]
            g = map(E, C, hashTable {}, Degree => 1)
            assert isWellDefined g
            assert isHomogeneous g
            assert(degree g == 1)
            assert not isFactorizationMorphism g --since the map has nonzero degree
            assert isCommutative g
            assert(source g == C)
            assert(target g == E)
///


TEST ///
--tests arising from constructing the 0 map and identity
            S = ZZ/101[a,b,c]
            R = S/(a^2+b^2+c^2);
	    m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
            f = map(D, C, 0)
            assert isWellDefined f
            assert isFactorizationMorphism f
            g = map(C, C, 0, Degree => 13)
            assert isWellDefined g
            assert(degree g == 13)
            assert not isFactorizationMorphism g
            assert isCommutative g
            assert isHomogeneous g
            assert(source g == C)
            assert(target g == C)
///


TEST ///
--test arising from applying map to a ZZdFactorizationMap
            S = ZZ/101[a,b,c]
            R = S/(a^2+b^2+c^2);
	    m = ideal vars R
            C = tailMF m
	    D = ZZdfactorization(C, Base => 1)
            f = map(D, C, dd^C, Degree => 0)
            assert isWellDefined f
            assert(degree f == 0)
            assert isCommutative f
            assert isFactorizationMorphism f
            assert not isFactorizationMorphism dd^C
///


TEST ///
--test for identity from documentation
            S = ZZ/101[a,b,c]
            R = S/(a^2+b^2+c^2);
	    m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
            f = id_C
            assert isWellDefined f
            assert isFactorizationMorphism f
	    g = id_D
	    assert isWellDefined g
            assert isFactorizationMorphism g
///


TEST ///
--tests for random factorization maps
            S = ZZ/101[a,b,c]
            R = S/(a^2+b^2+c^2);
	    m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
            f = randomFactorizationMap(C, D, Cycle=>true)
            source f
            assert isWellDefined f
            assert isFactorizationMorphism f
            assert(source f == D)
            assert(target f == C)
	    use S
	    F = randomTailMF(a^3 + b^3 + c^3)
            assert(source dd^F == F)
            assert(target dd^F == F)
            assert(degree dd^F == -1)
///


TEST ///
--test arising from direct sum documentation
            S = ZZ/101[a,b,c]
            R = S/(a^2+b^2+c^2);
	    m = ideal vars R
            C = tailMF m
            g1 = id_C
            g2 = randomFactorizationMap(C[1], C[2], Boundary => true)
            f = g1 ++ g2
            assert isWellDefined f
            L = components f
            L_0 === g1
            L_1 === g2
            indices f
            f' = (chicken => g1) ++ (nuggets => g2)
            components f'
            indices f'
	    assert isWellDefined f'_[chicken]
            assert isCommutative f'^[nuggets]
            assert isCommutative f^[0]
            assert isFactorizationMorphism f_[0]
///


TEST ///
--more tests arising from powers of factorization maps
            S = ZZ/101[a..c];
            C = koszulMF({a,b,c}, a^3 + b^3 + c^3)
            f = dd^C
            f^2
            assert(source f == target f)
            assert(degree f == -1)
            assert(degree f^2 == -2)
	    K' = linearMF(a^3+b^3+c^3, t)
	    g = K'.dd
	    assert(g^3 == potential K')
	    g = randomFactorizationMap(C, C, Degree => -1)
            assert isWellDefined(g^2)
	    assert(f^0 == id_C)
            assert(g^0 == id_C)
	    h = randomFactorizationMap(C, C)
            h^-1
            assert(h * h^-1 == id_C)
            h^-4
            assert(h^-4 * h^4 == id_C)
///


TEST ///
------tests arising from equality documentation
       S = ZZ/101[a,b,c];
       R = S/(a^2+b^2+c^2);
       m = ideal vars R
       C = tailMF m
       D = tailMF (m^2)
       f = id_C
       assert(f == 1)
       f === id_C[-1][1]
       assert(f == id_C[-1][1])
       assert(0 * id_C == 0)
       use S;
       E = koszulMF({a,b,c}, a^3 + b^3 + c^3)
       g = randomFactorizationMap(E, E)
       h = inducedMap(coker g, target g)
       assert(prune h == 0)
       D = prune image g
       p = D.cache.pruningMap
       p == 1
       assert(coker p == 0 and ker p == 0)
       assert(prune p == 1)
///



TEST ///
----tests from homomorphism documentation
            S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
            E = Hom(C,D)
            f = random(E_2, S^{-5})
            g = homomorphism(2, f, E)
            assert isWellDefined g
            assert not isCommutative g
	    h = randomFactorizationMap(E, ZZdfactorization(S^{-2}, 2), Cycle => true)
            f = h_0
            g = homomorphism(0, f, E)
            assert isWellDefined g
            assert isCommutative g
            assert(degree g === 0)
            assert(source g === C)
            assert(target g === D)
            assert(homomorphism' g == h)
	    f1 = randomFactorizationMap(E, ZZdfactorization( S^1, 2), Degree => 1)
            f2 = map(target f1, (source f1)[1], i -> f1_(i+1))
            assert isWellDefined f2
            g1 = homomorphism f1
            g2 = homomorphism f2
            assert(g1 == g2)
            assert isWellDefined g1
            assert isWellDefined g2
            assert (homomorphism' g1 == f1)
            assert (homomorphism' g2 == f1)
///


TEST ///
--misc test for inducedmap
            R = ZZ/101[a,b,c]
            C = koszulMF({a,b,c}, a^3+b^3+c^3)
	    f = randomFactorizationMap(C, C, Cycle => true, InternalDegree => 1)
	    assert(isWellDefined coker f)
	    assert((coker f).dd^2 == (a^3 + b^3 + c^3)*id_(coker f))
	    i1 = inducedMap(coker f, C)
	    i2 = inducedMap(C, image f)
	    assert(ker i1 == image i2)
	    S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
            f = randomFactorizationMap(D, C, Cycle => true, InternalDegree => 1)
            g = randomFactorizationMap(D, C, Boundary => true)
	    
///


TEST ///
----Tests arising from directSum documentation
            S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
	    use S;
            C1 = tailMF m
	    C2 = randomTailMF(a^3+b^3, 3, 5, 2)
	    D1 = randomTailMF(a^3+b^3, 2, 4, 2)
            D2 = tailMF (m^2)
            f = randomFactorizationMap(D1, C1, Cycle => true)
            g = randomFactorizationMap(D2, C2, Cycle => true, InternalDegree => 4)
            h = f ++ g
            assert isWellDefined h
            assert(h == map(D1 ++ D2, C1 ++ C2, {{f,0},{0,g}}))
	    directSum(f, g, f[2])
            h2 = directSum(peanut => f, butter => g, jelly => f[2])
            h2_[butter,jelly]
            assert(source oo == C2 ++ C1[2])
	    assert (h_[0]^[0] == f)
            assert(h_[1]^[1] == g)
            assert(h_[0]^[1] == 0)
            assert(h_[1]^[0] == 0)
	    assert(h_[0] == h * (C1 ++ C2)_[0])
            assert(h_[1] == h * (C1 ++ C2)_[1])
            assert(h^[0] == (D1 ++ D2)^[0] * h)
            assert(h^[1] == (D1 ++ D2)^[1] * h)
///


TEST ///
--tests for randomFactorizationMap from documentation
            S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
	    f = randomFactorizationMap(D,C)
            assert isWellDefined f
            assert not isCommutative f
	    g = randomFactorizationMap(D,C, Cycle => true, InternalDegree => 2)
            assert isWellDefined g
            assert isCommutative g
            assert isFactorizationMorphism g
	    h = randomFactorizationMap(D,C, Boundary => true)
            assert isWellDefined h
            assert isCommutative h
            assert isFactorizationMorphism h
	    p = randomFactorizationMap(D, C, Cycle => true, Degree => 1)
            assert isWellDefined p
            assert isCommutative p
            assert not isFactorizationMorphism p
            assert(degree p === 1)
	    q = randomFactorizationMap(D, C, Boundary => true, InternalDegree => 2)
            assert all({0,1,2}, i -> degree q_i === {2})
            assert isWellDefined q
            assert isCommutative q
            assert isFactorizationMorphism q
            assert(source q === C)
            assert(target q === D)
///



TEST ///
----homology tests arising from doc
            S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
	    f = randomFactorizationMap(D,C, Cycle => true, InternalDegree => 1)
	    g = Hom(f,C)
	    assert isCommutative g
	    (isZZdComplex source g, isZZdComplex target g)
	    h = HH(g)
            assert isWellDefined h
            prune h
            assert(source h == HH Hom(D,C))
            assert(target h == HH Hom(C,C))
            f2 = randomFactorizationMap(C, D, Cycle => true, Degree => 1, InternalDegree => 1)
            h2 = HH Hom(C,f2)
            assert isWellDefined h2
	    f3 = randomFactorizationMap(D, C, Boundary => true)
            h3 = HH Hom(C,f3)
            assert isWellDefined h3
            assert(h3 == 0)
///



TEST ///
--Tests arising from tensor commutativity
S = ZZ/101[a,b];
            R = S/(a^3+b^3);
            m = ideal vars R
            C = tailMF m
            D = tailMF (m^2)
	    F = C**D
	    G = D**C
            f = tensorCommutativity(C,D)
            assert isWellDefined f
            assert isFactorizationMorphism f
            assert(source f === F)
            assert(target f === G)
            assert(f_1 != id_(source f_1))
            assert(prune ker f == 0)
            assert(prune coker f == 0)
            g = f^-1
            assert isWellDefined g
            assert(g * f == 1)
            assert(f * g == 1)
	    h2 = tensorCommutativity(C**(coker vars S), D**(coker vars S));
            assert isWellDefined h2
            assert isFactorizationMorphism h2
            assert(ker h2 == 0)
            assert(coker h2 == 0)
            k = h2^-1;
            assert(h2*k == 1)
            assert(k*h2 == 1)
            h2_2
            assert(source h2_2 != target h2_2)
	    h1 = tensorCommutativity(D, C)
            assert isFactorizationMorphism h1
            assert(h1*f == id_(C**D))
            assert(f*h1 == id_(D**C))
///


