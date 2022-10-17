--------------------------------------------------------------------
-- package tests ---------------------------------------------------
--------------------------------------------------------------------
TEST ///
-*
restart
needsPackage "Complexes"
*-
  S = ZZ/101[a..d,e]
  F = freeResolution ideal"a3,b3"
  G = freeResolution ideal"a10,b10,c10"
  h = tensorCommutativity(F,G)
  isWellDefined h
  h^-1 * h == 1
  target h === G ** F
  source h === F ** G
///



TEST ///
  -- test creation of complexes 1: via free resolutions
-*
  restart
  needsPackage "Complexes"
*-
  R = ZZ/32003[vars(0..17)]
  m1 = genericMatrix(R,a,3,3)
  m2 = genericMatrix(R,j,3,3)
  I = ideal(m1*m2-m2*m1)
  CR = freeResolution I
  assert(ring CR === R)
  assert(length CR === 6)
  assert(isWellDefined CR)
  betti'ans = new BettiTally from {
      (0,{0},0) => 1, 
      (1,{2},2) => 8, 
      (2,{3},3) => 2, 
      (2,{4},4) => 31, 
      (3,{5},5) => 32,
      (3,{6},6) => 28, 
      (4,{6},6) => 3, 
      (4,{7},7) => 58, 
      (5,{8},8) => 32, 
      (6,{9},9) => 4, 
      (6,{10},10) => 1
      }
  assert(betti CR === betti'ans)
  assert(isWellDefined dd^CR)
  assert(CR_0 === R^1)
  assert(CR_-1 == 0)
  assert(rank CR_6 == 5)
  assert(CR_7 == 0)
  assert((0,6) == concentration CR)
  assert(isHomogeneous CR)
  assert(source dd^CR == CR)
  assert(target dd^CR == CR)  
  assert(degree dd^CR == -1)

  fC = map(CR[-1], CR, dd^CR, Degree=>0);

  assert(isWellDefined fC)
  assert(degree fC == 0)
  assert(source fC == CR)
  assert(target fC == CR[-1])
///

TEST ///
  -- test creation of complexes 2: from modules
-*
  restart
  needsPackage "Complexes"
*-

  S = ZZ/101[a..d]
  C0 = complex S^2
  f = dd^C0
  assert(source f == C0)
  assert(target f == C0)
  assert(degree f == -1)
  assert(f == 0)
  assert isWellDefined C0
  assert(C0 != 0)
  assert(length C0 == 0)
  assert(concentration C0 == (0,0))
  assert(not isExact C0)
  assert(isExact(C0, -3, -2))

  C1 = complex(S^2, Base=>3)
  assert(ring C1 === S)
  assert(C1 == C0[-3])
  assert(C1_3 == S^2)
  assert(C1_0 == 0)
  assert(concentration C1 == (3,3))

  C2 = complex S
  assert(ring C2 === S)
  I = ideal(a^2-b, c^3)
  C3 = complex I
  C4 = complex (S/I)
  assert(ring C3 === S)
  assert(ring C4 === S/I)
  assert(length C3 == 0)
  assert(length C4 == 0)
  
  C5 = complex S^0
  assert(C5 == 0)
  assert(0 == C5)
  assert(dd^C5 == 0)
  assert(C5_0 == 0)
  assert(ring C5 === S)
  assert(concentration C5 == (0,0))
  assert(concentration(C5[4]) == (-4,-4))
  assert(concentration prune(C5[4]) == (0,0))
  assert(isExact C5)

  R = QQ
  C = complex QQ
  D = C[3] ++ (complex QQ^2)
  assert(dd^D == 0)
  assert(D_-3 === D^3)
  assert(concentration C == (0,0))
  assert(concentration D == (-3,0))
///

TEST ///
  -- isWellDefined
  R = QQ[a..d];
  f0 = matrix {{-b^2+a*c, b*c-a*d, -c^2+b*d}}
  f1 = map(source f0,, {{d, c}, {c, b}, {b, a}})
  C = complex {f0, f1}
  assert isWellDefined C
  assert((dd^C)^2 == 0)
  assert(HH C != complex coker f0)
  assert(prune HH C == complex coker f0)

  -- a non-example
  g1 = map(source f0,, {{-d, c}, {c, b}, {b, a}})
  C = complex {f0, g1}
  assert not isWellDefined C
  assert((dd^C)^2 != 0)
///

TEST ///
  -- test creation of complexes 3: via constructors
-*
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[a..d]
  I = ideal(b^2-a*c, b*c-a*d, c^2-b*d)
  F1 = map(S^1,,matrix{{I_0, I_1, I_2}})
  F2 = map(source F1,,matrix{
         {0, I_2, -I_1},
         {-I_2, 0, I_0},
         {I_1, -I_0, 0}
         })
  F3 = map(source F2,,matrix{{I_0}, {I_1}, {I_2}})
  C = complex hashTable{1 => F1, 2 => F2, 3 => F3}
  assert isWellDefined C
  assert(ring C === S)
  assert(concentration C == (0,3))

  assert(rank C_2 == 3)
  assert(C^(-1) == C_1)
  assert(C_7 == 0)
  assert isWellDefined dd^C
  assert(dd^C_2 == F2)
  assert(length C == 3)

  G = gradedModule C
  assert(isWellDefined G)
  assert(G.dd == 0)
  assert(source G.dd == G)
  assert(target G.dd == G)
  assert(degree G.dd == -1)
  assert(concentration G == concentration C)
  (lo,hi) = concentration C
  for i from lo to hi do assert(C_i === G_i)

  G = gradedModule(complex S^0)
  assert(G == 0)
  assert(isWellDefined G)
  assert(G.dd == 0)
  assert(source G.dd == G)
  assert(target G.dd == G)
  assert(degree G.dd == -1)
  assert(concentration G == (0,0))

  C0 = complex({S^1, S^3, S^{-2}}, Base=>4)
  G = gradedModule C0
  assert(C0 == G)
  assert(isWellDefined C0)
  assert(isWellDefined G)
  
  concentration G
  C1 = complex{F1,F2,F3}
  assert isWellDefined C1
  assert(C1 == C)

  C2 = complex({F1,F2,F3}, Base => 3)
  assert(C2 != C1[-3])
  assert(concentration (C1[-3]) == concentration C2)
  assert(dd^C2_2 + dd^(C1[-3])_2 == 0)

  assert(HH C != 0)
  assert(prune HH_0 C == comodule I)
///

TEST ///
-*
restart
needsPackage "Complexes"
*-
  S = ZZ/101[a..d]
  C = freeResolution coker matrix{{a,b^2,c^3,d^4}}
  assert not isExact C
  assert(isExact(C,1,infinity))
  assert(not isExact(C,-infinity,infinity))
  assert(regularity C == 6)
  assert(length C == 4)
  f = poincare C
  use ring f
  assert(f == 1-T-T^2+2*T^5-T^8-T^9+T^10)
  p = poincareN C 
  use ring p
  assert(p == 1+S*T_0+S*T_0^2+S*T_0^3+S*T_0^4+S^2*T_0^3+S^2*T_0^4+2*S^2*T_0^5+S^2*T_0^6+
      S^2*T_0^7+S^3*T_0^6+S^3*T_0^7+S^3*T_0^8+S^3*T_0^9+S^4*T_0^10)
  D = C[3]
  assert(poincare D == -f)
  assert(ring poincareN C === ring p)
///

TEST ///
-- test of sum of a Complex, TODO: ComplexMap
-*
restart
needsPackage "Complexes"
*-
  S = ZZ/101[a..d]
  C = freeResolution coker matrix{{a,b^2,c^3,d^4}}
  F = sum C
  assert(degrees F == 
      {{0}, {1}, {2}, {3}, {4}, {3}, {4}, {5}, {5}, {6}, {7}, {6}, {7}, {8}, {9}, {10}}
      )
  D = freeResolution coker matrix{{a,b^3,c^3,c*d^4}}
  f = extend(C,D,id_(S^1))
  sum f
  g = map(C[3], D, f, Degree=>-3)
  assert isWellDefined g
  sum g
///

TEST ///
  -- Complex Array
  S = ZZ/101[a..d]
  C = freeResolution coker vars S
  dd^C_3
  D = C[1]
  assert(dd^D_2 == -dd^C_3)

  (lo,hi) = concentration C
  E = complex(for i from lo+1 to hi list dd^C_i, Base=>-1)
  assert(dd^E_2 == dd^C_3)
///

TEST ///
  S = ZZ/101[a..c]
  C = freeResolution coker vars S
  assert(concentration C == (0,3))
  D = C ++ C[5]
  assert(concentration D == (-5,3))

  assert(C_-1 == 0)
  assert(D_4 == 0)

  f1 = a*id_C
  E = ker f1
  assert(concentration E == (0,3))
  assert(concentration prune E == (0,0))
  assert(prune E == 0)
  assert(E == 0)

  C0 = (complex S^0)[4]
  assert(concentration C0 == (-4,-4))
  assert(prune C0 == 0)
  assert(concentration prune C0 == (0,0))
///

TEST ///
  S = ZZ/101[a..c]
  C = freeResolution coker vars S
  D = C[0]
  assert(C === D)

  (lo,hi) = concentration C
  E = complex for i from lo+1 to hi list 0*dd^C_i
  assert(dd^E == 0)
  assert(degree map(C,C,0*dd^C) == -1)
  assert(C != E)
  assert(E != 0)
  f = id_C
  D = coker f
  assert(D == 0)
///

TEST ///
  -- test of equality of complexes, mainly from doc examples
  S = ZZ/101[a..c]
  C = freeResolution coker vars S
  D = C[0]
  assert(C === D)

  (lo,hi) = concentration C
  assert((lo,hi) == (0,3))
  E = complex for i from lo+1 to hi list 0*dd^C_i
  assert all(toList (lo..hi), i -> C_i == E_i)
  isWellDefined E
  assert(dd^E == 0)
  assert(C != E)
  assert(E != 0)

  f = id_C
  D = coker f
  assert(D == 0)
  assert(D =!= 0)

  C0 = complex S^0
  C1 = C0[4]
  assert(concentration C0 == (0,0))
  assert(concentration C1 == (-4,-4))
  assert(C0 == C1)
  assert(C0 == 0)
  assert(C1 == 0)
  assert(concentration prune C1 == (0,0))
  
  R = QQ[a..d];
  f0 = matrix {{-b^2+a*c, b*c-a*d, -c^2+b*d}}
  f1 = map(source f0,, {{d, c}, {c, b}, {b, a}})
  C = complex {f0, f1}
  assert isWellDefined C
  assert(HH C != complex coker f0)
  assert(prune HH C == complex coker f0)
///

TEST ///
  -- test of homology, mainly from doc examples
  d1 = matrix {
      {1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, 
      {-1, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0}, 
      {0, -1, 0, 0, 0, -1, 0, 0, 0, 1, 1, 1, 0, 0, 0}, 
      {0, 0, -1, 0, 0, 0, -1, 0, 0, -1, 0, 0, 1, 1, 0}, 
      {0, 0, 0, -1, 0, 0, 0, -1, 0, 0, -1, 0, -1, 0, 1}, 
      {0, 0, 0, 0, -1, 0, 0, 0, -1, 0, 0, -1, 0, -1, -1}}
  d2 = matrix {
      {-1, -1, 0, 0, 0, 0, 0, 0, 0, 0}, 
      {0, 0, -1, -1, 0, 0, 0, 0, 0, 0}, 
      {1, 0, 1, 0, 0, 0, 0, 0, 0, 0}, 
      {0, 1, 0, 0, -1, 0, 0, 0, 0, 0}, 
      {0, 0, 0, 1, 1, 0, 0, 0, 0, 0}, 
      {0, 0, 0, 0, 0, -1, -1, 0, 0, 0}, 
      {-1, 0, 0, 0, 0, 0, 0, -1, 0, 0}, 
      {0, -1, 0, 0, 0, 1, 0, 0, 0, 0}, 
      {0, 0, 0, 0, 0, 0, 1, 1, 0, 0}, 
      {0, 0, -1, 0, 0, 0, 0, 0, -1, 0}, 
      {0, 0, 0, 0, 0, -1, 0, 0, 1, 0}, 
      {0, 0, 0, -1, 0, 0, -1, 0, 0, 0}, 
      {0, 0, 0, 0, 0, 0, 0, 0, -1, -1}, 
      {0, 0, 0, 0, 0, 0, 0, -1, 0, 1}, 
      {0, 0, 0, 0, -1, 0, 0, 0, 0, -1}}
  C = complex {d1,d2}
  assert isWellDefined C
  assert(concentration C == (0,2))
  H = HH C
  assert(dd^H == 0)
  D = complex{map(ZZ^1, ZZ^1/(ideal 2), 0)}
  assert(prune H == D)

  S = ZZ/101[a..d, DegreeRank=>4];
  I = intersect(ideal(a,b),ideal(c,d))
  C = dual freeResolution (S^1/I)
  Hd = prune HH C
  assert(isWellDefined Hd)
  assert(dd^Hd == 0)
  M = cokernel map(
      S^{{1,1,0,0},{0,0,1,1}},
      S^{{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}},
      {{b, a, 0, 0}, {0, 0, d, c}})
  assert(prune HH^2 C == M)
  assert(prune HH^1 C == 0)
  assert(prune HH^3 C == coker (vars S ** S^{{1,1,1,1}}))
///

TEST ///
  -- tests of direct sum
  S = ZZ/101[a,b,c];
  C1 = freeResolution coker vars S
  C1 ++ complex(S^13)[-2]
  C2 = complex (ideal(a,b,c))
  C1 ++ C2
  C4 = directSum(first => C1, second => C2)
  C4_[first] -- inclusion map C1 --> C4
  C4^[first] -- projection map C4 --> C1
  assert(C4^[first] * C4_[first] == 1)
  assert(C4^[second] * C4_[second] == 1)
  assert(C4^[first] * C4_[second] == 0)
  assert(C4^[second] * C4_[first] == 0)
  assert(C4_[first] * C4^[first] + C4_[second] * C4^[second] == 1)

  -- test zero complexes
  C0 = complex(S^0)
  C5 = (first => C1) ++ (second => C0)
  assert(C5^[first] * C5_[first] == 1)
  assert(C5^[second] * C5_[second] == 1)
  assert(C5^[first] * C5_[second] == 0)
  assert(C5^[second] * C5_[first] == 0)
  assert(C5_[first] * C5^[first] + C5_[second] * C5^[second] == 1)
///

TEST ///
  -- test of components, mainly from doc examples
  S = ZZ/101[a,b,c];
  C1 = freeResolution coker vars S
  C2 = complex (ideal(a,b,c))
  D = C1 ++ C2
  L = components D
  assert(L_0 === C1)
  assert(L_1 === C2)
  E = (mike => C1) ++ (greg => C2)
  assert(components E === L)

  assert(indices D == {0,1})
  assert(D^[0] == E^[mike])
  assert(indices E == {mike, greg})
  assert(E_[greg] == D_[1])
  
  C3 = complex S^0
  F = C3 ++ C2
  assert(indices F == {0,1})
  assert(prune F == prune C2)
///

TEST ///
  -- test of length, mainly from doc examples
  S = ZZ/101[a,b,c,d];
  C1 = freeResolution coker vars S
  assert(ring C1 === S)
  assert(length C1 == 4)
  assert(length(C1[5]) == 4)
  assert(length (C1 ++ C1[6]) == 10)

  M1 = S^1/(a*b, c*d, a*c, b*c)
  M2 = S^1/(a*b, c*d, a*c)
  C4 = freeResolution M1
  C5 = freeResolution M2
  f = map(M1, M2, 1)
  C6 = coker extend(C4, C5, matrix f)
  assert(concentration C6 == (0,3))
  assert(length C6 == 2)
  assert(length prune C6 == 2)
  assert(concentration prune C6 == (1,3))
///

TEST ///
  -- isHomogeneous, mainly from doc examples
  S = ZZ/101[a,b,c,d];
  I = minors(2, matrix{{a,b,c},{b,c,d}})
  C = freeResolution (S^1/I)
  assert isHomogeneous C
  J = minors(2, matrix{{a,b,c},{b,c,d^2}})
  D = freeResolution (S^1/J)
  assert not isHomogeneous D
///

TEST ///
  -- tensor product of complexes
  S = ZZ/101[a..c]
  Ca = complex {matrix{{a}}}
  Cb = complex {matrix{{b}}}
  Cc = complex {matrix{{c}}}
  Cab = Cb ** Ca
  dd^Cab
  assert isWellDefined Cab
  assert(prune HH Cab == complex coker matrix{{b,a}})
  assert(indices Cab_1 == {{0, 1}, {1, 0}})
  for i from 0 to 2 do assert (rank Cab_i == {1,2,1}_i)

  Cabc = Cc ** Cab
  assert isWellDefined Cabc
  assert(prune HH Cabc == complex coker matrix{{c,b,a}})
  assert(indices Cabc_1 == {{0, 1}, {1, 0}})
  for i from 0 to 3 do assert (rank Cabc_i == {1,3,3,1}_i)

  Cabc2 = (Cc ** Cb) ** Ca
  assert(Cabc2 == Cabc)
  assert(Ca ** Cb != Cab)

  D = (Cabc ** (S^1/(a,b,c)))
  assert(dd^D == 0)
  S^2 ** Cabc

  assert(Cab[1] == complex(S^1, Base=>-1) ** Cab)
  assert(Cab[-4] == complex(S^1, Base=>4) ** Cab)
  
  F = dd^Cc ** id_Cab
  G = - id_Cc ** dd^Cab
  source F == source G
  target F == target G
  source F == Cabc
  target F == Cabc
  F+G
  dd^Cabc
  -- WARNING TODO: are the signs consistent here?
///

TEST ///
  -- test of Hom of complexes, mainly from doc
  S = ZZ/101[a..c]
  C = freeResolution coker vars S
  D = Hom(C,C)
  dd^D
  assert(prune HH D == Hom(C, coker vars S))
  E = Hom(C, S^1)
  assert(prune HH E == complex(coker matrix{{c,b,a}} ** S^{3}, Base=>-3))
///

TEST ///
  -- f: C --> D
  -- cone(Hom(f,D)) == Hom(cone(f),D[1])
  S = ZZ/101[a..e]
  I = ideal(a*b,c*d,a*e)
  J = ideal(a*b,c*d)
  FI = freeResolution I
  FJ = freeResolution J
  g = extend(FI,FJ,id_(S^1))
  f = Hom(g, complex S^1)
  assert isWellDefined f
  E1 = cone Hom(f, target f)
  E2 = Hom(cone(f),(target f)[-1])
  -- TODO: it would be very nice if these would be equal on the nose.
  -- Can this be made to happen?
  E1 == E2
  (dd^E1_1, dd^E2_1)
///

TEST ///
  -- cone f.  Trivial and strange cases
  f1 = map(complex ZZ^1, complex ZZ^0, 0)
  f2 = map(complex ZZ^0, complex ZZ^1, 0)
  f3 = map(complex ZZ^1, complex(ZZ^0,Base=>-1), 0)
  f4 = map(complex ZZ^0, complex(ZZ^1,Base=>-1), 0)
  f5 = map(complex(ZZ^1, Base=>4), complex(ZZ^0,Base=>-3), 0)
  f6 = map(complex(ZZ^0, Base=>4), complex(ZZ^1,Base=>-3), 0)
  concentration source f5
  concentration target f5
  cone f1
  cone f2
  cone f3 
  cone f4 
  cone f5 -- lot's of zeros.... prune to get rid of zeros
  prune cone f5
  cone f6 -- lot's of zeros.... prune to get rid of zeros.
  prune cone f6
  assert(cone f5 == complex(ZZ^1, Base=>4))
///


TEST ///
  -- how to find a morphism of complexes
-*
restart
*-
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
-*
restart
*-
  needsPackage "Complexes"
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
  -- resolution of a module thought of as a complex
-*  
  restart
  needsPackage "Complexes"
*-  
  
  R = ZZ/101[a,b,c,d,e]
  
  -- case 1: free module:
  M = complex(R^3, Base=>1)
  f = resolutionMap M
  assert(isWellDefined f)
  assert(target f == M)
  assert(f_1 == 1)

  -- case 2: cokernel module
  M0 = coker vars R
  M = complex(M0)
  f = resolutionMap M
  assert(target f == M)
  assert(isWellDefined f)
  assert(source f == freeResolution M0)
  assert(prune HH source f == M) -- this might not need to be true in general, but is true here.

  -- case 3: image module
  M0 = image vars R
  M = complex(M0)
  f = resolutionMap M
  assert(target f == M)
  assert(isWellDefined f)
  assert(source f == freeResolution M0)
  assert(prune HH source f == prune M) -- this might not need to be true in general, but is true here.

  -- case 4: subquotient module
  M0 = image vars R ++ coker vars R
  M = complex(M0)
  f = resolutionMap M
  assert(target f == M)
  assert(isWellDefined f)
  assert(source f == freeResolution M0)
  assert(prune HH source f == prune M) -- this might not need to be true in general, but is true here.
///

TEST ///
  -- resolution of a complex
-*  
  restart
  needsPackage "Complexes"
*-  
  
  R = ZZ/101[a,b,c,d,e]
  I = intersect(ideal(a,b),ideal(c,d,e))
  C = (dual freeResolution I) [-4]
  f = resolutionMap C
  assert(target f === C)
  assert(isWellDefined f)
  assert(isQuasiIsomorphism f)
  assert(isQuasiIsomorphism(f, Concentration=>(-5,3)))
  assert(isQuasiIsomorphism(f, Concentration=>(3,3)))
  assert(isComplexMorphism f)
  assert(coker f == 0)
  assert(kernel HH f == 0)
  assert(cokernel HH f == 0)
  assert(resolution C == source f)
///

TEST ///
  -- naive truncation
-*  
  restart
  needsPackage "Complexes"
*-  
  R = ZZ/101[a,b,c,d,e]
  I = intersect(ideal(a,b),ideal(c,d,e))
  C = freeResolution I
  naiveTruncation(C,1,2)
  naiveTruncation(C,1,6)
  naiveTruncation(C,-13,2)
  naiveTruncation(C,-infinity,2)
  assert try (naiveTruncation(C,4,3); false) else true
  naiveTruncation(C,4,infinity)
  assert(C === naiveTruncation(C,-infinity,infinity))

  g = naiveTruncation(id_C, (0,2), (1,3))
  assert isWellDefined g
  assert not isComplexMorphism g
  
  g = naiveTruncation(id_C, (1,3))
  assert isWellDefined g
  assert isComplexMorphism g

  g = naiveTruncation(id_C, (0,1), (2,3))
  assert isWellDefined g
  assert isComplexMorphism g
  assert(g == 0)
///

TEST ///
  -- canonical truncation
-*  
  restart
  needsPackage "Complexes"
*-  
  R = ZZ/101[a,b,c,d,e]
  I = intersect(ideal(a,b),ideal(c,d,e))
  C = freeResolution I
  C1 = canonicalTruncation(C,1,2)
  assert isWellDefined C1
  C2 = canonicalTruncation(C,1,6)
  assert(C2 == canonicalTruncation(C,1,))
  C3 = canonicalTruncation(C,-13,2)
  assert(C3 == canonicalTruncation(C,,2))
  C4 = canonicalTruncation(C,-infinity,2)
  assert(C3 == C4)
  assert try (canonicalTruncation(C,4,3); false) else true
  canonicalTruncation(C,4,infinity)
  canonicalTruncation(C,-infinity,infinity) == C

  g = canonicalTruncation(id_C, (1, infinity))
  assert isWellDefined g
  assert isComplexMorphism g

  g = canonicalTruncation(id_C, (0,2))
  assert isWellDefined g
  assert isComplexMorphism g
  
  g = canonicalTruncation(id_C, (1,3))
  assert isWellDefined g
  assert isComplexMorphism g

  g = canonicalTruncation(id_C, (1,1))
  assert isWellDefined g
  assert isComplexMorphism g

///


TEST ///
  -- canonical truncation, more interesting example(s)
-*  
  restart
  needsPackage "Complexes"
*-  
  R = ZZ/101[a,b,c,d,e]
  I = intersect(ideal(a,b),ideal(c,d,e))
  J = ideal(a^2, b^2, a*d, b*c^2)
  K = intersect(ideal(a,b,c^2),ideal(c,d,e))
  
  I = intersect(ideal(a,b), ideal(c,d,e))
  J = intersect(ideal(a,b,c^2), ideal(c,d,e))
  K = intersect(ideal(a,b,c^2), ideal(b^2,c,d,e))
  C = dual freeResolution I
  D = dual freeResolution J
  E = dual freeResolution K
  HCD = Hom(C,D)
  prune HH^0(HCD)
  HDE = Hom(D,E)
  prune HH^0(HDE)
  HCE = Hom(C,E)
  prune HH^0(HCE)  
  ZHCD0 = ker dd^HCD_0;
  ZHDE0 = ker dd^HDE_0;
  ZHCE0 = ker dd^HCE_0;
  f0 = map(ZHCD0, R^1,  random(R^(numgens ZHCD0), R^1))
  f = homomorphism(0, f0, HCD)
  isWellDefined f
  isNullHomotopic f
  g0 = map(ZHDE0, R^1,  random(R^(numgens ZHDE0), R^1))
  g = homomorphism(0, g0, HDE)
  isWellDefined g
  isNullHomotopic g
  h = g * f
  isWellDefined h
  isNullHomotopic h
  f' = canonicalTruncation(f, (-3,-1));
  g' = canonicalTruncation(g, (-3,-1));
  h' = canonicalTruncation(h, (-3,-1));
  assert(g' * f' == h')

  f' = prune canonicalTruncation(f, (-3,-3));
  g' = prune canonicalTruncation(g, (-3,-3));
  h' = prune canonicalTruncation(h, (-3,-3));
  assert(g' * f' == h')
  f' = canonicalTruncation(f, (-3,-3));
  g' = canonicalTruncation(g, (-3,-3));
  h' = canonicalTruncation(h, (-3,-3));
  assert(g' * f' == h')

  f' = canonicalTruncation(f, (-4,-4));
  g' = canonicalTruncation(g, (-4,-4));
  h' = canonicalTruncation(h, (-4,-4));
  assert(g' * f' == h')

  f' = canonicalTruncation(f, (-4,-2));
  g' = canonicalTruncation(g, (-4,-2));
  h' = canonicalTruncation(h, (-4,-2));
  assert(g' * f' == h')

  g = canonicalTruncation(f, (-3,-2))
  isWellDefined g
  isComplexMorphism g
  
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  kk = ZZ/101
  S = kk[a..d]
  I = ideal"a2-bc,ab-cd"
  C = freeResolution(I, FastNonminimal=>true)
  dd^C
  minC = minimize C
  minC.dd

  kk = ZZ/101  
  S = kk[a..e]
  F = random(3,S)
  I = inverseSystem F
  C = freeResolution(I, FastNonminimal=>true)
  minC = minimize C
  betti minC  
  minimize(C ++ C[9])
///

TEST ///
  -- resolution of a complex
-*  
  restart
  debug needsPackage "Complexes"
*-  
  R = ZZ/101[a,b,c,d,e]
  I = intersect(ideal(a,b),ideal(c,d,e))
  C = Hom(freeResolution I, R^1/I)
  assert not isFree C
  elapsedTime f = resolutionMap C;
  assert(target f === C)
  assert(isWellDefined f)
  assert(isQuasiIsomorphism f)
  assert(isComplexMorphism f)
  assert(coker f == 0)
  assert(kernel HH f == 0)
  assert(cokernel HH f == 0)
  assert(resolution C == source f)
  assert(isFree source f)
  D = resolution C
  prune HH C

  MD = minimize D;
  assert(MD == D)
  assert isWellDefined MD
  p1 = MD.cache.minimizingMap;
  assert isWellDefined p1
  assert isComplexMorphism p1
  assert isQuasiIsomorphism p1
  
  assert(dd^MD ** coker vars R == 0)
  assert(dd^D ** coker vars R == 0)

  I = ideal"b2-ac,c2-bd,bcd-ad2"
  C = Hom(freeResolution I, R^1/I)
  elapsedTime D = resolution C;
  
  C1 = complex for i from -2 to 0 list dd^D_i
  isWellDefined C1
  dd^C1 ** coker vars ring D == 0
  D1 = minimize C1
  p1 = D1.cache.minimizingMap;
  assert isWellDefined p1
  assert isComplexMorphism p1
  assert isQuasiIsomorphism p1  
///

///
-*
  -- of minimize
  restart
  needsPackage "Complexes"
*-
  -- TODO: this test does not work, as the free resolutions take too long to complete.
  -- add this test back in once Strategy => 4, or FastNonminimal => true works.
  kk = ZZ/32003
  S = kk[a..e]
  F = random(3,S)
  I = inverseSystem F
  C = freeResolution(I, FastNonminimal=>true)
  elapsedTime minimize C

  S = kk[vars(0..8)]
  F = random(3,S)
  I = inverseSystem F;
  C = freeResolution(I, FastNonminimal=>true)
  ---- elapsedTime minimize C  -- very slow TODO: fix this

  -- good benchmark test:
  kk = ZZ/32003
  S = kk[vars(0..6)]
  F = random(3,S)
  I = inverseSystem F;
  C = freeResolution(I, FastNonminimal=>true)
  ---- elapsedTime minimize C  -- very slow, TODO: fix this
-*
  needsPackage "PruneComplex"  
  needsPackage "ChainComplexExtras"
  C' = res(I, FastNonminimal=>true)
  elapsedTime pruneComplex(C', UnitTest=>isScalar) -- 17.7 sec on my MBP
  C'' = res(ideal I_*, FastNonminimal=>true)  
  elapsedTime minimize C''  -- very slow, TODO: fix this
*-  
///


TEST ///  
-*
  restart
  debug needsPackage "Complexes"
*-
  R = ZZ/101[a,b,c,d,e]
  I = intersect(ideal(a,b),ideal(c,d,e))
  C = Hom(freeResolution I, R^1/I)
  f = resolutionMap C
  source f
  assert isWellDefined f
  assert isComplexMorphism f
  assert isQuasiIsomorphism f  

  R = ZZ/101[a,b,c,d]
  I = monomialCurveIdeal(R,{1,2,3})
  C = freeResolution I
  f = resolutionMap C

  -- the point of this example: the map is not the identity map, due to some
  -- "strange" choice of signs... 
  
  assert(target f === C)
  source f
  assert isWellDefined f
  assert isComplexMorphism f
  assert isQuasiIsomorphism f  

  -- a trivial complex
  debug needsPackage "Complexes"
  R = ZZ/101[a,b,c,d,e]
  C = complex {map(R^1, R^1, 1)}
  f = resolutionMap C
  source f
  assert isWellDefined f
  assert isComplexMorphism f
  assert isQuasiIsomorphism f  

-*
  -- this computation is way too slow.
  needsPackage "Complexes"
  R = ZZ/101[vars(0..17)]
  m1 = genericMatrix(R,a,3,3)
  m2 = genericMatrix(R,j,3,3)
  I = ideal(m1*m2-m2*m1)
  C = freeResolution I
  f = resolutionMap C -- this is slow.
*-
  
  source f
  assert isWellDefined f
  assert isComplexMorphism f
  assert isQuasiIsomorphism f  
///



TEST ///  
-*
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[a,b,c,d,e]
  C = complex {id_(S^1)}
  f = resolutionMap C
  assert(target f === C)
  assert isWellDefined f
  assert isComplexMorphism f
  assert isQuasiIsomorphism f  

  R = ZZ/101[a,b,c,d]
  I = monomialCurveIdeal(R,{1,2,3})
  C = freeResolution I
  f = resolutionMap C
  assert(target f === C)
  assert isWellDefined f
  assert isComplexMorphism f
  assert isQuasiIsomorphism f  
///

TEST ///  
  -- resolutions and lifting maps
-*
  restart
  debug needsPackage "Complexes"
*-
  S = ZZ/101[a,b,c,d,e]
  J = ideal(a*b, b*c*d, a*e, c*e)
  FJ = freeResolution J
  N = complex (S^1/J)
  f = map(N, FJ, hashTable{0=> map(N_0, FJ_0, 1)})
  assert isWellDefined f
  assert(liftMapAlongQuasiIsomorphism(f,f) == 1)

  -- test #2
  -- take a random morphism between two non-free complexes
  -- obtain the corresponding map between their resolutions.

  I = ideal(a*b, b*c*d, a*e, c*e, b*d*e)
  FI = freeResolution I

  C = prune Hom(FI, S^1/I)
  D = prune Hom(FJ, S^1/J)
  
  fC = resolutionMap C
  fD = resolutionMap D

  g = randomComplexMap(D,C,Cycle=>true)
  g' = liftMapAlongQuasiIsomorphism(g * fC, fD)
  h = homotopyMap g'
  assert not isQuasiIsomorphism g
  assert isWellDefined g'
  assert isCommutative g'
  assert(degree g' == 0)
  assert(g * fC == fD * g')
  assert isWellDefined h
  assert(degree h == 1)
  assert isNullHomotopyOf(h, g*fC-fD*g')

  -- test #3
  C1 = C ** S^{-1}
  g = randomComplexMap(D,C1,Cycle=>true)
  fC1 = resolutionMap C1
  fD = resolutionMap D
  g' = liftMapAlongQuasiIsomorphism(g * fC1, fD)
  assert not isQuasiIsomorphism g
  assert isWellDefined g'
  assert isComplexMorphism g'
  assert(g * fC1 == fD * g')
  h = homotopyMap g'
  assert isWellDefined h
  assert(degree h == 1)
  assert isNullHomotopyOf(h, g*fC1-fD*g')

  -- test #4
  I = ideal(a*b, b*c*d, a*e, c*e, b*d*e)
  J = I + ideal(a*b-c*d)
  FI = freeResolution I
  FJ = freeResolution J

  C = prune Hom(FI, S^1/I)
  D = prune Hom(FJ, S^1/J)
  C1 = C ** S^{-3}
  g = randomComplexMap(D,C1,Cycle=>true)
  fC1 = resolutionMap C1
  fD = resolutionMap D
  g' = liftMapAlongQuasiIsomorphism(g * fC1, fD);
  assert not isQuasiIsomorphism g
  assert isWellDefined g'
  assert isComplexMorphism g'
  assert(g * fC1 == fD * g')
  h = homotopyMap g'
  assert isWellDefined h
  assert(degree h == 1)
  assert isNullHomotopyOf(h, g*fC1-fD*g')
///

TEST ///
  -- test #6
-*  
  restart
  needsPackage "Complexes"
*-  
  S = ZZ/101[a,b,c]
  I = ideal"ab,ac"
  J = I + ideal(b*c-a^2)
  K = J + ideal(a^3+c^3)
  FI = freeResolution I
  FJ = freeResolution J
  FK = freeResolution K
  CI = (prune Hom(FI, S^1/I))
  CJ = (prune Hom(FJ, S^1/J))[-1]
  CK = (prune Hom(FK, S^1/K))[-1]
  g1 = randomComplexMap(CJ, CI, Cycle=>true)
  g2 = randomComplexMap(CK, CJ, Cycle=>true)
  assert isWellDefined g1
  assert isCommutative g1
  assert isWellDefined g2
  assert isCommutative g2
  fCI = resolutionMap CI
  fCJ = resolutionMap CJ
  fCK = resolutionMap CK
  g = g2 * g1;
  assert isWellDefined g
  assert isCommutative g
  g1' = liftMapAlongQuasiIsomorphism(g1 * fCI, fCJ);
  g2' = liftMapAlongQuasiIsomorphism(g2 * fCJ, fCK);
  assert isWellDefined g1'
  assert isCommutative g1'
  assert isWellDefined g2'
  assert isCommutative g2'
  g' = liftMapAlongQuasiIsomorphism(g * fCI, fCK);
  diffg' = g2' * g1' - g';
  assert isNullHomotopic diffg'
  h = nullHomotopy diffg';
  assert isWellDefined h
  assert isNullHomotopyOf(h, diffg')
  diffg'_-1 -- just to see the nontrivial-ness of the differentials
///


TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[a..d]
  I = monomialCurveIdeal(S,{1,3,4})
  E = Ext^2(S^1/I, S)

  h0 = basis(0,E)
  f = h0 * random(S^(numColumns h0), S^1)
  C = yonedaExtension f
  assert isWellDefined C
  assert(prune HH C == 0)
  assert(concentration C == (0,3))
  assert(C_3 == S^1)
  assert(C_0 == S^1/I)
  assert(C_1 == S^1)-- since we start with a free res of S^1/I

  g = yonedaMap f
  assert(degree g === -2)
  f2 = yonedaMap' g
  assert(f == f2)
  
  -- let's try the Yoneda extension corresponding to the zero map:
  f1 = 0*f
  source f1
  target f1
  degree f1
  C1 = yonedaExtension f1
  assert isWellDefined C1
  assert(prune HH C1 == 0)
  assert(concentration C1 == (0,3))
  assert(C1_3 == S^1)
  assert(C1_0 == S^1/I)
  assert(C1_1 == S^1)-- since we start with a free res of S^1/I
  g = yonedaMap f1
  f2 = yonedaMap' g
  assert(g == 0)
  assert(f1 == f2)
  assert(degree g == -2)

  -- now try an index larger than the projective dimension:  
  E = Ext^4(S^1/I, S)
  h0 = basis(0,E)
  f = h0 * random(S^(numColumns h0), S^1)
  C = yonedaExtension f
  assert isWellDefined C
  assert(prune HH C == 0)
  assert(concentration C == (0,5))
  assert(C_5 == S^1)
  assert(C_0 == S^1/I)
  assert(C_1 == S^1)-- since we start with a free res of S^1/I
  g = yonedaMap f
  assert(g == 0)
  f2 = yonedaMap' g
  assert(degree g == -4)
  assert(f == f2)

  -- now try an index larger than the projective dimension:  
  E = Ext^6(S^1/I, S)
  h0 = basis(0,E)
  f = h0 * random(S^(numColumns h0), S^1)
  C = yonedaExtension f
  assert isWellDefined C
  assert(prune HH C == 0)
  assert(concentration C == (0,7))
  assert(C_7 == S^1)
  assert(C_0 == S^1/I)
  assert(C_1 == S^1)-- since we start with a free res of S^1/I
  assert(C_5 == 0)
  g = yonedaMap f
  assert(g == 0)
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  R = ZZ/101[x,y,z]/(y^2*z-x*(x-z)*(x-2*z));
  M = truncate(1,R^1)
  prune Ext^3(M, M)
  B = basis(-4, Ext^3(M, M))
  f = B_{2}
  g = yonedaMap(f, LengthLimit => 8)
  assert isHomogeneous g
  assert isWellDefined g
  assert isCommutative g
  assert(degree g === -3)
  assert(yonedaMap' g == map(target f, R^1, f, Degree => -4))
  assert(isHomogeneous yonedaMap' g)

  -- Here is a homogeneous map, which is not of degree 0.
  -- If this test fails, that probably means that `homomorphism`
  -- has been fixed (git issue #1693), and 
  -- then the code `degree f + degree g`
  -- in yonedaMap is probably no longer needed, and should be
  -- changed to `degree g`.
  -- The previous comment did happen, and the yonedaMap code has now been changed to "degree g".
  f1 = map(target f, R^1, f, Degree => -4)
  g1 = yonedaMap(f1, LengthLimit => 8)
  assert isHomogeneous g1
  assert isWellDefined g1
  assert isCommutative g1
  assert(degree g1 === -3)
  assert(yonedaMap' g1 == f1)
  assert(isHomogeneous yonedaMap' g1)
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[a..d]
  I = monomialCurveIdeal(S,{1,3,4})
  E = Ext^0(module I, module I)

  h0 = basis(0,E)
  f = h0 * random(S^(numColumns h0), S^1)

  g = yonedaMap f
  assert(degree g === 0)
  f2 = yonedaMap' g
  assert(f == f2)
  
  E = Ext^0(module I, comodule I)
  h0 = basis(0,E)
  f = h0 * random(S^(numColumns h0), S^1)
  g = yonedaMap f
  assert(degree g === 0)
  f2 = yonedaMap' g
  assert(f == f2)

  E = Ext^(-1)(module I, comodule I)  
  h0 = basis(0,E)
  f = h0 * random(S^(numColumns h0), S^1)
  g = yonedaMap f
  assert(degree g === 1)
  f2 = yonedaMap' g
  assert(f == f2)
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[x,y,z]/(y^2*z-x*(x-z)*(x-2*z))
  M = truncate(1,S^1)
  E = Ext^1(M, S^1)

  h0 = basis(0,E)
  f = h0 * random(S^(numColumns h0), S^1)
  C = yonedaExtension f
  assert isWellDefined C
  assert(prune HH C == 0)
  assert(concentration C == (0,2))
  assert(C_2 == S^1)
  assert(C_0 == M)

  assert(yonedaExtension' C == f)
  fC = resolutionMap(C, LengthLimit => 5)
  -- as C is exact, fC is the zero map
  assert(fC == 0)
  assert(isWellDefined fC)
  assert(target fC == C)

  g = yonedaMap(f, LengthLimit => 4) -- TODO: what should we set this length too?
  assert isWellDefined g
  assert isCommutative g
  assert (degree g == -1)
  assert(g_1 != 0)
  f2 = yonedaMap' g
  assert(f2 == f)
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[x,y,z]
  M = truncate(1,S^1)
  N = S^{{-2}}/(x)
  E = Ext^1(M, N)

  h0 = basis(0,E)
  f = h0 * random(S^(numColumns h0), S^1)
  C = yonedaExtension f
  assert isWellDefined C
  assert(prune HH C == 0)
  assert(concentration C == (0,2))
  assert(C_2 == N)
  assert(C_0 == M)

  f1 = yonedaExtension' C
  C1 = yonedaExtension f1
  assert(f == f1)
  assert(C1 == C)
  assert isWellDefined f1
  assert isWellDefined C1

  g = yonedaMap f
  assert isWellDefined g
  assert isCommutative g
  assert (degree g == -1)
  f2 = yonedaMap' g
  assert(f2 == f)

  E = Ext^2(M, N ** S^{{-1}})  
  h0 = basis(0,E)
  f = h0 * random(S^(numColumns h0), S^1)
  C = yonedaExtension f
  f1 = yonedaExtension' C
  C1 = yonedaExtension f1
  assert(f == f1)
  assert(C1 == C)
  assert isWellDefined f1
  assert isWellDefined C1
  
  --  go from FM_d --> N to an element R^1 --> Ext^d(M,N).
  --  (and vice versa)
  --  length limit on resolutionMap (maybe), also can we just use freeResolution
  --    if it is a module?
  --  make this functorial?  Actually: perhaps only place this in as an example.
  --  Yoneda product
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[x,y,z]/(y^2*z-x*(x-z)*(x-2*z))
  M = truncate(1,S^1)
  E = Ext^0(M, M)
  
  h0 = basis(0,E)
  f = h0 * random(S^(numColumns h0), S^1)
  assert try (C = yonedaExtension f; false) else true
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[x,y,z]
  M = truncate(1,S^1)
  N = S^{{-2}}/(x)
  E2 = Ext^1(M, N)
  E1 = Ext^1(M ** S^{{1}},M)
  
  h2 = basis(0,E2)
  f2 = h2 * random(S^(numColumns h2), S^1)
  h1 = basis(0,E1)
  f1 = h1 * random(S^(numColumns h1), S^1)

  g = yonedaProduct(f1,f2)
  yonedaExtension g
  
  yonedaProduct(E1, E2)
  Ext^2(M ** S^{{1}}, N)

  yonedaProduct(Ext^0(M, M), E2)
///

TEST ///
  S = ZZ/101[x,y,z]
  R = S/(y^2*z-x^3)
  I = ideal(x,y)
  E0 = Ext^0(I,I)
  E1 = Ext^1(I,R^1/I)
  E0_{1}
  E1_{0}
  f = yonedaProduct(E0_{1}, E1_{0}+E1_{1})
  assert(f == yonedaExtension' yonedaExtension f)
///

TEST ///
  -- of length limits and free resolutions
  R = ZZ/32003[a..d]/(a^2-b*c)
  M = coker vars R;
  C1 = freeResolution(M, LengthLimit => 4);
  assert(M.cache.?Resolution)
  assert(M.cache.Resolution === C1)
  assert(M.cache.Resolution.cache.LengthLimit === length C1)
  assert(C1.cache.Module === M)
  C2 = freeResolution(M, LengthLimit=>10)
  assert(length C2 == 10)
  assert(M.cache.?Resolution)
  assert(M.cache.Resolution === C2)
  assert(M.cache.Resolution.cache.LengthLimit === length C2)
  assert(C2.cache.Module === M)
  C3 = freeResolution(M, LengthLimit=>9)
  assert(M.cache.?Resolution)
  assert(M.cache.Resolution === C2)
  assert(M.cache.Resolution =!= C3)
  assert(M.cache.Resolution.cache.LengthLimit === length C2)
  assert(length C3 == 9)
  assert(C3.cache.Module === M)
  C4 = freeResolution(M, LengthLimit => 1)
  assert(M.cache.?Resolution)
  assert(M.cache.Resolution === C2)
  assert(M.cache.Resolution.cache.LengthLimit === length C2)
  assert(length C4 == 1)
  assert(C4.cache.Module === M)
  C5 = freeResolution(M, LengthLimit => 0)
  assert(M.cache.?Resolution)
  assert(M.cache.Resolution === C2)
  assert(M.cache.Resolution.cache.LengthLimit === length C2)
  assert(length C5 == 0) 
  assert(C5.cache.Module === M)
  -- TODO? What behavior do we want here?
  --assert try (C6 = freeResolution(M, LengthLimit => -1); false) else true  -- this one?
  assert (C6 = freeResolution(M, LengthLimit => -1) == 0) -- or this one?
  assert(M.cache.?Resolution)
  assert(M.cache.Resolution === C2)
  assert(M.cache.Resolution.cache.LengthLimit === length C2)
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  R = ZZ/101[a..d]/(a^2-b*c, b^2-c*d)
  C = freeResolution(coker vars R, LengthLimit => 5)
  f = dd^C_2
  g = Hom(f, R^1/(a*c, b*d))
  D = complex{g}
  fD = resolutionMap D
  fD.cache.LengthLimit
  isWellDefined fD
  source fD
  fD = resolutionMap(D, LengthLimit=>7)
  assert(length source fD == 7)
  assert(fD.cache.LengthLimit == 7)
  fD = resolutionMap(D, LengthLimit=>4)
  assert(length source fD == 4)
  assert(D.cache.resolutionMap.cache.LengthLimit == 7)
///

TEST ///
  -- creation of chain complexes
-*
  restart
  needsPackage "Complexes"
*-
  R = QQ[a..d]
  C = freeResolution(coker vars R)

  CR2 = res coker matrix{{a,b,c,d}}
  maps = for i from 1 to 4 list CR2.dd_i
  C2 = complex maps

  assert(C == C2)
  assert(C != C[1])
  assert(C != 0)  

  -- test C_i for various values, some out of range
  for i from -3 to 5 do assert(isFreeModule C_i)
  ranks = {0,0,0,1,4,6,4,1,0}
  for i from 0 to #ranks-1 do assert(rank C_(i-3) == ranks#i)

  -- create identity
  id_C == 1
  
  -- test map access
  assert(dd^C != 0)
  assert((dd^C)^2 == 0)
  assert((dd^C)^2 != 1)
  for i from 1 to 4 do assert(dd^C_i == maps_(i-1))
  assert(dd^C_100 == 0)
  
  assert(dd^C * dd^C == 0)
  assert((dd^C)^2 == 0)
  assert try ((dd^C)^-1; false) else true
///

TEST ///
  -- creation of a complex with 2 non-zero maps, not contiguous
-*
  restart
  needsPackage "Complexes"
*-
  R = QQ[a..d]
  f1 = random(R^3, R^2)
  f2 = random(R^1, R^4)
  C = complex hashTable {1 => f1, 6 => f2}
  for i from 0 to 7 list C_i
  assert(C_0 === target f1)
  assert(C_1 == source f1)
  assert(C_5 == target f2)
  assert(C_6 == source f2)
  assert(dd^C_1 == f1)
  assert(dd^C_6 == f2)
  for i from -10 to 10 do if i != 1 and i != 6 then (
      if i != 0 and i != 5 then assert(C_i == 0);
      assert(dd^C_i == 0)
      );
  assert((dd^C)^2 == 0)
  assert(id_C == 1)
  assert((id_C)^(-1) == id_C)
///

TEST ///
  -- of Hom(f,g)
  -- Hom(f,source g) * Hom(target f,g) === Hom(f,g)
  -- Hom(target f,g) * Hom(g,source g) === (sign) Hom(f,g)
-*
  restart
  needsPackage "Complexes"
*-
  R = QQ[a..d]
  C = freeResolution minors(3,matrix{{a,b,c,d},{b,c,d,a},{b,d,a,c}})
  D = freeResolution coker matrix{{a^2, b^2, c^2}}
  f1 = a*id_C
  f = map(C[1],C,f1,Degree=>-1)
  g1 = d*id_D
  g = map(D[-3],D,g1,Degree=>3)
  h = Hom(f,g)
  -- necessary properties for the signs of Hom(f,g):
  assert(Hom(f,target g) * Hom(target f, g) == h)
  assert(Hom(source f,g) * Hom(f,source g) == -h)
///

TEST ///
  -- of f**g
  -- Hom(f,source g) * Hom(target f,g) === Hom(f,g)
  -- Hom(target f,g) * Hom(g,source g) === (sign) Hom(f,g)
-*
  restart
  needsPackage "Complexes"
*-
  R = QQ[a..d]
  C = freeResolution minors(3,matrix{{a,b,c,d},{b,c,d,a},{b,d,a,c}})
  D = freeResolution coker matrix{{a^2, b^2, c^2}}
  f1 = a*id_C
  f = map(C[1],C,f1,Degree=>-1)
  g1 = d*id_D
  g = map(D[-3],D,g1,Degree=>3)
  h = f**g

  assert(id_C ** id_D == C ** id_D)
  assert(id_C ** id_D == id_C ** D)
  assert(id_C ** id_D == id_(C**D))
  
  assert((f ** (target g)) * ((source f) ** g) == f**g)
  assert(((target f) ** g) * (f ** (source g)) == (-1)^((degree g) * (degree f)) * (f**g))
///

TEST ///
  -- of isComplexMorphism, isCommutative
-*
  restart
  needsPackage "Complexes"
*-
  R = QQ[a..d]
  C = freeResolution minors(3,matrix{{a,b,c,d},{b,c,d,a},{b,d,a,c}})
  D = freeResolution coker matrix{{a^2, b^2, c^2}}
  f1 = a*id_C
  assert isComplexMorphism f1
  f = map(C[1],C,f1,Degree=>-1)
  assert not isComplexMorphism f
  g1 = d*id_D
  g = map(D[-1],D,g1,Degree=>1)
  h = f**g -- differential anti-commutes with h
  assert not isCommutative h
  assert not isComplexMorphism h
///

TEST ///
-- test: creating complex morphism's
-*
  restart
  needsPackage "Complexes"
*-
  R = QQ[a..d]
  C = freeResolution minors(3,matrix{{a,b,c,d},{b,c,d,a},{b,d,a,c}})
  D = freeResolution coker matrix{{a^2, b^2, c^2}}
  f1 = a*id_C  
  f2 = (b^2-c)*id_D
  assert isComplexMorphism f1
  assert isComplexMorphism f2
  assert isComplexMorphism(f1 ++ f2)
  assert isComplexMorphism Hom(f1,f2)
  assert isComplexMorphism(f1 ** f2)
  assert isComplexMorphism (f1[3])
  assert isComplexMorphism(f1[-1] ** f2)
///

TEST ///
  -- note: inducedMap is here only for backward compatibility
  -- prefer canonicalMap when possible.
-*
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[a..d]
  I = ideal(a^3+b^3+c^3+d^3)
  J = ideal(a+b,c+d)
  C = freeResolution comodule I
  D = freeResolution comodule J
  g = extend(D,C,map(D_0,C_0,1))
  assert isComplexMorphism extend(D,C,map(D_0,C_0,1))
  h = inducedMap(coker g, target g)
  assert isWellDefined h
  assert isComplexMorphism h
  assert(degree h == 0)

  assert(canonicalMap(coker g, target g) == h)
  i = inducedMap(source g, ker g)
  assert(canonicalMap(source h, ker h) ==  inducedMap(source h, ker h))
  
  -- test dual of a complex map
  assert(dual dual g == g)
  assert(isWellDefined dual g)
  assert(isWellDefined dual h)
  assert(isWellDefined dual i)
  assert(dual i == 0)
///


TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[a..d]
  I = ideal(a^2, b^2, c^2, d^2)
  J = ideal(a,b,c,d)
  J1 = monomialIdeal J
  assert isWellDefined complex J
  assert isWellDefined complex J1
  M = (complex S^0)[5]
  assert isWellDefined M
  C = freeResolution comodule I
  D = freeResolution comodule J
  assert isWellDefined C
  assert isWellDefined D
  C1 = complex(I/I)
  assert isWellDefined C1
  assert isWellDefined (dd^C)
  assert isWellDefined (dd^C1)
  C1 ++ C1[3]
  (complex (S^1/I))
  (complex (S/I)^1)
  (complex (S/I))[6]
  assert try (C1 ++ C1[3] ++ (complex (S/I))[6]; false) else true  -- gives error message as desired.
  assert isWellDefined (C1 ++ C1[3] ++ (complex (S^1/I))[6])
  assert isComplexMorphism extend(D,C,map(D_0,C_0,1))
  F = extend(D[4],C[4],map(S^1,S^1,1), (-4,-4))
  assert isComplexMorphism F
  assert isWellDefined F
  F = extend(D,C,map(D_0,C_0,1))
  assert isComplexMorphism F
  assert isWellDefined F
  betti C
  betti D
  betti (C[4])
  betti (C**D)
  assert isHomogeneous F  
  assert isHomogeneous source F
  assert isHomogeneous target F
  kerF = ker F
  assert(prune kerF == 0)
  
  S = ZZ/101[a..d]
  I = ideal(a^2, b^2, c^2, d^2-a)
  J = ideal(a,b,c,d)
  C = freeResolution comodule I
  D = freeResolution comodule J
  assert isComplexMorphism extend(D,C,map(D_0,C_0,1))
  assert isComplexMorphism extend(D[4], C[4], map(S^1,S^1,1), (-4,-4))
  F = extend(D,C,map(D_0,C_0,1))
  assert not isHomogeneous F  
  assert not isHomogeneous source F
  assert isHomogeneous target F

  S = ZZ/101[a..d]
  I = ideal(a^3+b^3+c^3+d^3)
  J = ideal(a+b,c+d)
  C = freeResolution comodule I
  D = freeResolution comodule J
  g = extend(D,C,map(D_0,C_0,1))
  assert isComplexMorphism extend(D,C,map(D_0,C_0,1))

  S = ZZ/101[a..d]
  I = monomialCurveIdeal(S, {1,3,4})
  J = truncate(4, I)
  C = freeResolution comodule J
  D = freeResolution comodule I
  g = extend(D,C,map(D_0,C_0,1))
  assert isComplexMorphism extend(D,C,map(D_0,C_0,1))
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  R = QQ[a..d]
  C = freeResolution minors(3,matrix{{a,b,c,d},{b,c,d,a},{b,d,a,c}})
  D = freeResolution coker matrix{{a^2, b^2, c^2}}
  f1 = a*id_C  
  assert(ker f1 == 0)
  Cf = coker f1
  assert isWellDefined Cf
  imf = image f1
  assert isWellDefined imf

  E = cone f1
  assert isWellDefined E
  F1 = canonicalMap(cone f1, target f1)
  assert isWellDefined F1
  F2 = canonicalMap((source f1)[-1], cone f1)
  assert isWellDefined F2
  assert(F2 * F1 == 0)
  assert(ker F2 == image F1)
  imf2 = prune imf
  g = imf2.cache.pruningMap
  assert(coker g == 0 and ker g == 0)
  
  E = cylinder f1
  assert isWellDefined E
  G1 = canonicalMap(E, target f1, UseTarget=>true)
  G2 = canonicalMap(E, source f1, UseTarget=>false)
  G3 = canonicalMap(target f1, E)
  G4 = canonicalMap(cone f1, E)  
  assert isWellDefined G1
  assert isWellDefined G2
  assert isWellDefined G3
  assert isWellDefined G4
  assert(G4 * G2 == 0)
  assert(kernel G4 == image G2)

  assert(coimage F1 == prune image F1)
  assert(coimage G2 == prune image G2)

  -- ker, coker, image, coimage canonical maps
  f = G2
  h1 = canonicalMap(source f, kernel f)
  h2 = canonicalMap(coimage f, source f)
  h3 = canonicalMap(target f, image f)
  h4 = canonicalMap(cokernel f, target f)
  assert isWellDefined h1
  assert isWellDefined h2
  assert isWellDefined h3
  assert isWellDefined h4
  assert(h2 * h1 == 0)
  assert(kernel h2 == image h1)
  assert(h4 * h3 == 0)
  assert(kernel h4 == image h3)
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  R = ZZ/101[a..f]
  A = freeResolution coker matrix{{a,b}}
  B = freeResolution monomialCurveIdeal(R,{1,2,3})
  C = freeResolution monomialCurveIdeal(R,{1,3,4})

  f = tensorAssociativity(A,B,C);
  isWellDefined f
  assert(ker f == 0)
  assert(coker f == 0)
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-

  S = ZZ/101[a..d, Degrees=>{2:{1,0},2:{0,1}}]
  B = ideal(a,b) * ideal(c,d)
  Ext^1(B, S)
  F = random({1,2}, S)
  f = map(S^1, S^{-degree F}, {{F}})
  assert isHomogeneous f
  g = map(S^1/F, S^1, 1)
  FB = freeResolution comodule B
  Hg = Hom(FB, g)
  Hf = Hom(FB, f)
  assert isWellDefined Hg
  assert isWellDefined Hf
  assert isShortExactSequence(Hg, Hf)
  delta = connectingMap(Hg, Hf)
  assert isWellDefined delta
  delta' = prune delta
  det matrix delta'_-1 
///

TEST ///
  -- Slightly different version, matching construction of vector bundle of
  -- rank 2 on an elliptic curve.  
-*
restart
needsPackage "Complexes"
*-
  S = ZZ/101[x,y,z]
  I = ideal(y^2*z-x^3-x*z^2-z^3)
  OC = S^1/I
  OCp 
  R = S/I
  OCp = coker lift(relations prune Hom(ideal(x,z), R), S)
  basis(0, Ext^1(truncate(1,OC), OCp))

  M = prune truncate(1, Hom(ideal(x,z), R))
  N = truncate(1,R^1)
  E = Ext^1(M, N)
  f = basis(0, E)
  source f
  target f == E

  pses = prune yonedaExtension f
  mods = for i from 0 to 2 list coker lift(relations pses_i, S)
  maps = hashTable for i from 1 to 2 list i => map(mods_(i-1), mods_i, lift(matrix dd^pses_i, S))
  ses = complex maps  
  assert isWellDefined ses
  assert isShortExactSequence(dd^ses_1, dd^ses_2)

  B = module (ideal vars S)^[1]
  FB = freeResolution B
  m1 = Hom(FB, dd^ses_1)
  m2 = Hom(FB, dd^ses_2)
  assert isShortExactSequence(m1,m2)  

  prune HH target m1
  prune HH source m1
  prune connectingMap(m1,m2) 
  assert(oo != 0)

  -- TODO: do more examples, 
  --   make sure that both connecting homom functions are computing
  --   the same thing.  Only then, which is faster?
///

TEST ///
  -- example of computing part.
-*
  restart
  needsPackage "Complexes" 
*-
  kk = ZZ/32003
  S = kk[a..d]
  I = ideal"ab, ad, bc, c3"
  F = freeResolution comodule I
  assert(part(-10, F) == 0)
  assert isWellDefined part(-10, F)
  assert(part(-1, F) == 0)
  assert isWellDefined part(-1, F)
  assert (part(0, F) == complex kk^1)
  assert isWellDefined part(0, F)
  assert (part(1, F) == complex kk^4)
  assert isWellDefined part(1, F)
  assert (C = part(2, F); rank C_0 == 10 and rank C_1 == 3)
  assert isWellDefined part(2, F)
  assert(ring part(2, F) === kk)

  assert (C = part(6, F); 
      (for i from 0 to length C list rank C_i) == {84, 125, 54, 1})
  assert isWellDefined part(6, F)

  kk = ZZ/32003[s,t]
  S = kk[a..d]
  psi = map(kk, S)
  I = ideal"sab, tad, (s-t)bc, tc3"

  isHomogeneous I
  F = freeResolution comodule I

  assert(part(-10, F) == 0)
  assert isWellDefined part(-10, F)
  assert(part(-1, F) == 0)
  assert isWellDefined part(-1, F)
  assert (part(0, F) == complex kk^1)
  assert isHomogeneous part(0, F)
  assert isWellDefined part(0, F)
  assert (part(1, F) == complex kk^4)
  assert isHomogeneous part(1, F) 
  assert isWellDefined part(1, F)
  assert (C = part(2, F); rank C_0 == 10 and rank C_1 == 3)
  assert isHomogeneous part(2, F)
  assert isWellDefined part(2, F)
  assert (C = part(6, F); 
      (for i from 0 to length C list rank C_i) == {84, 125, 68, 15})
  assert isHomogeneous part(6, F)
  assert isWellDefined part(6, F)
  assert(ring part(6, F) === kk)

  C = part(2, F)
  dd^C
  isHomogeneous dd^C_1 -- want this to be true.
///

TEST ///  
-*
  restart
  needsPackage "Complexes"
*-
  -- multi-graded example of part(d, C)
  needsPackage "NormalToricVarieties"
  kk = ZZ/32003
  X = hirzebruchSurface(6, CoefficientRing => kk)
  S = ring X
  F = random({-4,2}, S)
  I = ideal jacobian ideal F
  C = freeResolution I
  Cd = part({1,1}, C)
  assert isWellDefined Cd
  assert(ring Cd === coefficientRing S)
  
  Cd = part({-1,3}, C)
  assert isWellDefined Cd
  assert(prune HH Cd == 0)
  
  J = I + ideal X
  CJ = freeResolution J
  f = randomComplexMap(CJ, C, Cycle => true)
  fd = part({1,1}, f)
  assert isWellDefined fd
  assert(ring fd === coefficientRing S)
  assert isComplexMorphism fd
  prune HH fd
///

TEST ///
-*
  restart
  needsPackage "Complexes" 
*-
  kk = ZZ/32003
  S = kk[a..d]
  I = ideal"ab, ad, bc, c3"
  J = I + ideal"c2"
  FI = freeResolution comodule I
  FJ = freeResolution comodule J
  H = Hom(FI, FJ)
  f = randomComplexMap(FJ, FI, Cycle => true)
  assert isWellDefined f
  assert isCommutative f
  assert isComplexMorphism f

  f2 = part(2, f)
  assert(ring f2 === kk) 
  assert isWellDefined f2
  assert isComplexMorphism f2

  f5 = part(5, f);
  assert(ring f5 === kk) 
  assert isWellDefined f5
  assert isComplexMorphism f5
///

TEST ///
-*
restart
needsPackage "Complexes"
*-
  R = QQ[x];
  X = complex(R^1);
  assert(naiveTruncation(X,-3,-1) == 0)
///

TEST ///
-*
restart
needsPackage "Complexes"
*-
  R = QQ[x];
  X = complex(R^1) ++ complex(R^1)[-2]
  resolution X
///

TEST ///
-*
restart
needsPackage "Complexes"
*-
  R = QQ[x];
  X = complex({map(R^1,R^2,matrix{{1_R,1_R}})})
  resolution X
  resolution(minimize X)
///
