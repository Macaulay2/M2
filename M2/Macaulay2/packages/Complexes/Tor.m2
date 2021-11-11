-- Currently, the definition of Tor is provided by the Core.
-- We anticipate migrating that code and def to this file (June 2021).

Tor(ZZ, Matrix, Module) := Matrix => opts -> (i,f,N) -> (
    R := ring f;
    if not isCommutative R then error "'Tor' not implemented yet for noncommutative rings.";
    if R =!= ring N then error "expected modules over the same ring";
    if i < 0 then map(R^0, R^0, {})
    else if i === 0 then f ** N
    else (
        g := freeResolution(f, LengthLimit => i+1);
        Tsrc := Tor_i(source f, N);
        Ttar := Tor_i(target f, N);
        inducedMap(Ttar, Tsrc, g_i ** N)
        )
    )

Tor(ZZ, Module, Matrix) := Matrix => opts -> (i,M,f) -> (
    R := ring f;
    if not isCommutative R then error "'Tor' not implemented yet for noncommutative rings.";
    if R =!= ring M then error "expected modules over the same ring";
    if i < 0 then map(R^0, R^0, {})
    else if i === 0 then M ** f
    else (
        F := freeResolution(M, LengthLimit => i+1);
        g := F ** f;
        HH_i g
        )
    )

torSymmetry = method()
torSymmetry(ZZ, Module, Module) := Matrix => (i, M, N) -> (
    FM := freeResolution M;
    FN := freeResolution N;
    TorMN := Tor_i(M,N);
    TorNM := Tor_i(N,M);
    alpha := gens TorMN;
    -- The minus sign below is introduced to be compatible with
    -- signs arising in the tensor product of complexes.
    for j from 0 to i-1 do (
        beta := ((dd^FM_(i-j) ** FN_j) * alpha) // 
                    ((-1)^(i-j) * (FM_(i-j-1) ** dd^FN_(j+1)));
        alpha = beta);
    delta := (tensorCommutativity(M, FN_i) * (map(M, FM_0, 1) ** FN_i) * alpha) 
            // inducedMap(FN_i ** M, ker (dd^FN_i ** M));
    map(TorNM, TorMN, delta)
    )

connectingTorMap = method(Options => {Concentration => null})
connectingTorMap(Module, Matrix, Matrix) := ComplexMap => opts -> (M, g, f) -> (
    F := freeResolution M;
    connectingMap(F ** g, F ** f, opts)
    )

connectingTorMap(Matrix, Matrix, Module) := ComplexMap => opts -> (g, f, M) -> (
    (g', f') := horseshoeResolution(g, f);
    connectingMap(g' ** M, f' ** M, opts)
    )

TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  S = ZZ/101[a..f];
  m = genericSymmetricMatrix(S, 3)
  I = trim minors(2, m)
  M = S^1/I  
  N = coker vars S
  f1 = torSymmetry(2,M,N)
  f2 = torSymmetry(2,N,M)
  assert(f1*f2 == 1)
  assert(f2*f1 == 1)

  -- Now we try to relate this to maps of complexes.
  FM = freeResolution M
  FN = freeResolution N
  (lo, hi) = concentration FM
  E = directSum for i from lo to hi list complex(cover HH_i(FM ** N), Base => i)
  FMN = FM ** N
  f = map(FMN, E, i -> map(FMN_i, E_i, gens HH_i(FMN)))
  g = map(FMN, FM ** FN, FM ** augmentationMap FN)
  assert isWellDefined f
  assert isWellDefined g
  assert isQuasiIsomorphism g  
  assert(target f === target g)
  h = liftMapAlongQuasiIsomorphism(f, g)
  assert isWellDefined h
  assert isComplexMorphism h
  assert(g * (f // g) == f)
  MFN = M ** FN;
  g' = map(MFN,  FM ** FN, augmentationMap FM ** FN);
  assert isWellDefined g'
  assert isComplexMorphism g'
  FNM = FN ** M;
  h' = map(FNM, source h, 
      tensorCommutativity(complex M, FN) * g' * h);
  assert isWellDefined h'

  K = ker dd^FNM;
  p = canonicalMap(FNM, K);
  q = inducedMap(HH FNM, K);
  assert(source p === source q)
  r = q * liftMapAlongQuasiIsomorphism(h',p);
  assert(p * (h' // p) == h')
  assert all(lo..hi, i -> matrix r_i == matrix torSymmetry(i, M, N))
///

TEST ///
-*
  restart
  needsPackage "Complexes"
*-
  S = QQ[a];
  C1 = complex {matrix{{1_S,1},{1,1},{1,1}}}
  C2 = complex {matrix{{a,a,a,a},{a,a,a,a},{a,a,a,a},{a,a,a,a},{a,a,a,a}}}
  C12 = C1 ** C2
  C21 = C2 ** C1
  f1 = tensorCommutativity(C1, C2)
  assert isWellDefined f1
  assert isComplexMorphism f1
///

TEST ///
  S = ZZ/101[a..d];
  C = koszulComplex{a,b,c,d}
  D = koszulComplex{a^2,b^3,c^4}
  f1 = tensorCommutativity(C,D)
  assert isWellDefined f1
  assert isComplexMorphism f1
///

TEST ///
  -- Example: where M is not a quotient of S^1.  
  S = ZZ/101[a..f]
  m = genericSymmetricMatrix(S, 3)
  I = trim minors(2, m)
  M = prune truncate(1, S^1/I)
  elapsedTime Tor_2(M,M);
  elapsedTime f1 = torSymmetry(2,M,M);
  elapsedTime f2 = torSymmetry(2,M,M);
  assert(f1*f2 == 1)
  assert(f2*f1 == 1)
///

TEST ///
  S = ZZ/101[a..d];
  I = ideal(c^3-b*d^2, b*c-a*d)
  J = ideal(a*c^2-b^2*d, b^3-a^2*c)
  g = map(S^1/(I+J), S^1/I ++ S^1/J, {{1,1}})
  f = map(S^1/I ++ S^1/J, S^1/intersect(I,J), {{1},{-1}})
  assert isShortExactSequence(g,f)
  kk = coker vars S
  delta = connectingTorMap(kk, g, f)
  delta' = connectingTorMap(g, f, kk)
  assert isWellDefined delta
  assert isWellDefined delta'
  F = freeResolution kk
  LES = longExactSequence(F ** g, F ** f);
  assert all(3, i -> dd^LES_(3*(i+1)) == delta_(i+1))
  assert(HH LES == 0)
  -- another way
  (g',f') = horseshoeResolution(g,f);
  assert isShortExactSequence(g',f')
  LES' = longExactSequence(g' ** kk, f' ** kk);
  assert(HH LES' == 0)
  assert all(3, i -> dd^LES'_(3*(i+1)) == delta'_(i+1))
  -- now we show commutativity of some squares 
  -- which show that delta, delta' are isomorphic.
  h2 = torSymmetry(1, kk, source f) * delta_2
  ts = torSymmetry(2, kk, target g)
  ts' = map(source delta'_2, source ts, ts)
  h2' = delta'_2 * ts'
  assert(h2 == h2')
  -- now for delta_3
  h3 = torSymmetry(2, kk, source f) * delta_3
  ts3 = torSymmetry(3, kk, target g)
  ts3' = map(source delta'_3, source ts3, ts3)
  h3' = delta'_3 * ts3'
  assert(h3 == h3')
///
