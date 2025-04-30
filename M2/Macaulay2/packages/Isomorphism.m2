newPackage(
    "Isomorphism",
    Version => "1.0",
    Date => "April 27, 2022",
    Headline => "Probabilistic test of isomorphism between modules",
    Authors => {{Name => "David Eisenbud", 
                  Email => "de@msri.org", 
                  HomePage => "http://www.msri.org/~de"}
                  },
    Keywords => {"Commutative Algebra", "Homological Algebra", "Projective Algebraic Geometry"},
    DebuggingMode => false
    )

export {
    "isIsomorphic",
    "isomorphism",
    "checkDegrees",
    }

importFrom_Core {
    "sortBy",
}

-* Code section *-

-----------------------------------------------------------------------------
-- randomMinimalDegreeHomomorphism
-----------------------------------------------------------------------------

randomMinimalDegreeHomomorphism=method()
randomMinimalDegreeHomomorphism(Matrix, Matrix, ZZ) := Matrix => (n,m,d) -> (
    --m,n homogeneous, minimal over a ring with degree length 1 (this restridd
    
    --given free presentations
    --M = coker m:M1 -> M0 
    --N = coker n: N1 -> N0
    --(ring M)^diffdegs has the same degrees as N,
    --so if diffdegs  is positive, then 
    -- M is generated in higher degrees than N, and
    --the iso M->N has degree -diffdegs
    
    --efficiently compute 
    --the matrices of a random degree -diffdegs 
    --homomorphisms M -> N of degree  -diffdegs.
    
    --check that the hypotheses are satisfied:
    S := ring m;    
    resField := S^1/(ideal gens S);
    if not 
       (degreeLength S == 1 and 
	isHomogeneous m and 
	isHomogeneous n and
	m**resField == 0 and
	n**resField == 0) then 
	error"Unsuitable ring, modules or presentations.";
	
    M0:= target m;
    m' := transpose m;
    M0' := source m';
    M1' := target m';
    N0 := target n;

    h := syz(m'**N0 | M1'**n, 
             SyzygyRows => rank N0*rank M0', 
	     DegreeLimit => -{d});
	 
    p := positions(degrees source h, e -> e == -{d});
    hp := h_p;
    a := hp*random(source (hp), S^{d}); --represents general map of degree -diffdegs
    map(coker n, coker m, matrix reshape(N0, M0, a))
    )

-----------------------------------------------------------------------------
-- checkDegrees
-----------------------------------------------------------------------------

-- test whether a list of lists has all zero entries
isDegreeListZero = L -> all(L, s -> all(s, e -> e === 0))

checkDegrees = method(Options => {
	Verbose => false,
	Strict  => false, -- make the homogeneous case preserve degrees
    })
checkDegrees(Module, Module) := Sequence => o -> (A,B) -> (
    v := o.Verbose;
    S := ring A;
    mm := ideal gens S;
    Abar := A/(mm*A);
    Bbar := B/(mm*B);
    if numgens Abar != numgens Bbar then (
		if v then <<"numbers of generators are different"<<endl;
	        return (false, null)
	        );
    if not (isHomogeneous A and isHomogeneous B) then (
                if v then <<"numbers of generators agree"<<endl;
		return (true,{"inhomogeneous"}));
	    
	dA := sort degrees Abar;
        dB := sort degrees Bbar;
	if o.Strict == true and not dA == dB then return(false, );
	
        degdiffs := for i from 0 to #dA-1 list dA_i-dB_i;
        matches := all(degdiffs, s-> s == degdiffs_0);
        if matches then(
        	--now the degrees of the generators are equal.
        if v and not isDegreeListZero degdiffs then 
	       <<"To make the degree sequences equal, tensor "<<A<<"with ring " << A << "to " << {dA_0-dB_0} <<endl;
               return (true, dA_0-dB_0)
	               );
	        --now matches == false
  	if v then <<"degree sequences don't match"<<endl;
	(false, null)
    )
checkDegrees(Matrix, Matrix) := Sequence => o-> (m,n) -> checkDegrees(target m, target n, o)

-----------------------------------------------------------------------------
-- isIsomorphism and isomorphism
-----------------------------------------------------------------------------

-- the default number of attempts for randomized algorithms
-- e.g. 145 tries in char 2, 10 in char 32003, and 1 in char 0
defaultNumTries = p -> ceiling(0.1 + 100 / log p)
--apply({2, 32003, 0}, defaultNumTries)

-- give an isomorphism between two free modules with same degrees
-- FIXME: because of https://github.com/Macaulay2/M2/issues/3719,
-- this might not give the most "natural" isomorphism
isIsomorphismFree = (N, M0, o) -> (
    if rank N != rank M0 then return (false, null) else
    if not o.Homogeneous then return (true, map(N, M0, 1));
    (d1, d2) := (degrees N, degrees M0);
    if o.Strict then M := M0 else d2 = degrees(
	M = M0 ** (ring M0)^{min d2 - min d1});
    if sort d1 != sort d2 then return (false, null);
    p1 := first \ (sortBy last) toList pairs d1;
    p2 := first \ (sortBy last) toList pairs d2;
    (true, map(N, M0, id_M^p2 // id_N^p1)))

-- key to cache known isomorphisms
protect Isomorphisms

isIsomorphic = method(Options => {
	Homogeneous => true,  -- if false, allow an inhomogeneous isomorphism
	Strict      => false, -- forces strict equality of degrees
	Verbose     => false,
	Tries       => null,  -- number of attempts at generating a random map
    })
isIsomorphic(Module, Module) := Sequence => o -> (N, M) -> (
    -- returns a pair (false, null) or (true, f), where f is an isomorphism M --> N.
    if M === N then return (true, id_M);
    --
    M.cache.Isomorphisms ??= new MutableHashTable;
    if M.cache.Isomorphisms#?N then
    return (true, M.cache.Isomorphisms#N);
    --
    S := ring M;
    if S =!= ring N then error "expected objects over the same ring";
    --
    if isFreeModule M and isFreeModule N then
    return isIsomorphismFree(N, M, o);
    --
    tries := o.Tries ?? defaultNumTries char S;
    if tries > 1 then (
	for c to tries - 1 do (
	    (bool, isom) := isIsomorphic(N, M, o, Tries => 1);
	    if bool then return (true, M.cache.Isomorphisms#N = isom));
	return (false, null));
    --
        if o.Homogeneous == true and 
	        not (isHomogeneous M and isHomogeneous N) then 
	        error"inputs not homogeneous";
	resS := S/(ideal gens S);

    	m := presentation M;

	if m**resS == 0 then 
	    (M1 := M; 
	     m1 := map(M, coker m, 1)) else
	     (m = presentation (M1 = prune M);
	     m1 = M1.cache.pruningMap
    	     );--iso from M1 to M
	 
    	n := presentation N;
	if n**resS == 0 then 
	    (N1 := N;
	    n1 := map(N,coker n, 1)) else
	    (n = presentation (N1 = prune N);
	    n1 = N1.cache.pruningMap); --iso from N1 to N

	--handle the cases where one of M,N is 0
	isZM1 := target m ==0;
	isZN1 := target n ==0;	
    	if isZM1 =!= isZN1 then return (false, null );
	if isZM1 and isZN1 then return (true, map(N,M,0));

	-- from now on, M1 and N1 are nonzero and pruned
	if o.Homogeneous then (
	df := checkDegrees (N1,M1,Verbose => o.Verbose, Strict => o.Strict);
	if class df_1 =!= List  then return (false, null));
	--now there is a chance at an isomorphism up to shift, 
	--and df is the degree diff.

	--compute an appropriate random map g
	if o.Homogeneous and degreeLength S == 1 then
	g := randomMinimalDegreeHomomorphism(n,m, -df_1_0) else (
        H := Hom(M1,N1);       
	kk := ultimate(coefficientRing, S);
	if o.Homogeneous === true then
	      sH := select(H_*, f-> degree f == -df_1) else 
	      sH = H_*;
	if #sH == 0 then return false;
    	g = sum(sH, f-> random(kk)*homomorphism matrix f)
	);
        
	--test g to see if it's surjective:
	kmodule := coker vars S;
	gbar := kmodule ** g;

	t1 := prune coker gbar == 0;
	if t1 == false then return (false, null);
	
	t2 := prune ker g == 0;
	if t2 then (true, n1*g*(m1^-1)) else (false, null)
    )
isIsomorphic(Matrix, Matrix) := Sequence => o -> (n, m) -> isIsomorphic(coker m, coker n, o)

isomorphism = method(Options => options isIsomorphic)
isomorphism(Module, Module) := Matrix => o -> (N, M) -> (
    if first isIsomorphic(N, M, o) then M.cache.Isomorphisms#N
    else error "modules are not isomorphic")

-----------------------------------------------------------------------------
-* Documentation section *-
-----------------------------------------------------------------------------

beginDocumentation()

doc ///
Node
  Key
    Isomorphism
  Headline
    probabilistic test for isomorphism of modules
  Description
    Text
      Two modules are isomorphic if there is a surjection in each direction.
      These routines produce random combinations of the generators of @TO Hom@
      and test whether these are surjections.
  SeeAlso
    isIsomorphic
    isomorphism
    checkDegrees
  Contributors
    Mike Stillman

Node
  Key
    checkDegrees
   (checkDegrees, Module, Module)
   (checkDegrees, Matrix, Matrix)
   [checkDegrees, Verbose]
  Headline
    compares the degrees of generators of two modules
  Usage
    d = checkDegrees(N,M)
    d = checkDegrees(n,m)
  Inputs
    N:Module
    n:Matrix -- presentation of N
    M:Module
    m:Matrix -- presentation of M
  Outputs
    d:Sequence
      (Boolean, a degree in the ring of M and N)
  Description
    Text
      This is to be used with @TO isIsomorphic@.

      The routine compares the sorted lists of degrees of generators of the two modules;
      the degreeLength (can be anything).
      If the numbers of generators of M,N are different, the modules are not isomorphic,
      and the routine returns (false, null).

      If the numbers are the same, and all the corresponding degrees pairs differ
      by the same amount (so that the modules might become isomorphic after a shift,
      then if Strict => false (the default)
      the output (true, e) tells how to adjust the modules to make the degrees equal:
      either tensor N with (ring N)^{e} or tensor M with (ring M)^{-e}.

      If Strict => true, then the output is (false, null) unless
      the offset e is 0.
    Example
      S = ZZ/101[a,b,Degrees => {{1,0},{0,1}}]
      A = S^{{2,1}}
      B = S^{{1,1}}
      B' = S^{{3,3}}**B
      C = S^{{1,1}, {2,3}}
      checkDegrees(A,B)
      checkDegrees(A,C)

      d = checkDegrees(B',B)
      degrees (S^{d_1}**B') == degrees B
      degrees (B') == degrees (S^{-d_1}**B)
      checkDegrees(B',B,Strict=>true)
  SeeAlso
    isIsomorphic
    [checkDegrees, Strict]

Node
  Key
    [checkDegrees, Strict]
  Headline
    forces strict equality of degrees
  Usage
    d = checkDegrees(N, M, Strict =>true)
  Description
    Text
      Used with Strict=>false, the default,
      checkDegrees(N,M) returns (true, deg) if
      degrees M and degrees N are equal up to a shift d.
      With Strict => true the degree lists must be equal.
    Example
      S = ZZ/101[a,b,Degrees => {{1,0},{0,1}}]
      B = S^{{1,1}}
      B' = S^{{3,3}}**B
      d = checkDegrees(B',B)
      degrees (S^{d_1}**B') == degrees B
      degrees (B') == degrees (S^{-d_1}**B)
      checkDegrees(B',B,Strict=>true)
  SeeAlso
    checkDegrees

Node
  Key
    isIsomorphic
   (isIsomorphic, Module, Module)
   (isIsomorphic, Matrix, Matrix)
   [isIsomorphic, Verbose]
   [isIsomorphic, Homogeneous]
   [isIsomorphic, Strict]
   [isIsomorphic, Tries]
  Headline
    probabilistic test for isomorphism of modules
  Usage
    t = isIsomorphic(N, M)
    t = isIsomorphic(n, m)
  Inputs
    M:Module
    m:Matrix -- presentation of M
    N:Module
    n:Matrix -- presentation of N
    Homogeneous => Boolean
    Verbose     => Boolean
    Strict      => Boolean
    Tries       => ZZ  -- the number of attempts at generating random map
  Outputs
    :Sequence
      (true, Matrix) or (false, null)
  Description
    Text
      In case both modules are homogeneous the program first uses @TO checkDegrees@
      to see whether an isomorphism is possible. This may be an isomorphism up to shift
      if Strict => false (the default) or on the nose if Strict => true.

      If this test is passed, the program uses a variant of the Hom command
      to compute a random map of minimal possible degree from M to N,
      and checks whether this is surjective and injective.

      In the inhomogeneous case (or with Homogeneous => false) the random map is
      a random linear combination of the generators of the module of homomorphisms.

      If the output has the form (true, g), then g is guaranteed to be an
      isomorphism. If the output is (false, null), then the
      conclusion of non-isomorphism is only probabilistic.
    Example
      setRandomSeed 0
      S = ZZ/32003[x_0..x_3]
      m = random(S^3, S^{4:-2});
      A = random(target m, target m)
      B = random(source m, source m)
      m' = A*m*B;
      isIsomorphic (S^{-3}**coker m, coker m)
      isIsomorphic (S^{-3}**coker m, coker m, Strict => true)
      isIsomorphic (coker m, coker m')
    Text
      The following example checks two of the well-known isomorphism
      in homological algebra.
    Example
      setRandomSeed 0
      S = ZZ/32003[x_0..x_3]
      I = monomialCurveIdeal(S,{1,3,5})
      codim I
      W = Ext^2(S^1/I, S^1)
      W' = Hom(S^1/I, S^1/(I_0,I_1) )
      isIsomorphic(W,W')
      mm = ideal gens S
      (isIsomorphic(Tor_1(W, S^1/(mm^3)), Tor_1(S^1/(mm^3), W)))_0
  Caveat
    A negative result means that a random choice of homomorphism
    was not an isomorphism; especially when the ground field is small,
    this may not be definitive.
  SeeAlso
    checkDegrees

Node
  Key
    isomorphism
   (isomorphism, Module, Module)
   [isomorphism, Verbose]
   [isomorphism, Homogeneous]
   [isomorphism, Strict]
   [isomorphism, Tries]
  Headline
    retrieve an isomorphism of modules
  Usage
    f = isomorphism(N, M)
  Inputs
    M:Module
    N:Module
    Homogeneous => Boolean -- if false, allow an inhomogeneous isomorphism
    Verbose     => Boolean -- whether to print debugging information
    Strict      => Boolean -- whether to force strict equality of degrees
    Tries       => ZZ      -- the number of attempts at generating random map
  Outputs
    f:Matrix -- an isomorphism $M \to N$
  Description
    Text
      This method retrieves the isomorphism between the given modules.
    Example
      S = ZZ/32003[x_0..x_3]
      m = random(S^3, S^{4:-2});
      A = random(target m, target m);
      B = random(source m, source m);
      N = coker(A*m*B);
      M = coker m;
      f = isomorphism(N, M)
      isIsomorphism f
  SeeAlso
    isIsomorphic
    isIsomorphism
///

-----------------------------------------------------------------------------
-* Test section *-
-----------------------------------------------------------------------------

TEST ///
-- getting the degree shift right
  S = ZZ/32003[x_1..x_3]
  m = random(S^3, S^{4:-2})
  A = random(target m, target m)
  B = random(source m, source m)
  m' = A*m*B
  assert(checkDegrees (S^{-3}**coker m, coker m') == (true, {3}))
  assert((isIsomorphic (S^{-3}**coker m, coker m'))_0 == true)
///

TEST ///
-- testing randomMinimalDegreeHomomorphism
  debug needsPackage "Isomorphism"
  S = ZZ/101[x,y]
  m = matrix{{x,y}}
  n = matrix{{x^2, y^2}}

  setRandomSeed 0
  assert all flatten for a from -2 to 2 list for b from -2 to 2 list (
      M := S^{a}**(n++m);
      N := S^{b}**(m++n);
      (v, diffdegs) = checkDegrees(M, N);
      0 == coker randomMinimalDegreeHomomorphism(M, N, -diffdegs_0)
  )
///

TEST ///
-- the inhomogeneous case
  setRandomSeed 0
  S = ZZ/101[a,b, Degrees => {{1,0},{0,1}}]
  C = S^{{1,1}, {2,3}}
  C' = S^{{1,1}, {2,4}}
  assert not first isIsomorphic(C, C')
  (t,g) = isIsomorphic(C,C', Homogeneous => false)
  assert t
  assert isWellDefined g
  assert(source g == C')
  assert(target g == C)
  assert( coker g == 0)
  assert(kernel g == 0)
///

TEST ///
-- the non-minimally presented case
  setRandomSeed 0
  S = ZZ/101[a,b]
  m = map(S^{2}++S^1, S^{2}++S^{-1}, {{1,0},{0,a}})
  C = coker m
  a = random(target m, target m)
  b = random(source m, source m)
  C' = coker(a*m*b)
  (t,g) = isIsomorphic(C,C')
  assert t
  assert isWellDefined g
  assert(source g == C')
  assert(target g == C)
  assert( coker g == 0)
  assert(kernel g == 0)
///

TEST ///
-- checkDegrees
  setRandomSeed 0
  S = ZZ/101[a,b, Degrees => {{1,0},{0,1}}]
  A = S^{{2,1}}
  B = S^{{1,1}}
  B' = S^{{3,3}}**B
  C = S^{{1,1}, {2,3}}

  checkDegrees(A,B)
  assert(checkDegrees(A,B) == (true, {-1,0}))
  assert(checkDegrees(A,C) == (false, null))

  d = checkDegrees(B',B)
  assert(degrees B  == degrees(S^{d_1}**B'))
  assert(degrees B' == degrees(S^{-d_1}**B))
  assert(checkDegrees(B',B, Strict => true) == (false, null))
///

TEST ///
-- isIsomorphic
  needsPackage "Points"
  canonicalIdeal = method()
  canonicalIdeal Ideal := Ideal => I ->(
      S := ring I;
      R := S/I;
      F := res I;
      omega := coker sub(transpose F.dd_(length F), R);
      H := Hom(omega,R^1);
      deg := max degrees source gens H;
      g := (gens H)*random(source gens H, R^-deg);
      trim sub(ideal g,R) ---canonical ideal of a 1-dim ring.
  )

  kk = ZZ/32003
  S = kk[x,y,z]

  d = 15
  I = points randomPointsMat(S,d);
  elapsedTime W = canonicalIdeal I;
  R = ring W;
  n =2
  M = module(trim W^n)
  N = Hom(M, R^1);
  (t,g) = isIsomorphic (N, M);
  assert t
  assert isWellDefined g
  assert(source g == M)
  assert(target g == N)
  assert( coker g == 0)
  assert(kernel g == 0)
///

TEST ///
  S = ZZ/101[a,b, Degrees => {{1,0},{0,1}}]
  A = S^{{2,1}}
  B1 = S^{{1,1}}
  B = S^{{1,1}, {2,3}}
  d = checkDegrees(A,B, Verbose => true)
  assert(d == (false, null))
  d = checkDegrees(A,B1)
  B' = S^{{3,3}}**B
  d = checkDegrees(B',B)
  e = checkDegrees(S^{d_1}**B', B)
  e = checkDegrees(B', S^{-d_1}**B)
  assert (e_0 == true)
  ---
  S = ZZ/101[a,b, Degrees => {{1,0},{0,1}}]
  C = S^{{1,1}}
  B = S^{{1,1}, {2,3}}
  A = coker random(B, S^{2:-{2,2}})
  assert(checkDegrees(A,B) == (true, {0,0}))
  assert(checkDegrees(A,C) == (false, ))
  assert(checkDegrees(B,C) == (false, ))
///

TEST ///
  S = ZZ/32003[a,b,Degrees => {{1,0},{0,1}}]
  B1 = S^{{1,1}}
  B = S^{{1,1}, {2,3}}
  A = coker random(B, S^{2:-{3,3}})
  A1 = coker (a = random(B1^3, S^{2:-{3,3}}))
  A2 = coker (random(target a, target a)*a*random(source a,source a))
  C1 = coker (a = random(B1^3, S^{2:-{3,3}, -{4,5}}))
  C2 = coker (matrix random(S^3, S^3)*matrix a*matrix random(S^3,S^3))

  assert try ( isIsomorphic(C1,C2); false) else true
  assert first isIsomorphic(C1,C2, Homogeneous => false)
  assert first isIsomorphic(C2,C1, Homogeneous => false)
  assert isIsomorphism last isIsomorphic(C1,C2)

  assert first isIsomorphic(A1, A2)
  assert isIsomorphism last isIsomorphic(A1,A2)
  assert first isIsomorphic(A,A)
  assert first isIsomorphic(B1,B1)
  assert not first isIsomorphic(A,B1)
  assert not first isIsomorphic(A1,B1)
///

end--

-----------------------------------------------------------------------------
-* Development section *-
-----------------------------------------------------------------------------

uninstallPackage "Isomorphism"
restart
check "Isomorphism"
installPackage "Isomorphism"
viewHelp "Isomorphism"
