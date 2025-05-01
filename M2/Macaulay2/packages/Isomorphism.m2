newPackage(
    "Isomorphism",
    Version => "2.0",
    Date => "April 30, 2025",
    Headline => "probabilistic test of isomorphism between modules",
    Authors => {
	{ Name => "David Eisenbud", Email => "de@berkeley.edu", HomePage => "https://eisenbud.github.io" },
	{ Name => "Mahrud Sayrafi", Email => "mahrud@umn.edu",  HomePage => "https://mahrud.github.io" }
    },
    Keywords => {"Commutative Algebra", "Homological Algebra"},
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
-- things to move to Core
-----------------------------------------------------------------------------

--leadCoefficient Number := x -> x

-- not strictly speaking the "lead" coefficient, but the first nonzero coefficient
leadCoefficient' = m -> if zero m then 0 else (
    for c to numcols m - 1 do for r to numrows m - 1 do (
	if not zero m_(r,c) then return leadCoefficient m_(r,c)))

-- divides by the coefficient of the leading term
reduceCoefficient = m -> if zero m then m else (
    c := leadCoefficient' m;
    if not isField ring c then return m;
    map(target m, source m, cover m // c, Degree => degree m))

-----------------------------------------------------------------------------
-- randomMinimalDegreeHomomorphism
-----------------------------------------------------------------------------

-- also see degreeZeroSurjection from EagonResolution
-- and isDegreeZeroSurjection and the (deprecated) isIsomorphic in TateOnProducts
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
    map(coker n, coker m, matrix reshape(N0, M0, a), Degree => -d)
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

-- key to cache known isomorphisms
protect Isomorphisms

-- currently we cache isomorphisms ONLY in the source
setIsomorphism = (N, M, f, o) -> (
    f = reduceCoefficient f;
    o = { o.Strict or all(degree f, zero),
	o.Homogeneous or isHomogeneous f };
    M.cache.Isomorphisms ??= new MutableHashTable;
    M.cache.Isomorphisms#(N, o) = f;
    true)

-- we check isomorphisms cached in source OR target
getIsomorphism = (N, M, o) -> (
    if M.cache.?Isomorphisms then
    for isom in keys M.cache.Isomorphisms do (
	(N', o') := isom;
	if N' === N
	and ( not o.Strict      or o.Strict      and o'#0 )
	and ( not o.Homogeneous or o.Homogeneous and o'#1 )
	then return M.cache.Isomorphisms#isom);
    if N.cache.?Isomorphisms then
    for isom in keys N.cache.Isomorphisms do (
	(M', o') := isom;
	if M' === M
	and ( not o.Strict      or o.Strict      and o'#0 )
	and ( not o.Homogeneous or o.Homogeneous and o'#1 )
	then return inverse N.cache.Isomorphisms#isom))

hasIsomorphism = (N, M, o) -> null =!= getIsomorphism(N, M, o)

-- give an isomorphism between two free modules with same degrees
isIsomorphismFree = (N, M0, o) -> (
    if rank N != rank M0 then return false;
    (d1, d2) := (degrees N, degrees M0);
    diffdegs := min d2 - min d1;
    if o.Strict then M := M0 else d2 = degrees(
	M = M0 ** (ring M0)^{diffdegs});
    if sort d1 != sort d2 then
    return if o.Homogeneous then false
    else setIsomorphism(N, M0, map(N, M0, 1), o);
    -- FIXME: because of https://github.com/Macaulay2/M2/issues/3719,
    -- this might not give the most "natural" isomorphism
    p1 := first \ (sortBy last) toList pairs d1;
    p2 := first \ (sortBy last) toList pairs d2;
    setIsomorphism(N, M0, map(N, M0, id_M^p2 // id_N^p1,
	    Degree => -diffdegs), o))

-- returns a minimization M1, presented as
-- a cokernel of m, and an isomorphism back to M
minimization = M -> (
    k := quotient ideal vars ring M;
    m := presentation M;
    iso := if m ** k == 0
    then map(M, M1 := coker m, 1)
    else (
	m = presentation(M1 = prune M);
	M1.cache.pruningMap);
    (M1, m, iso))

-- TODO: hookify and split into strategies, then add strategies that:
-- - check if pruning of the modules is cached already
-- - check if Hilbert polynomial, regularity, dimension, etc. are known
isIsomorphic = method(
    TypicalValue => Boolean,
    Options => {
	Homogeneous => true,  -- if false, allow an inhomogeneous isomorphism
	Strict      => false, -- forces strict equality of degrees
	Verbose     => false,
	Tries       => null,  -- number of attempts at generating a random map
    })
isIsomorphic(Module, Module) := Boolean => o -> (N, M) -> (
    -- returns a pair (false, null) or (true, f), where f is an isomorphism M --> N.
    if M === N or hasIsomorphism(N, M, o) then return true;
    --
    S := ring M;
    if S =!= ring N then error "expected objects over the same ring";
    --
    if isFreeModule M and isFreeModule N then
    return isIsomorphismFree(N, M, o);
    --
    tries := o.Tries ?? defaultNumTries char S;
    if tries > 1 then return any(tries,
	i -> isIsomorphic(N, M, o, Tries => 1));
    --
    if o.Homogeneous == true and not (
	isHomogeneous M and isHomogeneous N)
    then error "inputs not homogeneous";

    (M1, m, isoM) := minimization M;
    (N1, n, isoN) := minimization N;

	--handle the cases where one of M,N is 0
	isZM1 := target m ==0;
	isZN1 := target n ==0;	
	if isZM1 =!= isZN1 then return false;
	if isZM1 and isZN1 then return setIsomorphism(N, M, map(N, M, 0), o);

	-- from now on, M1 and N1 are nonzero and pruned
	if o.Homogeneous then (
	df := checkDegrees (N1,M1,Verbose => o.Verbose, Strict => o.Strict);
	if class df_1 =!= List  then return false);
	--now there is a chance at an isomorphism up to shift, 
	--and df is the degree diff.

    --compute an appropriate random map g
    g := if o.Homogeneous and degreeLength S == 1
    then randomMinimalDegreeHomomorphism(n, m, -df_1_0)
    else (
        H := Hom(M1,N1);       
	kk := ultimate(coefficientRing, S);
	sH := if not o.Homogeneous then H_*
	else select(H_*, f -> degree f == df_1);
	if #sH == 0 then return false;
    	g = sum(sH, f-> random(kk)*homomorphism matrix f)
    );

    if o.Homogeneous and not isHomogeneous g then return false;

	--test g to see if it's surjective:
	kmodule := coker vars S;
	gbar := kmodule ** g;

    if coker gbar != 0 or kernel g != 0 then false
    else setIsomorphism(N, M, isoN * g * inverse isoM, o)
    )
isIsomorphic(Matrix, Matrix) := Boolean => o -> (n, m) -> isIsomorphic(coker m, coker n, o)

isomorphism = method(Options => options isIsomorphic)
isomorphism(Module, Module) := Matrix => o -> (N, M) -> (
    if isIsomorphic(N, M, o) then getIsomorphism(N, M, o)
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
    Mike Stillman and Devlin Mallory contributed to this package.

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
    :Boolean -- whether the modules are isomorphic
  Description
    Text
      In case both modules are homogeneous the program first uses @TO checkDegrees@
      to see whether an isomorphism is possible. This may be an isomorphism up to shift
      if Strict => false (the default) or on the nose if Strict => true.

      If this test is passed, the program uses a variant of the Hom command
      to compute a random map of minimal possible degree from M to N,
      and checks whether this is surjective and injective.
      If @TT "Tries => n"@ is provided, this is attempted $n$ times,
      otherwise a heuristic based on the characteristic of the field is used
      to determine the number of attempts to make (1 in characteristic zero,
      ~10 in characteristic 32003, and ~150 in characteristic 2).

      In the inhomogeneous case (or with Homogeneous => false) the random map is
      a random linear combination of the generators of the module of homomorphisms.

      If the output is false, then the conclusion of non-isomorphism is only probabilistic.
      If the output is true, then as certificate an isomorphism $M \to N$ is cached
      in the module and can be retried using the @TO isomorphism@ method.
    Example
      S = ZZ/32003[x_0..x_3]
      m = random(S^3, S^{4:-2});
      A = random(target m, target m);
      B = random(source m, source m);
      N = coker(A*m*B);
      M = coker m;
      assert isIsomorphic(S^{-3} ** M, M)
      assert not isIsomorphic(S^{-3} ** M, M, Strict => true)
      isIsomorphic(N, M)
      isomorphism(N, M)
    Text
      The following examples checks two well-known isomorphisms in homological algebra.
    Example
      S = ZZ/32003[x_0..x_3]
      I = monomialCurveIdeal(S, {1,3,5})
      c = codim I
      W = Ext^c(S^1/I, S^1)
      H = Hom(S^1/I, S^1/(I_0,I_1))
      isIsomorphic(W, H)
      isomorphism(W, H)
    Example
      mm = ideal gens S
      T1 = Tor_1(W, S^1/(mm^3))
      T2 = Tor_1(S^1/(mm^3), W)
      elapsedTime isIsomorphic(T1, T2)
      elapsedTime isomorphism(T1, T2)
  Caveat
    A negative result means that a random choice of homomorphism
    was not an isomorphism; especially when the ground field is small,
    this may not be definitive. Passing @TT "Tries => N"@ may be helpful.
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
      Note that this isomorphism is already cached if a previous call
      to @TO isIsomorphic@ returned true, but otherwise that function is called.
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
  assert isIsomorphic (S^{-3}**coker m, coker m')
///

TEST ///
-- testing randomMinimalDegreeHomomorphism
  debug needsPackage "Isomorphism"
  S = ZZ/101[x,y]
  m = matrix{{x,y}}
  n = matrix{{x^2, y^2}}

  assert all flatten for a from -2 to 2 list for b from -2 to 2 list (
      M := S^{a}**(n++m);
      N := S^{b}**(m++n);
      (v, diffdegs) = checkDegrees(M, N);
      0 == coker randomMinimalDegreeHomomorphism(M, N, -diffdegs_0)
  )
///

TEST ///
-- the inhomogeneous case
  S = ZZ/101[a,b, Degrees => {{1,0},{0,1}}]
  C = S^{{1,1}, {2,3}}
  C' = S^{{1,1}, {2,4}}
  assert not isIsomorphic(C, C')
  assert isIsomorphic(C,C', Homogeneous => false)
  g = isomorphism(C,C', Homogeneous => false)
  assert isWellDefined g
  assert(source g == C')
  assert(target g == C)
  assert( coker g == 0)
  assert(kernel g == 0)
///

TEST ///
-- the non-minimally presented case
  S = ZZ/101[a,b]
  m = map(S^{2}++S^1, S^{2}++S^{-1}, {{1,0},{0,a}})
  C = coker m
  a = random(target m, target m)
  b = random(source m, source m)
  C' = coker(a*m*b)
  assert isIsomorphic(C,C')
  g = isomorphism(C,C')
  assert isWellDefined g
  assert(source g == C')
  assert(target g == C)
  assert( coker g == 0)
  assert(kernel g == 0)
///

TEST ///
-- checkDegrees
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
  assert isIsomorphic(N, M)
  g = isomorphism(N, M)
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

  assert try ( isIsomorphic(C1,C2); false ) else true
  assert isIsomorphic(C1,C2, Homogeneous => false)
  assert isIsomorphic(C2,C1, Homogeneous => false)
  assert isIsomorphism isomorphism(C1,C2, Homogeneous => false)
  assert try ( isomorphism(C1,C2); false ) else true

  assert isIsomorphic(A1, A2)
  assert isIsomorphism isomorphism(A1,A2)
  assert isIsomorphic(A,A)
  assert isIsomorphic(B1,B1)
  assert not isIsomorphic(A,B1)
  assert not isIsomorphic(A1,B1)
///

TEST ///
  debug Isomorphism
  R = QQ[x,y, DegreeRank => 2]
  M = R^{{1,1}, {0,1}}
  N = R^{{0,1}, {1,1}}
  assert isIsomorphic(N, M, Strict => false, Homogeneous => false)
  f = isomorphism(N, M,     Strict => false, Homogeneous => false)
  assert same(f, map(N, M, {{0, 1}, {1, 0}}), M.cache.Isomorphisms#(N, {true, true}))
  assert isWellDefined f
  assert isHomogeneous f
  assert(source f === M)
  assert(target f === N)
  assert(degree f == {0,0})
  assert isIsomorphic(N, M, Strict => false, Homogeneous => true)
  assert isIsomorphic(N, M, Strict => true,  Homogeneous => false)
  assert isIsomorphic(N, M, Strict => true,  Homogeneous => true)
  f' = isomorphism(M, N,    Strict => false, Homogeneous => false)
  assert same(f', map(M, N, {{0, 1}, {1, 0}}), inverse f)
  assert(1 == #M.cache.Isomorphisms)

  M = R^{{1,1}, {0,1}}
  N = R^{{0,1}, {1,1}} ** R^{{2,2}}
  assert isIsomorphic(N, M, Strict => false, Homogeneous => false)
  f = isomorphism(N, M,     Strict => false, Homogeneous => false)
  assert same(f, map(N, M, {{0, 1}, {1, 0}}, Degree => {-2,-2}), M.cache.Isomorphisms#(N, {false, true}))
  assert isWellDefined f
  assert isHomogeneous f
  assert(source f === M)
  assert(target f === N)
  assert(degree f == {-2,-2})
  assert isIsomorphic(N, M, Strict => false, Homogeneous => true)
  assert isIsomorphic(N, M, Strict => true,  Homogeneous => false)
  f = isomorphism(N, M,     Strict => true,  Homogeneous => false)
  assert same(f, map(N, M, {{1, 0}, {0, 1}}), M.cache.Isomorphisms#(N, {true, false}))
  assert isWellDefined f
  assert not isHomogeneous f
  assert(source f === M)
  assert(target f === N)
  assert not isIsomorphic(N, M, Strict => true, Homogeneous => true)
  assert(2 == #M.cache.Isomorphisms)
///

TEST ///
  debug Isomorphism
  R = QQ[x,y]
  M = coker map(R^{{1}, {0}}, , {{x},{0}})
  N = coker map(R^{{0}, {1}}, , {{0},{x}})
  assert isIsomorphic(N, M, Strict => false, Homogeneous => false)
  f = isomorphism(N, M,     Strict => false, Homogeneous => false)
  -- TODO: ideally, if a homogeneous isomorphism _can_ be found, we should find it
  --assert(f === M.cache.Isomorphisms#(N, {true, true}))
  assert isWellDefined f
  --assert isHomogeneous f
  assert(source f === M)
  assert(target f === N)
  assert(degree f == {0})
  assert isIsomorphic(N, M, Strict => false, Homogeneous => true)
  assert isIsomorphic(N, M, Strict => true,  Homogeneous => false)
  assert isIsomorphic(N, M, Strict => true,  Homogeneous => true)
  f' = isomorphism(M, N,    Strict => false, Homogeneous => false)
  assert(inverse f === f')
  --assert(1 == #M.cache.Isomorphisms)

  N = N ** R^{{2}}
  assert isIsomorphic(N, M, Strict => false, Homogeneous => false)
  f = isomorphism(N, M,     Strict => false, Homogeneous => false)
  --assert(f === M.cache.Isomorphisms#(N, {false, true}))
  assert isWellDefined f
  --assert isHomogeneous f
  assert(source f === M)
  assert(target f === N)
  --assert(degree f == {-2})
  assert isIsomorphic(N, M, Strict => false, Homogeneous => true)
  assert isIsomorphic(N, M, Strict => true,  Homogeneous => false)
  f = isomorphism(N, M,     Strict => true,  Homogeneous => false)
  assert same(f, M.cache.Isomorphisms#(N, {true, false}))
  assert isWellDefined f
  assert not isHomogeneous f
  assert(source f === M)
  assert(target f === N)
  assert not isIsomorphic(N, M, Strict => true, Homogeneous => true)
  --assert(2 == #M.cache.Isomorphisms)
///

TEST ///
  debug Isomorphism
  R = QQ[x,y, DegreeRank => 2]
  M = coker map(R^{{1,1}, {0,1}}, , {{x},{0}})
  N = coker map(R^{{0,1}, {1,1}}, , {{0},{x}})
  assert isIsomorphic(N, M, Strict => false, Homogeneous => false)
  f = isomorphism(N, M,     Strict => false, Homogeneous => false)
  -- TODO: ideally, if a homogeneous isomorphism _can_ be found, we should find it
  --assert(f === M.cache.Isomorphisms#(N, {true, true}))
  assert isWellDefined f
  --assert isHomogeneous f
  assert(source f === M)
  assert(target f === N)
  assert(degree f == {0,0})
  assert isIsomorphic(N, M, Strict => false, Homogeneous => true)
  assert isIsomorphic(N, M, Strict => true,  Homogeneous => false)
  assert isIsomorphic(N, M, Strict => true,  Homogeneous => true)
  f' = isomorphism(M, N,    Strict => false, Homogeneous => false)
  assert(inverse f === f')
  --assert(1 == #M.cache.Isomorphisms)

  N = N ** R^{{2,2}}
  assert isIsomorphic(N, M, Strict => false, Homogeneous => false)
  f = isomorphism(N, M,     Strict => false, Homogeneous => false)
  --assert(f === M.cache.Isomorphisms#(N, {false, true}))
  assert isWellDefined f
  --assert isHomogeneous f
  assert(source f === M)
  assert(target f === N)
  --assert(degree f == {-2,-2})
  assert isIsomorphic(N, M, Strict => false, Homogeneous => true)
  assert isIsomorphic(N, M, Strict => true,  Homogeneous => false)
  f = isomorphism(N, M,     Strict => true,  Homogeneous => false)
  assert same(f, M.cache.Isomorphisms#(N, {true, false}))
  assert isWellDefined f
  assert not isHomogeneous f
  assert(source f === M)
  assert(target f === N)
  assert not isIsomorphic(N, M, Strict => true, Homogeneous => true)
  --assert(2 == #M.cache.Isomorphisms)
///

TEST ///
  R = ZZ/101[x,y,z,w];
  C = res coker vars R
  N = image C.dd_2
  M = ker C.dd_1
  assert isIsomorphic(N, M)

  R = ZZ/101[x,y,z,w, Degrees => {2:{1,0},2:{0,1}}];
  C = res coker vars R
  N = image C.dd_2
  M = ker C.dd_1
  assert isIsomorphic(N, M)
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
