debug needsPackage "FGLM" -- for LUincremental

-- e.g. given a tower such as K[x][y]/I, returns K
-- TODO: for ambient GF 4 returns ZZ/2, which may not be desired
-- this is because isField doesn't actually test being a field
-- TODO: should localRandom use this?
groundField = method()
groundField Ring := R -> (
    K := ultimate(K -> if isField K then K else coefficientRing K, R);
    if K =!= ZZ then K else error "no ground field found")

-- e.g. given GF(4) or ZZ/2[a]/(a^2+a+1) returns {0, 1, a, a + 1}
fieldElements = method()
fieldElements QuotientRing := R -> R.cache.elements ??= (
    --if not isField R then error "expected a field";
    if isFinitePrimeField R then apply(R.order, i -> i_R)        -- if R = ZZ/p
    else join({0_R, x := 1_R}, while (x = R_0 * x) != 1 list x)) -- if R = GF(q)
fieldElements GaloisField  := GFq -> GFq.cache.elements ??= (
    prepend_(0_GFq) apply(GFq.order - 1, i -> GFq_0^i))

-- e.g. given a field isomorphic to GF(p,e), returns e
-- FIXME: doesn't work for ZZ/2[a]/(a^2+a+1), since groundField gives ZZ/2
fieldExponent = method()
fieldExponent GaloisField  := GFq -> GFq.degree
fieldExponent QuotientRing := R -> (
    L := groundField R;
    (p, e) := (char L, 1);
    if p == 0 then return e;
    a := L_0; -- primitive element of L
    while true do (
	if a^(p^e) != a
	then e = e + 1
	else break e))

TEST ///
  debug needsPackage "DirectSummands"

  assert try groundField(ZZ/4) else true
  assert(groundField(ZZ/5) === ZZ/5)
  assert(groundField(GF 5) === GF 5)
  assert(groundField(GF 4) === GF 4)
  -- TODO
  --assert(groundField ambient GF 5 === ???)
  --assert(groundField ambient GF 4 === ???)

  assert try fieldElements(ZZ/4) else true
  assert(fieldElements(ZZ/5) == apply(5, identity))
  assert(fieldElements(GF 5) == {0, 1, 2, -1, -2})
  assert(fieldElements(GF 4) == {0, 1, a, a + 1})
  assert(fieldElements ambient GF 5 == {0, 1, 2, -1, -2})
  assert(fieldElements ambient GF 4 == {0, 1, a, a + 1})

  assert try fieldExponent(ZZ/4) else true
  assert(fieldExponent(ZZ/5) == 1)
  assert(fieldExponent(GF 5) == 1)
  assert(fieldExponent(GF 4) == 2)
  assert(fieldExponent(GF 8) == 3)
  assert(fieldExponent ambient GF 5 == 1)
  -- FIXME: assert(fieldExponent ambient GF 4 == 2)
///

-- given {({e},c),...} make c*m^e + ...
evalListForm = (L, m) -> sum(L, (e, c) -> c * m ^ (first e))

-- given {a,b,c,...} make a+bt+ct^2+...
polynomial = (L, t) -> evalListForm(apply(#L, i -> ({i}, L#i)), t)

-- TODO: adjust RingElement Array to check if the input is a Matrix?
RingElement Matrix := (f, m) -> evalListForm(listForm f, m)

-- FIXME: given r in CC and m in RR[t], this doesn't work
-- RingElement * Matrix := (r, m) -> (
--     if ring r =!= ring m then try r = promote(r, ring m) else m = promote(m, ring r);
--     map(target m, source m, reduce(target m, raw r * raw m)))

-- finds the characteristic polynomial of a matrix mod the maximal ideal
char Matrix := A -> A.cache.char ??= (
    if source A =!= target A then error "expected an endomorphism";
    b := symbol b;
    T := (groundField ring A)(monoid[b]);
    B := sub(cover A, T);
    I := id_(source B);
    -- TODO: this is a major step in large examples
    det(B - T_0 * I, Strategy => Bareiss))

minimalPolynomial' = A -> A.cache.minimalPolynomial ??= (
    kk := ring A;
    t := local t;
    T := kk(monoid[t]);
    -- naive exact method
    B := id_(target A);
    m := transpose flatten B;
    while syz m == 0 do m |= transpose flatten(B *= A);
    -- naive probabilistic method
    -- m := v := random(target A, kk^1);
    -- while syz m == 0 do m |= (v = A * v);
    lambda := (syz m)_{0};
    lambda  = lambda // last entries lambda_0;
    polynomial(entries lambda_0, T_0))

minimalPolynomial = A -> A.cache.minimalPolynomial ??= (
    kk := ring A;
    t := local t;
    T := kk(monoid[t]);
    -- incremental method
    n := numcols A;
    N := n^2;
    P := toList(0..N-1);
    B := id_(target A);
    v := mutableMatrix reshape(kk^N, kk^1, B);
    LU := mutableMatrix map(kk^N, kk^(n+1), 0);
    LUincremental(P, LU, v, s := 0);
    while not isZero LU_(s, s) do (
	s = s + 1;
	v = mutableMatrix reshape(kk^N, kk^1, B *= A);
	P = LUincremental(P, LU, v, s));
    lambda := mutableMatrix map(kk^s, kk^1, 0);
    backSub(submatrix(LU, toList(0..s-1), toList(0..s)), lambda, s);
    T_{s} - polynomial(flatten entries lambda, T_0))

projectorsFromMinimalPolynomial = (f, mp) -> (
    --F := groundField ring mp;
    --if groundField ring f =!= F then f = extendGroundField(F, f);
    -- FIXME: see the RingElement * Matrix issue above
    -- if instance(F, InexactField) then return apply(roots mp,
    -- 	y -> minimalProjectorFromEigenvalue(f - y, f - y));
    L := select(value \ toList factor mp,
	p -> degree mp > degree p and degree p > {0});
    apply(L, p -> evalListForm(listForm p, f)))

-- TODO: is there an efficient algorithm to find multiplicity
-- of an eigenvalue in the minimal polynomial? Short of that,
-- given f (or f minus eigenvalue), this finds smallest f^(2^(k+n))
-- such that 2^k \geq multiplicity of 0 in the min. poly. of f
-- and also 2^n \geq number of degree blocks in f, so that all
-- entries corresponding to eigenvalue 0 in the top-right block vanish
minimalProjectorFromEigenvalue = (f, fm) -> (
    c := 2^(ceiling log_2 length unique degrees target f);
    e := 1; while rank ker fm != rank ker(fm = fm * fm) do e += e; f^(c*e))

-- TODO: is it faster to search over fieldElements for finite fields?
roots' = f -> (
    R := ring f;
    p := char R;
    L := listForm f;
    F := groundField R;
    if instance(F, InexactField) then return roots f;
    -- fallback for characteristic 0 or very large fields
    if p == 0 or not F.?order or F.order > 1000
    then flatten rationalPoints ideal f
    else select(fieldElements F, e -> zero evalListForm(L, e)))

-- Linear algebra 101+epsilon algorithm
eigenvalues' = A -> roots' minimalPolynomial A

-- Naive search over finite fields
eigenvalues'' = A -> (
    R := ring A;
    p := char R;
    F := groundField R;
    I := id_(target A);
    -- fallback to LA101 algorithm for characteristic 0 or very large fields
    if p == 0 or not F.?order or F.order > 1000 then return eigenvalues' A;
    select(fieldElements F, e -> zero det(A - e * I)))

isNilpotent = A -> A == 0 or (
    -- TODO: is this the fastest way?
    mp := minimalPolynomial A;
    mp == (ring mp)_(degree mp))

end--

check "DirectSummands"

///
  restart
  debug needsPackage "DirectSummands"

  K = ZZ/32003;
  f = random(K^50, K^50);
  elapsedTime eigenvalues'' f -- 0.02s
  elapsedTime eigenvalues' f  -- 5s
  profileSummary

  R = K[b]
  f = random(R^250, R^250);
  elapsedTime select(7, i -> 0 == det(f - i * id_(target f))) -- only 33s for 250x250 over ZZ/7
  elapsedTime select(K.order, i -> 0 == det(f - K_0^i * id_(target f))) -- only 33s for 250x250 over ZZ/7

  scan(20, n -> elapsedTime eigenvalues' random(R^(n+5), R^(n+5)));
///

restart
debug needsPackage "DirectSummands"
  R = ZZ/32003[x,y,z]/(x^3, x^2*y, x*y^2, y^4, y^3*z)
  C = res(coker vars R, LengthLimit => 3)
  D = res(coker transpose C.dd_3, LengthLimit => 3)
  M = coker D.dd_3
  errorDepth=2
findProjectors M

K = quotient ideal gens R;
F = groundField R
elapsedTime h = generalEndomorphism M;
A = h0 = sub(K ** cover h, groundField R)

f = minimalPolynomial h0
f(h0)
tally apply(100, i -> (minimalPolynomial h0) h0)

elapsedTime f = minimalPolynomial h0
assert(f(h0) == 0)

elapsedTime eigenvalues' h0
elapsedTime benchmark"eigenvalues'' h0"
elapsedTime benchmark"eigenvalues''' h0"

I = h0^0
L = listForm f
elapsedTime select(fieldElements F, e -> zero det(h0 - e * I))
elapsedTime select(fieldElements F, e -> zero evalListForm(L, e))
elapsedTime roots' f
