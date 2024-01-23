--needsPackage "PushForward"
--needsPackage "Polyhedra" -- for lattice points
--needsPackage "Complexes"

myPushForward = (f, M) -> (
    directSum apply(cachedSummands M,
	N -> pushFwd(f, N))
    -- doesn't work for matrices:
    -- pushForward(f, M)
    -- pushForward(f, M, UseHilbertFunction => false)
    )

protect FrobeniusRing
protect FrobeniusFormation
frobeniusRing = method(TypicalValue => Ring)
frobeniusRing(ZZ, Ring) := (e, R) -> (
    if not R.?cache then R.cache = new CacheTable;
    (Rp0, e0) := if R.cache.?FrobeniusFormation then R.cache.FrobeniusFormation else (R, 0);
    if Rp0.cache#?(symbol FrobeniusRing, e0 + e) then Rp0.cache#(symbol FrobeniusRing, e0 + e)
    else Rp0.cache#(symbol FrobeniusRing, e0 + e) = (
	Rpe := newRing(Rp0, Degrees => (char Rp0)^(e0 + e) * degrees Rp0);
	Rpe.cache = new CacheTable;
	Rpe.cache.FrobeniusFormation = (Rp0, e0 + e);
	Rpe)
    )

-- FIXMEEE:
quotient Ideal := QuotientRing => opts -> (cacheValue symbol quotient) (I -> (ring I) / I)
--ideal Ring := (cacheValue symbol ideal) (R -> ideal map(R^1, R^0, 0))

-- FIXME: this might forget some information
RingMap ** Module := Module => (f, M) -> directSum apply(cachedSummands M, N -> tensor(f, N))

-- TODO: should also work if S is a finite field
-- defined as a QuotientRing rather than GaloisField
frobeniusTwistMap = (e, S) -> (
    k := coefficientRing S;
    if char S == 0 or k_0 == 1 then return map(S, S);
    a := k_0;
    p := char k;
    map(S, S, gens S | {a^(p^e)}))

/// -- TODO: add as test:
  R = QQ[x,y] -- or ZZ/p
  assert(R === frobeniusTwist(1, R))
///

protect FrobeniusTwist
frobeniusTwist = method()
frobeniusTwist(ZZ, Ring) := Ring => (e, S) -> (
    k := coefficientRing S;
    if char S == 0 or k_0 == 1 then return S;
    if not S.?cache then S.cache = new CacheTable;
    -- FIMXE: towers of pushforwards
    if S.cache#?(symbol FrobeniusTwist, e) then S.cache#(symbol FrobeniusTwist, e)
    else S.cache#(symbol FrobeniusTwist, e) = (
	F := frobeniusTwistMap(e, ring ideal S);
	quotient F ideal S))
frobeniusTwist(ZZ, Ideal) := Ideal => (e, I) -> frobeniusTwist(e, module I)
frobeniusTwist(ZZ, Module) := Module => (e, M) -> (
    S := ring M;
    R := frobeniusTwist(e, S);
    F := frobeniusTwistMap(e, S);
    -- TODO: is this correct?
    F ** M ** R)
frobeniusTwist(ZZ, Matrix) := Matrix => (e, f) -> (
    map(frobeniusTwist(e, target f), frobeniusTwist(e, source f),
	frobeniusTwistMap(e, ring f) ** f))

frobeniusMap = method(TypicalValue => RingMap)
frobeniusMap(Ring, ZZ) := (R, e) -> frobeniusMap(e, R)
frobeniusMap(ZZ, Ring) := (e, R) -> (
    map(Re := frobeniusTwist(e, R), frobeniusRing(e, R),
	apply(gens Re, g -> g^((char R)^e))))

decomposeFrobeniusPresentation = (e, f) -> (
    p := char ring f;
    tardegrees := degrees target f;
    srcdegrees := degrees source f;
    cube := flatten \ entries \ latticePoints hypercube(degreeLength ring f, 0, p^e - 1);
    tarclasses := apply(cube, i -> positions(tardegrees, deg -> deg % p^e == i));
    srcclasses := apply(cube, i -> positions(srcdegrees, deg -> deg % p^e == i));
    -- sorts the degrees of source and column
    tarclasses = apply(tarclasses, ell -> ell_(last \ sort \\ reverse \ toList pairs tardegrees_ell));
    srcclasses = apply(srcclasses, ell -> ell_(last \ sort \\ reverse \ toList pairs srcdegrees_ell));
    apply(tarclasses, srcclasses, (tarclass, srcclass) -> submatrix(f, tarclass, srcclass)))

protect FrobeniusPushforward
frobeniusPushforward = method()
--frobeniusPushforward(Thing, ZZ)   := (T, e) -> frobeniusPushforward(e, T)
frobeniusPushforward(ZZ, Ring)    := (e, R) -> frobeniusPushforward(e, module R)
frobeniusPushforward(ZZ, Ideal)   := (e, I) -> frobeniusPushforward(e, quotient I)
-- TODO: cache in a way that the second pushforward is the same as applying pushforward twice
frobeniusPushforward(ZZ, Module)  := (e, M) -> (
    if  M.cache#?(FrobeniusPushforward, e)
    then M.cache#(FrobeniusPushforward, e)
    else M.cache#(FrobeniusPushforward, e) = (
    f := presentation myPushForward(
	frobeniusMap(e, ring M),
	frobeniusTwist(e, M));
    if not isHomogeneous f then coker f
    else directSum apply(decomposeFrobeniusPresentation(e, f), coker)))
--
frobeniusPushforward(ZZ, Matrix)  := (e, f) -> (
    if  f.cache#?(FrobeniusPushforward, e)
    then f.cache#(FrobeniusPushforward, e)
    else f.cache#(FrobeniusPushforward, e) = (
    g := myPushForward(
	frobeniusMap(e, ring f),
	frobeniusTwist(e, f));
    if not isHomogeneous g then g
    else directSum decomposeFrobeniusPresentation(e, g)))
--frobeniusPushforward(ZZ, Complex) := (e, C) -> () -- TODO

frobeniusPushforward(ZZ, SheafOfRings)  := (e, N0) -> frobeniusPushforward(e, N0^1) -- TODO: is this cached?
frobeniusPushforward(ZZ, CoherentSheaf) := (e, N) -> (
    if  N.cache#?(FrobeniusPushforward, e)
    then N.cache#(FrobeniusPushforward, e)
    else N.cache#(FrobeniusPushforward, e) = (
    R := ring N;
    p := char R;
    FN := first components frobeniusPushforward(e, module N);
    -- slow alternative:
    -- FN = myPushForward(frobeniusMap(e, R), module N);
    -- prune sheaf image basis(p^e * (max degrees FN // p^e), FN)
    Fmatrix := sub(presentation FN, R);
    (tardegs, srcdegs) := toSequence(-degrees Fmatrix // p^e);
    -- TODO: how long does this take? is it worth caching?
    sheaf prune coker map(R^tardegs, R^srcdegs, Fmatrix))
    )

protect FrobeniusPullback
frobeniusPullback = method()
--frobeniusPullback(Thing, ZZ)  := (T, e) -> frobeniusPullback(e, T)
frobeniusPullback(ZZ, Module) := (e, M) -> (
    if  M.cache#?(FrobeniusPullback, e)
    then M.cache#(FrobeniusPullback, e)
    else M.cache#(FrobeniusPullback, e) = (
	R := ring M;
	p := char R;
	F := frobeniusMap(R, e);
	R0 := source F;
	A := presentation M;
	A0 := sub(A, R0);
	coker(F ** map(R0^(-(p^e) * degrees target A0), , A0))))
frobeniusPullback(ZZ, CoherentSheaf) := (e, F) -> sheaf frobeniusPullback(e, module F)

end--
restart
needsPackage "DirectSummands"
needsPackage "NormalToricVarieties"

-- Two cubics on P^2_(ZZ/2)
X = toricProjectiveSpace(2, CoefficientRing => ZZ/2)
S = ring X
I = ideal(x_0^3+x_1^3+x_2^3)
J = ideal(x_0^3+x_0^2*x_1+x_1^3+x_0*x_1*x_2+x_2^3)

R = quotient I
assert(rank \ summands frobeniusPushforward(1, OO_(Proj R)) == {2})
assert(rank \ summands frobeniusPushforward(1, R) == {2,2})
--M = coker frobeniusPushforward(char S, I) -- TODO: consolidate with toric version

R = quotient J
assert(rank \ summands frobeniusPushforward(1, OO_(Proj R)) == {1,1})
assert(rank \ summands frobeniusPushforward(1, R) == {1, 1, 2}) -- FIXME: this is not correct
--M = coker frobeniusPushforward(char S, J) -- TODO: consolidate with toric version

--
R = quotient I
M = frobeniusPushforward(1, R);
N1 = frobeniusPushforward(2, R)
N2 = frobeniusPushforward(1, M)
assert(N1 == N2) -- FIXME: why is this different?
N2' = prune coker frobeniusPushforward(1, presentation M)
assert(N2 == N2')


--
S=(ZZ/2)[x_0..x_2,y_0..y_2,Degrees=>{{1,0},{1,0},{1,0},{0,1},{0,1},{0,1}}];
J=ideal(x_0*y_0+x_1*y_1+x_2*y_2);
B=S/J;
Y=Proj B;
 frobeniusPushforward(B,1)
--why is this giving 0? (if the degrees are standard it doesn't)
--this is now fixed with the new code for decomposeFrobeniusPresentation -DM


---
restart
needs "frobenius.m2"
debug PushForward
S=(ZZ/2)[x_0,x_1,x_2,y_0,y_1,y_2,Degrees=>{{1,0},{1,0},{1,0},{0,1},{0,1},{0,1}}]
S0=(ZZ/2)[x_0,x_1,x_2]**(ZZ/2)[y_0,y_1,y_2];
S0=tensor((ZZ/2)[x_0,x_1,x_2], (ZZ/2)[y_0,y_1,y_2], DegreeMap => null)
e=1
errorDepth=1
target presentation myPushForward(frobeniusMap(e, ring S^1), frobeniusTwist(e, S^1))
target presentation myPushForward(frobeniusMap(e, ring S0^1), frobeniusTwist(e, S0^1))

g' = g
peek g

degrees (pushAuxHgs g')_0
degrees (pushAuxHgs g'')_0
--why are the degrees right for S but not S0?

RB = RA = S0
tensor(RB, RA, Join => false)
tensor(RB, RA, Join => true)
