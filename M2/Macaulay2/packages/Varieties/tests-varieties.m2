TEST /// -- twisted global section module
  ringP3 = ZZ/101[x_0..x_3]
  ringP1 = ZZ/101[s,t]
  cubicMap = map(ringP1, ringP3, {s^3, s^2*t, s*t^2, t^3})
  idealCubic = kernel cubicMap
  Cubic = Proj(ringP3/idealCubic)
  Omega = cotangentSheaf Cubic
  -- certain changes in the code for twisted global sections
  -- (e.g. omitting MinimalGenerators => true in calling Hom)
  -- add random coefficients in the presentation of Omega.
  -- TODO: can we get back to this output?
  --assert(module Omega(1) === coker matrix(ring Cubic, {{-x_3, -x_2, -x_1}, {x_2, x_1, x_0}}))
  assert(module Omega(1) === coker matrix(ring Cubic, {{-2*x_3, -2*x_2, -2*x_1}, {x_2, x_1, x_0}}))
///

TEST /// -- twisted cubic curve
  S = (ZZ/13)[x,y,z,w];
  I = minors(2, matrix{{x,y,z}, {y,z,w}});
  X = Proj(S/I);
  Omega = cotangentSheaf X;
  F = Omega ^** 3;
  G = OO_X(-2);
  -- note G is OO_(P^1)(-6)
  assert(prune F === G);
  maxRegs = max(regularity F.module, regularity G.module) + 1 -- 3
  M = truncate(maxRegs, F.module, MinimalGenerators => false)
  N = truncate(maxRegs, G.module, MinimalGenerators => false)
  assert(prune sheaf M === prune F)
  assert(prune sheaf N === G)
  assert isIsomorphic(M, N, Strict => true)
///

TEST ///
  X = Spec ZZ/101[x,y]/(y^2-x^3)
  assert(toString ring X == "(ZZ/101)[x..y]/(-x^3+y^2)")
///

TEST ///
X = Proj(ZZ[x]/(2*x))
assert(char X == 2)
///

end

-- multigraded Proj
restart
needsPackage "VirtualResolutions"

X = Proj(ZZ/101[x_0..x_1]) ** Proj(ZZ/101[y_0..y_2])
S = ring X
B = ideal X -- FIXME: should be the irrelevant ideal

-- First goal: compute regularity on a curve in the sense of Prop 1.2 of GLP83
r = 3
I = monomialCurveIdeal(kk[s_0..s_3], {1,2,3})
I = sub(curveFromP3toP1P2 I, vars S)
R = S/I
C = Proj R
regularity I -- 2, for reference
b = degree C + 2 - r -- see Thm 1.1 in GLP

-- 0 -> M_L -> HH^0(C, L) ** OO_C -> L -> 0
errorDepth = 1
L = OO_C(1,1) -- TODO: is this right??
H = OO_C ^ (rank HH^0(C, L)) -- FIXME

-----------------------------------------------------------------------------
-- https://github.com/Macaulay2/M2/issues/1358
restart
R = QQ[x,y,z]
I = ideal(z^2-y,y*z-x,y^2-x*z)
C = res I;
F = sheaf(Spec R, C_2)
G = sheaf(Spec R, image C.dd_2)
H = sheaf(Spec R, coker C.dd_2)

-- test cotangent sheaf, canonical bundle, and dual
restart
R = QQ[x_0..x_2]
X = Proj R
prune cotangentSheaf X === prune sheaf(X, ker vars R)
prune dual canonicalBundle X === OO_X(3)

-- TODO: picard group with torsion
-- https://github.com/Macaulay2/M2/issues/1910#issuecomment-785266688
-- see examples in Eisenbud and Hartshorne

-- add tests for sheaves on Spec k
-- TODO: define f^# for the map on SheafOfRings?

-- TODO: ^++ for direct sums
-- TODO: maps of sheaves?
-- TODO: isLocallyFree, isSmooth

-- TODO: document creating a ruled surface as Proj(E) with E a
-- vector bundle on a curve, e.g. to create Hirzebruch surfaces
restart
debug Core
kk = QQ
C = PP_kk^1 -- a curve
S = ring C
H = PP sheaf(C, S^{0,-3}) -- Hirzebruch surface
degrees ring H

PP^2
PP^{1,2}
PP(1,2,3)
PP_QQ^2
PP_QQ^{1,2}
PP_QQ(1,2,3)
PP sheaf_C S^{0,-3}

-- TODO: speed this up
-- https://groups.google.com/g/macaulay2/c/q64H_WLg5Uc/m/D_IRGdL2xCQJ
restart
k = ZZ/32749
R = k[x_1,y_1..y_3,z_1..z_3,t];
A = matrix{{x_1,y_1,y_2},{z_1,x_1,y_3},{z_2,z_3,-2*x_1}};
Id = id_(k^3);
p = det(A - Id);
q = det(A + Id);
r = det A;
I = ideal(p-r,q-r);
Ihom = saturate homogenize(I,t);
X0p = Proj(R/Ihom)
euler X0p -- involves 28 hh computations

-- https://groups.google.com/g/macaulay2/c/E4pZNqFgEXE/m/wNDM0XZGnPsJ

-- TODO: Singular locus in weighted case
-- https://groups.google.com/g/macaulay2/c/IcKiDyJCDxU/m/JU66myT3AwAJ
-- https://groups.google.com/g/macaulay2/c/MIS9jJx_Us4/m/SkL0nMjxftUJ

-- TODO: cohomology without Serre duality
-- https://groups.google.com/g/macaulay2/c/JUpi4KJaHxo/m/qA0cY-xiAwAJ

-----------------------------------------------------------------------------
-- TODO: cotangentSheaf for weighted projective spaces, see c564ec04
-- https://groups.google.com/g/macaulay2/c/Nq1krjG3tpM/m/rWRBCiFKAgAJ

-- The quintic in P^4 has hh^(1,1)(X)=1 and hh^(1,2)(X)=101.
-- This can be verified in M2 with
restart
S = ZZ/101[x_0..x_4]
f = x_0^4*x_1+x_1^4*x_2+x_2^4*x_3+x_3^4*x_4+x_4^5
isHomogeneous f
X = Proj(S/f)
elapsedTime assert(hh^(1,1) X == 1)
elapsedTime assert(hh^(1,2) X == 101)
elapsedTime assert(hh((0,3), X) == 1)

-- The mirror, however, should have hh^(1,1)=101 and hh^(1,2)=1.
-- I'm trying to verify this but I keep getting hh^(1,1)=hh^(1,2)(X)=0.
S = ZZ/101[x_0..x_4,Degrees=>{{64},{48},{52},{51},{41}}]
f = x_0^4+x_0*x_1^4+x_1*x_2^4+x_2*x_3^4+x_3*x_4^5
isHomogeneous f
X = Proj(S/f)
-- FIXME
errorDepth = 1
hh^(1,1)(X) == 101
hh^(1,2)(X) == 1
-----------------------------------------------------------------------------

-*
- use the caching style of hilbertSeries for hilbertSamuelFunction of local rings
  functors.m2: id, HH
  ext.m2:      Ext
  tor.m2:      Tor
  varieties.m2: OO, hh
  ringmap.m2:  RingMap_*, RingMap^*
  Add:
   Tot, PP (including for line bundles?)
-- R^i: derived functor
*-


-----------------------------------------------------------------------------
-- RingMap^*: pullback functor
-----------------------------------------------------------------------------
-- TODO: is it possible to get this as the adjoint to f_*?

pullback(RingMap, Module) := Module -> {} >> o -> (f, M) -> notImplemented()

RingMap^* := Functor => f -> new Functor from {
    argument => X -> pullback functorArgs(f, X),
    symbol net     => "pullback by " | net f,
    symbol texMath => texMath f | texMath symbol^*,
    }

--restart
--errorDepth=1
S = ZZ/101[x,y]
R = S/ideal basis_2 S
p = map(R, S)
f = p_*
p^*

document {
     Key => "fibers of a map between varieties",
     }

--Singular locus of a ring or variety with nonstandard ZZ_(>0) grading

nonstandardSingularIdeal = method()    
nonstandardSingularIdeal Ring := S ->(
    K := coefficientRing S;
    d := lcm flatten((gens S)/degree);
    B := basis(d,S^1);
    n := numcols B;
    t := symbol t;
    T := K[t_0..t_(n-1)];
    I := ker (f = map(S,T,B));
    trim f (ideal presentation singularLocus (T/I))
    )
nonstandardSingularIdeal Ideal             := I -> nonstandardSingularIdeal ring I -- why?
nonstandardSingularIdeal AffineVariety     :=
nonstandardSingularIdeal ProjectiveVariety := X -> nonstandardSingularIdeal ring X

nonstandardSingularLocus = method()
nonstandardSingularLocus ProjectiveVariety := X -> Proj((ring X) / nonstandardSingularIdeal ring X)
nonstandardSingularLocus AffineVariety     := X -> Spec((ring X) / nonstandardSingularIdeal ring X)

S = QQ[x,y,z, Degrees =>{{1},{1},{2}}]
nonstandardSingularIdeal S
nonstandardSingularIdeal ideal(0_S)

X = Proj S
nonstandardSingularLocus X
X = Spec S
nonstandardSingularLocus X

-- Functors

restart
X = Proj(kk[x,y])
OO_X
texMath OO_X

methods ((symbol_, OO), Variety) -- FIXME
methods Variety

netList keys Variety
installMethod(symbol _, OO, Variety, X -> sheaf_X ring X)
lookup(symbol_, OO, Variety)
lookup((symbol_, OO), Variety)
lookup(((symbol_, OO), Variety), Variety)
methods OO
methods Variety

-- 
restart
needsPackage("SheafMaps", FileName => "~/papers/code/SheafMaps.m2")

S = kk[x,y,z]
X = Proj S
m = ideal vars S

F = sheaf module truncate(4, S)
HH^0(F(>=3))
prune F

f = (sheaf vars S) ** OO_X(1)
g = Hom(module m^[4], matrix f)
HH^0(OO_X(1))
HH^0(g) =>
map(HH^0 target f, HH^0 source f, sub(basis(0, g), kk))

gens m^[4]


I = ideal(x,y)
F = sheaf comodule I -- skyscraper sheaf8
HH^0 F(>=(-infinity)) -- 
HH^0 F(>=(-10))

G = sheaf module truncate(4, S)
HH^0 G(>=(-infinity)) == S^1
HH^0 G(>=(10)) -- Is this a bug?
F = G
bound = 4
prune Hom(image matrix {apply(generators A, g -> g^4)}, M)




H = sheaf S^{2}
prune H
H' = HH^0 H(>=0)
basis(-3, H')
HH^0 H(-3)

sheaf prune module truncate(4, S)
prune sheaf module truncate(4, S)


F
HH^0 F(>=(-0))
