-- test of canUseHilbertHint, Hilbert hint code with M2.
restart
R1 = ZZ/101[x,y,z, Degrees => {1,2,3}];
assert canUseHilbertHint R1
R2 = ZZ[x,y,z];
assert not canUseHilbertHint R2
R3 = (GF 8)[x,y,z]
assert canUseHilbertHint R3
use R1
R4 = R1[u,v, Join => false]/(x*u+y*v)
R5 = first flattenRing R4
assert not canUseHilbertHint R4 -- we might want to change this behavior
assert canUseHilbertHint R5

-- test in presence of multi-degrees
R = ZZ/101[a,b,c,d, Degrees => {2:{1,0}, 2:{0,1}}]
I = ideal for i from 1 to 3 list random({2,3}, R)
hf = poincare I
assert not canUseHilbertHint R
assert not canUseHilbertHint I

Rlex = ZZ/101[a,b,c,d, Degrees => {2:{1,0}, 2:{0,1}}, MonomialOrder => Lex]
Ilex = sub(I, Rlex)
gblex = gens gb(Ilex, Hilbert => hf) -- gives warning, but no error
assert(numgens ideal gblex == 37) -- happens with given random seed, at least....

-- git issue # 3937
p = 32003
S=(ZZ/p)[x_1,x_2,y_1,y_2,Degrees=>{{1,0},{1,0},{0,1},{0,1}}]
I=ideal random({2,2},S)
R=S/I
primaryDecomposition ideal 0_R -- crash in 1.26.05, for instance, now ok.

-- git issue # 1935
R = QQ[x,y,z,w,u]/(u^4 - x*y*z*w)
S = R/(x^3 - y^2*z)
T = S/(y*w^2 - z*x^2)
f = map(T, S)
assert not isWellDefined inverse f -- kernel of a non-welldefined ring map is undefined behavior, but should not crash.
(ans, err) = trap kernel inverse f -- internal error: incorrect Hilbert function given, aborting, crashses.
assert(ans === null)
-- now it doesn't crash, but the error message is not so good either...
assert(toString err === "incorrect Hilbert function given")

-- git issue # 4355
S = QQ[x,y,z];
f = x^4 + y^4 + z^4;
R = S / ideal(f);
I = ideal(x^3, x^2+y^2);
integralClosure(I) -- crashed, now ok.
