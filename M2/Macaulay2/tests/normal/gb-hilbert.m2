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


