TEST ///

p = 2;
A = makeBurnsideMackeyFunctor p;
B = makeBurnsideMackeyFunctor p;

-- check that A and B have projective dimension 0, i.e.
-- Tor_i(A,-) and Tor_i(B,-) and Ext^i(A,-) and Ext^i(B,-) are zero for i > 0
-- and that A and B have injective dimension 1, i.e.
-- Ext^i(-,A) and Ext^i(-,B) are zero for i > 1
M = makeRandomCpMackeyFunctor p;
for i from 1 to 5 do (
    assert(isTrivialMackeyFunctor Tor_i(A,M));
    assert(isTrivialMackeyFunctor Tor_i(B,M));
    assert(isTrivialMackeyFunctor Ext^i(A,M));
    assert(isTrivialMackeyFunctor Ext^i(B,M));
);
for i from 2 to 5 do (
    assert(isTrivialMackeyFunctor Ext^i(M,A));
    assert(isTrivialMackeyFunctor Ext^i(M,B));
);

///
