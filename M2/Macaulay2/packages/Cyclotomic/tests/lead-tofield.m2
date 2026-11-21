-- Finite fields made with toField(ZZ/p[c]/(f)), handled like the Galois
-- field GF(ZZ/p[c]/(f)) of the same presentation (decided by Thomas on
-- 2026-11-02).
S5 = (ZZ/5)[getSymbol "c"];
A = S5/(S5_0^2 - 2);
K = toField A;
G = GF A;
toG = map(G, K, {G_0});
g = rootOfUnityGenerator K;
-- the generator corresponds to the one of the Galois field, and has order 24
assert(toG g == rootOfUnityGenerator G and rootOfUnityOrder g == 24);
-- discrete logarithms of all 24 nonzero elements, and exponents
x = 1_K;
for k from 0 to 23 do (assert(discreteLog x == k and rootOfUnityExponent x == k/24); x = x * g);
assert((cyclotomicRoot(K, 1/8))^8 == 1 and rootOfUnityOrder cyclotomicRoot(K, 1/8) == 8);
-- K contains the 3rd and 8th roots of unity; the 13th need GF(5^4)
assert(cyclotomicField(3, K) === K and cyclotomicField(8, K) === K);
L = cyclotomicField(13, K);
assert(L === cyclotomicField(13, ZZ/5) and L.order == 625);
phi = cyclotomicInclusion(K, L);
assert((phi K_0)^2 == 2);
scan(24, k -> assert(phi cyclotomicRoot(K, k/24) == cyclotomicRoot(L, k/24)));
-- the same images as for the Galois field of the same presentation
assert(phi K_0 == (cyclotomicInclusion(G, L)) G_0);
-- polynomial rings over K
R = K[getSymbol "x", getSymbol "y"];
T = cyclotomicExtension(R, 13);
assert(coefficientRing T === L and cyclotomicExtension(R, 3) === R);
psi = cyclotomicInclusion(R, T);
assert((psi R_0) == T_0 and (psi promote(K_0, R))^2 == 2);
-- the Frobenius
s = galoisAutomorphism(K, 1);
assert(s K_0 == K_0^5 and s s K_0 == K_0);
-- joinCyclotomic over K and over GF(5^4)
U = L[getSymbol "x", getSymbol "y"];
J = joinCyclotomic {ideal(R_0 - K_0), ideal(U_0 - L_0)};
assert(coefficientRing ring first J === L and ring J#0 === ring J#1);
