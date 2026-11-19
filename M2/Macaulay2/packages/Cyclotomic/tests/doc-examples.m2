-- doc-examples.m2
-- Every example of the documentation, with the outputs that the
-- documentation shows turned into assertions.

-- top page
e0 = cyclotomicRoot(1/6);
F3 = cyclotomicField 3;
assert(ring e0 === F3);
assert(e0 == F3_0 + 1);
F = cyclotomicField(8, ZZ/5);
assert(instance(F, GaloisField));
assert(F.order == 25);
assert(toString F_0 == "ww_24");
r = cyclotomicRoot(F, 1/8);
assert(r == -F_0 - 2);
assert(r^8 == 1 and r^4 == -1);
assert(discreteLog r == 3);

-- cyclotomicField
S = cyclotomicField 5;
r = S_0;
assert(toString r == "ww_5");
assert(r^4 == -r^3 - r^2 - r - 1);
assert(r^5 == 1);
assert(cyclotomicField 5 === S);
assert(cyclotomicField(6, QQ) === cyclotomicField 3);
T = cyclotomicField(4, cyclotomicField 3);
assert(T === cyclotomicField 12);
assert(toString T_0 == "ww_12");
assert(T_0^4 - T_0^2 + 1 == 0);
G = cyclotomicField(3, ZZ/5);
assert(instance(G, GaloisField) and G.order == 25);
G = cyclotomicField(5, GF 9);
assert(instance(G, GaloisField) and G.order == 81);

-- cyclotomicPoly
R = QQ[w];
assert(cyclotomicPoly(6, w) == w^2 - w + 1);
assert(cyclotomicPoly(105, w) == w^48+w^47+w^46-w^43-w^42-2*w^41-w^40-w^39+w^36+w^35+w^34+w^33+w^32+w^31-w^28-w^26-w^24-w^22-w^20+w^17+w^16+w^15+w^14+w^13+w^12-w^9-w^8-2*w^7-w^6-w^5+w^2+w+1);

-- cyclotomicRoot
e0 = cyclotomicRoot(1/3);
assert(ring e0 === cyclotomicField 3 and e0 == (cyclotomicField 3)_0);
F12 = cyclotomicField 12;
e0 = cyclotomicRoot(F12, 1/3);
assert(e0 == F12_0^2 - 1);
e0 = cyclotomicRoot(1/2);
assert(ring e0 === QQ and e0 == -1);
K = GF 25;
e0 = cyclotomicRoot(K, 1/3);
assert(e0 == 2*K_0 + 1);
assert(e0^3 == 1);

-- rootOfUnityGenerator
assert(rootOfUnityGenerator QQ == -1);
assert(ring rootOfUnityGenerator QQ === QQ);
F3 = cyclotomicField 3;
assert(rootOfUnityGenerator F3 == F3_0 + 1);
F4 = cyclotomicField 4;
assert(rootOfUnityGenerator F4 == F4_0);
g = rootOfUnityGenerator(ZZ/7);
assert(ring g === ZZ/7 and g == 3);
L = GF(ZZ/3[c]/(c^2+1));
g = rootOfUnityGenerator L;
assert(ring g === L);
assert(g == -L_0 - 1);

-- rootOfUnityExponent
F = cyclotomicField 3;
assert(rootOfUnityExponent F_0 == 1/3);
assert(rootOfUnityExponent(-F_0) == 5/6);
assert(rootOfUnityExponent(2*F_0) === null);
assert(rootOfUnityExponent(6_(ZZ/7)) == 1/2);

-- rootOfUnityOrder
F = cyclotomicField 3;
assert(rootOfUnityOrder F_0 == 3);
assert(rootOfUnityOrder(-F_0) == 6);
assert(rootOfUnityOrder(2*F_0) === null);
K = toField(QQ[a]/(a^2+1));
assert(rootOfUnityOrder a == 4);
assert(rootOfUnityOrder(2_(ZZ/7)) == 3);

-- discreteLog
K = ZZ/32003;
k = discreteLog 12345_K;
assert(k == 2702);
assert((rootOfUnityGenerator K)^k == 12345_K);
L = GF(2^20);
k = discreteLog(L_0^1000 + 1);
assert(k == 387868);
assert((rootOfUnityGenerator L)^k == L_0^1000 + 1);

-- cyclotomicInclusion
phi = cyclotomicInclusion(cyclotomicField 3, cyclotomicField 12);
F12 = cyclotomicField 12;
assert(instance(phi, RingMap));
assert(target phi === F12 and source phi === cyclotomicField 3);
assert(phi (cyclotomicField 3)_0 == F12_0^2 - 1);
assert(phi (cyclotomicField 3)_0 == cyclotomicRoot(cyclotomicField 12, 1/3));
K = GF(ZZ/3[c]/(c^2+1));
L = cyclotomicField(5, K);
psi = cyclotomicInclusion(K, L);
assert(instance(L, GaloisField) and L.order == 81);
assert(source psi === K and target psi === L);
assert(psi K_0 == L_0^3 + L_0^2 + 1);
assert((psi K_0)^2 == -1);
R = QQ[x,y]; T = cyclotomicExtension(R, 3);
phi = cyclotomicInclusion(R, T);
assert(source phi === R and target phi === T);
assert(phi R_0 == T_0 and phi R_1 == T_1);

-- cyclotomicExtension
R = QQ[x,y, Degrees => {2,3}, MonomialOrder => Lex];
T = cyclotomicExtension(R, 3);
assert(degrees T == {{2}, {3}});
assert(cyclotomicExtension(R, 3) === T);
assert(cyclotomicExtension(R, 2) === R);

-- galoisAutomorphism
F = cyclotomicField 5;
sigma = galoisAutomorphism(F, 2);
assert(source sigma === F and target sigma === F);
assert(sigma F_0 == F_0^2);
K = GF 25;
assert((galoisAutomorphism(K, 1)) K_0 == K_0^5);

-- findRootPower
S = cyclotomicField 5;
assert(findRootPower(S[x,y]) == 5);
assert(findRootPower(QQ[x]) == 2);

-- joinCyclotomic
F = cyclotomicField 3; G = cyclotomicField 4;
R = F[t]; I = ideal(t - F_0^2);
S = G[t]; J = ideal(t^2 - G_0);
L = joinCyclotomic {I, J};
assert(instance(L, List) and #L == 2);
T = ring L#0;
assert(ring L#1 === T);
assert(coefficientRing T === cyclotomicField 12);
z = (cyclotomicField 12)_0;
assert(L#0 == ideal(T_0 + z^2));
assert(L#1 == ideal(T_0^2 - z^3));
