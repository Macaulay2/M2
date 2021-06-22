-- used to be in Macaulay2Doc

R = ZZ/101[a,b]
S = ZZ/101[a,b,c]
M = cokernel matrix{{c^3}}
f = map(S, R)
assert( R^{0,-1,-2} == pushForward(f, M) )

-- https://github.com/Macaulay2/M2/issues/1522
R = QQ[x,y,z,w,u]/(u^4 - x*y*z*w)
S = R/(x^3 - y^2*z)
T = S/(y*w^2 - z*x^2)
f = map(T, S)
N1 = coker gens ideal (T_0^4,T_1^4);
N2 = comodule ideal (T_2^6,T_3^7);
M = N1 ++ N2
A = pushForward(f, M, Strategy => Quotient)
M = N1 ++ N2
B = pushForward(f, M, Strategy => Default)
assert(A == B)

--
P3 = ZZ/32003[a..i];
M = comodule monomialCurveIdeal(P3, {1,3,8,9,12,13,17,21});

P2 = ZZ/32003[a,b,c,d,e,f];
F = map(P3, P2, random(P3^1, P3^{-1,-1,-1,-1,-1,-1}));
elapsedTime N = pushForward(F, M);
assert(hilbertPolynomial M == hilbertPolynomial N)
ann N

m = random(P3^1, P3^{-1,-1,-1,-1,-1,-1})
G = map(P3, P2, m);
elapsedTime pushForward(G, M);
G' = map(P3, P2, m);
elapsedTime pushForward(G', M);
-- checking for a cache hit on a pushforward computation occurs
--importFrom_Core { "cacheHit", "PushforwardComputation" }
--cacheHit PushforwardComputation := x -> error "cache hit!"
--elapsedTime assert try (pushForward(G', M); false) else true

