-- Adaptive precision (Precision => infinity) vs fixed double on the examples of
--   Bates, Hauenstein, Sommese, Wampler, "Adaptive multiprecision path tracking",
--     SIAM J. Numer. Anal. 2008: Griewank-Osborne, chemical equilibrium, Chebyshev
--   Timme, "Mixed precision path tracking...", Adv. Comput. Math. 2021: hyperbola family
-- Usage: M2 --script adaptive_precision_bench.m2
needsPackage "NumericalAlgebraicGeometry"
NAGtrace 0
numTBBThreads = 0

report = (name, H, solsS, tol, infThreshold, check) -> (
    ta := elapsedTiming trackHomotopy(H, solsS, Precision=>infinity, CorrectorTolerance=>tol, InfinityThreshold=>infThreshold, EndZoneFactor=>0);
    -- tStepMin as in the adaptive run at 53 bits
    t53 := elapsedTiming trackHomotopy(H, solsS, Precision=>53, CorrectorTolerance=>tol, InfinityThreshold=>infThreshold, EndZoneFactor=>0, tStepMin=>2.^-38);
    << "-- " << name << ": " << #solsS << " paths, tolerance " << tol << endl;
    << "   adaptive: " << toString tally apply(ta#1, status) 
      << ", max precision " << toString tally apply(ta#1, s -> s.cache#"MaxPrecision")
      << ", " << ta#0 << "s, " << check ta#1 << endl << flush;
    << "   fixed 53: " << toString tally apply(t53#1, status) << ", " << t53#0 << "s, " << check t53#1 << endl << flush;
    )

-------------------------------------------------------------------------------
-- 1. Hyperbola x^2 - (t-1/2)^2 - rho^2: the two paths come within 2*rho of
--    each other at t = 1/2; do they stay on their own branch?
<< "=== Hyperbola family x^2 - (t-1/2)^2 - rho^2" << endl << flush
R = CC[x,t]
for k from 1 to 7 do (
    rho := 10.^-k;
    H := matrix{{x^2 - (t-1/2)^2 - rho^2}};
    r0 := sqrt(1/4 + rho^2);
    solsS := {point{{r0}}, point{{-r0}}};
    onBranch := sols -> toString(#select(2, i -> areEqual(sols#i, solsS#i, Tolerance=>1e-6)) ) | " of 2 on their branch";
    report("rho = 1e-" | toString k, H, solsS, 1e-6, 1e6, onBranch);
    )

-------------------------------------------------------------------------------
-- 2. Chemical equilibrium (badly scaled): 8 finite regular solutions, two with
--    coordinates ~1e5.
<< "=== Chemical equilibrium system (3 variables, total degree 12)" << endl << flush
R = CC[z1,z2,z3]
F = {14*z1^2 + 6*z1*z2 + 5*z1 - 72*z2^2 - 18*z2 - 850*z3 + 0.000000002,
     0.5*z1*z2^2 + 0.01*z1*z2 + 0.13*z2^2 + 0.04*z2 - 40000,
     0.03*z1*z3 + 0.04*z3 - 850}
(S,solsS) = totalDegreeStartSystem F;
H = segmentHomotopy(S, F, gamma=>0.6+0.8*ii)
finiteRegular = sols -> toString(#select(sols, s -> status s === Regular and norm matrix s < 1e7)) | " finite regular (expect 8)"
for tol in {1e-8, 1e-12} do report("tolerance " | toString tol, H, solsS, tol, 1e8, finiteRegular)

-------------------------------------------------------------------------------
-- 3. Scaled Chebyshev T_0 = 2, T_1 = x, T_i = x T_{i-1} - T_{i-2}/4: size 2^-n
--    on [-1,1] while sum |coefficients| ~ 1.2^n, so the evaluation error swamps
--    the function unless the precision is high enough (the test of Psi).
--    Coefficients are exact (over QQ; rounded to double they are off by 4e-9,
--    next to a polynomial of size 8e-31) and normalized to sum |coeff| = 1
--    (otherwise paths run out to |x| ~ 30 and x^101 overflows the error model).
--    Outcome: degree 11 at 53 bits; 21, 51 at 128 (one root of T_51 near x = 1
--    ends 1e-6 off: final-step acceptance); 101 at 256 bits, all roots to 1e-15.
--    Fixed double fails every path from degree 51 on.
<< "=== Scaled Chebyshev polynomials T_{n+1} (exact coefficients, normalized), tolerance 1e-10" << endl << flush
Q = QQ[x]
cheb = n -> (T0 := 2_Q; T1 := x; for i from 2 to n do (T2 := x*T1 - T0/4; T0 = T1; T1 = T2); if n == 0 then T0 else T1)
for n in {10, 20, 50, 100} do (
    T := {cheb(n+1)};
    T = {T#0 / sum(listForm T#0, (e,c) -> abs c)};
    roots := apply(n+1, k -> cos((2*n+1-2*k)*pi/(2*n+2)));
    (S,solsS) := totalDegreeStartSystem T;
    H := segmentHomotopy(S, T, gamma=>0.6+0.8*ii);
    found := sols -> (
	ends := apply(select(sols, s -> status s === Regular), s -> (coordinates s)#0);
	dists := apply(ends, e -> min apply(roots, r -> abs(r - e)));
	matched := # unique apply(select(#ends, i -> dists#i < 1e-8), i -> position(roots, r -> abs(r - ends#i) < 1e-8));
	toString matched | " of " | toString(n+1) | " roots found, "
	| toString(# unique apply(ends, e -> round(6, realPart e))) | " distinct endpoints, worst distance to a root "
	| toString(if #dists > 0 then max dists else 0));
    report("degree " | toString(n+1), H, solsS, 1e-10, 1e6, found);
    )

-------------------------------------------------------------------------------
-- 4. Griewank-Osborne: triple root at the origin where Newton diverges. Tracked
--    to t = 1 - eps, where the paths are ~eps^(1/3) from the origin.
<< "=== Griewank-Osborne, tracked to t = 1 - eps" << endl << flush
R = CC[z1,z2,t]
f = {29/16*z1^3 - 2*z1*z2, z2 - z1^2}
g = {z1^3 - 1, z2^2 - 1}
solsS = flatten apply(3, a -> apply(2, b -> point{{exp(2*pi*ii*a/3), (-1)^b}}))
gam = 0.6+0.8*ii
for eps in {1e-4, 1e-8, 1e-12} do (
    s := t*(1-eps);
    H := transpose matrix{apply(2, i -> (1-s)*gam*g#i + s*f#i)};
    nearOrigin := sols -> toString(#select(sols, s -> status s === Regular and norm matrix s < 10*eps^(1/3))) | " of 3 near the origin";
    report("eps = " | toString eps, H, solsS, 1e-6, 1e6, nearOrigin);
    )
