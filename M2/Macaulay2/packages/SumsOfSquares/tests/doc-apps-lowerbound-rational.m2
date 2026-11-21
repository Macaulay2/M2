-- Tests written from the documentation alone (independent tester, applications).
-- Topic: lowerBound for rational functions ("f: a polynomial or a rational
-- function"), doc node lowerBound "Optimizing rational functions" and
-- [BPT13] section 3.2.2 (p/q with q > 0: max t s.t. p - t q is SOS).
-- Only denominators that are positive (or nonnegative) are tested here; see
-- tests/open/doc-FINDING-rational-sign-denominator.m2 for sign changes and
-- tests/open/doc-FINDING-recover-rational.m2 for the minimizer.
-- Irrational minima are compared exactly: e.g. b <= (1 - sqrt 2)/2 iff
-- 1 - 2b > 0 and (1 - 2b)^2 >= 2.

ratRealRoots = g -> apply(select(roots g, r -> abs imaginaryPart r < 1e-8), realPart);
ratAt = (f, a) -> sub(numerator f, a) / sub(denominator f, a);

-- doc example: f = (x^2-x)/(x^2+1); f' has numerator x^2+2x-1, so the
-- minimum is (1 - sqrt 2)/2 = -0.2071... at x = sqrt 2 - 1 ------------------
R = QQ[x];
f = (x^2 - x)/(x^2 + 1);
r = lowerBound f;
assert(instance(r, Sequence) and #r == 2);
(bound, sol) = r;
assert(instance(sol, SDPResult));
assert(instance(bound, QQ));
assert(1 - 2*bound > 0 and (1 - 2*bound)^2 >= 2);        -- bound <= min, exactly
assert(bound >= (1 - sqrt 2)/2 - 5e-2);
assert(bound <= ratAt(f, {x => 41421/100000}));           -- exact, at a rational point
-- with finer rounding the bound approaches the minimum and stays below it
for rt in {12, 20} do (
    (b, s) = lowerBound(f, RoundTol => rt);
    print("doc example, RoundTol " | toString rt);
    assert(instance(b, QQ));
    assert(1 - 2*b > 0 and (1 - 2*b)^2 >= 2);
    assert(b >= (1 - sqrt 2)/2 - 1e-3));
(bR, solR) = lowerBound(f, RoundTol => infinity);
assert(instance(bR, RR));
assert(abs(bR - (1 - sqrt 2)/2) < 1e-6);
-- degree bound D: still a valid lower bound
for D in {2, 4} do (
    (b, s) = lowerBound(f, D);
    print("doc example, D = " | toString D);
    assert(instance(b, QQ) and 1 - 2*b > 0 and (1 - 2*b)^2 >= 2));
-- the same function with the sign moved to the denominator: -(x^2-x)/(x^2+1)
-- has minimum -(1 + sqrt 2)/2, i.e. b <= that iff -2b-1 > 0 and (-2b-1)^2 >= 2
fneg = (x^2 - x)/(-x^2 - 1);
(bn, soln) = lowerBound fneg;
assert(instance(bn, QQ) and -2*bn - 1 > 0 and (-2*bn - 1)^2 >= 2);
assert(bn >= -(1 + sqrt 2)/2 - 5e-2);

-- [BPT13] Exercise 3.55: (x^3-8x+1)/(x^4+x^2+12), global min and max -------
g = (x^3 - 8*x + 1)/(x^4 + x^2 + 12);
p = numerator g; q = denominator g;
crit = ratRealRoots(diff(x, p)*q - p*diff(x, q));
vals = apply(crit, c -> sub(p, x => c)/sub(q, x => c));
gmin = min vals; gmax = max vals;                         -- about -0.443383 and 0.577141
(bg, solg) = lowerBound g;
assert(instance(bg, QQ));
assert(bg <= ratAt(g, {x => 1171/1000}));                  -- exact
assert(bg <= gmin + 1e-9 and bg >= gmin - 1e-1);
(bgR, solgR) = lowerBound(g, RoundTol => infinity);
assert(abs(bgR - gmin) < 1e-5);
-- maximum via the minimum of -g
(bm, solm) = lowerBound(-g);
assert(instance(bm, QQ) and bm <= -gmax + 1e-9 and bm >= -gmax - 1e-1);
assert(bm <= -ratAt(g, {x => -110171/100000}));            -- exact
(bmR, solmR) = lowerBound(-g, RoundTol => infinity);
assert(abs(bmR + gmax) < 1e-5);

-- (x^4+1)/(x^2+1): minimum 2 sqrt 2 - 2 at x^2 = sqrt 2 - 1 (two minimizers);
-- b <= 2 sqrt 2 - 2 iff b + 2 > 0 and (b + 2)^2 <= 8 -------------------------
h = (x^4 + 1)/(x^2 + 1);
(bh, solh) = lowerBound h;
assert(instance(bh, QQ) and bh + 2 > 0 and (bh + 2)^2 <= 8);
assert(bh >= 2*sqrt 2 - 2 - 5e-2);
(bhR, solhR) = lowerBound(h, RoundTol => infinity);
assert(abs(bhR - (2*sqrt 2 - 2)) < 1e-6);

-- nonnegative denominators that vanish ------------------------------------
-- 1/x^2: infimum 0, not attained
(b1, s1) = lowerBound(1/x^2);
assert(instance(b1, QQ) and b1 <= 0 and b1 >= -1e-2);
-- (x^2+1)/x^2 = 1 + 1/x^2: infimum 1, not attained
(b2, s2) = lowerBound((x^2 + 1)/x^2);
assert(instance(b2, QQ) and b2 <= 1 and b2 >= 1 - 1e-2);
-- denominator written negative: 1/(-x^2-1) = -1/(x^2+1), minimum -1 at x = 0
(b3, s3) = lowerBound(1/(-x^2 - 1));
assert(instance(b3, QQ) and b3 <= -1 and b3 >= -1 - 1e-2);
-- a polynomial given as an element of the fraction field behaves like the
-- polynomial: ((x-1)^2+(x+3)^2)/1 has minimum 8
(b4, s4) = lowerBound(((x - 1)^2 + (x + 3)^2)/1_R);
assert(instance(b4, QQ) and b4 <= 8 and b4 >= 8 - 1e-2);

-- several variables: (x+y)/(1+x^2+y^2) has minimum -1/sqrt 2 at
-- x = y = -1/sqrt 2; b <= -1/sqrt 2 iff b < 0 and b^2 >= 1/2 ---------------
R = QQ[x,y];
k = (x + y)/(1 + x^2 + y^2);
(bk, solk) = lowerBound k;
assert(instance(bk, QQ) and bk < 0 and bk^2 >= 1/2);
assert(bk >= -1/sqrt 2 - 1e-1);
(bkR, solkR) = lowerBound(k, RoundTol => infinity);
assert(abs(bkR + 1/sqrt 2) < 1e-5);
-- a rational function with a polynomial part: (x^2+y^2+1)^2/(x^2+y^2+1) - 2x
-- = x^2 + y^2 + 1 - 2x, minimum 0 at (1,0)
(bl, soll) = lowerBound((x^2 + y^2 + 1)^2/(x^2 + y^2 + 1) - 2*x);
assert(instance(bl, QQ) and bl <= 0 and bl >= -1e-2);

-- rational function with equality constraints (h, D): (x^2-x)/(x^2+1) on
-- {x^2 = 1} = {-1, 1} takes the values 1 and 0, so the minimum is 0 --------
R = QQ[x];
f = (x^2 - x)/(x^2 + 1);
for D in {2, 4} do (
    rr = lowerBound(f, matrix{{x^2 - 1}}, D);
    print("rational with constraint, D = " | toString D);
    assert(instance(rr, Sequence) and #rr == 3);
    assert(instance(rr#0, QQ) and rr#0 <= 0 and rr#0 >= -1e-2);
    assert(instance(rr#2, Matrix) and numRows rr#2 == 1 and numColumns rr#2 == 1));
