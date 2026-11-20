-- Open finding (independent tester, applications): lowerBound of a rational
-- function whose denominator changes sign returns a number that is not a
-- lower bound.
-- Doc node lowerBound: "f: a polynomial or a rational function", "bound: a
-- lower bound on f", "This method finds a lower bound for a polynomial or
-- rational function x |-> f(x)"; no condition on the denominator.  The
-- relaxation max t s.t. p - t q is SOS ([BPT13] section 3.2.2) is only valid
-- for q > 0 ([BPT13] assumes "q(x) is strictly positive", Exercise 3.56).
-- Original: lowerBound((x^2+1)/x) returns 2 with the certificate
-- (x^2+1) - 2x = (x-1)^2, but f(-1) = -2 < 2 and f is unbounded below;
-- lowerBound(1/x) returns 0, but 1/x is unbounded below;
-- lowerBound((x^2+y^2+1)/(x-y)) returns 11/8, but it is unbounded below.
-- Expected: no number is returned as a lower bound (null, or an error, or a
-- documented restriction to positive denominators and a check).
-- This file FAILS on the original (version 2.2).

lbOrNull = f -> (r := try lowerBound f else null; if r === null then null else r#0);

R = QQ[x];
f = (x^2 + 1)/x;
b = lbOrNull f;
print("lowerBound((x^2+1)/x) returned " | toString b | "; f(-1) = -2, f unbounded below");
assert(not instance(b, Number));

g = 1/x;
b = lbOrNull g;
print("lowerBound(1/x) returned " | toString b | "; 1/x is unbounded below");
assert(not instance(b, Number));

-- two variables: (x^2+y^2+1)/(x-y) is unbounded below (x - y -> 0 from
-- below); the original returns 11/8 (while x/y correctly gives no bound)
R = QQ[x,y];
b = lbOrNull((x^2 + y^2 + 1)/(x - y));
print("lowerBound((x^2+y^2+1)/(x-y)) returned " | toString b);
assert(not instance(b, Number));
