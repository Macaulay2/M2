-- BUGS.md 1: the rational rounding of parametric problems returns a Gram
-- matrix that does not satisfy the linear constraints.  Correct (decided,
-- LOG.md 1.7 item 2): an exact result is always an exactly checked
-- certificate; otherwise the real result is returned.
isPSD = Q -> (
    n := numRows Q;
    T := local T; A := QQ[T];
    p := det(A_0 * id_(A^n) - sub(Q, A));
    all(0..n, k -> (-1)^(n-k) * coefficient(A_0^k, p) >= 0))
-- f with the parameters replaced by the returned values
atParameters = (f, sol) -> (
    p := sol#Parameters;
    if p === null or numRows p == 0 then return f;
    R := ring f;
    sub(f, apply(numRows p, i -> R_i => p_(i,0))))
-- an exact result must be a certificate; a real result is acceptable
okResult = (f, sol) -> (
    (mon, Q) := (sol#Monomials, sol#GramMatrix);
    if Q === null or ring Q =!= QQ then return true;
    g := atParameters(f, sol);
    isPSD Q and sub(g, ring mon) == (transpose mon * Q * mon)_(0,0))

R = QQ[x][t];
f = x^2 + 1 - 1/3*x^3 + t*x^3;          -- SOS exactly for t = 1/3
assert okResult(f, solveSOS f)
R = QQ[x][s];
f = x^2 + 1 + (3*s - 1)*x^4;
assert okResult(f, solveSOS(f, 2))
-- natural inputs (code verifier)
R = QQ[x];
h = matrix{{x^3 + 3*x, x^2 - 7*x}};
setRandomSeed 0;
(sol, mult) = sosInIdeal(h, 4);
assert(sol#GramMatrix === null or ring sol#GramMatrix =!= QQ or (h*mult)_(0,0) == value sosPoly sol)
f = (x^4 + 3)/(3*x^4 + 3);                -- inf f = 1/3, not attained
(b, sol) = lowerBound(f, 2);
assert(b === null or ring b =!= QQ or (b <= 1/3 and (x^4+3) - b*(3*x^4+3) == value sosPoly sol))
