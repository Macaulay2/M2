-- Open finding (independent tester, applications): recoverSolution after
-- lowerBound of a rational function does not return the minimizer.
-- Doc node lowerBound: "In some cases the minimizer can be extracted with
-- the method recoverSolution", followed by the example "Optimizing rational
-- functions" f = (x^2-x)/(x^2+1), printed as (-1/4, {x => .382683}).
-- The minimizer is x = sqrt 2 - 1 = .414214 (f' has numerator x^2 + 2x - 1),
-- f(.382683) = -.20606 > -.207107 = f(sqrt 2 - 1).  The moment matrix
-- | .853553 .353553 ; .353553 .146447 | is rank one and encodes the right
-- point: .353553/.853553 = .414214; the returned value is
-- sqrt(.146447) = .382683, i.e. the moment vector is not normalized by its
-- entry for the monomial 1 (which is 1/q(x*) for p/q, not 1).
-- Expected (documented intent): the recovered point is the minimizer.
-- This file FAILS on the original (version 2.2).

R = QQ[x];
f = (x^2 - x)/(x^2 + 1);
(bound, sol) = lowerBound f;
pt = recoverSolution sol;
print("recovered " | toString pt | ", minimizer x = sqrt 2 - 1 = .414214");
assert(abs(pt#0#1 - (sqrt 2 - 1)) < 1e-3);

-- [BPT13] Exercise 3.55: (x^3-8x+1)/(x^4+x^2+12), minimizer about 1.17107
g = (x^3 - 8*x + 1)/(x^4 + x^2 + 12);
(bg, solg) = lowerBound g;
ptg = recoverSolution solg;
print("recovered " | toString ptg | ", minimizer about 1.17107");
assert(abs(ptg#0#1 - 1.17107) < 1e-3);

-- two variables: (x+y)/(1+x^2+y^2), minimizer x = y = -1/sqrt 2
R = QQ[x,y];
k = (x + y)/(1 + x^2 + y^2);
(bk, solk) = lowerBound k;
ptk = recoverSolution solk;
print("recovered " | toString ptk | ", minimizer x = y = -.707107");
assert(all(ptk, o -> abs(o#1 + 1/sqrt 2) < 1e-3));
