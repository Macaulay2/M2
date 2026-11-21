-- BUGS.md 23 (found in step 2): with a degree bound D and homogeneous input,
-- only the monomials of degree exactly D/2 are used: solveSOS(f, D) with
-- D > deg f, lowerBound of a homogeneous rational function and sosInIdeal(R, D)
-- in a homogeneous quotient ring miss results.  The documentation: "the method will use the vector
-- of monomials of degree at most D/2", with which f is SOS.
R = QQ[x,y];
for D in {6, 8} do (
    f := x^4 + y^4;
    sol := solveSOS(f, D);
    assert(sol#GramMatrix =!= null);
    -- exact, or real with a small residual (after the test audit: the fix
    -- "all monomials of degree <= D/2" gives a singular problem)
    s := sosPoly sol;
    assert(if coefficientRing ring s === QQ then f == value s
        else norm(sub(f, ring s) - value s) < 1e-6))
assert((solveSOS(x^2 + y^2, 4))#GramMatrix =!= null)
-- with a parameter: min t such that x^4 + (t+1) x^2y^2 + y^4 is SOS is -3
-- (x^4 + t x^2y^2 + y^4 itself makes CSDP stop at D = 4 already, BUGS.md 15)
R = QQ[x,y][t];
sol = solveSOS(x^4 + t*x^2*y^2 + y^4 + x^2*y^2, t, 6, RoundTol => 12);
assert(sol#Parameters =!= null and abs(sol#Parameters_(0,0) + 3) < 1e-2)
-- a rational function with homogeneous numerator and denominator of the same
-- degree: min (x^4+y^4)/(x^2+y^2)^2 = 1/2 (independent check of step 2)
R = QQ[x,y];
b = first lowerBound((x^4 + y^4)/(x^2 + y^2)^2, 6);
assert(b =!= null and b <= 1/2 and b > 1/2 - 1e-2)
-- a homogeneous quotient ring: x^2 = 0 is an SOS of degree 2 <= 4
S = QQ[x,y]/ideal(x^2, x*y);
setRandomSeed 0;
assert((sosInIdeal(S, 4))#GramMatrix =!= null)
