-- BUGS.md 3: real input with large coefficients gives "No Gram matrix exists"
-- for an SOS polynomial (absolute tolerance 1e-10).  Correct: a tolerance
-- relative to the size of the data.
R = RR[x,y];
for c in {1e6, 1e8} do (
    f := c * (x^2 + y^2 + 1)^2;
    sol := solveSOS f;
    assert(sol#GramMatrix =!= null);
    (mon, Q) := (sol#Monomials, sol#GramMatrix);
    assert(norm(f - (transpose mon * Q * mon)_(0,0)) < 1e-6 * c))
