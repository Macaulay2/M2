-- BUGS.md 17: an objective from the coefficient ring is misread as a
-- parameter, or dropped.  Correct: the error "Objective should be a linear
-- function of the parameters" for every objective that is not an affine
-- function of the parameters alone.
R = QQ[x][t];
f = x^2 + t*x + 1;
for obj in {x, -x, x_R, x + t} do (
    r := try solveSOS(f, obj, RoundTol => 12) else "error";
    if r =!= "error" then error("objective " | toString obj | " accepted"))
-- affine objectives stay valid
assert((solveSOS(f, 2*t + 1, RoundTol => 12))#Parameters =!= null)
