-- BUGS.md 22 (found in step 2): a monomial vector in the ring of f, when f
-- has parameters (or lives in a ring K[x][p]), gives a silent "No Gram matrix
-- exists".  Correct: the same result as with the monomials in K[x], or an
-- error that says which ring the monomials must be in.
R = QQ[x];
S = R[t];
r = try solveSOS(sub(x^2 + 1, S), matrix{{1_S}, {x_S}}) else "error";
assert(r === "error" or r#GramMatrix =!= null)
r = try solveSOS(x^2 - 3*x - t, -t, matrix{{1_S}, {x_S}}, RoundTol => 12) else "error";
assert(r === "error" or (r#Parameters =!= null and abs(r#Parameters_(0,0) + 9/4) < 1e-2))
Q = QQ[x,y]/ideal(x^2 + y^2 - 1);
S = Q[t];
r = try solveSOS(10 - x^2 - y - t, -t, matrix{{1_S}, {x_S}, {y_S}}, RoundTol => 12) else "error";
assert(r === "error" or r#GramMatrix =!= null)
