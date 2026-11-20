-- BUGS.md 29: sosPoly(R, polys, coeffs) accepts negative weights and lists of
-- different lengths.  The documentation of SOSPoly: "the d_i are positive
-- weights".  Correct: an error for both; nonnegative weights, the empty
-- SOSPoly and weights from a numerical decomposition are accepted.
R = QQ[x,y];
assert((try sosPoly(R, {x}, {-1}) else "error") === "error")
assert((try sosPoly(R, {x, y}, {1}) else "error") === "error")
assert((try sosPoly({x}, {1, 2}) else "error") === "error")
assert instance(sosPoly(R, {x, y}, {1, 0}), SOSPoly)
assert instance(sosPoly(R, {}, {}), SOSPoly)
S = RR[x,y];
s = sosPoly solveSOS(x^2 + x*y + y^2);
assert(instance(s, SOSPoly) and all(coefficients s, c -> c >= 0))
assert((try sosPoly(S, {x_S}, {-1e-3}) else "error") === "error")
