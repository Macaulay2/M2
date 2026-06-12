-- homogeneous ring map with interesting degree map
R = kk[a, b, DegreeGroup => ZZ^2];
S = kk[s, t, u, DegreeGroup => ZZ^3]/ideal {s^3, t^5, u^7};
f = map(S, R, {s^2, t^2*u^3}, DegreeMap => d -> d_0 * {2, 0, 0} + d_1 * {0, 2, 3});
P = pushForward(f, S^1);

-- homogeneity is preserved
assert(isHomogeneous f)
assert(isHomogeneous S^1)
assert(isHomogeneous P)

-- the computed pushforward looks like what we expect
assert(numcols basis P == numcols basis S^1)