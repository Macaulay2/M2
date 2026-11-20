-- BUGS.md 9: integer coefficients give "Polynomial should depend affinely on
-- the parameters".  Decided (LOG.md 1.7 item 7): a clear error naming the
-- coefficient ring, no promotion.
R = ZZ[x,y];
(err, out) = capture "solveSOS(x^2 + y^2)";
assert(err and match("coefficient ring", out) and not match("affinely", out))
(err, out) = capture "lowerBound(x^2 + 1)";
assert(err and match("coefficient ring", out))
