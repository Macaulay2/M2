-- BUGS.md 10: Parameters of a problem without parameters is null or a 0x1
-- matrix.  Decided (LOG.md 1.7 item 8): always null.
R = QQ[x,y];
assert((solveSOS(x^2 + y^2))#Parameters === null)
assert((solveSOS(x^2 + y^2, RoundTol => infinity))#Parameters === null)
R = RR[x,y];
assert((solveSOS(x^2 + y^2))#Parameters === null)
S = QQ[x,y]/ideal(x^2 + y^2 - 1);
assert((solveSOS(10 - x^2 - y, 2, RoundTol => infinity))#Parameters === null)
