-- BUGS.md 5: the empty SOSPoly (0*s): value is the integer 0, sub fails.
R = QQ[x];
s = sosPoly(R, {x}, {1});
z = 0 * s;
assert(value z === 0_R)
S = QQ[x,y];
assert(value sub(z, S) === 0_S)
assert(z == 0_R and z + s == s)
