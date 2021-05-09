R = ZZ/101[a..d]
m = symmetricPower(2,vars R)
assert( m === matrix {{a^2, a*b, a*c, a*d, b^2, b*c, b*d, c^2, c*d, d^2}} )
assert( isHomogeneous m )
R = ZZ/101[a,b]
M = symmetricPower(2,image vars R)
assert( M == cokernel map(R^{3:-2},R^{2:-3},{{-b, 0}, {a, -b}, {0, a}}) )
assert( isHomogeneous M )
M = symmetricPower(3,image vars R)
assert( M == cokernel map(R^{{-3},{-3},{-3},{-3}},R^{{-4},{-4},{-4}},{{-b, 0, 0}, {a, -b, 0}, {0, a, -b}, {0, 0, a}}) )
assert( isHomogeneous M )
