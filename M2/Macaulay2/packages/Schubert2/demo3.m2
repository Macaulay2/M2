-- Here are some examples and tests of our own.

compactMatrixForm = false
path = prepend("../", path)
needsPackage "Schubert2"
F = flagBundle ( {4,2}, VariableNames => {,c} )
AF = intersectionRing F
schubertCycle'((0,1),F)
F_(0,1)
integral oo
assert( oo == 1 )
F_(4,5)
assert( oo == 1 )
t = {1, c_1, c_2, c_1^2-c_2, c_1*c_2, c_2^2, c_1^3-2*c_1*c_2, c_1^2*c_2-c_2^2, c_1*c_2^2, c_2^3, c_1^4-3*c_1^2*c_2+c_2^2, c_1^3*c_2-2*c_1*c_2^2, c_1^2*c_2^2-c_2^3, c_1*c_2^3, c_2^4}
b = reverse flatten for i from 0 to 5 list for j from i+1 to 5 list F_(i,j)
assert( b == t )
d = flatten for i from 0 to 4 list for j from 0 to i list F_{i,j}
assert( d == t )

