-- Here are some examples and tests of our own.

compactMatrixForm = false
loadPackage "Schubert2"

F = flagBundle ( {4,2}, VariableNames => {,c} )
AF = intersectionRing F
schubertCycle(F,(0,1))
F_(0,1)
integral oo
assert( oo == 1 )
F_(4,5)
assert( oo == 1 )
b = reverse flatten for i from 0 to 5 list for j from i+1 to 5 list F_(i,j)
assert( b == {1, c_1, c_2, c_1^2-c_2, c_1*c_2, c_2^2, c_1^3-2*c_1*c_2, c_1^2*c_2-c_2^2, c_1*c_2^2, c_2^3, c_1^4-3*c_1^2*c_2+c_2^2, c_1^3*c_2-2*c_1*c_2^2, c_1^2*c_2^2-c_2^3, c_1*c_2^3, c_2^4})
