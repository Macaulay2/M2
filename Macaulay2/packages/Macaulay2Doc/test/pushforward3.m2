R = QQ[a,b];
F = R^3
M = F/(F_0-F_1, a*F_0+b*F_2)
S = symmetricAlgebra M;
compactMatrixForm = false
transpose presentation S
basis_2 S
errorDepth = 0
f = basis(2,S,SourceRing=>R,Degree =>{2,0})
assert isHomogeneous oo
kernel f
assert isHomogeneous oo
N = coimage f
assert isHomogeneous oo
N' = symmetricPower_2 M
assert isHomogeneous oo
assert( N === N' )
symmetricPower_3 M
assert isHomogeneous oo
symmetricPower_11 M
assert isHomogeneous oo

