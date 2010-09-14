R = QQ[a,b];
F = R^3
M = F/(F_0-F_1, a*F_0+b*F_2)
S = symmetricAlgebra M;
compactMatrixForm = false
transpose presentation S
basis_2 S
f = basis(2,S,SourceRing=>R,Degree =>{2,0})
assert isHomogeneous oo
kernel f
assert isHomogeneous oo
N = coimage f
assert isHomogeneous oo
M2 = symmetricPower_2 M
assert isHomogeneous oo
assert( N === M2 )
M3 = symmetricPower_3 M
assert( M3 === cokernel matrix {{a, 0, 0}, {b, a, 0}, {0, b, a}, {0, 0, b}})
assert isHomogeneous M3
M11 = symmetricPower_11 M
assert isHomogeneous M11
assert( M11 === cokernel matrix {{a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {b, a, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, b, a, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, b, a, 0, 0, 0, 0, 0,
      0, 0}, {0, 0, 0, b, a, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, b, a, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, b, a, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, b, a, 0, 0, 0}, {0,
      0, 0, 0, 0, 0, 0, b, a, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, b, a, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, b, a}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, b}})

R = QQ[a,b][x]
errorDepth = 0
truncate(3,R^1)
assert isHomogeneous oo
M = coker matrix {{a*x^2,b*x^4,x^5}}
truncate(1,M)
assert isHomogeneous oo
truncate(2,M)
assert isHomogeneous oo
truncate(3,M)
assert isHomogeneous oo
truncate(4,M)
assert isHomogeneous oo
truncate(5,M)
assert isHomogeneous oo
truncate(6,M)
assert isHomogeneous oo
