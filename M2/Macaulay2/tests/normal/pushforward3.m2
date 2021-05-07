R = QQ[a,b];
F = R^3
M = F/(F_0-F_1, a*F_0+b*F_2)
S = symmetricAlgebra M;
compactMatrixForm = false
g = transpose presentation S
B = ring g
assert( g === map(B^{{1,0},{1,1}},B^1,{{p_0-p_1}, {a*p_0+b*p_2}}) )
basis_2 S
assert( oo === map(S^1,S^{{-2,0},{-2,0},{-2,0}},{{p_1^2, p_1*p_2, p_2^2}}) )
f = basis(2,S,SourceRing=>R,Degree =>{2,0})
assert( f === map(S^1,R^3,map(S,R,{a, b}),{{p_1^2, p_1*p_2, p_2^2}},Degree=>{2, 0}) )
assert isHomogeneous f
kernel f
assert isHomogeneous kernel f
assert( kernel f === image map(R^3,R^{{-1},{-1}},{{a, 0}, {b, a}, {0, b}}) )
N = coimage f
assert isHomogeneous N
M2 = symmetricPower_2 M
assert isHomogeneous M2
assert( N === M2 )
M3 = symmetricPower_3 M
assert( M3 === cokernel matrix {{a, 0, 0}, {b, a, 0}, {0, b, a}, {0, 0, b}})
assert isHomogeneous M3
M11 = symmetricPower_11 M
assert isHomogeneous M11
assert( M11 === cokernel matrix {{a, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {b, a, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, b, a, 0, 0, 0, 0, 0, 0, 0, 0}, {0, 0, b, a, 0, 0, 0, 0, 0,
      0, 0}, {0, 0, 0, b, a, 0, 0, 0, 0, 0, 0}, {0, 0, 0, 0, b, a, 0, 0, 0, 0, 0}, {0, 0, 0, 0, 0, b, a, 0, 0, 0, 0}, {0, 0, 0, 0, 0, 0, b, a, 0, 0, 0}, {0,
      0, 0, 0, 0, 0, 0, b, a, 0, 0}, {0, 0, 0, 0, 0, 0, 0, 0, b, a, 0}, {0, 0, 0, 0, 0, 0, 0, 0, 0, b, a}, {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, b}})

L = coker transpose matrix {{a,b}}
assert( symmetricPower_1 L === L )
assert( (symmetricPower_2 L) === cokernel map(R^{{2},{2},{2}},R^{{1},{1}},{{a, 0}, {b, a}, {0, b}}) )
assert( (symmetricPower_3 L) === cokernel map(R^{{3},{3},{3},{3}},R^{{2},{2},{2}},{{a, 0, 0}, {b, a, 0}, {0, b, a}, {0, 0, b}}) )
assert( (symmetricPower_4 L) === cokernel map(R^{{4},{4},{4},{4},{4}},R^{{3},{3},{3},{3}},{{a, 0, 0, 0}, {b, a, 0, 0}, {0, b, a, 0}, {0, 0, b, a}, {0, 0, 0, b}}) )

R = QQ[a,b][x]
errorDepth = 0
basis(3,R^1)
assert isHomogeneous oo
M = coker matrix {{a*x^2,b*x^4,x^5}}
basis(1,M)
assert isHomogeneous oo
basis(2,M)
assert isHomogeneous oo
basis(3,M)
assert isHomogeneous oo
basis(4,M)
assert isHomogeneous oo
basis(5,M)
assert isHomogeneous oo
basis(6,M)
assert isHomogeneous oo
