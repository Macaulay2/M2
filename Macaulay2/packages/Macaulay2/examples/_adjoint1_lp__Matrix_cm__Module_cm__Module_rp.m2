R = QQ[x_1 .. x_12];
f = genericMatrix(R,6,2)
g = adjoint1(f,R^2,R^3)
isHomogeneous g
