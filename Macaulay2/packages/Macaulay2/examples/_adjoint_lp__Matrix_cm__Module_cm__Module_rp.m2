R = QQ[x_1 .. x_12];
f = genericMatrix(R,2,6)
g = adjoint(f,R^2,R^{-1,-1,-1})
isHomogeneous g
