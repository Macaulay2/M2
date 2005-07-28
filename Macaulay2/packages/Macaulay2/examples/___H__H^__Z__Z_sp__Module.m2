R = QQ[a,b];
HH^2 (R^{-3})
HH^2 (R^{-4})
R = ZZ/101[x_0..x_4];
I = ideal(x_1*x_4-x_2*x_3, x_1^2*x_3+x_1*x_2*x_0-x_2^2*x_0, x_3^3+x_3*x_4*x_0-x_4^2*x_0)
M = R^1/module(I)
HH^1(M)
HH^2(M)
