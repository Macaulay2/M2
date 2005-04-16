ZZ/101[x,y]/(x^2-y^2) ** ZZ/101[a,b]/(a^3+b^3)
T = tensor(ZZ/101[x,y], ZZ/101[a,b], MonomialOrder => Eliminate 2)
options tensor
R = QQ[x,y]/(x^3-y^2);
T = R ** R
generators T
{T_0 + T_1, T_0 + T_2}
U = tensor(R,R,Variables => {x,y,x',y'})
x + y + x' + y'
