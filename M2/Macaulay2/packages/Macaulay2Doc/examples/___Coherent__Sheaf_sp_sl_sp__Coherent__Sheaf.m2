X = Proj(QQ[x,y,z])
I = ideal(y^2*z-x*(x-z)*(x-11*z))
N = (sheaf module I)/(sheaf module I^2)
G = OO_X^1/I
HH^1(G)
HH^1(N)
