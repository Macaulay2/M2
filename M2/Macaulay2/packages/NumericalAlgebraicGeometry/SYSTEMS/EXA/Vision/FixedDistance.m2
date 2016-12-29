restart
FF = QQ
FF = ZZ/101
-- fixed distance between cameras; vector c is of length 1
-- R = FF[a_(1,1)..a_(3,3),b_(1,1)..b_(3,3),c_1,c_2,c_3]/ideal(c_1^2+c_2^2+c_3^2-1)
R = FF[b_(1,1)..b_(3,3),c_1,c_2,c_3]/ideal(c_1^2+c_2^2+c_3^2-1)
S = FF[f_(1,1)..f_(3,3)]
-- A = transpose genericMatrix(R,a_(1,1),3,3)
A = id_(R^3)
B = transpose genericMatrix(R,b_(1,1),3,3)
C = genericMatrix(R,c_1,3,1)
O = matrix{{0},{0},{0}}
M = (A|O)||(B|C)
L1 = reverse subsets(3,2) 
L2 = reverse subsets(3,2) + toList(3:{3,3})
matrix apply(3, i -> apply(3, j -> det M^(L1#i|L2#j)))
phi = map(R, S, flatten apply(3, i -> apply(3, j -> det M^(L1#i|L2#j))))
-- determinant of fundamental matrix
ker phi
F = transpose genericMatrix(S,f_(1,1),3,3)
det F
ideal det F == ker phi

