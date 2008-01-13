R = QQ[a,b,c];
ideal(a^2-b,a^3) == ideal(b^2, a*b, a^2-b)
L = ideal(a^2-a-1,a^3+a+3)
L == 1
L == 0
m = matrix{{a,b},{c,a}}
n = map(R^2,R^2,m)
m == n
source m == source n
m-n == 0
A = QQ[x,y,z]; B = QQ[x,y,z];
A == B
image matrix {{2,a},{1,5}} == R^2
image matrix {{2,a},{0,5}} == R^2
