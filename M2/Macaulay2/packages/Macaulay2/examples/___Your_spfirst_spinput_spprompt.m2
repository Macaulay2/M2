2+2
3/5 + 7/11
1*2*3*4
2^200
40!
100!
1;2;3*4
4*5;
oo
o5 + 1
"hi there"
s = "hi there"
s | " - " | s
s || " - " || s
{1, 2, s}
10*{1,2,3} + {1,1,1}
f = i -> i^3
f 5
g = (x,y) -> x * y
g(6,9)
apply({1,2,3,4}, i -> i^2)
apply({1,2,3,4}, f)
apply(1 .. 4, f)
apply(5, f)
scan(5, i -> print (i, i^3))
j=1; scan(10, i -> j = 2*j); j
R = ZZ/5[x,y,z];
(x+y)^5
R
describe R
F = R^3
F_1
F_{1,2}
F_{2,1,1}
f = matrix {{x,y,z}}
image f
ideal (x,y,z)
kernel f
generators oo
poincare kernel f
rank kernel f
presentation kernel f
cokernel f
N = kernel f ++ cokernel f
generators N
relations N
prune N
C = resolution cokernel f
C.dd
C.dd^2 == 0
betti C
R = ZZ/101[a .. r];
g = genericMatrix(R,a,3,6)
M = cokernel g
time C = resolution M
betti C
S = ZZ/101[t_1 .. t_9, u_1 .. u_9];
m = genericMatrix(S, t_1, 3, 3)
n = genericMatrix(S, u_1, 3, 3)
m*n
j = flatten(m*n - n*m)
gb j
generators gb j;
betti gb j
