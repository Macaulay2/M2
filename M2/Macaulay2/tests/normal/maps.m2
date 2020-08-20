R = QQ[x,y]
p = map(R,QQ)
f = matrix {{x-y, x+2*y, 3*x-y}};
assert( kernel f === image map(R^{{-1},{-1},{-1}},R^{{-1},{-2}},{{-7, -x-2*y}, {-2, x-y}, {3, 0}}) )
g = map(R^1,QQ^3,p,f)
assert not isHomogeneous g
assert( kernel g == image map(QQ^3,QQ^1,{{-7}, {-2}, {3}}) )
assert( coimage g == cokernel map(QQ^3,QQ^1,{{-7}, {-2}, {3}}) )
g = map(R^1,,p,f,Degree => {1})
assert isHomogeneous g
R = QQ[x, Degrees => {{2:1}}]
M = R^1
S = QQ[z];
N = S^1
p = map(R,S,{x},DegreeMap => x -> join(x,x))
assert isHomogeneous p
f = matrix {{x^3}}
g = map(M,N,p,f,Degree => {3,3})
assert isHomogeneous g
assert (kernel g == 0)
assert( coimage g == S^1 )

