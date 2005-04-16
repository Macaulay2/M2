R = ZZ/32003[x,y,z];
P = R^3;
M = image(x*P_{1}+y*P_{2} | z*P_{0})
N = image(x^4*P_{1} + x^3*y*P_{2} + x*y*z*P_{0})
h = inducedMap(M,N)
source h == N
target h == M
ambient M == ambient N
