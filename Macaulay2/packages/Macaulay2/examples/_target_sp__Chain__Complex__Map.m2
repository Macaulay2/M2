R = ZZ[x,y,z];
M = R^1/(x,y,z);
N = R^1/(x^2,y^2,x*y*z,z^2);
g = map(N,M,x*y);
f = res g;
target f
