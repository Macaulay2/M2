R = ZZ/101[x,y,z];
M = R^4
m = matrix{{x,y,z},{y,z,0}}
target m == R^2
degrees M
F = R^{1,4:2,3,3:4}
degrees F
S = ZZ[a,b,c, Degrees=>{{1,2},{2,0},{3,3}}]
N = S ^ {{-1,-1},{-4,4},{0,0}}
degree N_0
degree (a*b*N_1)
