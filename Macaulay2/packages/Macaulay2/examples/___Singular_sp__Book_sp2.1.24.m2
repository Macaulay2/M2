A = QQ[x,y,z]
N = image matrix{{x*y,0},{0,x*z},{y*z,z^2}}
N + x*N
f = matrix{{x*y,x*z},{y*z,z^2}}
M = image f
g = gens M
f == g
N = cokernel f
presentation N
presentation M
syz f
kernel f
