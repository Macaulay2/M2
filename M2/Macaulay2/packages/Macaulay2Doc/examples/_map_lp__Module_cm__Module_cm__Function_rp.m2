R = ZZ[a..c];
f = map(R^3,R^{0,-1,-2,-3},(i,j) -> R_i^j)
isHomogeneous f
g = map(R^3,4,(i,j) -> R_i^j)
degrees g
isHomogeneous g
h = matrix table(3,4,(i,j) -> R_i^j)
degrees h
isHomogeneous h
