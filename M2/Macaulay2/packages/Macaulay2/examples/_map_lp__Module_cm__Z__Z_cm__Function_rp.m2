R = GF(9,Variable=>a)[x,y,z];
f = map(R^1, 3, (i,j) -> (a^j * x - y)^(j+1))
source f
isHomogeneous f
