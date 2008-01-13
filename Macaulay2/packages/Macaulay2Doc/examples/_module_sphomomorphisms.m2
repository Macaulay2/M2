R = QQ[x,y]/(y^2-x^3);
M = module ideal(x,y)
F = map(R^1,M,matrix{{y,x^2}})
source F
target F == R^1
matrix F
isWellDefined F
isIsomorphism F
inc = inducedMap(R^1, M)
G = F // inc
target G == M and source G == M
inc * G == F
isWellDefined G
isIsomorphism G
prune coker G
kernel G == 0
