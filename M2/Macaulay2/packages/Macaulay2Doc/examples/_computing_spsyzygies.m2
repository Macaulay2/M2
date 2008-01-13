R = QQ[x..z];
f = vars R
K = kernel f
L = super K
L == source f
g = generators K
f*g
f*g == 0
