R = QQ[x,y];
M = coker vars R
N = coker matrix {{x}}
f = inducedMap(M,N)
g = res f
g * (source g).dd == (target g).dd * g
F = cone g
prune HH_0 F
prune HH_1 F
prune kernel f
C = res M
id_C
x * id_C
inducedMap(C ** R^1/x,C)
g ** R^1/x
q = map(C,C,i -> (i+1) * id_(C_i))
C.dd * q == q * C.dd
