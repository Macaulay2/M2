R = QQ[a..z]
f = genericMatrix(R,3,5)
M = coker gens minors_3 f
C = res M
D = eagonNorthcott f
H = prune HH D
assert( H_0 == M and H_1 == 0 and H_2 == 0 and H_3 == 0 )
