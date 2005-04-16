R = ZZ/101[x,y,z]
m = image vars R
m2 = image symmetricPower(2,vars R)
M = R^1/m2
N = R^1/m
C = cone extend(resolution N,resolution M,id_(R^1))
prune HH_0 C
prune HH_1 C
prune (m/m2)
