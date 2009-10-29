restart
--errorDepth = 0
needs "pieriHom.m2"
setRandomSeed 0
launchSimpleSchubert((2,4),{1,0},{1,0})
launchSimpleSchubert((3,7),{2,1,0},{1,1,0})
peek H	

restart
R = CC[x]
lift(5_R,coefficientRing R)
