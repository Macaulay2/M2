R = ZZ[x,y,z]
f = y^2 - y - x^3 + x^2
f = homogenize(f,z)
T = R/f
dim T
