-- Test of exterior power of a matrix, and determinants
-- and associated optional computation arguments

R = ZZ/101[a..d]
m = matrix{{a,b},{c,d}}
minors(2,m)
minors(1,m)
minors(0,m)
minors(-1,m)
minors(3,m)

exteriorPower(2,m)
exteriorPower(1,m)
exteriorPower(0,m)

R = ZZ/101[vars(0..14)]
m = genericMatrix(R,a,3,5)
exteriorPower(3,m)
m = genericMatrix(R,a,2,5)
mm = transpose exteriorPower(2,m)
transpose mm

R = ZZ[vars(0..24)]
m = genericMatrix(R,a,5,5)
minors(1,m)
minors(2,m)
minors(3,m)
minors(4,m)
minors(5,m)
minors(6,m)
minors(0,m)
minors(-1,m)
exteriorPower(-1,m)
exteriorPower(0,m)
exteriorPower(1,m)
exteriorPower(2,m)
exteriorPower(3,m)
exteriorPower(4,m)
exteriorPower(5,m)

R = ZZ[vars(0..24)]
m = genericMatrix(R,a,3,6)
minors(1,m)
minors(2,m)
minors(3,m)
minors(4,m)
minors(5,m)
minors(6,m)
minors(0,m)
minors(-1,m)
exteriorPower(-1,m)
exteriorPower(0,m)
time exteriorPower(1,m)
time exteriorPower(2,m)
time exteriorPower(3,m)
time exteriorPower(4,m)
time exteriorPower(5,m)

x = symbol x
R = ZZ[x_1 .. x_36]
m = genericMatrix(R,x_1,6,6)
time exteriorPower(3,m)
time exteriorPower(4,m)
time exteriorPower(5,m)
time exteriorPower(6,m)

x = symbol x
R = ZZ[x_1 .. x_49]
m = genericMatrix(R,x_1,7,7)
time exteriorPower(3,m);
time exteriorPower(4,m);

R = ZZ/101
F = R^4
wedgeProduct(1,2,F)
wedgeProduct(0,0,F)
wedgeProduct(0,1,F)
exteriorPower(0,F)  -- doesn't even accept this yet!
target exteriorPower(0,id_F)
F == target exteriorPower(1,id_F)
target exteriorPower(2,id_F)
