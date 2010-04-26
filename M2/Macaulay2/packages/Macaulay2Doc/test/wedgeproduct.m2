-- test of wedge product and koszul

R = QQ[a..d]
koszul(-1, vars R)
koszul(0, vars R)
koszul(1, vars R)
koszul(2, vars R)
koszul(3, vars R)
koszul(4, vars R)
koszul(5, vars R)
koszul(6, vars R)

F = R^10
M = wedgeProduct(3,4,F);
assert(numRows M == 120)
assert(numColumns M == 25200)
-- how else to check wedgeProduct??