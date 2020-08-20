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

F = R^5
M = wedgeProduct(2,3,F);
assert(numRows M == 1)
assert(numColumns M == 100)
ansM = matrix(R, {{
            0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
            0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 
            0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 
            0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 
            0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 
            0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 
            0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 
            0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 
            0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 
            1, 0, 0, 0, 0, 0, 0, 0, 0, 0}})
assert(M == ansM)
