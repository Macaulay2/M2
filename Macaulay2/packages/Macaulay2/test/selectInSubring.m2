R = QQ[x..z, MonomialOrder => Eliminate 2]
assert(selectInSubring(0,vars R) == matrix{{x,y,z}})
assert(selectInSubring(1,vars R) == matrix{{z}})
assert(selectInSubring(2,vars R) == 0)

R = QQ[x..z, MonomialOrder => Eliminate 2, Degrees => {1,0,1}]
assert(selectInSubring(0,vars R) == matrix{{x,y,z}})
assert(selectInSubring(1,vars R) == matrix{{z}})
assert(selectInSubring(2,vars R) == 0)
