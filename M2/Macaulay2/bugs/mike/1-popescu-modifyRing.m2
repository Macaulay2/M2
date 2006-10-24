R = ZZ/101[a,b,Degrees=>{{1,2},{3,4}}]

-- These 3 do not work:
modifyRing(R, DegreeRank=>1) -- error
modifyRing(R, DegreeRank=>1, Degrees=>{1,1}) -- monomial order wrong
modifyRing(R, Degrees=>{1,1}) -- error
