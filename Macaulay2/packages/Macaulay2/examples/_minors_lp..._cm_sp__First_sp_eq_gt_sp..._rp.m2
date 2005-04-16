R = ZZ[a..f];
M = matrix{{a,b,c},{d,e,f}}
minors(2,M,First=>{{0,1},{0,2}})
minors(2,M,First=>{{0,1},{0,2}},Limit=>1)
