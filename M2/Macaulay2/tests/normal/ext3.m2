-- this code from Hal Schenck crashes, or it used to.  Now it works
R = ZZ/11[a..d, Degrees=>{{1,0},{ -2,1},{1,0},{0,1}},Heft=>{1,4}]
M = coker matrix{{c,b*c}}
C = res M
E = Ext^1(M,M)

S = ZZ/11[a..d, Degrees=>{{1,0},{ -2,1},{1,0},{0,1}}]
M = coker matrix{{c,b*c}}
C' = res M
E' = Ext^1(M,M)

f = map(R,S)

assert( C_0 == tensor(f,C'_0) )
assert( C_1 == tensor(f,C'_1) )
assert( E == tensor(f,E') )
