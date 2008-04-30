-- this code from Hal Schenck crashes
R=ZZ/11[a..d, Degrees=>{{1,0},{ -2,1},{1,0},{0,1}},Heft=>{1,3}]
M = coker matrix{{c,b*c}}
res M
Ext^1(M,M)

R=ZZ/11[a..d, Degrees=>{{1,0},{ -2,1},{1,0},{0,1}}]
M = coker matrix{{c,b*c}}
assert try (res M; false) else true;
assert try(Ext^1(M,M); false) else true

