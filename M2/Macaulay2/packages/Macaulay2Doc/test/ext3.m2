-- this code from Hal Schenck crashes
R=ZZ/11[a..d, Degrees=>{{1,0},{ -2,1},{1,0},{0,1}},Heft=>{1,3}]
M = coker matrix{{c,b*c}}
res M
Ext^1(M,M)

R=ZZ/11[a..d, Degrees=>{{1,0},{ -2,1},{1,0},{0,1}}]
M = coker matrix{{c,b*c}}
res M
Ext^1(M,M)

gbTrace=3
res(M, Strategy=>0) -- works...
res(M, Strategy=>1) -- crashes
res(M, Strategy=>2) -- never ends
res(M, Strategy=>3) -- incorrect HF given... ?!
gens gb(relations M, Algorithm=>Sugarless)
 syz(relations M, Algorithm=>Sugarless)
syz gb(relations M, Algorithm=>Sugarless, Syzygies=>true)
