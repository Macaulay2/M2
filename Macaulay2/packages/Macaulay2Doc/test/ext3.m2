-- this code from Hal Schenck crashes
R=ZZ/11[a..d, Degrees=>{{1,0},{ -2,1},{1,0},{0,1}}];
OM = ker matrix{{a}};
A = coker matrix{{c,b*c}};
Ext^2(A,OM);
