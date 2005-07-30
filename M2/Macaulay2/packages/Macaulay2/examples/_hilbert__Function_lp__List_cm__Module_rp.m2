R = QQ[a..d, Degrees=>{4:{1,1}}];
M = coker matrix {{a,c,d},{c,b,d}}
hilbertFunction({2,2}, M)
B = basis({2,2},M)
numgens source B
R = QQ[a..d];
M = coker matrix {{a,c,d},{c,b,d}}
hilbertFunction(2, M)
basis(2,M)
