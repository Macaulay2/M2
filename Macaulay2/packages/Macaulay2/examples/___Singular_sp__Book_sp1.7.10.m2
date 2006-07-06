A = QQ[x,y];
I = ideal "x10+x9y2,y8-x2y7";
transpose gens gb I
A1 = QQ[x,y,MonomialOrder=>Lex];
I = substitute(I,A1)
transpose gens gb I
B = QQ[x,y,MonomialOrder=>{Weights=>{-1,-1},2},Global=>false];
I = substitute(I,B)
transpose gens gb I
B = QQ[x,y,MonomialOrder=>{Weights=>{-1,0},Weights=>{0,-1}},Global=>false];
I = substitute(I,B)
transpose gens gb I
M = matrix{{1,1,1},{0,-1,-1},{0,0,-1}}
mo = apply(entries M, e -> Weights => e)
C = QQ[t,x,y,MonomialOrder=>mo];
I = homogenize(substitute(I,C),t)
transpose gens gb I
substitute(transpose gens gb I, {t=>1})
