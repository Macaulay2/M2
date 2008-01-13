S = QQ[x,y,z];
I = ideal(y*(x-1), z*(x-1));
dim I
gens gb I
y % I
R = QQ[x,y,z,MonomialOrder=>{Weights=>{-1,-1,-1},RevLex},Global=>false];
J = substitute(I,R)
gens gb J
dim J
y % J
J = substitute(J, {x=>x+1})
dim J
use ring I
I1 = substitute(I, {x=>x+1})
dim I1
