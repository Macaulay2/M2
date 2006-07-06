A2 = QQ[x,y,z];
A2 = QQ[x,y,z,MonomialOrder=>GRevLex];
f = x^3*y*z+y^5+z^4+x^3+x*y^2
A1 = QQ[x,y,z,MonomialOrder=>Lex];
substitute(f,A1)
A3 = QQ[x,y,z,MonomialOrder=>{Weights=>{1,1,1},Lex}];
substitute(f,A3)
A4 = QQ[x,y,z,MonomialOrder=>{Weights=>{5,3,2},Lex}];
substitute(f,A4)
A = QQ[x,y,z,MonomialOrder=>{1,2}];
substitute(f,A)
A = QQ[x,y,z,MonomialOrder=>{Weights=>{-1,0,0},Weights=>{0,-1,0},Weights=>{0,0,-1}},Global=>false];
substitute(f,A)
A = QQ[x,y,z,MonomialOrder=>{Weights=>{-1,-1,-1},GRevLex},Global=>false];
substitute(f,A)
