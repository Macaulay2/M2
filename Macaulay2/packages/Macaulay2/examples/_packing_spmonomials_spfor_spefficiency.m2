A = QQ[a..d,MonomialSize=>8]
B = QQ[x,y,z,w,MonomialSize=>16,MonomialOrder=>Lex]     
C = QQ[a,b,c,x,y,z,w,MonomialOrder=>{MonomialSize=>8,3,MonomialSize=>32,Lex=>4}];
D = QQ[a..d,MonomialOrder=>Lex];
a^1000000000
E = QQ[a..d,MonomialSize=>16,MonomialOrder=>Lex];
F = QQ[a..d,MonomialSize=>8,MonomialOrder=>Lex];
