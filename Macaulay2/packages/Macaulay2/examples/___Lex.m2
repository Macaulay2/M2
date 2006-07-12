R = QQ[a..d, MonomialOrder => Lex];
a^3 + a^2*b^2 + b*c
B = QQ[a..d,MonomialOrder=>Lex,MonomialSize=>16];
a^(2^15-1)
C = QQ[a..d,MonomialOrder=>Lex,MonomialSize=>8];
try a^(2^15-1) else "failed"
a^(2^7-1)
B = QQ[a..d,MonomialSize=>16,MonomialOrder=>{Weights => {1,2,3,4}, Lex}];
a^2 + b+ c + b*d
