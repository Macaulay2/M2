R = QQ[a..d];
a^3 + b^2 + b*c
S = QQ[a..d, MonomialOrder => GRevLex => {1,2,3,4}];
a^3 + b^2 + b*c
B1 = QQ[a..d,MonomialSize=>16,MonomialOrder=>GRevLex];
B = QQ[a..d,MonomialSize=>16];
a^(2^15-1)
C = QQ[a..d,MonomialSize=>8,MonomialOrder=>GRevLex];
try a^(2^15-1) else "failed"
a^(2^7-1)
