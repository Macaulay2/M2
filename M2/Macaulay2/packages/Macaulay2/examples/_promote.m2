R = QQ[a..d]; f = a^2;
S = R/(a^2-b-1);
promote(2/3,S)
F = map(R,QQ);  F(2/3)
promote(f,S)
G = map(S,R); G(f)
use R;
I = ideal(a^2,a^3,a^4)
promote(I,S)
m = image matrix{{a^2,a^3,a^4}}
promote(gens m,S)
G m
m ** S
