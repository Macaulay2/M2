R = QQ[a..d];
S = QQ[s,t];
F = map(R,matrix{{s^4,s^3*t,s*t^3,t^4}})
kernel F
