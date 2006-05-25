R = QQ[a,b,c]; S = QQ[s,t];
F = map(S,R,{s^3-t^2, s^3-t, s-t})
target F
source F
F.matrix
F (a+b)
I = kernel F
F I
J = preimage(F, ideal(s-3))
isSubset(F J, ideal(s-3))
G = map(R,R,{a=>b*c,b=>a*c,c=>a*b})
G*G
ker G == 0
isInjective G
coimage G
