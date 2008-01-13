R = QQ[a..d];
F = matrix{{a,b},{c,d}}
isInjective F
G = substitute(F, R/(det F))
isInjective G
S = QQ[r,s,t];
phi = map(S,R,{r^3, r^2*s, r*s*t, s^3})
isInjective phi
S' = coimage phi
phi' = phi * map(R,S')
isInjective phi'
