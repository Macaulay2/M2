R = QQ[a..d]
S = QQ[s,t]
F = map(S,R,{s^4,s^3*t,s*t^3,t^4})
f = matrix{{a,b,c,d}}
tensor(F,f)
tensor(F,image f)
tensor(S,F,f)
tensor(S,F,image f)
