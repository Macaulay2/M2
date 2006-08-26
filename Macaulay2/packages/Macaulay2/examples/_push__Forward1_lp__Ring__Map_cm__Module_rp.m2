R = ZZ/101[a..d];
S = ZZ/101[s,t];
F = map(S,R,matrix{{s^4, s^3*t, s*t^3, t^4}})
pushForward1(F,S^1)
F = map(S,R,matrix{{s^4, s^3*t, s*t^3, t^4}})     
time pushForward1(F,S^1,MonomialOrder=>ProductOrder)
F = map(S,R,matrix{{s^4, s^3*t, s*t^3, t^4}})
pushForward1(F,S^1,DegreeLimit=>8)
pushForward1(F,S^1,StopBeforeComputation => true)
