R = ZZ/101[a..d];
S = ZZ/101[s,t];
f = map(S,R,matrix{{s^4, s^3*t, s*t^3, t^4}})
pushForward1(f,S^1)
pushForward1(f,S^1,MonomialOrder=>ProductOrder)
f = map(S,R,matrix{{s^4, s^3*t, s*t^3, t^4}})
pushForward1(f,S^1,DegreeLimit=>4)
pushForward1(f,S^1,PairLimit=>0)
