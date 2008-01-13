R = QQ[a..d];
S = QQ[s,t];
F = map(S,R,{s^3, s^2*t, s*t^2, t^3})
ker F
G = map(S,R,{s^5, s^3*t^2-t, s*t-s, t^5})
ker(G, SubringLimit=>1)
