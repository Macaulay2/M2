R = QQ[a..d];
S = QQ[s,t];
F = map(S,R,{s^5, s^3*t^2-t, s*t-s, t^5})
J = ker F;
numgens J
G = map(S,R,{s^5, s^3*t^2-t, s*t-s, t^5})
K = ker(G, SubringLimit=>1);
numgens K
