R = QQ[a..d];
S = QQ[s,t];
F = map(S,R,{s^5, s^3*t^2-t, s*t-s, t^5})
J = ker F;
G = map(S,R,{s^5, s^3*t^2-t, s*t-s, t^5})
gbTrace=3
time ker(G, SubringLimit=>1);

-- Doesn't stop!!
