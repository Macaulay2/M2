R = QQ[a..d];
S = QQ[s,t];
F = map(S,R,{s^5, s^3*t^2-t, s*t-s, t^5})
J = ker F;
G = map(S,R,{s^5, s^3*t^2-t, s*t-s, t^5})
gbTrace=3
time ker(G, SubringLimit=>1);
numgens oo == 1

A = QQ[s,t,a..d,h,MonomialOrder=>Eliminate 2, Degrees=>{1,1,5,5,2,5,1}]
I = ideal({a,b,c,d} - {s^5, s^3*t^2-t*h^4, s*t-s*h, t^5})
gbTrace=3
J = gens gb(I, SubringLimit=>1);
J1 = selectInSubring(1,J)
J = gens gb(I, SubringLimit=>1);
J1 = selectInSubring(1,J)
assert(numgens source J1 == 1)
-- Local Variables:
-- M2-send-to-buffer: "*gud*"
-- End:
