n = 5
R = ZZ/32003[vars(0..n-1)]
S = ZZ/32003[vars(26..26+n)]
phi = map(R,S,random(R^1,R^{(numgens S):-2}))
gbTrace=3
ker phi;
betti oo
R = ZZ/32003[a..f]
S = ZZ/32003[A..G]
phi = map(R,S,random(R^1,R^{7:-2}))
gbTrace=3
ker phi;

syz phi.matrix
