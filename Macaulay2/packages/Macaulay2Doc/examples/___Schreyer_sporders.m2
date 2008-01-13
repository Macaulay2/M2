R = ZZ/101[a..f];
m = matrix{{a,b,c,d}};
m1 = schreyerOrder m
F = source m1
g = syz m1
leadTerm g
schreyerOrder target m
schreyerOrder source g
R = QQ[a..f];
I = ideal"abc-def,a2c-d2f,aef-bcd,a3-d3,af2-cd2"
F = syz gens I
betti gens gb syz F
G = schreyerOrder F
betti gens gb syz G     
