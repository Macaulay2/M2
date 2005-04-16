R = ZZ/101[a..d];
m = matrix{{a,b,c,d}};
m1 = schreyerOrder m
F = source m1
g = syz m1
leadTerm g
schreyerOrder target m
schreyerOrder source g
