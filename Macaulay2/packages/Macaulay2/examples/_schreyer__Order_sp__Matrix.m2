R = ZZ/101[a..d];
m = matrix{{a,b,c,d}};
f = schreyerOrder m
g = syz f
leadTerm g
