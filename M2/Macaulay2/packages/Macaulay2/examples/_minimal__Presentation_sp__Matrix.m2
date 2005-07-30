R = ZZ/32003[a..d];
f = map(coker matrix {{a,1,b},{c,3,b+d}},R^2)
g = prune f
source g
target g
m = matrix{{a,1,b},{c,3,b+d}}
prune m
