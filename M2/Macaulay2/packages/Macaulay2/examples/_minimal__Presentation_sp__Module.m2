R = ZZ/32003[a..d];
M = coker matrix {{a,1,b},{c,3,b+d}}
N = minimalPresentation M
peek N.cache
g = N.cache.pruningMap
g^-1
