gbTrace 3
R = ZZ/101[a,b,c,d]
f = matrix{{a*b-c,b^2-d}}
I = ideal f
S = R/I
gb f
