R = ZZ/32003[x,y,z]
I = ideal"xy-z2,x2-yz"
A = R/I
comps = apply(decompose I, J -> R/J)
oo/integralClosure

