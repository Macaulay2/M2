R = QQ[a..e];
g = matrix{{a^3+b^3+c^3+d^3+e^3-d^2*e-a*b*c-a*d*e}}
f = fromDual g
I = ideal f
res I
betti oo
toDual(3,f)
f = matrix{{a*b,c*d,e^2}}
toDual(1,f)
toDual(2,f)
toDual(3,f)
g = toDual(4,f)
fromDual g
