R=QQ[w,x,y,z]
I=ideal(x^2-y*w,x^3-z*w^2)
L=irreducibleCharacteristicSeries I
apply(L_0, i-> L_1(i))
