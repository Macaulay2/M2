R = ZZ/101[x,y]
C = chainComplex{matrix{{x,y}},matrix{{x*y},{-x^2}}}
C.dd^2 == 0
HH C
