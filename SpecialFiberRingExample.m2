--Special Fiber Ring Example
restart
R=ZZ/23[a,b,c,d]
msq=ideal(a^2, a*b, b^2,a*c,b*c, c^2,a*d, b*d, c*d, d^2)
sfi=specialFiberIdeal(msq)
S=ring sfi
T=ZZ/23[S_0,S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_8,S_9]
M=matrix{{S_0,S_1,S_3,S_6},{S_1,S_2,S_4,S_7},{S_3,S_4,S_5,S_8},{S_6,S_7,S_8,S_9}}
i=minors(2,M)
j=i+ideal(a,b,c,d)
sfi==j

