R = QQ[s,t,x,y,z, MonomialOrder=>Eliminate 2];
I = ideal(x-s^3-s*t-1, y-t^3-3*t^2-t, z-s*t^3)
time leadTerm gens gb I
selectInSubring(1,gens gb I)

R = QQ[x,y,z,s,t];
I = ideal(x-s^3-s*t-1, y-t^3-3*t^2-t, z-s*t^3)
time eliminate(I,{s,t})

R1 = QQ[x,y,z,s,t, Degrees=>{3,3,4,1,1}];
I1 = substitute(I,R1);
time eliminate(I1,{s,t})

A = QQ[s,t]
B = QQ[x,y,z]
F = map(A,B,{s^3+s*t+1, t^3+3*t^2+t, s*t^3})
time kernel F

use ring I
time f1 = resultant(I_0,I_2,s)
time f2 = resultant(I_1,f1,t)

