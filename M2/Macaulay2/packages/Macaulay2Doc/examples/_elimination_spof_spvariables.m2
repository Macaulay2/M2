R = QQ[s,t,x,y,z, MonomialOrder=>Eliminate 2];
I = ideal(x-s^3-s*t-1, y-t^3-3*t^2-t, z-s*t^3)
time leadTerm gens gb I
G = selectInSubring(1,gens gb I)
ans1 = G_(0,0)
R = QQ[x,y,z,s,t];
I = ideal(x-s^3-s*t-1, y-t^3-3*t^2-t, z-s*t^3)
time G = eliminate(I,{s,t})
ans2 = G_0
R1 = QQ[x,y,z,s,t, Degrees=>{3,3,4,1,1}];
I1 = substitute(I,R1);
time G = eliminate(I1,{s,t})
ans3 = G_0
A = QQ[s,t];
B = QQ[x,y,z];
F = map(A,B,{s^3+s*t+1, t^3+3*t^2+t, s*t^3})
time G = kernel F
ans4 = G_0
use ring I
time f1 = resultant(I_0,I_2,s)
time f2 = resultant(I_1,f1,t)
ans5 = -f2
L = {ans1,ans2,ans3,ans4,ans5};
L = apply(L, f -> substitute(f,B));
length unique L
