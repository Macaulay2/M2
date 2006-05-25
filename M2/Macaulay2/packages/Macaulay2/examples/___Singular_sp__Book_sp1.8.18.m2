A = QQ[x,y,z];
B = QQ[a,b];
phi = map(B,A,{a^2,a*b,b^2})
kernel phi
C = QQ[x,y,z,a,b]
H = ideal(x-a^2, y-a*b, z-b^2);
eliminate(H, {a,b})
