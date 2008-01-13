R = ZZ[a,b,c];
F = a^3-b^2*c-11*c^2
I = ideal(F,diff(a,F),diff(b,F),diff(c,F))
I : (ideal(a,b,c))^3
S = QQ[x,y,z];
J = image vars S
I = image symmetricPower(2,vars S)
(I++I) : (J++J)
(I++I) : x+y+z
quotient(I,J)
quotient(gens I, gens J)        
I = ideal(x^2-y^2, y^3)
J = ideal((x+y+z)^3, z^2)
L = intersect(I,J)
L : z^2
L : I == J
