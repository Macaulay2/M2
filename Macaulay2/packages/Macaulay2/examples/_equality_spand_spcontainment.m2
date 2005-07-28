R = QQ[a..d];
I = ideal (a^2*b-c^2, a*b^2-d^3, c^5-d);
J = ideal (a^2,b^2,c^2,d^2);
I == J
I != J
(1+a+a^3+a^4) % J
(1+a+a^3+a^4) % J == 0
a^4 % J == 0
isSubset(I,J)
isSubset(I,I+J)
isSubset(I+J,I)
I = ideal (a^2-1,a^3+3);
I == 1
S = R/I
S == 0
