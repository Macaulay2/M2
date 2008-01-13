A = QQ[x,y];
I = ideal(x^10+x^9*y^2, y^8-x^2*y^7);
f = x^2*y^7+y^14;
f % I
f = x*y^13+y^12;
f % I
K = ideal(f,x^2*y^7+y^14);
(gens K) % I
isSubset(K,I)
K == I
K = ideal(f,y^14+x*y^12);
(gens K) % I
isSubset(K,I)
K == I
