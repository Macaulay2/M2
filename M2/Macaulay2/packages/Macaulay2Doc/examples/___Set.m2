A = set {1,2};
R = QQ[a..d];
B = set{a^2-b*c,b*d}
toList B
member(1,A)
member(-b*c+a^2,B)
A ** A
A^**2
set{1,3,2} - set{1}
set{4,5} + set{5,6}
set{4,5} * set{5,6}
set{1,3,2} === set{1,2,3}
I = ideal(a,b); J = ideal(b,a);
I == J
I === J
C = set(ideal(a,b),ideal(b,a))
C1 = set(trim ideal(a,b),trim ideal(b,a))
