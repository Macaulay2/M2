R = QQ[a..d,MonomialOrder=>ProductOrder{1,3}];
I = ideal(a*b-c*d, a*c-b*d)
leadTerm(1,I)
