n = 6
S = ZZ/101[vars(0..n-1)];
i1 = monomialCurveIdeal(S, 1..n-1)
i2 = monomialCurveIdeal(S, 1..n-1)
j1 = ideal(map(S^1,S^n, (p,q)->S_q^5))
j2 = ideal(map(S^1,S^n, (p,q)->S_q^5))
time quotient(i1^3,j1^2,Strategy=>Iterate);
time quotient(i2^3,j2^2,Strategy=>Quotient);
S =ZZ/101[vars(0..4)];
i =ideal vars S;
j =ideal vars S;
i3 = i^3; i5 = i^5;
j3 = j^3; j5 = j^5;
time quotient(i5,i3,Strategy=>Iterate);
time quotient(j5,j3,Strategy=>Quotient);
