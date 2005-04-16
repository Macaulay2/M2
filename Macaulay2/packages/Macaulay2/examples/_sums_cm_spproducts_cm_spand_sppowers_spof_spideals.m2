R = ZZ/101[a..d]/(b*c-a*d,c^2-b*d,b^2-a*c);
I = ideal (a*b-c,d^3);
J = ideal (a^3,b*c-d);
I+J
I*J
I^2
