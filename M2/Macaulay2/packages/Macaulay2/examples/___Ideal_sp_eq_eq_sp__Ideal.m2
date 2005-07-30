R = QQ[a..d];
I = ideal(a^2,b);
J = ideal(a,c);
I + J == ideal(a,b,c)
