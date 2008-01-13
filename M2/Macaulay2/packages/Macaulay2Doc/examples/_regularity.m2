R=ZZ/32003[a..d];
I=ideal(a^20,b^20,a*c^19-b*d^19);
regularity I
J=ideal(a^3,a^2*b,a*b^6,a^2*c);
C=resolution J
betti C
regularity C
