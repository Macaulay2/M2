S = ZZ/3[x,y,z];
isQuotientRing S
R = S/(x^2-y*z);
isQuotientRing R
ambient R
symAlg = symmetricAlgebra R^2;
isQuotientRing symAlg
sing = singularLocus R;
isQuotientRing sing
