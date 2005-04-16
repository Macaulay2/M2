R = ZZ/101[a..d];
I = ideal(a*b-c*d, (a*c-b*d)^2);
primaryDecomposition I
primaryDecomposition(I, Strategy => EHV)
