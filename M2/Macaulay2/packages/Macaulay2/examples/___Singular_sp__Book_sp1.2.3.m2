A = QQ[x,y,z,MonomialOrder=>Lex];
f = y^4*z^3+2*x^2*y^2*z^2+3*x^5+4*z^4+5*y^2
leadMonomial f
exponents leadMonomial f
leadTerm f
leadCoefficient f
someTerms(f,1,size f - 1)
someTerms(f,1,-1)
