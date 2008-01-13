P = projectiveHilbertPolynomial 3
s = hilbertSeries P
numerator s
R = QQ[a..h];
I = ideal (a*b, c*d, e*f);
P=hilbertPolynomial(I)
s = hilbertSeries P
numerator s
