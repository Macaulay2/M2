R = ZZ/101[x_0..x_2];
V = Proj R;
S = sheaf(image matrix {{x_0^3+x_1^3+x_2^3}})
h = hilbertPolynomial S
hilbertPolynomial(S, Projective=>false)
