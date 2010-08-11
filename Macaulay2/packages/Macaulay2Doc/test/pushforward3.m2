R = QQ[a,b];
F = R^3
M = F/(F_0-F_1, a*F_0+b*F_2)
S = symmetricAlgebra M;
compactMatrixForm = false
transpose presentation S
basis_2 S
basis(2,S,SourceRing=>R)
symmetricPower(ZZ,Module) := (d,M) -> coimage basis(d,symmetricAlgebra M,SourceRing => ring M);
symmetricPower_2 M
symmetricPower_3 M
symmetricPower_11 M

