Quintic = Proj(QQ[x_0..x_4]/ideal(x_0^5+x_1^5+x_2^5+x_3^5+x_4^5-101*x_0*x_1*x_2*x_3*x_4))
singularLocus(Quintic)
omegaQuintic = cotangentSheaf(Quintic);
h11 = rank HH^1(omegaQuintic)
h12 = rank HH^2(omegaQuintic)
h21 = rank HH^1(cotangentSheaf(2,Quintic))
hh^(2,1)(Quintic)
hh^(1,1)(Quintic)
euler(Quintic)
SchoensQuintic = Proj(QQ[x_0..x_4]/ideal(x_0^5+x_1^5+x_2^5+x_3^5+x_4^5-5*x_0*x_1*x_2*x_3*x_4))
Z = singularLocus(SchoensQuintic)
degree Z
II'Z = sheaf module ideal Z
defect = rank HH^1(II'Z(5))
h11 = defect + 1
quinticsJac = numgens source basis(5,ideal Z)
h21 = rank HH^0(II'Z(5)) - quinticsJac
chiW = euler(Quintic)+2*degree(Z)
