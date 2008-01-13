K3 = Proj(QQ[x_0..x_3]/(x_0^4+x_1^4+x_2^4+x_3^4-11*x_0*x_1*x_2*x_3))
omega1 = cotangentSheaf(1,K3);
HH^1(omega1)
FermatQuintic = Proj(QQ[x_0..x_4]/(x_0^5+x_1^5+x_2^5+x_3^5+x_4^5))
omega1 = cotangentSheaf(1,FermatQuintic);
HH^1(omega1)
omega2 = cotangentSheaf(2,FermatQuintic);
HH^1(omega2)
HH^2(omega1)
