R = QQ[x,y]/(x^3,y^2);
N = cokernel matrix {{x^2, x*y}}
H = Ext(N,N);
ring H
S = ring H;
H
isHomogeneous H
rank source basis( {-2,-3}, H)
rank source basis( {-3}, Ext^2(N,N) )
