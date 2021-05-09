Y = Proj (QQ[x])
HH^0 ( OO_Y , Degree => 0 )
G = OO_Y
M = module G
e = 0
M = M / saturate 0_M
A = ring M;
F = presentation A
R = ring F
N = coker lift(presentation M,R) ** coker F
r = numgens R
wR = R^{-r}
E1 = Ext^(r-1)(N,wR)
degrees E1
min degrees E1
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test varieties.out"
-- End:
