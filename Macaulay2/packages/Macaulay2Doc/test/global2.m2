a=-2; b=3; i=1; j=0; n=0;
S = QQ[x..z];
X = Proj S;
m = ideal vars S;
F = sheaf_X (m^i * S^{-a});
G = sheaf_X (m^j * S^{-b}/(y,z));
HF = HH^0 F(>=0)
HG = HH^0 G(>=0)
HFG = HH^0 (F++G)(>=0)
r = rank source basis(n, HF)
s = rank source basis(n, HG)
t = rank source basis(n, HFG)
assert(r+s == t);


end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test global2.out"
-- End:
