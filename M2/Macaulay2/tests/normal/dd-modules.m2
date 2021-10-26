loadPackage "Dmodules"
-- Dtrace 666
R = QQ[x,y]
A = deRhamAll(x^2+y^3)
assert A.?TransferCycles
B = deRhamAll(x^2+y^2)
assert B.?TransferCycles				    -- seems to fail for homogeneous polynomials
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test d-modules.out"
-- End:
