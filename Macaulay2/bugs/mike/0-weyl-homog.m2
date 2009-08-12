-- fixed.  Make into a test.

loadPackage "Dmodules"
W = makeWA(QQ[x_1..x_4])
WW = QQ[gens W, WeylAlgebra=>W.monoid.Options.WeylAlgebra,
    Degrees => {{-1,-2},{-1,-1},{-1,0},{-1,0},{1,2},{1,1},{1,0},{1,0}}]                                                                                                                                   
assert isHomogeneous WW
apply(1..4, i -> degree(x_i * dx_i))

