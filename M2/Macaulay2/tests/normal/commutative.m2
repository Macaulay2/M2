assert isCommutative (ZZ)
assert isCommutative (ZZ[x])
assert isCommutative (ZZ/101)
assert isCommutative (ZZ/101[x]/x^2)
assert isCommutative (QQ)
assert isCommutative (GF(4))
assert not isCommutative (ZZ[x,SkewCommutative => true])
assert not isCommutative (ZZ[x,y,WeylAlgebra => {x => y}])
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test commutative.out"
-- End:
