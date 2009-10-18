R = ZZ/32003[vars(0..24)]
m1 = genericMatrix(R,R_0,5,5)
m2 = transpose genericMatrix(R,R_0,5,5)
m12 = m1 + m2
n12 = m1 - m2
p12 = m1 + m1
assert( source p12 == source m1 )
q12 = m1 - m1
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test testmat.out"
-- End:
