A = ZZ[x,y,z]
assert( 0 == 11 % ideal singularLocus Proj( A/homogenize(y^2 - y - x^3 + x^2,z) ) )
assert( 0 == 11 % ideal singularLocus Proj( A/homogenize(y^2 - y - x^3 + x^2,z) ) )
assert( 0 == 11 % ideal singularLocus Proj( A/homogenize(y^2 - y - x^3 + x^2,z) ) )
assert( 0 == 11 % ideal singularLocus Proj( A/homogenize(y^2 - y - x^3 + x^2,z) ) )
assert( 0 == 11 % ideal singularLocus Proj( A/homogenize(y^2 - y - x^3 + x^2,z) ) )
assert( 0 == 11 % ideal singularLocus Proj( A/homogenize(y^2 - y - x^3 + x^2,z) ) )
assert( 0 == 11 % ideal singularLocus Proj( A/homogenize(y^2 - y - x^3 + x^2,z) ) )

-- Boiled down, here is the current problem with this. Fixed 1/8/00 MES 
A = ZZ[x,y,z]
F = homogenize(y^2 - y - x^3 + x^2,z)
I = ideal F + minors(1,jacobian ideal F)
M = transpose vars A
B = A/I
M = M ** B
ideal syz gb(M, Syzygies=>true)

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test elliptic.out"
-- End:
