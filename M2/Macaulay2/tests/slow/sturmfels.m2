    -- Example 3, page 269, Mathematics of Computation 1998, Number 221, Volume 67
    -- with point (1,1,1,1,1,1,1,1,1,1) translated to the origin and homogenized

R = ZZ/7[t,x_1..x_10 ,MonomialOrder => Eliminate 1]
I = ideal(
     x_1^2 + t*x_1+t*x_2+t*x_3+t*x_4+t*x_5+t*x_6+t*x_7+t*x_8+t*x_9+t*x_10,
     x_2^2 + t*x_1+t*x_2+t*x_3+t*x_4+t*x_5+t*x_6+t*x_7+t*x_8+t*x_9+t*x_10,
     x_3^2 + t*x_1+t*x_2+t*x_3+t*x_4+t*x_5+t*x_6+t*x_7+t*x_8+t*x_9+t*x_10,
     x_4^2 + t*x_1+t*x_2+t*x_3+t*x_4+t*x_5+t*x_6+t*x_7+t*x_8+t*x_9+t*x_10,
     x_5^2 + t*x_1+t*x_2+t*x_3+t*x_4+t*x_5+t*x_6+t*x_7+t*x_8+t*x_9+t*x_10,
     x_6^2 + t*x_1+t*x_2+t*x_3+t*x_4+t*x_5+t*x_6+t*x_7+t*x_8+t*x_9+t*x_10,
     x_7^2 + t*x_1+t*x_2+t*x_3+t*x_4+t*x_5+t*x_6+t*x_7+t*x_8+t*x_9+t*x_10,
     x_8^2 + t*x_1+t*x_2+t*x_3+t*x_4+t*x_5+t*x_6+t*x_7+t*x_8+t*x_9+t*x_10,
     x_9^2 + t*x_1+t*x_2+t*x_3+t*x_4+t*x_5+t*x_6+t*x_7+t*x_8+t*x_9+t*x_10,
     x_10^2 + t*x_1+t*x_2+t*x_3+t*x_4+t*x_5+t*x_6+t*x_7+t*x_8+t*x_9+t*x_10)
time assert( 638 == degree( saturate( ideal(leadTerm gens gb I),ideal(t))) )
     -- used 232.26 seconds
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test/slow sturmfels.out"
-- End:
