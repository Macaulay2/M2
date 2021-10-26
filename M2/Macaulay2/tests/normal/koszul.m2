S = ZZ/101[x_0..x_4];
g = matrix {
     {-x_3, -x_4, 0, 0, 0}, {x_2, 0, -x_4, 0, 0}, {-x_1, 0, 0, -x_4, 0}, {x_0, 0, 0, 0, -x_4},
     {0, x_2, x_3, 0, 0}, {0, -x_1, 0, x_3, 0}, {0, x_0, 0, 0, x_3}, {0, 0, -x_1, -x_2, 0},
     {0, 0, x_0, 0, -x_2}, {0, 0, 0, x_0, x_1}};
f = koszul(4,vars S)
assert( f - g == 0 )

end

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test koszul.out"
-- End:
