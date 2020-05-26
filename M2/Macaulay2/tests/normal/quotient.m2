R=ZZ[x,y];
assert ( coker vars R / coker vars R == 0 )

i = ideal(6*x^3, 9*x*y, 8*y^2);
j = ideal(-3, x^2, 4*y);
j1 = ideal(-3, x^2);
j2 = ideal(4*y);
intersect(i:j1,i:j2)
a = i:j							    -- used to take forever
assert( a == ideal(8*y^2,3*x*y,6*x^3) )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test quotient.out"
-- End:
