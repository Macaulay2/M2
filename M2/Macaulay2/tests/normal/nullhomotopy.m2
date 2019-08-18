-- from our chapter on complete intersections, with QQ replaced by ZZ/101

A = ZZ/101[x,y]
f = x;
L = coker (f | matrix {{2*x},{x+y}})
C = res L
U = C.dd_1
s = nullhomotopy (-f * id_C)
V = s_0
q = -f * id_C_0
V = q // U
assert(0 == q % U)
U*V - q
assert( U*V - q == 0 )

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test nullhomotopy.out "
-- End:
