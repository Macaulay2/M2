--		Copyright 1995 by Daniel R. Grayson

-- syzygy varieties, from David Eisenbud

<< "-- we need to fix this one up so it can start a resolution" << endl
<< "-- with the presentation given" << endl
exit 0

errorDepth = 1

try1 = () -> (
     R = ZZ/101[a,b,c];
     M = cokernel matrix {{ a^2, b^2, c^2 - a*b, a*c, b*c }};
     C = resolution M;
     v = C_2_{0};
     ss = syzygyScheme(C,2,v);
     )

try1()

try2 = () -> (
     R = ZZ/101[a,b];
     p = symmetricPower(6,vars R);
     S = ZZ/101[c,d,e,f,g,h,i];
     q = generators kernel map(R, S, p);
     C = resolution q;
     v = C_4_{0};
     ss = syzygyScheme(C,4,v);
     )

try2()
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test eisenbud.out"
-- End:
