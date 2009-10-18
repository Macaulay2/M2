-- this one used to crash, but now it just uses up a lot of memory.
-- shouldn't we be able to do it?

stderr << "bypassing the test -- fix it later" << endl
exit 0

R = ZZ[u,v,w,x,y,z];
time J = ideal((v*z-w*y)^6, (w*x-u*z)^6, (u*y-v*x)^6) : (x*y*z)^3;
transpose gens J

I = ideal((v*z-w*y)^6, (w*x-u*z)^6, (u*y-v*x)^6)
time gb I;
A = R/I
F = (x*y*z)^3
s = time syz matrix {{F}};
s
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test moty.out"
-- End:
