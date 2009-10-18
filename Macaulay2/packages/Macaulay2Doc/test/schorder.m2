-- Some tests of computing with Schreyer orders

R = ZZ/101[a..d]/(b*d)
I = ideal(a*b-c^2, a*b*c-c*d^2, a*d^2-c*a*d)
I = ideal(a*b-c^2, a*b*c-c*d^2)
C = res(coker gens I, Strategy=>2)
M = C.dd_2
F = target M
gbTrace = 3
M1 = syz M
M2 = C.dd_3
image M1 == image M2   -- since M1 fails above, this should fail, but
                       -- it gives true anyway!
gens gb M2 -- is zero right now...
leadTerm M
M
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test schorder.out"
-- End:
