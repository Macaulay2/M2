
stderr << currentFileName << ": test deferred" << endl
exit 0

R = ZZ/101[x,y]/(x^2-y^3)
I = ideal(x,y)
M = Hom(I,I)
C = res M
-- Local Variables:
-- compile-command: "make res2.okay "
-- End:
