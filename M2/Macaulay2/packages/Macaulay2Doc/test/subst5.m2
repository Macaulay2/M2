R = QQ[x,y,z]
f = 1/101 * x + 1/100 * y
S = ZZ/101[x,y,z]
try substitute (f, S)					    -- used to crash
-- Ensure substitute returns an error:
try substitute (f, S) then assert(false) else 0
-- Also try:
try substitute(1/101,ZZ/101)
try substitute(1/101,ZZ/101) then assert(false) else 0
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test subst5.out"
-- End:
