-----------------------------------------------------------------------------

-- a problem with matrices formed from lists of vectors in a subquotient module

R = QQ[x..z]
m = ideal vars R
M = m/m^2
N = saturate 0_M					    -- low priority, dan will fix this later

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test saturate.out"
-- End:
