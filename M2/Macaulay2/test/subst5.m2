R = QQ[x,y,z]
f = 1/101 * x + 1/100 * y
S = ZZ/101[x,y,z]
try substitute (f, S)					    -- used to crash
-- Local Variables:
-- compile-command: "make subst5.okay "
-- End:
