-- Test of multi-homogeniety.
-- This should include: hilbert functions, gb's, syzygies, resolutions
--   basis, homogeneity (?), ...

R1 = ZZ/101[a..d,Degrees=>{{1,-1},{1,0},{2,-2},{3,4}}]
R2 = ZZ/101[a..d,MonomialOrder=>Eliminate 2,Degrees=>{{1,-1},{1,0},{2,-2},{3,4}}]
--R3 = ZZ/101[a..f,Degrees=>Multi]
-- Local Variables:
-- compile-command: "make testmulti.okay"
-- End:
