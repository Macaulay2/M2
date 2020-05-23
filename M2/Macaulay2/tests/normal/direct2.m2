-- Date: Tue, 17 Jun 2003 16:26:32 -0400 (EDT)
-- From: Greg Smith <ggsmith@cpw.math.columbia.edu>
-- To: Dan Grayson <dan@math.uiuc.edu>
-- Subject: M2 bug??
-- 
-- 
-- Dan:
-- 
-- I would like to index the direct sum of matrices by
-- something other than the usual consecutive small integers.
-- When I follow the construction for modules, I get the error(s)
-- listed below.  How should I work around this problem?
-- 
-- Thanks,
--   Greg.
-- 

f = matrix{{1,1},{0,1}}

-- i1 : f = matrix{{1,1},{0,1}}
-- 
-- o1 = | 1 1 |
--      | 0 1 |
-- 
--               2        2
-- o1 : Matrix ZZ  <--- ZZ
-- 

g = matrix{{1,-1},{0,1}}

-- i2 : g = matrix{{1,-1},{0,1}}
-- 
-- o2 = | 1 -1 |
--      | 0 1  |
-- 
--               2        2
-- o2 : Matrix ZZ  <--- ZZ
-- 

k = directSum(f,g)

-- i3 : directSum(f,g)
-- 
-- o3 = | 1 1 0 0  |
--      | 0 1 0 0  |
--      | 0 0 1 -1 |
--      | 0 0 0 1  |
-- 
--               4        4
-- o3 : Matrix ZZ  <--- ZZ
-- 

h = directSum({1,1} => f, {1,2} => g)

-- i4 : directSum({1,1} => f, {1,2} => g)
-- /usr/local/Macaulay2-0.9.2/lib/Macaulay2-0.9.2/m2/matrix.m2:342:29:
-- attempted to modify an immutable hash table
-- /usr/local/Macaulay2-0.9.2/lib/Macaulay2-0.9.2/m2/matrix.m2:328:20:
-- --backtrace--
-- stdio:4:1: --backtrace--
-- 

M = directSum({1,1} => ZZ^2, {1,2} => ZZ^2)

assert( h == M_[{1,1}] * f * M^[{1,1}] + M_[{1,2}] * g * M^[{1,2}] )

-- i5 : directSum({1,1} => ZZ^1, {1,2} => ZZ^2)
-- 
--        3
-- o5 = ZZ
-- 
-- o5 : ZZ-module, free
-- 
-- i6 :
-- 


end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test direct.out"
-- End:
