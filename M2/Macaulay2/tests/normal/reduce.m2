f = matrix "2;0"
f%f -- this crashes, sometimes.  no more??

-- starting with M2 1.20, we  disallow division by 0.
R = QQ[x]
assert try (x // 0; false) else true
assert try (x % 0; false) else true
-- Should we disallow these? (Probably not.  Factoring through a map is somewhat different 
-- than division?).
f = matrix(R,{{x}})
g = matrix(R,{{0}})
assert( f // g == 0 )
assert( f % g == f )
