debug Core
needsPackage "Truncations"
R = QQ[a,b,c];
D = res ideal(a*b, a*c, b*c, a^2-b^2)
f = truncate(3, D.dd_1)
g = truncate(3, D.dd_2)
-- because of this example, we can't skip reduce
-- when the target doesn't have any relations.
-- c.f. https://github.com/Macaulay2/M2/issues/3035
r = map(target f, source g, raw f * raw g)
assert(0 == reduce(target r, raw r))
-- but we don't want reduce to be called
-- when only raw operations are used
-- c.f. https://github.com/Macaulay2/M2/issues/2898
assert(0 != r)
assert(0 != r_{3})

---

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
