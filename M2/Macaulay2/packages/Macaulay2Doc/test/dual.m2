-- This test exists to make sure that duals of free modules are free modules, and
-- that code for Hom(--,N) doesn't slow down "dual f", which was happening.
-- Still possible problem: 
R = ZZ/5[x]
f = matrix{{x}}
g = dual f
source g
target g
assert isFreeModule source g
assert isFreeModule target g

h = map(module ideal x, target f, {{1}})
h * f
assert(dual(h*f) == dual(f) * dual(h))
N = R^1
assert(Hom(h*f, N) == Hom(f,N) * Hom(h,N))
assert(dual(h*f) == (transpose f ** N) * dual(h))

N = R^{1,2,3}
assert(Hom(h*f, N) == Hom(f,N) * Hom(h,N))

H = Hom(R^2, R^{1,1,1})
p = x * homomorphism(H_{2})
assert isHomogeneous p
assert(degree p =={0})

g1 = Hom(f, R^1)

N = R^1
transpose matrix f
Hom(source f, N) -- would prefer this to be a free module
dual source f -- this one is, since the code for dual here doesn't call Hom if source f and N are free.
Hom(target f, N)
transpose f ** N  -- this one is a map of free modules, but doesn't compose?

-- And now: test the speed
R = QQ[a..d]
time f = random(R^100, R^{100:-1});
time tf1 = dual f;
time tf2 = transpose f;
assert(tf1 == tf2)

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test dual.out"
-- End:
