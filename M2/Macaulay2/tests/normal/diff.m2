-- check the documentation of diff(Matrix,Matrix)

R = ZZ[a..d]
m = matrix {{a,b},{c,d}}
n = matrix {{a*b*c*d, a+b+c+d}, {a^2+b^2+c^2+d^2, a^3+b^3+c^3+d^3}}
h = diff(m,n)
F = target m
P = source m
G = target n
Q = source n
f = rank F
p = rank P
g = rank G
q = rank Q

scan(f, i ->
scan(g, j ->
scan(p, k ->
scan(q, l -> (
     -- << "diff(m_(i,k),n_(j,l)) = " << diff(m_(i,k),n_(j,l)) << endl;
     -- << "h_(g*i+j,q*k+l)       = " << h_(g*i+j,q*k+l) << endl;
     assert( diff(m_(i,k),n_(j,l)) == h_(g*i+j,q*k+l) )
)))))

R = ZZ/101[x]
m = matrix{{x}}
source m

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test diff.out"
-- End:
