R = QQ[a,b,c]
f = R_{1,2,3}
S = R/(a^3+b^3+c^3)
g = R_{1,2,3}
h = S_{1,2,3}
assert( f == g )
assert( class f == R )
assert( class g == R )
assert( class h == S )
-- Local Variables:
-- compile-command: "make rings.okay "
-- End:
