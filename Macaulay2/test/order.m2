R = ZZ/101[s]
p = (3*s+8)*(s+2)*(s+1)*(3*s+5)*(3*s+4)*(3*s+7)
q = factor p
assert(q == factor p)
assert(q == factor p)
assert(q == factor p)
