errorDepth = 0

R = QQ[s]
p = (s+2)*(s+35)*(s-31)*(s+1)*(s-32)*(s+36)*(-20)
assert(same(apply(10,i -> (t := factor p; print t; t))))

R = QQ[s]
p = (s+2)*(s+35)*(s-31)*(s+1)*(2/5*s-32)*(1/3*s+36)
assert(same(apply(10,i -> (t := factor p; print t; t))))
     
R = ZZ/101[s]
p = (s+2)*(s+35)*(s-31)*(s+1)*(s-32)*(s+36)*(-20)
assert(same(apply(10,i -> (t := factor p; print t; t))))
     
R = ZZ/103[s]
p = (s+2)*(s+35)*(s-31)*(s+1)*(s-32)*(s+36)*(-20)
assert(same(apply(10,i -> (t := factor p; print t; t))))
