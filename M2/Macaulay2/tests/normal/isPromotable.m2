debug Core
assert all({ZZ, ZZ/11, QQ, RR_53, CC_53, GF 4},
    k -> isPromotable(k, k[x,y]) and not isPromotable(k[x,y], k))
