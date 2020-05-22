R = QQ[a..d,SkewCommutative=>true]
try(I = monomialIdeal(0_R);error "monomial ideal accepted a skew commuting ring") else true

R = QQ[a..d]/(a-d)
try(I = monomialIdeal(a);error "monomial ideal accepted a quotient ring") else true
