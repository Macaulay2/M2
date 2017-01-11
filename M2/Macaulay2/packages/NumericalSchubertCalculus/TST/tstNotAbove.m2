load "NumericalSchubertCalculus/service-functions.m2"
l = {2,1} --in G(4,8)
a = notAboveLambda(l,4,8)
assert(#a==8)

l = {2,2} --in G(4,8)
a = notAboveLambda(l,4,8)
assert(#a==17)

end
restart
load "NumericalSchubertCalculus/TST/tstNotAbove.m2"
load "NumericalSchubertCalculus/LR-makePolynomials.m2"
print getA(4,8,l)

