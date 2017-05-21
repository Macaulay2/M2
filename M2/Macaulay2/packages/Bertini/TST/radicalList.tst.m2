needsPackage "Bertini"

l1=     radicalList({2.000,1.999})
l2=     radicalList({2.000,1.999},1e-10)         
l3=     radicalList({2.000,1.999},1e-2)         
assert(#l1==2)
assert(#l2==2)
assert(#l3==1)
